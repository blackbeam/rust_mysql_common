// Copyright (c) 2020 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Binlog-related structures and functions. This implementation assumes
//! binlog version >= 4 (MySql >= 5.0.0).
//!
//! All structures of this module contains raw data that may not necessarily be valid.
//! Please consult the MySql documentation.

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use saturating::Saturating as S;

use std::{
    borrow::Cow,
    cmp::min,
    convert::TryFrom,
    fmt,
    hash::{Hash, Hasher},
    io::{
        self, Error,
        ErrorKind::{InvalidData, Other, UnexpectedEof},
        Read, Write,
    },
};

use crate::{
    constants::{ColumnType, ItemResult, UnknownColumnType},
    io::{ReadMysqlExt, WriteMysqlExt},
};

const MAX_U8: usize = std::u8::MAX as usize;
const MAX_U16: usize = std::u16::MAX as usize;
const MAX_U32: usize = std::u32::MAX as usize;

/// Depending on the MySQL Version that created the binlog the format is slightly different.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum BinlogVersion {
    /// MySQL 3.23 - < 4.0.0
    Version1,
    /// MySQL 4.0.0 - 4.0.1
    Version2,
    /// MySQL 4.0.2 - < 5.0.0
    Version3,
    /// MySQL 5.0.0+
    Version4,
}

impl TryFrom<u16> for BinlogVersion {
    type Error = u16;
    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Version1),
            2 => Ok(Self::Version2),
            3 => Ok(Self::Version3),
            4 => Ok(Self::Version4),
            x => Err(x),
        }
    }
}

/// Binlog Event Type
#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum EventType {
    /// Ignored event.
    UNKNOWN_EVENT = 0x00,
    /// A start event is the first event of a binlog for binlog-version 1 to 3.
    ///
    /// Superseded by `FORMAT_DESCRIPTION_EVENT` since mysql v5.0.0.
    START_EVENT_V3 = 0x01,
    /// A `QUERY_EVENT` is created for each query that modifies the database,
    /// unless the query is logged row-based.
    QUERY_EVENT = 0x02,
    /// A `STOP_EVENT` has no payload or post-header.
    STOP_EVENT = 0x03,
    /// The rotate event is added to the binlog as last event
    /// to tell the reader what binlog to request next.
    ROTATE_EVENT = 0x04,
    INTVAR_EVENT = 0x05,
    LOAD_EVENT = 0x06,
    /// Ignored event.
    SLAVE_EVENT = 0x07,
    CREATE_FILE_EVENT = 0x08,
    APPEND_BLOCK_EVENT = 0x09,
    EXEC_LOAD_EVENT = 0x0a,
    DELETE_FILE_EVENT = 0x0b,
    NEW_LOAD_EVENT = 0x0c,
    RAND_EVENT = 0x0d,
    USER_VAR_EVENT = 0x0e,
    ///  A format description event is the first event of a binlog for binlog-version 4. It describes how the other events are layed out.
    ///
    /// # Note
    ///
    /// Added in MySQL 5.0.0 as replacement for START_EVENT_V3
    FORMAT_DESCRIPTION_EVENT = 0x0f,
    XID_EVENT = 0x10,
    BEGIN_LOAD_QUERY_EVENT = 0x11,
    EXECUTE_LOAD_QUERY_EVENT = 0x12,
    TABLE_MAP_EVENT = 0x13,
    PRE_GA_WRITE_ROWS_EVENT = 0x14,
    PRE_GA_UPDATE_ROWS_EVENT = 0x15,
    PRE_GA_DELETE_ROWS_EVENT = 0x16,
    WRITE_ROWS_EVENT_V1 = 0x17,
    UPDATE_ROWS_EVENT_V1 = 0x18,
    DELETE_ROWS_EVENT_V1 = 0x19,
    INCIDENT_EVENT = 0x1a,
    HEARTBEAT_EVENT = 0x1b,
    IGNORABLE_EVENT = 0x1c,
    ROWS_QUERY_EVENT = 0x1d,
    WRITE_ROWS_EVENT = 0x1e,
    UPDATE_ROWS_EVENT = 0x1f,
    DELETE_ROWS_EVENT = 0x20,
    GTID_EVENT = 0x21,
    ANONYMOUS_GTID_EVENT = 0x22,
    PREVIOUS_GTIDS_EVENT = 0x23,
    TRANSACTION_CONTEXT_EVENT = 0x24,
    VIEW_CHANGE_EVENT = 0x25,
    /// Prepared XA transaction terminal event similar to Xid.
    XA_PREPARE_LOG_EVENT = 0x26,
    /// Extension of UPDATE_ROWS_EVENT, allowing partial values according
    /// to binlog_row_value_options.
    PARTIAL_UPDATE_ROWS_EVENT = 0x27,
    /// Total number of known events.
    ENUM_END_EVENT,
}

impl TryFrom<u8> for EventType {
    type Error = u8;
    fn try_from(byte: u8) -> Result<Self, u8> {
        match byte {
            0x00 => Ok(Self::UNKNOWN_EVENT),
            0x01 => Ok(Self::START_EVENT_V3),
            0x02 => Ok(Self::QUERY_EVENT),
            0x03 => Ok(Self::STOP_EVENT),
            0x04 => Ok(Self::ROTATE_EVENT),
            0x05 => Ok(Self::INTVAR_EVENT),
            0x06 => Ok(Self::LOAD_EVENT),
            0x07 => Ok(Self::SLAVE_EVENT),
            0x08 => Ok(Self::CREATE_FILE_EVENT),
            0x09 => Ok(Self::APPEND_BLOCK_EVENT),
            0x0a => Ok(Self::EXEC_LOAD_EVENT),
            0x0b => Ok(Self::DELETE_FILE_EVENT),
            0x0c => Ok(Self::NEW_LOAD_EVENT),
            0x0d => Ok(Self::RAND_EVENT),
            0x0e => Ok(Self::USER_VAR_EVENT),
            0x0f => Ok(Self::FORMAT_DESCRIPTION_EVENT),
            0x10 => Ok(Self::XID_EVENT),
            0x11 => Ok(Self::BEGIN_LOAD_QUERY_EVENT),
            0x12 => Ok(Self::EXECUTE_LOAD_QUERY_EVENT),
            0x13 => Ok(Self::TABLE_MAP_EVENT),
            0x14 => Ok(Self::PRE_GA_WRITE_ROWS_EVENT),
            0x15 => Ok(Self::PRE_GA_UPDATE_ROWS_EVENT),
            0x16 => Ok(Self::PRE_GA_DELETE_ROWS_EVENT),
            0x17 => Ok(Self::WRITE_ROWS_EVENT_V1),
            0x18 => Ok(Self::UPDATE_ROWS_EVENT_V1),
            0x19 => Ok(Self::DELETE_ROWS_EVENT_V1),
            0x1a => Ok(Self::INCIDENT_EVENT),
            0x1b => Ok(Self::HEARTBEAT_EVENT),
            0x1c => Ok(Self::IGNORABLE_EVENT),
            0x1d => Ok(Self::ROWS_QUERY_EVENT),
            0x1e => Ok(Self::WRITE_ROWS_EVENT),
            0x1f => Ok(Self::UPDATE_ROWS_EVENT),
            0x20 => Ok(Self::DELETE_ROWS_EVENT),
            0x21 => Ok(Self::GTID_EVENT),
            0x22 => Ok(Self::ANONYMOUS_GTID_EVENT),
            0x23 => Ok(Self::PREVIOUS_GTIDS_EVENT),
            x => Err(x),
        }
    }
}

bitflags! {
    /// Binlog Event Flags
    pub struct EventFlags: u16 {
        /// Gets unset in the `FORMAT_DESCRIPTION_EVENT`
        /// when the file gets closed to detect broken binlogs.
        const LOG_EVENT_BINLOG_IN_USE_F = 0x0001;

        /// Unused.
        const LOG_EVENT_FORCED_ROTATE_F = 0x0002;

        /// event is thread specific (`CREATE TEMPORARY TABLE` ...).
        const LOG_EVENT_THREAD_SPECIFIC_F = 0x0004;

        /// Event doesn't need default database to be updated (`CREATE DATABASE`, ...).
        const LOG_EVENT_SUPPRESS_USE_F = 0x0008;

        /// Unused.
        const LOG_EVENT_UPDATE_TABLE_MAP_VERSION_F = 0x0010;

        /// Event is created by the slaves SQL-thread and shouldn't update the master-log pos.
        const LOG_EVENT_ARTIFICIAL_F = 0x0020;

        /// Event is created by the slaves IO-thread when written to the relay log.
        const LOG_EVENT_RELAY_LOG_F = 0x0040;

        /// Setting this flag will mark an event as Ignorable.
        const LOG_EVENT_IGNORABLE_F = 0x0080;

        /// Events with this flag are not filtered (e.g. on the current
        /// database) and are always written to the binary log regardless of
        /// filters.
        const LOG_EVENT_NO_FILTER_F = 0x0100;

        /// MTS: group of events can be marked to force its execution in isolation from
        /// any other Workers.
        const LOG_EVENT_MTS_ISOLATE_F = 0x0200;
    }
}

struct LimitedRead<T> {
    limit: S<usize>,
    read: T,
}

impl<T> LimitedRead<T> {
    fn new(read: T, limit: S<usize>) -> Self {
        Self { read, limit }
    }

    fn limit(&self) -> usize {
        self.limit.0
    }
}

impl<T: Read> Read for LimitedRead<T> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let limit = min(buf.len(), self.limit.0);
        let count = self.read.read(&mut buf[..limit])?;
        self.limit -= S(count);
        Ok(count)
    }
}

struct LimitedWrite<T> {
    limit: S<usize>,
    write: T,
}

impl<T> LimitedWrite<T> {
    fn new(write: T, limit: S<usize>) -> Self {
        Self { write, limit }
    }

    fn nest(&mut self, limit: S<usize>) -> LimitedWrite<&mut Self> {
        LimitedWrite { write: self, limit }
    }

    fn limit(&self) -> usize {
        self.limit.0
    }
}

impl<T: Write> Write for LimitedWrite<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let limit = min(buf.len(), self.limit.0);
        let count = self.write.write(&buf[..limit])?;
        self.limit -= S(count);
        Ok(count)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.write.flush()
    }
}

/// Binlog event.
///
/// For structs that aren't binlog events `event_size` and `fde` parameters are ignored
/// (one can use `FormatDescriptionEvent::new` constructor).
pub trait BinlogStruct {
    /// An event type, associated with this struct (if any).
    const EVENT_TYPE: Option<EventType>;

    /// Will read this struct from the given stream.
    ///
    /// *   implementation must error with `UnexpectedEof` if `event_size` is less than minimum
    ///     event size for this struct,
    /// *   implementation must error with (`Other`, `"bytes remaining on stream"`) if `event_size`
    ///     is greater than the event.
    ///
    /// Requires that if `Self::EVENT_TYPE` isn't `None`, then `event_size` and `data`
    /// are both without checksum-related suffix of length:
    ///
    /// *   `BINLOG_CHECKSUM_ALG_DESC_LEN + BINLOG_CHECKSUM_LEN` for `FormatDescriptionEvent`;
    /// *   `BINLOG_CHECKSUM_LEN` for other events.
    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized;

    /// Will write this struct to the given stream.
    ///
    /// # Notes
    ///
    /// *   implementation must error with `WriteZero` if field exceeds its maximum length.
    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()>;

    /// Returns serialized length of this struct in bytes.
    ///
    /// *   implementation must truncate each field to its maximum length.
    fn len(&self, version: BinlogVersion) -> usize;
}

/// A binlog file starts with a Binlog File Header `[ fe 'bin' ]`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct BinlogFileHeader;

impl BinlogFileHeader {
    /// Length of a binlog file header.
    pub const LEN: usize = 4;
    /// Value of a binlog file header.
    pub const VALUE: [u8; Self::LEN] = [0xfe, b'b', b'i', b'n'];
}

impl BinlogStruct for BinlogFileHeader {
    const EVENT_TYPE: Option<EventType> = None;

    /// Event size and post-header length will be ignored for this struct.
    fn read<T: Read>(
        _event_size: usize,
        _fde: &FormatDescriptionEvent,
        _version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(Self::LEN));

        let mut buf = [0_u8; Self::LEN];
        input.read_exact(&mut buf)?;

        if buf != Self::VALUE {
            return Err(Error::new(InvalidData, "invalid binlog file header"));
        }

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self)
    }

    fn write<T: Write>(&self, _version: BinlogVersion, mut output: T) -> io::Result<()> {
        output.write_all(&Self::VALUE)
    }

    fn len(&self, _version: BinlogVersion) -> usize {
        Self::LEN
    }
}

/// Binlog file.
///
/// It's an iterator over events in a binlog file.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BinlogFile<T> {
    fde: FormatDescriptionEvent,
    read: T,
}

impl<T: Read> BinlogFile<T> {
    /// Creates new binlog file.
    ///
    /// It'll try to read binlog file header.
    pub fn new(version: BinlogVersion, mut read: T) -> io::Result<Self> {
        let fde = FormatDescriptionEvent::new(version);
        BinlogFileHeader::read(
            BinlogFileHeader::LEN,
            &fde,
            BinlogVersion::Version4,
            &mut read,
        )?;
        Ok(Self { fde, read })
    }
}

impl<T: Read> Iterator for BinlogFile<T> {
    type Item = io::Result<Event>;

    fn next(&mut self) -> Option<Self::Item> {
        let event = Event::read(0, &self.fde, BinlogVersion::Version4, &mut self.read);
        match event {
            Ok(event) => {
                if event.header.get_event_type() == Ok(EventType::FORMAT_DESCRIPTION_EVENT) {
                    self.fde = match event.read_event::<FormatDescriptionEvent>() {
                        Ok(mut fde) => {
                            fde.footer = event.footer;
                            fde
                        }
                        Err(err) => return Some(Err(err)),
                    }
                }
                Some(Ok(event))
            }
            Err(err) if err.kind() == UnexpectedEof => None,
            Err(err) => Some(Err(err)),
        }
    }
}

/// Parsed event data.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum EventData {
    UnknownEvent,
    /// Ignored by this implementation
    StartEventV3(Vec<u8>),
    QueryEvent(QueryEvent),
    StopEvent,
    RotateEvent(RotateEvent),
    IntvarEvent(IntvarEvent),
    LoadEvent(LoadEvent),
    SlaveEvent,
    CreateFileEvent(CreateFileEvent),
    AppendBlockEvent(AppendBlockEvent),
    ExecLoadEvent(ExecLoadEvent),
    DeleteFileEvent(DeleteFileEvent),
    NewLoadEvent(NewLoadEvent),
    RandEvent(RandEvent),
    UserVarEvent(UserVarEvent),
    FormatDescriptionEvent(FormatDescriptionEvent),
    XidEvent(XidEvent),
    BeginLoadQueryEvent(BeginLoadQueryEvent),
    ExecuteLoadQueryEvent(ExecuteLoadQueryEvent),
    TableMapEvent(TableMapEvent),
    /// Ignored by this implementation
    PreGaWriteRowsEvent(Vec<u8>),
    /// Ignored by this implementation
    PreGaUpdateRowsEvent(Vec<u8>),
    /// Ignored by this implementation
    PreGaDeleteRowsEvent(Vec<u8>),
    /// Ignored by this implementation
    WriteRowsEventV1(Vec<u8>),
    /// Ignored by this implementation
    UpdateRowsEventV1(Vec<u8>),
    /// Ignored by this implementation
    DeleteRowsEventV1(Vec<u8>),
    IncidentEvent(IncidentEvent),
    HeartbeatEvent,
    IgnorableEvent(Vec<u8>),
    RowsQueryEvent(RowsQueryEvent),
    WriteRowsEvent(WriteRowsEvent),
    UpdateRowsEvent(UpdateRowsEvent),
    DeleteRowsEvent(DeleteRowsEvent),
    /// Not yet implemented.
    GtidEvent(Vec<u8>),
    /// Not yet implemented.
    AnonymousGtidEvent(Vec<u8>),
    /// Not yet implemented.
    PreviousGtidsEvent(Vec<u8>),
    /// Not yet implemented.
    TransactionContextEvent(Vec<u8>),
    /// Not yet implemented.
    ViewChangeEvent(Vec<u8>),
    /// Not yet implemented.
    XaPrepareLogEvent(Vec<u8>),
    /// Not yet implemented.
    PartialUpdateRowsEvent(Vec<u8>),
}

impl EventData {
    /// Calls `BinlogStruct::write` for this variant.
    pub fn write<T: Write>(&self, version: BinlogVersion, mut output: T) -> io::Result<()> {
        match self {
            EventData::UnknownEvent => Ok(()),
            EventData::StartEventV3(ev) => output.write_all(&ev),
            EventData::QueryEvent(ev) => ev.write(version, output),
            EventData::StopEvent => Ok(()),
            EventData::RotateEvent(ev) => ev.write(version, output),
            EventData::IntvarEvent(ev) => ev.write(version, output),
            EventData::LoadEvent(ev) => ev.write(version, output),
            EventData::SlaveEvent => Ok(()),
            EventData::CreateFileEvent(ev) => ev.write(version, output),
            EventData::AppendBlockEvent(ev) => ev.write(version, output),
            EventData::ExecLoadEvent(ev) => ev.write(version, output),
            EventData::DeleteFileEvent(ev) => ev.write(version, output),
            EventData::NewLoadEvent(ev) => ev.write(version, output),
            EventData::RandEvent(ev) => ev.write(version, output),
            EventData::UserVarEvent(ev) => ev.write(version, output),
            EventData::FormatDescriptionEvent(ev) => ev.write(version, output),
            EventData::XidEvent(ev) => ev.write(version, output),
            EventData::BeginLoadQueryEvent(ev) => ev.write(version, output),
            EventData::ExecuteLoadQueryEvent(ev) => ev.write(version, output),
            EventData::TableMapEvent(ev) => ev.write(version, output),
            EventData::PreGaWriteRowsEvent(ev) => output.write_all(&ev),
            EventData::PreGaUpdateRowsEvent(ev) => output.write_all(&ev),
            EventData::PreGaDeleteRowsEvent(ev) => output.write_all(&ev),
            EventData::WriteRowsEventV1(ev) => output.write_all(&ev),
            EventData::UpdateRowsEventV1(ev) => output.write_all(&ev),
            EventData::DeleteRowsEventV1(ev) => output.write_all(&ev),
            EventData::IncidentEvent(ev) => ev.write(version, output),
            EventData::HeartbeatEvent => Ok(()),
            EventData::IgnorableEvent(ev) => output.write_all(&ev),
            EventData::RowsQueryEvent(ev) => ev.write(version, output),
            EventData::WriteRowsEvent(ev) => ev.write(version, output),
            EventData::UpdateRowsEvent(ev) => ev.write(version, output),
            EventData::DeleteRowsEvent(ev) => ev.write(version, output),
            EventData::GtidEvent(ev) => output.write_all(&ev),
            EventData::AnonymousGtidEvent(ev) => output.write_all(&ev),
            EventData::PreviousGtidsEvent(ev) => output.write_all(&ev),
            EventData::TransactionContextEvent(ev) => output.write_all(&ev),
            EventData::ViewChangeEvent(ev) => output.write_all(&ev),
            EventData::XaPrepareLogEvent(ev) => output.write_all(&ev),
            EventData::PartialUpdateRowsEvent(ev) => output.write_all(&ev),
        }
    }
}

/// Enumeration spcifying checksum algorithm used to encode a binary log event.
#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum BinlogChecksumAlg {
    /// Events are without checksum though its generator is checksum-capable New Master (NM).
    BINLOG_CHECKSUM_ALG_OFF = 0,
    /// CRC32 of zlib algorithm
    BINLOG_CHECKSUM_ALG_CRC32 = 1,
}

impl TryFrom<u8> for BinlogChecksumAlg {
    type Error = u8;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::BINLOG_CHECKSUM_ALG_OFF),
            1 => Ok(Self::BINLOG_CHECKSUM_ALG_CRC32),
            value => Err(value),
        }
    }
}

/// Raw binlog event.
///
/// A binlog event starts with a Binlog Event header and is followed by a Binlog Event Type
/// specific data part.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Event {
    /// Format description event.
    pub fde: FormatDescriptionEvent,
    /// Common header of an event.
    pub header: BinlogEventHeader,
    /// An event-type specific data.
    ///
    /// Checksum-related suffix is truncated:
    ///
    /// *   checksum algorithm description (for fde) will go to `footer`;
    /// *   checksum will go to `checksum`.
    pub data: Vec<u8>,
    /// Log event footer.
    pub footer: BinlogEventFooter,
    /// Event checksum.
    ///
    /// Makes sense only if checksum algorithm is defined in `footer`.
    pub checksum: [u8; BinlogEventFooter::BINLOG_CHECKSUM_LEN],
}

impl Event {
    /// Read event-type specific data as a binlog struct.
    pub fn read_event<T: BinlogStruct>(&self) -> io::Result<T> {
        BinlogStruct::read(
            // we'll use data.len() here because of truncated event footer
            BinlogEventHeader::LEN + self.data.len(),
            &self.fde,
            BinlogVersion::Version4,
            &*self.data,
        )
    }

    /// Reads event data. Returns `None` if event type is unknown.
    pub fn read_data(&self) -> io::Result<Option<EventData>> {
        use EventType::*;

        let event_type = match self.header.get_event_type() {
            Ok(event_type) => event_type,
            _ => return Ok(None),
        };

        let event_data = match event_type {
            ENUM_END_EVENT | UNKNOWN_EVENT => EventData::UnknownEvent,
            START_EVENT_V3 => EventData::StartEventV3(self.data.clone()),
            QUERY_EVENT => EventData::QueryEvent(self.read_event()?),
            STOP_EVENT => EventData::StopEvent,
            ROTATE_EVENT => EventData::RotateEvent(self.read_event()?),
            INTVAR_EVENT => EventData::IntvarEvent(self.read_event()?),
            LOAD_EVENT => EventData::LoadEvent(self.read_event()?),
            SLAVE_EVENT => EventData::SlaveEvent,
            CREATE_FILE_EVENT => EventData::CreateFileEvent(self.read_event()?),
            APPEND_BLOCK_EVENT => EventData::AppendBlockEvent(self.read_event()?),
            EXEC_LOAD_EVENT => EventData::ExecLoadEvent(self.read_event()?),
            DELETE_FILE_EVENT => EventData::DeleteFileEvent(self.read_event()?),
            NEW_LOAD_EVENT => EventData::NewLoadEvent(self.read_event()?),
            RAND_EVENT => EventData::RandEvent(self.read_event()?),
            USER_VAR_EVENT => EventData::UserVarEvent(self.read_event()?),
            FORMAT_DESCRIPTION_EVENT => {
                let mut fde: FormatDescriptionEvent = self.read_event()?;
                fde.footer = self.footer;
                EventData::FormatDescriptionEvent(fde)
            }
            XID_EVENT => EventData::XidEvent(self.read_event()?),
            BEGIN_LOAD_QUERY_EVENT => EventData::BeginLoadQueryEvent(self.read_event()?),
            EXECUTE_LOAD_QUERY_EVENT => EventData::ExecuteLoadQueryEvent(self.read_event()?),
            TABLE_MAP_EVENT => EventData::TableMapEvent(self.read_event()?),
            PRE_GA_WRITE_ROWS_EVENT => EventData::PreGaWriteRowsEvent(self.data.clone()),
            PRE_GA_UPDATE_ROWS_EVENT => EventData::PreGaUpdateRowsEvent(self.data.clone()),
            PRE_GA_DELETE_ROWS_EVENT => EventData::PreGaDeleteRowsEvent(self.data.clone()),
            WRITE_ROWS_EVENT_V1 => EventData::WriteRowsEventV1(self.data.clone()),
            UPDATE_ROWS_EVENT_V1 => EventData::UpdateRowsEventV1(self.data.clone()),
            DELETE_ROWS_EVENT_V1 => EventData::DeleteRowsEventV1(self.data.clone()),
            INCIDENT_EVENT => EventData::IncidentEvent(self.read_event()?),
            HEARTBEAT_EVENT => EventData::HeartbeatEvent,
            IGNORABLE_EVENT => EventData::IgnorableEvent(self.data.clone()),
            ROWS_QUERY_EVENT => EventData::RowsQueryEvent(self.read_event()?),
            WRITE_ROWS_EVENT => EventData::WriteRowsEvent(self.read_event()?),
            UPDATE_ROWS_EVENT => EventData::UpdateRowsEvent(self.read_event()?),
            DELETE_ROWS_EVENT => EventData::DeleteRowsEvent(self.read_event()?),
            GTID_EVENT => EventData::GtidEvent(self.data.clone()),
            ANONYMOUS_GTID_EVENT => EventData::AnonymousGtidEvent(self.data.clone()),
            PREVIOUS_GTIDS_EVENT => EventData::PreviousGtidsEvent(self.data.clone()),
            TRANSACTION_CONTEXT_EVENT => EventData::TransactionContextEvent(self.data.clone()),
            VIEW_CHANGE_EVENT => EventData::ViewChangeEvent(self.data.clone()),
            XA_PREPARE_LOG_EVENT => EventData::XaPrepareLogEvent(self.data.clone()),
            PARTIAL_UPDATE_ROWS_EVENT => EventData::PartialUpdateRowsEvent(self.data.clone()),
        };

        Ok(Some(event_data))
    }

    /// Calculates checksum for this event.
    pub fn calc_checksum(&self, alg: BinlogChecksumAlg) -> u32 {
        let is_fde = self.header.event_type == EventType::FORMAT_DESCRIPTION_EVENT as u8;

        let mut hasher = crc32fast::Hasher::new();
        let mut header = [0_u8; BinlogEventHeader::LEN];
        self.header
            .write(
                self.fde
                    .get_binlog_version()
                    .unwrap_or(BinlogVersion::Version4),
                &mut header[..],
            )
            .expect("should not fail");
        hasher.update(&header);
        hasher.update(&self.data);
        if is_fde {
            hasher.update(&[alg as u8][..]);
        }
        hasher.finalize()
    }
}

impl BinlogStruct for Event {
    const EVENT_TYPE: Option<EventType> = None;

    fn read<T: Read>(
        _event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        mut input: T,
    ) -> io::Result<Self> {
        let binlog_header_len = BinlogEventHeader::len(version);
        let mut fde = fde.clone();

        let header =
            BinlogEventHeader::read(BinlogEventHeader::len(version), &fde, version, &mut input)?;

        let mut data = vec![0_u8; (S(header.event_size as usize) - S(binlog_header_len)).0];
        input.read_exact(&mut data).unwrap();

        let is_fde = header.event_type == EventType::FORMAT_DESCRIPTION_EVENT as u8;
        let mut bytes_to_truncate = 0;
        let mut checksum = [0_u8; BinlogEventFooter::BINLOG_CHECKSUM_LEN];

        let footer = if is_fde {
            let footer = BinlogEventFooter::read(&data)?;
            if !footer.checksum_alg.is_none() {
                // truncate checksum algorithm description
                bytes_to_truncate += BinlogEventFooter::BINLOG_CHECKSUM_ALG_DESC_LEN;
            }
            // We'll update dummy fde footer
            fde.footer = footer;
            footer
        } else {
            fde.footer
        };

        // fde will always contain checksum (see WL#2540)
        let contains_checksum =
            !footer.checksum_alg.is_none() && (is_fde || footer.checksum_alg != Some(0));

        if contains_checksum {
            // truncate checksum
            bytes_to_truncate += BinlogEventFooter::BINLOG_CHECKSUM_LEN;
            checksum.copy_from_slice(&data[data.len() - BinlogEventFooter::BINLOG_CHECKSUM_LEN..]);
        }

        data.truncate(data.len() - bytes_to_truncate);

        Ok(Self {
            header,
            fde,
            data,
            footer,
            checksum,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let is_fde = self.header.event_type == EventType::FORMAT_DESCRIPTION_EVENT as u8;
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        self.header.write(version, &mut output)?;
        output.write_all(&self.data)?;

        match self.footer.get_checksum_alg() {
            Ok(Some(alg)) => {
                if is_fde {
                    output.write_u8(alg as u8)?;
                }
                if alg != BinlogChecksumAlg::BINLOG_CHECKSUM_ALG_OFF || is_fde {
                    output.write_u32::<LittleEndian>(self.calc_checksum(alg))?;
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let is_fde = self.header.event_type == EventType::FORMAT_DESCRIPTION_EVENT as u8;
        let mut len = S(0);

        len += S(BinlogEventHeader::len(version));
        len += S(self.data.len());
        match self.footer.get_checksum_alg() {
            Ok(Some(alg)) => {
                if is_fde {
                    len += S(BinlogEventFooter::BINLOG_CHECKSUM_ALG_DESC_LEN);
                }
                if is_fde || alg != BinlogChecksumAlg::BINLOG_CHECKSUM_ALG_OFF {
                    len += S(BinlogEventFooter::BINLOG_CHECKSUM_LEN);
                }
            }
            _ => (),
        }

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

/// The binlog event header starts each event and is 19 bytes long assuming binlog version >= 4.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct BinlogEventHeader {
    /// Seconds since unix epoch.
    pub timestamp: u32,
    /// Binlog Event Type.
    ///
    /// This field contains raw value. Use [`Self::get_event_type()`] to get the actual event type.
    pub event_type: u8,
    /// Server-id of the originating mysql-server.
    ///
    /// Used to filter out events in circular replication.
    pub server_id: u32,
    /// Size of the event (header, post-header, body).
    pub event_size: u32,
    /// Position of the next event.
    pub log_pos: u32,
    /// Binlog Event Flag.
    ///
    /// This field contains raw value. Use [`Self::get_flags()`] to get the actual flags.
    pub flags: u16,
}

impl BinlogEventHeader {
    /// Binlog event header length for version >= 4.
    pub const LEN: usize = 19;

    /// Returns either parsed event type, or raw value if event type is unknown.
    pub fn get_event_type(&self) -> Result<EventType, u8> {
        EventType::try_from(self.event_type)
    }

    /// Returns parsed flags. Unknown bits will be dropped.
    pub fn get_flags(&self) -> EventFlags {
        EventFlags::from_bits_truncate(self.flags)
    }

    /// Returns binlog event header length.
    pub fn len(_version: BinlogVersion) -> usize {
        Self::LEN
    }
}

impl fmt::Debug for BinlogEventHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BinlogEventHeader")
            .field("timestamp", &self.timestamp)
            .field(
                "event_type",
                &match self.get_event_type() {
                    Ok(event_type) => format!("{:?}", event_type),
                    Err(id) => format!("Unknown event type with id {}", id),
                },
            )
            .field("server_id", &self.server_id)
            .field("event_size", &self.event_size)
            .field("log_pos", &self.log_pos)
            .field(
                "flags",
                &format!(
                    "{:?} (Unknown flags: {:016b})",
                    self.get_flags(),
                    self.flags & (std::u16::MAX ^ EventFlags::all().bits())
                ),
            )
            .finish()
    }
}

impl BinlogStruct for BinlogEventHeader {
    const EVENT_TYPE: Option<EventType> = None;

    /// Event size will be ignored for this struct.
    fn read<T: Read>(
        _event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(BinlogEventHeader::len(version)));

        let timestamp = input.read_u32::<LittleEndian>()?;
        let event_type = input.read_u8()?;
        let server_id = input.read_u32::<LittleEndian>()?;
        let event_size = input.read_u32::<LittleEndian>()?;
        let log_pos = input.read_u32::<LittleEndian>()?;
        let flags = input.read_u16::<LittleEndian>()?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            timestamp,
            event_type,
            server_id,
            event_size,
            log_pos,
            flags,
        })
    }

    fn write<T: Write>(&self, _version: BinlogVersion, mut output: T) -> io::Result<()> {
        output.write_u32::<LittleEndian>(self.timestamp)?;
        output.write_u8(self.event_type)?;
        output.write_u32::<LittleEndian>(self.server_id)?;
        output.write_u32::<LittleEndian>(self.event_size)?;
        output.write_u32::<LittleEndian>(self.log_pos)?;
        output.write_u16::<LittleEndian>(self.flags)?;
        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        Self::len(version)
    }
}

/// Binlog event footer.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct BinlogEventFooter {
    /// Raw checksum algorithm description.
    pub checksum_alg: Option<u8>,
}

impl BinlogEventFooter {
    /// Length of the checksum algorithm description.
    pub const BINLOG_CHECKSUM_ALG_DESC_LEN: usize = 1;
    /// Length of the checksum.
    pub const BINLOG_CHECKSUM_LEN: usize = 4;
    /// Minimum MySql version that supports checksums.
    pub const CHECKSUM_VERSION_PRODUCT: (u8, u8, u8) = (5, 6, 1);

    /// Returns parsed checksum algorithm, or raw value if algorithm is unknown.
    pub fn get_checksum_alg(&self) -> Result<Option<BinlogChecksumAlg>, u8> {
        self.checksum_alg
            .map(BinlogChecksumAlg::try_from)
            .transpose()
    }

    /// Reads binlog event footer from the given buffer.
    ///
    /// Requires that buf contains `FormatDescriptionEvent` data.
    pub fn read(buf: &[u8]) -> io::Result<Self> {
        let checksum_alg = if buf.len()
            >= FormatDescriptionEvent::SERVER_VER_OFFSET + FormatDescriptionEvent::SERVER_VER_LEN
        {
            let mut server_version = vec![0_u8; FormatDescriptionEvent::SERVER_VER_LEN];
            (&buf[FormatDescriptionEvent::SERVER_VER_OFFSET..]).read_exact(&mut server_version)?;
            server_version[FormatDescriptionEvent::SERVER_VER_LEN - 1] = 0;
            let version = crate::misc::split_version(&server_version);
            if version < Self::CHECKSUM_VERSION_PRODUCT {
                None
            } else {
                let offset = buf.len()
                    - (BinlogEventFooter::BINLOG_CHECKSUM_ALG_DESC_LEN
                        + BinlogEventFooter::BINLOG_CHECKSUM_LEN);
                Some(buf[offset])
            }
        } else {
            None
        };

        Ok(Self { checksum_alg })
    }
}

impl Default for BinlogEventFooter {
    fn default() -> Self {
        BinlogEventFooter {
            checksum_alg: Some(BinlogChecksumAlg::BINLOG_CHECKSUM_ALG_OFF as u8),
        }
    }
}

impl fmt::Debug for BinlogEventFooter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BinlogEventFooter")
            .field(
                "checksum_alg",
                &match self.get_checksum_alg() {
                    Ok(alg) => format!("{:?}", alg),
                    Err(x) => format!("Unknown checksum algorithm {}", x),
                },
            )
            .finish()
    }
}

/// A format description event is the first event of a binlog for binlog-version 4.
///
/// It describes how the other events are layed out.
#[derive(Clone)]
pub struct FormatDescriptionEvent {
    /// Version of this binlog format.
    pub binlog_version: u16,

    /// Version of the MySQL Server that created the binlog (len=50).
    ///
    /// The string is evaluted to apply work-arounds in the slave.
    pub server_version: [u8; FormatDescriptionEvent::SERVER_VER_LEN],

    /// Seconds since Unix epoch when the binlog was created.
    pub create_timestamp: u32,

    // pub event_header_length: u8, // It's always 19, so ignored.
    /// An array indexed by Binlog Event Type - 1 to extract the length of the event specific
    /// header.
    ///
    /// Use [`Self::get_event_type_header_length`] to get header length for particular event type.
    pub event_type_header_lengths: Vec<u8>,

    /// This event structure also stores a footer containig checksum algorithm description.
    ///
    /// # Note
    ///
    /// Footer must be assigned manualy after `Self::read`
    pub footer: BinlogEventFooter,
}

impl FormatDescriptionEvent {
    /// Length of a server version string.
    pub const SERVER_VER_LEN: usize = 50;
    /// Offset of a server version string.
    pub const SERVER_VER_OFFSET: usize = 2;

    // Other format-related constants
    /// Length of a query event post-header, where 3.23, 4.x and 5.0 agree.
    pub const QUERY_HEADER_MINIMAL_LEN: usize = (4 + 4 + 1 + 2);
    /// Length of a query event post-header, where 5.0 differs: 2 for length of N-bytes vars.
    pub const QUERY_HEADER_LEN: usize = Self::QUERY_HEADER_MINIMAL_LEN + 2;
    /// Length of a stop event post-header.
    pub const STOP_HEADER_LEN: usize = 0;
    /// Length of a start event post-header.
    pub const START_V3_HEADER_LEN: usize = 2 + Self::SERVER_VER_LEN + 4;
    /// Length of a rotate event post-header.
    pub const ROTATE_HEADER_LEN: usize = 8;
    /// Length of an intvar event post-header.
    pub const INTVAR_HEADER_LEN: usize = 0;
    /// Length of an append block event post-header.
    pub const APPEND_BLOCK_HEADER_LEN: usize = 4;
    /// Length of a delete file event post-header.
    pub const DELETE_FILE_HEADER_LEN: usize = 4;
    /// Length of a rand event post-header.
    pub const RAND_HEADER_LEN: usize = 0;
    /// Length of a user var event post-header.
    pub const USER_VAR_HEADER_LEN: usize = 0;
    /// Length of a fde event post-header.
    pub const FORMAT_DESCRIPTION_HEADER_LEN: usize =
        (Self::START_V3_HEADER_LEN + EventType::ENUM_END_EVENT as usize);
    /// Length of a xid event post-header.
    pub const XID_HEADER_LEN: usize = 0;
    /// Length of a begin load query event post-header.
    pub const BEGIN_LOAD_QUERY_HEADER_LEN: usize = Self::APPEND_BLOCK_HEADER_LEN;
    /// Length of a v1 rows query event post-header.
    pub const ROWS_HEADER_LEN_V1: usize = 8;
    /// Length of a table map event post-header.
    pub const TABLE_MAP_HEADER_LEN: usize = 8;
    /// Length of an execute load query event extra header.
    pub const EXECUTE_LOAD_QUERY_EXTRA_HEADER_LEN: usize = (4 + 4 + 4 + 1);
    /// Length of an execute load query event post-header.
    pub const EXECUTE_LOAD_QUERY_HEADER_LEN: usize =
        (Self::QUERY_HEADER_LEN + Self::EXECUTE_LOAD_QUERY_EXTRA_HEADER_LEN);
    /// Length of an incident event post-header.
    pub const INCIDENT_HEADER_LEN: usize = 2;
    /// Length of a heartbeat event post-header.
    pub const HEARTBEAT_HEADER_LEN: usize = 0;
    /// Length of an ignorable event post-header.
    pub const IGNORABLE_HEADER_LEN: usize = 0;
    /// Length of a rows events post-header.
    pub const ROWS_HEADER_LEN_V2: usize = 10;
    /// Length of a gtid events post-header.
    pub const GTID_HEADER_LEN: usize = 42;
    /// Length of an incident event post-header.
    pub const TRANSACTION_CONTEXT_HEADER_LEN: usize = 18;
    /// Length of a view change event post-header.
    pub const VIEW_CHANGE_HEADER_LEN: usize = 52;
    /// Length of a xa prepare event post-header.
    pub const XA_PREPARE_HEADER_LEN: usize = 0;

    /// Creates format description event suitable for `FormatDescriptionEvent::read`.
    pub fn new(binlog_version: BinlogVersion) -> Self {
        Self {
            binlog_version: binlog_version as u16,
            server_version: [0_u8; Self::SERVER_VER_LEN],
            create_timestamp: 0,
            event_type_header_lengths: Vec::new(),
            footer: Default::default(),
        }
    }

    /// Returns either parsed binlog version, or raw value if version is unknown.
    pub fn get_binlog_version(&self) -> Result<BinlogVersion, u16> {
        BinlogVersion::try_from(self.binlog_version)
    }

    /// Returns the `mysql_server_version` field value as a string.
    pub fn get_server_version(&self) -> Cow<str> {
        let null_pos = self
            .server_version
            .iter()
            .position(|x| *x == 0)
            .unwrap_or(self.server_version.len());
        String::from_utf8_lossy(&self.server_version[..null_pos])
    }

    /// Returns header length for the given event type, if defined.
    pub fn get_event_type_header_length(&self, event_type: EventType) -> u8 {
        if event_type == EventType::UNKNOWN_EVENT {
            return 0;
        }

        self.event_type_header_lengths
            .get(usize::from(event_type as u8).saturating_sub(1))
            .copied()
            .unwrap_or_else(|| match event_type {
                EventType::UNKNOWN_EVENT => 0,
                EventType::START_EVENT_V3 => Self::START_V3_HEADER_LEN,
                EventType::QUERY_EVENT => Self::QUERY_HEADER_LEN,
                EventType::STOP_EVENT => Self::STOP_HEADER_LEN,
                EventType::ROTATE_EVENT => Self::ROTATE_HEADER_LEN,
                EventType::INTVAR_EVENT => Self::INTVAR_HEADER_LEN,
                EventType::LOAD_EVENT => 0,
                EventType::SLAVE_EVENT => 0,
                EventType::CREATE_FILE_EVENT => 0,
                EventType::APPEND_BLOCK_EVENT => Self::APPEND_BLOCK_HEADER_LEN,
                EventType::EXEC_LOAD_EVENT => 0,
                EventType::DELETE_FILE_EVENT => Self::DELETE_FILE_HEADER_LEN,
                EventType::NEW_LOAD_EVENT => 0,
                EventType::RAND_EVENT => Self::RAND_HEADER_LEN,
                EventType::USER_VAR_EVENT => Self::USER_VAR_HEADER_LEN,
                EventType::FORMAT_DESCRIPTION_EVENT => Self::FORMAT_DESCRIPTION_HEADER_LEN,
                EventType::XID_EVENT => Self::XID_HEADER_LEN,
                EventType::BEGIN_LOAD_QUERY_EVENT => Self::BEGIN_LOAD_QUERY_HEADER_LEN,
                EventType::EXECUTE_LOAD_QUERY_EVENT => Self::EXECUTE_LOAD_QUERY_HEADER_LEN,
                EventType::TABLE_MAP_EVENT => Self::TABLE_MAP_HEADER_LEN,
                EventType::PRE_GA_WRITE_ROWS_EVENT => 0,
                EventType::PRE_GA_UPDATE_ROWS_EVENT => 0,
                EventType::PRE_GA_DELETE_ROWS_EVENT => 0,
                EventType::WRITE_ROWS_EVENT_V1 => Self::ROWS_HEADER_LEN_V1,
                EventType::UPDATE_ROWS_EVENT_V1 => Self::ROWS_HEADER_LEN_V1,
                EventType::DELETE_ROWS_EVENT_V1 => Self::ROWS_HEADER_LEN_V1,
                EventType::INCIDENT_EVENT => Self::INCIDENT_HEADER_LEN,
                EventType::HEARTBEAT_EVENT => 0,
                EventType::IGNORABLE_EVENT => Self::IGNORABLE_HEADER_LEN,
                EventType::ROWS_QUERY_EVENT => Self::IGNORABLE_HEADER_LEN,
                EventType::WRITE_ROWS_EVENT => Self::ROWS_HEADER_LEN_V2,
                EventType::UPDATE_ROWS_EVENT => Self::ROWS_HEADER_LEN_V2,
                EventType::DELETE_ROWS_EVENT => Self::ROWS_HEADER_LEN_V2,
                EventType::GTID_EVENT => Self::GTID_HEADER_LEN,
                EventType::ANONYMOUS_GTID_EVENT => Self::GTID_HEADER_LEN,
                EventType::PREVIOUS_GTIDS_EVENT => Self::IGNORABLE_HEADER_LEN,
                EventType::TRANSACTION_CONTEXT_EVENT => Self::TRANSACTION_CONTEXT_HEADER_LEN,
                EventType::VIEW_CHANGE_EVENT => Self::VIEW_CHANGE_HEADER_LEN,
                EventType::XA_PREPARE_LOG_EVENT => Self::XA_PREPARE_HEADER_LEN,
                EventType::PARTIAL_UPDATE_ROWS_EVENT => Self::ROWS_HEADER_LEN_V2,
                EventType::ENUM_END_EVENT => 0,
            } as u8)
    }
}

impl fmt::Debug for FormatDescriptionEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FormatDescriptionEvent")
            .field("binlog_version", &self.binlog_version)
            .field("mysql_server_version", &self.get_server_version())
            .field("create_timestamp", &self.create_timestamp)
            .field("event_type_header_lengths", &self.event_type_header_lengths)
            .field("footer", &self.footer)
            .finish()
    }
}

impl PartialEq for FormatDescriptionEvent {
    fn eq(&self, other: &Self) -> bool {
        self.binlog_version == other.binlog_version
            && &self.server_version[..] == &other.server_version[..]
            && self.create_timestamp == other.create_timestamp
            && self.event_type_header_lengths == other.event_type_header_lengths
    }
}

impl Eq for FormatDescriptionEvent {}

impl Hash for FormatDescriptionEvent {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.binlog_version.hash(state);
        (&self.server_version[..]).hash(state);
        self.create_timestamp.hash(state);
        self.event_type_header_lengths.hash(state);
    }
}

impl BinlogStruct for FormatDescriptionEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::FORMAT_DESCRIPTION_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        _version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::LEN));

        let binlog_version = input.read_u16::<LittleEndian>()?;

        let mut server_version = [0_u8; Self::SERVER_VER_LEN];
        input.read_exact(&mut server_version[..])?;

        let create_timestamp = input.read_u32::<LittleEndian>()?;

        input.read_u8()?; // skip event_header_length

        let mut event_type_header_lengths = vec![0_u8; input.limit()];
        input.read_exact(&mut event_type_header_lengths)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            binlog_version,
            server_version,
            create_timestamp,
            event_type_header_lengths,
            footer: Default::default(),
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u16::<LittleEndian>(self.binlog_version)?;
        output.write_all(&self.server_version)?;
        output.write_u32::<LittleEndian>(self.create_timestamp)?;
        output.write_u8(BinlogEventHeader::LEN as u8)?;
        output.write_all(&self.event_type_header_lengths)?;

        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(2);
        len += S(Self::SERVER_VER_LEN);
        len += S(4);
        len += S(1);
        len += S(self.event_type_header_lengths.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

/// The rotate event is added to the binlog as last event
/// to tell the reader what binlog to request next.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct RotateEvent {
    // post-header
    /// Only available if binlog version > 1 (zero otherwise).
    pub position: u64,

    // payload
    /// Name of the next binlog.
    pub name: Vec<u8>,
}

impl RotateEvent {
    /// Returns the `name` field value as a string.
    pub fn get_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.name[..])
    }
}

impl BinlogStruct for RotateEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::ROTATE_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let position = input.read_u64::<LittleEndian>()?;

        let mut name = vec![0_u8; input.limit()];
        input.read_exact(&mut name)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self { position, name })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u64::<LittleEndian>(self.position)?;
        output.write_all(&self.name)?;

        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(8);
        len += S(self.name.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

impl fmt::Debug for RotateEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RotateEvent")
            .field("position", &self.position)
            .field("name", &self.get_name())
            .finish()
    }
}

/// A query event is created for each query that modifies the database, unless the query
/// is logged row-based.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct QueryEvent {
    // post-header fields
    /// The ID of the thread that issued this statement. It is needed for temporary tables.
    pub thread_id: u32,
    /// The time from when the query started to when it was logged in the binlog, in seconds.
    pub execution_time: u32,
    /// Error code generated by the master. If the master fails, the slave will fail with
    /// the same error code.
    pub error_code: u16,

    // payload
    /// Zero or more status variables (`status_vars_length` bytes).
    ///
    /// Each status variable consists of one byte identifying the variable stored, followed
    /// by the value of the variable. Please consult the MySql documentation.
    ///
    /// Only available if binlog version >= 4 (empty otherwise).
    pub status_vars: Vec<u8>,
    /// The currently selected database name (`schema-length` bytes).
    pub schema: Vec<u8>,
    /// The SQL query.
    pub query: Vec<u8>,
}

impl QueryEvent {
    pub fn status_vars_iter(&self) -> StatusVarsIterator {
        StatusVarsIterator::new(&self.status_vars[..])
    }

    /// Returns raw value of a status var by its key.
    pub fn get_status_var(&self, needle: StatusVarKey) -> Option<&[u8]> {
        self.status_vars_iter()
            .find_map(|(key, val)| if key == needle { Some(val) } else { None })
    }

    /// Returns the `schema` field value as a string.
    pub fn get_schema(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.schema[..])
    }

    /// Returns the `query` field value as a string.
    pub fn get_query(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.query[..])
    }
}

impl BinlogStruct for QueryEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::QUERY_EVENT);

    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let post_header_len = fde.get_event_type_header_length(Self::EVENT_TYPE.unwrap());

        let thread_id = input.read_u32::<LittleEndian>()?;
        let execution_time = input.read_u32::<LittleEndian>()?;
        let schema_len = input.read_u8()? as usize;
        let error_code = input.read_u16::<LittleEndian>()?;

        let status_vars_len = input.read_u16::<LittleEndian>()? as usize;

        for _ in 0..(post_header_len.saturating_sub(4 + 4 + 1 + 2 + 2)) {
            input.read_u8()?;
        }

        let mut status_vars = vec![0_u8; status_vars_len];
        input.read_exact(&mut status_vars)?;

        let mut schema = vec![0_u8; schema_len];
        input.read_exact(&mut schema)?;

        input.read_u8()?;

        let mut query = vec![0_u8; input.limit()];
        input.read_exact(&mut query)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            thread_id,
            execution_time,
            error_code,
            status_vars,
            schema,
            query,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.thread_id)?;
        output.write_u32::<LittleEndian>(self.execution_time)?;
        output.write_u8(min(self.schema.len(), MAX_U8) as u8)?;
        output.write_u16::<LittleEndian>(self.error_code)?;
        output.write_u16::<LittleEndian>(min(self.status_vars.len(), MAX_U16) as u16)?;
        output.nest(S(MAX_U16)).write_all(&self.status_vars)?;
        output.nest(S(MAX_U8)).write_all(&self.schema)?;
        output.write_u8(0)?;
        output.write_all(&self.query)?;

        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(4);
        len += S(4);
        len += S(1);
        len += S(2);
        len += S(2);
        len += S(min(self.status_vars.len(), MAX_U16));
        len += S(min(self.schema.len(), MAX_U8));
        len += S(1);
        len += S(self.query.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

impl fmt::Debug for QueryEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("QueryEvent")
            .field("thread_id", &self.thread_id)
            .field("execution_time", &self.execution_time)
            .field("error_code", &self.error_code)
            .field("status_vars", &self.status_vars_iter())
            .field("schema", &self.get_schema())
            .field("query", &self.get_query())
            .finish()
    }
}

/// Binlog query event status vars keys.
#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum StatusVarKey {
    /// Contains `Flags2` flags.
    Flags2 = 0,
    /// Contains `SqlMode` flags.
    SqlMode,
    /// Contains values in the following order:
    ///
    /// *   1 byte `length`,
    /// *   `length` bytes catalog,
    /// *   NULL byte.
    ///
    /// `length + 2` bytes in total.
    Catalog,
    /// Contains values in the following order:
    ///
    /// *   2 bytes unsigned little-endian auto_increment_increment,
    /// *   2 bytes unsigned little-endian auto_increment_offset.
    ///
    /// Four bytes in total.
    AutoIncrement,
    /// Contains values in the following order:
    ///
    /// *   2 bytes unsigned little-endian character_set_client,
    /// *   2 bytes unsigned little-endian collation_connection,
    /// *   2 bytes unsigned little-endian collation_server.
    ///
    /// Six bytes in total.
    Charset,
    /// Contains values in the following order:
    ///
    /// *   1 byte `length`,
    /// *   `length` bytes timezone.
    ///
    /// `length + 1` bytes in total.
    TimeZone,
    /// Contains values in the following order:
    ///
    /// *   1 byte `length`,
    /// *   `length` bytes catalog.
    ///
    /// `length + 1` bytes in total.
    CatalogNz,
    /// Contains 2 bytes code identifying a table of month and day names.
    ///
    /// The mapping from codes to languages is defined in sql_locale.cc.
    LcTimeNames,
    /// Contains 2 bytes value of the collation_database system variable.
    CharsetDatabase,
    /// Contains 8 bytes value of the table map that is to be updated
    /// by the multi-table update query statement.
    TableMapForUpdate,
    /// Contains 4 bytes bitfield.
    MasterDataWritten,
    /// Contains values in the following order:
    ///
    /// *   1 byte `user_length`,
    /// *   `user_length` bytes user,
    /// *   1 byte `host_length`,
    /// *   `host_length` bytes host.
    ///
    /// `user_length + host_length + 2` bytes in total.
    Invoker,
    /// Contains values in the following order:
    ///
    /// *   1 byte `count`,
    /// *   `count` times:
    ///     *   null-terminated db_name.
    ///
    /// `1 + db_names_lens.sum()` bytes in total.
    UpdatedDbNames,
    /// Contains 3 bytes unsigned little-endian integer.
    Microseconds,
    CommitTs,
    CommitTs2,
    /// Contains 1 byte boolean.
    ExplicitDefaultsForTimestamp,
    /// Contains 8 bytes unsigned little-endian integer carrying xid info of 2pc-aware
    /// (recoverable) DDL queries.
    DdlLoggedWithXid,
    /// Contains 2 bytes unsigned little-endian integer carrying
    /// the default collation for the utf8mb4 character set.
    DefaultCollationForUtf8mb4,
    /// Contains 1 byte value.
    SqlRequirePrimaryKey,
    /// Contains 1 byte value.
    DefaultTableEncryption,
}

impl TryFrom<u8> for StatusVarKey {
    type Error = u8;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(StatusVarKey::Flags2),
            1 => Ok(StatusVarKey::SqlMode),
            2 => Ok(StatusVarKey::Catalog),
            3 => Ok(StatusVarKey::AutoIncrement),
            4 => Ok(StatusVarKey::Charset),
            5 => Ok(StatusVarKey::TimeZone),
            6 => Ok(StatusVarKey::CatalogNz),
            7 => Ok(StatusVarKey::LcTimeNames),
            8 => Ok(StatusVarKey::CharsetDatabase),
            9 => Ok(StatusVarKey::TableMapForUpdate),
            10 => Ok(StatusVarKey::MasterDataWritten),
            11 => Ok(StatusVarKey::Invoker),
            12 => Ok(StatusVarKey::UpdatedDbNames),
            13 => Ok(StatusVarKey::Microseconds),
            14 => Ok(StatusVarKey::CommitTs),
            15 => Ok(StatusVarKey::CommitTs2),
            16 => Ok(StatusVarKey::ExplicitDefaultsForTimestamp),
            17 => Ok(StatusVarKey::DdlLoggedWithXid),
            18 => Ok(StatusVarKey::DefaultCollationForUtf8mb4),
            19 => Ok(StatusVarKey::SqlRequirePrimaryKey),
            20 => Ok(StatusVarKey::DefaultTableEncryption),
            x => Err(x),
        }
    }
}

/// Iterator over status vars of a `QueryEvent`.
///
/// It will stop iteration if vars can't be parsed.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct StatusVarsIterator<'a> {
    pos: usize,
    status_vars: &'a [u8],
}

impl<'a> StatusVarsIterator<'a> {
    /// Creates new instance.
    pub fn new(status_vars: &'a [u8]) -> StatusVarsIterator<'a> {
        Self {
            pos: 0,
            status_vars,
        }
    }
}

impl fmt::Debug for StatusVarsIterator<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<'a> Iterator for StatusVarsIterator<'a> {
    type Item = (StatusVarKey, &'a [u8]);

    fn next(&mut self) -> Option<Self::Item> {
        let key = *self.status_vars.get(self.pos)?;
        let key = StatusVarKey::try_from(key).ok()?;
        self.pos += 1;

        macro_rules! get_fixed {
            ($len:expr) => {{
                self.pos += $len;
                self.status_vars.get((self.pos - $len)..self.pos)?
            }};
        }

        macro_rules! get_var {
            ($suffix_len:expr) => {{
                let len = *self.status_vars.get(self.pos)? as usize;
                get_fixed!(1 + len + $suffix_len)
            }};
        }

        let val = match key {
            StatusVarKey::Flags2 => get_fixed!(4),
            StatusVarKey::SqlMode => get_fixed!(8),
            StatusVarKey::Catalog => get_var!(1),
            StatusVarKey::AutoIncrement => get_fixed!(4),
            StatusVarKey::Charset => get_fixed!(6),
            StatusVarKey::TimeZone => get_var!(0),
            StatusVarKey::CatalogNz => get_var!(0),
            StatusVarKey::LcTimeNames => get_fixed!(2),
            StatusVarKey::CharsetDatabase => get_fixed!(2),
            StatusVarKey::TableMapForUpdate => get_fixed!(8),
            StatusVarKey::MasterDataWritten => get_fixed!(4),
            StatusVarKey::Invoker => {
                let user_len = *self.status_vars.get(self.pos)? as usize;
                let host_len = *self.status_vars.get(self.pos + 1 + user_len)? as usize;
                get_fixed!(1 + user_len + 1 + host_len)
            }
            StatusVarKey::UpdatedDbNames => {
                let mut total = 1;
                let count = *self.status_vars.get(self.pos)? as usize;
                for _ in 0..count {
                    while *self.status_vars.get(self.pos + total)? != 0x00 {
                        total += 1;
                    }
                    total += 1;
                }
                get_fixed!(total)
            }
            StatusVarKey::Microseconds => get_fixed!(3),
            StatusVarKey::CommitTs => get_fixed!(0),
            StatusVarKey::CommitTs2 => get_fixed!(0),
            StatusVarKey::ExplicitDefaultsForTimestamp => get_fixed!(1),
            StatusVarKey::DdlLoggedWithXid => get_fixed!(8),
            StatusVarKey::DefaultCollationForUtf8mb4 => get_fixed!(2),
            StatusVarKey::SqlRequirePrimaryKey => get_fixed!(1),
            StatusVarKey::DefaultTableEncryption => get_fixed!(1),
        };

        Some((key, val))
    }
}

bitflags! {
    /// Empty flags of a `LoadEvent`.
    pub struct EmptyFlags: u8 {
        const FIELD_TERM_EMPTY = 0x01;
        const ENCLOSED_EMPTY   = 0x02;
        const LINE_TERM_EMPTY  = 0x04;
        const LINE_START_EMPTY = 0x08;
        const ESCAPE_EMPTY     = 0x10;
    }
}

bitflags! {
    /// Opt flags of a `LoadEvent`.
    pub struct OptFlags: u8 {
        const DUMPFILE_FLAG     = 0x01;
        const OPT_ENCLOSED_FLAG = 0x02;
        const REPLACE_FLAG      = 0x04;
        const IGNORE_FLAG       = 0x08;
    }
}

/// Load event.
///
/// Used for LOAD DATA INFILE statements from MySQL 3.23 to 4.0.0.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct LoadEvent {
    pub thread_id: u32,
    pub exec_time: u32,
    pub skip_lines: u32,
    pub field_term: u8,
    pub enclosed_by: u8,
    pub line_term: u8,
    pub line_start: u8,
    pub escaped_by: u8,
    pub opt_flags: u8,
    pub empty_flags: u8,
    pub field_names: Vec<Vec<u8>>,
    pub table_name: Vec<u8>,
    pub schema_name: Vec<u8>,
    pub file_name: Vec<u8>,
}

impl fmt::Debug for LoadEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LoadEvent")
            .field("thread_id", &self.thread_id)
            .field("exec_time", &self.exec_time)
            .field("skip_lines", &self.skip_lines)
            .field("field_term", &self.get_field_term())
            .field("enclosed_by", &self.get_enclosed_by())
            .field("line_term", &self.get_line_term())
            .field("line_start", &self.get_line_start())
            .field("escaped_by", &self.get_escaped_by())
            .field("opt_flags", &{
                let flags = self.opt_flags;
                format!(
                    "{:?} (Unknown flags: {:08b}",
                    flags,
                    self.opt_flags & (std::u8::MAX ^ OptFlags::all().bits())
                )
            })
            .field("empty_flags", &{
                let flags = self.empty_flags;
                format!(
                    "{:?} (Unknown flags: {:08b}",
                    flags,
                    self.empty_flags & (std::u8::MAX ^ EmptyFlags::all().bits())
                )
            })
            .field(
                "field_names",
                &self
                    .field_names
                    .iter()
                    .map(|name| String::from_utf8_lossy(name))
                    .collect::<Vec<_>>(),
            )
            .field("table_name", &self.get_table_name())
            .field("schema_name", &self.get_schema_name())
            .field("file_name", &self.get_file_name())
            .finish()
    }
}

impl BinlogStruct for LoadEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::LOAD_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let thread_id = input.read_u32::<LittleEndian>()?;
        let exec_time = input.read_u32::<LittleEndian>()?;
        let skip_lines = input.read_u32::<LittleEndian>()?;
        let table_name_len = input.read_u8()? as usize;
        let schema_len = input.read_u8()? as usize;
        let num_fields = input.read_u32::<LittleEndian>()? as usize;

        let field_term = input.read_u8()?;
        let enclosed_by = input.read_u8()?;
        let line_term = input.read_u8()?;
        let line_start = input.read_u8()?;
        let escaped_by = input.read_u8()?;
        let opt_flags = input.read_u8()?;
        let empty_flags = input.read_u8()?;

        let mut field_name_lengths = vec![0_u8; num_fields];
        input.read_exact(&mut field_name_lengths[..])?;

        let field_names = {
            let mut field_names = Vec::with_capacity(num_fields);

            for i in 0..num_fields {
                let len = field_name_lengths[i] as usize;
                let mut field_name = vec![0_u8; len];
                input.read_exact(&mut field_name)?;
                input.read_u8()?; // skip null
                field_names.push(field_name);
            }

            field_names
        };

        let mut table_name = vec![0_u8; table_name_len];
        input.read_exact(&mut table_name)?;
        input.read_u8()?; // skip null

        let mut schema_name = vec![0_u8; schema_len];
        input.read_exact(&mut schema_name)?;
        input.read_u8()?; // skip null

        let mut file_name = vec![0_u8; input.limit()];
        input.read_exact(&mut file_name)?;
        input.read_u8()?; // skip null

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            thread_id,
            exec_time,
            skip_lines,
            field_term,
            enclosed_by,
            line_term,
            line_start,
            escaped_by,
            opt_flags,
            empty_flags,
            field_names,
            table_name,
            schema_name,
            file_name,
        })
    }

    /// Writes a [`LOAD_EVENT`] to the given stream.
    ///
    /// Will write `self.len()` bytes.
    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.thread_id)?;
        output.write_u32::<LittleEndian>(self.exec_time)?;
        output.write_u32::<LittleEndian>(self.skip_lines)?;
        output.write_u8(min(self.table_name.len(), MAX_U8) as u8)?;
        output.write_u8(min(self.schema_name.len(), MAX_U8) as u8)?;
        output.write_u32::<LittleEndian>(self.field_names.len() as u32)?;

        output.write_u8(self.field_term)?;
        output.write_u8(self.enclosed_by)?;
        output.write_u8(self.line_term)?;
        output.write_u8(self.line_start)?;
        output.write_u8(self.escaped_by)?;
        output.write_u8(self.opt_flags)?;
        output.write_u8(self.empty_flags)?;

        for field_name in self.field_names.iter() {
            output.write_u8(min(field_name.len(), MAX_U8) as u8)?;
        }

        for field_name in self.field_names.iter() {
            output.nest(S(MAX_U8)).write_all(&field_name)?;
            output.write_u8(0)?;
        }

        output.nest(S(MAX_U8)).write_all(&self.table_name)?;
        output.write_u8(0)?;

        output.nest(S(MAX_U8)).write_all(&self.schema_name)?;
        output.write_u8(0)?;

        output.write_all(&self.file_name)?;
        output.write_u8(0)?;

        Ok(())
    }

    /// Returns length of this load event in bytes.
    fn len(&self, version: BinlogVersion) -> usize {
        let table_name_len = min(self.table_name.len(), MAX_U8);
        let schema_len = min(self.schema_name.len(), MAX_U8);
        let num_fields = min(self.field_names.len(), MAX_U32);

        let mut len = S(0);

        len += S(4);
        len += S(4);
        len += S(4);
        len += S(1);
        len += S(1);
        len += S(4);

        len += S(1);
        len += S(1);
        len += S(1);
        len += S(1);
        len += S(1);
        len += S(1);
        len += S(1);

        len += S(num_fields);
        len += S(self
            .field_names
            .iter()
            .take(num_fields)
            .map(Vec::len)
            .map(|len| min(len, MAX_U8))
            .sum::<usize>());
        len += S(num_fields);
        len += S(table_name_len);
        len += S(1);
        len += S(schema_len);
        len += S(1);
        len += S(self.file_name.len());
        len += S(1);

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

impl LoadEvent {
    /// Returns a he `field_term` field as a character.
    pub fn get_field_term(&self) -> char {
        char::from(self.field_term)
    }

    /// Returns a e `enclosed_by` field as a character.
    pub fn get_enclosed_by(&self) -> char {
        char::from(self.enclosed_by)
    }

    /// Returns the `line_term` field as a character.
    pub fn get_line_term(&self) -> char {
        char::from(self.line_term)
    }

    /// Returns a he `line_start` field as a character.
    pub fn get_line_start(&self) -> char {
        char::from(self.line_start)
    }

    /// Returns a he `escaped_by` field as a character.
    pub fn get_escaped_by(&self) -> char {
        char::from(self.escaped_by)
    }

    /// Returns the given field name as a string.
    pub fn get_field_name(&self, index: usize) -> Option<Cow<str>> {
        self.field_names
            .get(index)
            .map(|field_name| String::from_utf8_lossy(&field_name[..]))
    }

    /// Returns the `table_name` field value as a string.
    pub fn get_table_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.table_name[..])
    }

    /// Returns the `schema_name` field value as a string.
    pub fn get_schema_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.schema_name[..])
    }

    /// Returns the `file_name` field value as a string.
    pub fn get_file_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.file_name[..])
    }

    /// Returns parsed opt flags of this load event. Unknown bits will be dropped.
    pub fn get_opt_flags(&self) -> OptFlags {
        OptFlags::from_bits_truncate(self.opt_flags)
    }

    /// Returns parsed empty flags of this load event. Unknown bits will be dropped.
    pub fn get_empty_flags(&self) -> EmptyFlags {
        EmptyFlags::from_bits_truncate(self.empty_flags)
    }
}

/// New load event.
///
/// Used for LOAD DATA INFILE statements from MySql 4.0.0 to 5.0.3.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct NewLoadEvent {
    pub thread_id: u32,
    pub exec_time: u32,
    pub skip_lines: u32,

    pub field_term: Vec<u8>,
    pub enclosed_by: Vec<u8>,
    pub line_term: Vec<u8>,
    pub line_start: Vec<u8>,
    pub escaped_by: Vec<u8>,
    pub opt_flags: u8,

    pub field_names: Vec<Vec<u8>>,
    pub table_name: Vec<u8>,
    pub schema_name: Vec<u8>,
    pub file_name: Vec<u8>,
}

impl fmt::Debug for NewLoadEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NewLoadEvent")
            .field("thread_id", &self.thread_id)
            .field("exec_time", &self.exec_time)
            .field("skip_lines", &self.skip_lines)
            .field("field_term", &self.get_field_term())
            .field("enclosed_by", &self.get_enclosed_by())
            .field("line_term", &self.get_line_term())
            .field("line_start", &self.get_line_start())
            .field("escaped_by", &self.get_escaped_by())
            .field("opt_flags", &{
                let flags = self.opt_flags;
                format!(
                    "{:?} (Unknown flags: {:08b}",
                    flags,
                    self.opt_flags & (std::u8::MAX ^ OptFlags::all().bits())
                )
            })
            .field(
                "field_names",
                &self
                    .field_names
                    .iter()
                    .map(|name| String::from_utf8_lossy(name))
                    .collect::<Vec<_>>(),
            )
            .field("table_name", &self.get_table_name())
            .field("schema_name", &self.get_schema_name())
            .field("file_name", &self.get_file_name())
            .finish()
    }
}

impl BinlogStruct for NewLoadEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::NEW_LOAD_EVENT);

    /// Reads a [`NEW_LOAD_EVENT`] from the given stream.
    ///
    /// Will read `event_size - BinlogEventHeader::LEN` bytes.
    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let slave_proxy_id = input.read_u32::<LittleEndian>()?;
        let exec_time = input.read_u32::<LittleEndian>()?;
        let skip_lines = input.read_u32::<LittleEndian>()?;
        let table_name_len = input.read_u8()? as usize;
        let schema_len = input.read_u8()? as usize;
        let num_fields = input.read_u32::<LittleEndian>()? as usize;

        macro_rules! len_enc {
            () => {{
                let len = input.read_u8()? as usize;
                len_enc!(len)
            }};
            ($len:expr) => {{
                let mut val = vec![0_u8; $len as usize];
                input.read_exact(&mut val[..])?;
                val
            }};
        }

        let field_term = len_enc!();
        let enclosed_by = len_enc!();
        let line_term = len_enc!();
        let line_start = len_enc!();
        let escaped_by = len_enc!();

        let opt_flags = input.read_u8()?;

        let field_name_lengths = len_enc!(num_fields);

        let field_names = {
            let mut field_names = Vec::with_capacity(num_fields);

            for i in 0..num_fields {
                let field_name = len_enc!(field_name_lengths[i]);
                input.read_u8()?; // skip null
                field_names.push(field_name);
            }

            field_names
        };

        let table_name = len_enc!(table_name_len);
        input.read_u8()?; // skip null

        let schema_name = len_enc!(schema_len);
        input.read_u8()?; // skip null

        let file_name = len_enc!(input.limit());

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            thread_id: slave_proxy_id,
            exec_time,
            skip_lines,
            field_term,
            enclosed_by,
            line_term,
            line_start,
            escaped_by,
            opt_flags,
            field_names,
            table_name,
            schema_name,
            file_name,
        })
    }

    /// Writes a [`NEW_LOAD_EVENT`] to the given stream.
    ///
    /// Will write `self.len()` bytes.
    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.thread_id)?;
        output.write_u32::<LittleEndian>(self.exec_time)?;
        output.write_u32::<LittleEndian>(self.skip_lines)?;
        output.write_u8(min(self.table_name.len(), MAX_U8) as u8)?;
        output.write_u8(min(self.schema_name.len(), MAX_U8) as u8)?;
        output.write_u32::<LittleEndian>(self.field_names.len() as u32)?;

        macro_rules! len_enc {
            ($field:expr) => {{
                output.write_u8(min($field.len(), MAX_U8) as u8)?;
                output.nest(S(MAX_U8)).write_all(&$field)?;
            }};
        }

        len_enc!(self.field_term);
        len_enc!(self.enclosed_by);
        len_enc!(self.line_term);
        len_enc!(self.line_start);
        len_enc!(self.escaped_by);

        output.write_u8(self.opt_flags)?;

        for field_name in self.field_names.iter() {
            output.write_u8(min(field_name.len(), MAX_U8) as u8)?;
        }

        for field_name in self.field_names.iter() {
            output.nest(S(MAX_U8)).write_all(&field_name)?;
            output.write_u8(0)?;
        }

        output.nest(S(MAX_U8)).write_all(&self.table_name)?;
        output.write_u8(0)?;

        output.nest(S(MAX_U8)).write_all(&self.schema_name)?;
        output.write_u8(0)?;

        output.write_all(&self.file_name)?;

        Ok(())
    }

    /// Returns length of this load event in bytes.
    fn len(&self, version: BinlogVersion) -> usize {
        let num_fields = min(self.field_names.len(), MAX_U32);

        let mut len = S(0);

        len += S(4);
        len += S(4);
        len += S(4);
        len += S(1);
        len += S(1);
        len += S(4);

        len += S(1);
        len += S(min(self.field_term.len(), MAX_U8));
        len += S(1);
        len += S(min(self.enclosed_by.len(), MAX_U8));
        len += S(1);
        len += S(min(self.line_term.len(), MAX_U8));
        len += S(1);
        len += S(min(self.line_start.len(), MAX_U8));
        len += S(1);
        len += S(min(self.escaped_by.len(), MAX_U8));
        len += S(1);

        len += S(num_fields);
        len += S(self
            .field_names
            .iter()
            .take(num_fields)
            .map(Vec::len)
            .map(|len| min(len, MAX_U8))
            .sum::<usize>());
        len += S(num_fields);
        len += S(min(self.table_name.len(), MAX_U8));
        len += S(1);
        len += S(min(self.schema_name.len(), MAX_U8));
        len += S(1);
        len += S(self.file_name.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

impl NewLoadEvent {
    /// Returns a he `field_term` field as a string (lossy converted).
    pub fn get_field_term(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.field_term)
    }

    /// Returns a e `enclosed_by` field as a string (lossy converted).
    pub fn get_enclosed_by(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.enclosed_by)
    }

    /// Returns the `line_term` field as a string (lossy converted).
    pub fn get_line_term(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.line_term)
    }

    /// Returns a he `line_start` field as a string (lossy converted).
    pub fn get_line_start(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.line_start)
    }

    /// Returns a he `escaped_by` field as a string (lossy converted).
    pub fn get_escaped_by(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.escaped_by)
    }

    /// Returns the given field name as a string.
    pub fn get_field_name(&self, index: usize) -> Option<Cow<str>> {
        self.field_names
            .get(index)
            .map(|field_name| String::from_utf8_lossy(&field_name[..]))
    }

    /// Returns the `table_name` field value as a string.
    pub fn get_table_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.table_name[..])
    }

    /// Returns the `schema_name` field value as a string.
    pub fn get_schema_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.schema_name[..])
    }

    /// Returns the `file_name` field value as a string.
    pub fn get_file_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.file_name[..])
    }

    /// Returns parsed opt flags of this load event. Unknown bits will be dropped.
    pub fn get_opt_flags(&self) -> OptFlags {
        OptFlags::from_bits_truncate(self.opt_flags)
    }
}

/// Create file event.
///
/// Used for LOAD DATA INFILE statements in MySQL 4.0 and 4.1.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CreateFileEvent {
    pub file_id: u32,
    pub block_data: Vec<u8>,
}

impl BinlogStruct for CreateFileEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::CREATE_FILE_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let file_id = input.read_u32::<LittleEndian>()?;

        let mut block_data = vec![0_u8; input.limit()];
        input.read_exact(&mut block_data)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            file_id,
            block_data,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.file_id)?;
        output.write_all(&self.block_data)?;

        Ok(())
    }

    /// Returns length of this load event in bytes.
    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(4);
        len += S(self.block_data.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

/// Append block event.
///
/// Used for LOAD DATA INFILE statements as of MySQL 4.0.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AppendBlockEvent {
    pub file_id: u32,
    pub block_data: Vec<u8>,
}

impl BinlogStruct for AppendBlockEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::APPEND_BLOCK_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let file_id = input.read_u32::<LittleEndian>()?;

        let mut block_data = vec![0_u8; input.limit()];
        input.read_exact(&mut block_data)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            file_id,
            block_data,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.file_id)?;
        output.write_all(&self.block_data)?;

        Ok(())
    }

    /// Returns length of this load event in bytes.
    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(4);
        len += S(self.block_data.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

/// Exec load event.
///
/// Used for LOAD DATA INFILE statements in 4.0 and 4.1.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ExecLoadEvent {
    pub file_id: u32,
}

impl BinlogStruct for ExecLoadEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::EXEC_LOAD_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));
        let file_id = input.read_u32::<LittleEndian>()?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self { file_id })
    }

    fn write<T: Write>(&self, _version: BinlogVersion, mut output: T) -> io::Result<()> {
        output.write_u32::<LittleEndian>(self.file_id)?;
        Ok(())
    }

    /// Returns length of this load event in bytes.
    fn len(&self, _version: BinlogVersion) -> usize {
        4
    }
}

/// Begin load query event.
///
/// Used for LOAD DATA INFILE statements as of MySQL 5.0.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BeginLoadQueryEvent {
    pub file_id: u32,
    pub block_data: Vec<u8>,
}

impl BinlogStruct for BeginLoadQueryEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::BEGIN_LOAD_QUERY_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let file_id = input.read_u32::<LittleEndian>()?;

        let mut block_data = vec![0_u8; input.limit()];
        input.read_exact(&mut block_data)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            file_id,
            block_data,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.file_id)?;
        output.write_all(&self.block_data)?;

        Ok(())
    }

    /// Returns length of this load event in bytes.
    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(4);
        len += S(self.block_data.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

/// Variants of this enum describe how LOAD DATA handles duplicates.
#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum LoadDuplicateHandling {
    LOAD_DUP_ERROR = 0,
    LOAD_DUP_IGNORE,
    LOAD_DUP_REPLACE,
}

impl TryFrom<u8> for LoadDuplicateHandling {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::LOAD_DUP_ERROR),
            1 => Ok(Self::LOAD_DUP_IGNORE),
            2 => Ok(Self::LOAD_DUP_REPLACE),
            _ => Err(value),
        }
    }
}

/// Execute load query event.
///
/// Used for LOAD DATA INFILE statements as of MySQL 5.0.
///
/// It similar to Query_log_event but before executing the query it substitutes original filename
/// in LOAD DATA query with name of temporary file.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ExecuteLoadQueryEvent {
    // post-header
    pub thread_id: u32,
    pub execution_time: u32,
    pub error_code: u16,

    pub status_vars: Vec<u8>,
    pub schema: Vec<u8>,
    pub query: Vec<u8>,

    // payload
    /// File_id of a temporary file.
    pub file_id: u32,
    /// Pointer to the part of the query that should be substituted.
    pub start_pos: u32,
    /// Pointer to the end of this part of query
    pub end_pos: u32,
    /// How to handle duplicates.
    pub dup_handling: u8,
}

impl ExecuteLoadQueryEvent {
    /// Returns the `schema` field as a string (lossy converted).
    pub fn get_schema(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.schema)
    }

    /// Returns the `query` field as a string (lossy converted).
    pub fn get_query(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.query)
    }

    /// Get duplicate handling.
    pub fn get_dup_handling(&self) -> Result<LoadDuplicateHandling, u8> {
        LoadDuplicateHandling::try_from(self.dup_handling)
    }
}

impl fmt::Debug for ExecuteLoadQueryEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExecuteLoadQueryEvent")
            .field("thread_id", &self.thread_id)
            .field("execution_time", &self.execution_time)
            .field("error_code", &self.error_code)
            .field("status_vars", &self.status_vars)
            .field("schema", &self.get_schema())
            .field("query", &self.get_query())
            .field("file_id", &self.file_id)
            .field("start_pos", &self.start_pos)
            .field("end_pos", &self.end_pos)
            .field("dup_handling_flags", &self.dup_handling)
            .finish()
    }
}

impl BinlogStruct for ExecuteLoadQueryEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::EXECUTE_LOAD_QUERY_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let thread_id = input.read_u32::<LittleEndian>()?;
        let execution_time = input.read_u32::<LittleEndian>()?;
        let schema_len = input.read_u8()? as usize;
        let error_code = input.read_u16::<LittleEndian>()?;
        let status_vars_len = input.read_u16::<LittleEndian>()? as usize;
        let file_id = input.read_u32::<LittleEndian>()?;
        let start_pos = input.read_u32::<LittleEndian>()?;
        let end_pos = input.read_u32::<LittleEndian>()?;
        let dup_handling = input.read_u8()?;

        let mut status_vars = vec![0_u8; status_vars_len];
        input.read_exact(&mut status_vars)?;

        let mut schema = vec![0_u8; schema_len];
        input.read_exact(&mut schema)?;
        input.read_u8()?;

        let mut query = vec![0_u8; input.limit()];
        input.read_exact(&mut query)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            thread_id,
            execution_time,
            error_code,
            status_vars,
            schema,
            file_id,
            start_pos,
            end_pos,
            dup_handling,
            query,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.thread_id)?;
        output.write_u32::<LittleEndian>(self.execution_time)?;
        output.write_u8(min(self.schema.len(), MAX_U8) as u8)?;
        output.write_u16::<LittleEndian>(self.error_code)?;
        output.write_u16::<LittleEndian>(min(self.status_vars.len(), MAX_U16) as u16)?;
        output.write_u32::<LittleEndian>(self.file_id)?;
        output.write_u32::<LittleEndian>(self.start_pos)?;
        output.write_u32::<LittleEndian>(self.end_pos)?;
        output.write_u8(self.dup_handling)?;
        output.nest(S(MAX_U16)).write_all(&self.status_vars)?;
        output.nest(S(MAX_U8)).write_all(&self.schema)?;
        output.write_u8(0)?;
        output.write_all(&self.query)?;

        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(4); // thread_id
        len += S(4); // query_exec_time
        len += S(1); // db_len
        len += S(2); // error_code
        len += S(2); // status_vars_len
        len += S(4); // file_id
        len += S(4); // start_pos
        len += S(4); // end_pos
        len += S(1); // dup_handling_flags
        len += S(min(self.status_vars.len(), MAX_U16 - 13)); // status_vars
        len += S(min(self.schema.len(), MAX_U8)); // db_len
        len += S(1); // null-byte
        len += S(self.query.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

/// Delete file event.
///
/// Used for LOAD DATA INFILE statements as of MySQL 4.0.
///
/// Created when the LOAD_DATA query fails on the master for some reason, and the slave should
/// be notified to abort the load.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DeleteFileEvent {
    pub file_id: u32,
}

impl BinlogStruct for DeleteFileEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::DELETE_FILE_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));
        let file_id = input.read_u32::<LittleEndian>()?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self { file_id })
    }

    fn write<T: Write>(&self, _version: BinlogVersion, mut output: T) -> io::Result<()> {
        output.write_u32::<LittleEndian>(self.file_id)
    }

    fn len(&self, _version: BinlogVersion) -> usize {
        4
    }
}

/// Rand event.
///
/// Logs random seed used by the next `RAND()`, and by `PASSWORD()` in 4.1.0. 4.1.1 does not need
/// it (it's repeatable again) so this event needn't be written in 4.1.1 for `PASSWORD()`
/// (but the fact that it is written is just a waste, it does not cause bugs).
///
/// The state of the random number generation consists of 128 bits, which are stored internally
/// as two 64-bit numbers.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RandEvent {
    pub seed1: u64,
    pub seed2: u64,
}

impl BinlogStruct for RandEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::RAND_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let seed1 = input.read_u64::<LittleEndian>()?;
        let seed2 = input.read_u64::<LittleEndian>()?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self { seed1, seed2 })
    }

    fn write<T: Write>(&self, _version: BinlogVersion, mut output: T) -> io::Result<()> {
        output.write_u64::<LittleEndian>(self.seed1)?;
        output.write_u64::<LittleEndian>(self.seed2)?;
        Ok(())
    }

    fn len(&self, _version: BinlogVersion) -> usize {
        8
    }
}

/// Xid event.
///
/// Generated for a commit of a transaction that modifies one or more tables of an XA-capable
/// storage engine.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct XidEvent {
    pub xid: u64,
}

impl BinlogStruct for XidEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::XID_EVENT);

    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let post_header_len = fde.get_event_type_header_length(Self::EVENT_TYPE.unwrap());

        for _ in 0..post_header_len {
            input.read_u8()?;
        }

        let xid = input.read_u64::<LittleEndian>()?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self { xid })
    }

    fn write<T: Write>(&self, _version: BinlogVersion, mut output: T) -> io::Result<()> {
        output.write_u64::<LittleEndian>(self.xid)
    }

    fn len(&self, _version: BinlogVersion) -> usize {
        8
    }
}

/// Type of an `InvarEvent`.
#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum IntvarEventType {
    INVALID_INT_EVENT,
    /// Indicates the value to use for the `LAST_INSERT_ID()` function in the next statement.
    LAST_INSERT_ID_EVENT,
    /// Indicates the value to use for an `AUTO_INCREMENT` column in the next statement.
    INSERT_ID_EVENT,
}

impl TryFrom<u8> for IntvarEventType {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::INVALID_INT_EVENT),
            1 => Ok(Self::LAST_INSERT_ID_EVENT),
            2 => Ok(Self::INSERT_ID_EVENT),
            x => Err(x),
        }
    }
}

/// Integer based session-variables event.
///
/// Written every time a statement uses an AUTO_INCREMENT column or the LAST_INSERT_ID() function;
/// precedes other events for the statement. This is written only before a QUERY_EVENT
/// and is not used with row-based logging. An INTVAR_EVENT is written with a "subtype"
/// in the event data part.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct IntvarEvent {
    /// Contains raw value.
    ///
    /// Use `Self::get_type` function.
    pub subtype: u8,
    pub value: u64,
}

impl IntvarEvent {
    /// Returns parsed subtype of this event, or raw value if subtype is unknown.
    pub fn get_type(&self) -> Result<IntvarEventType, u8> {
        IntvarEventType::try_from(self.subtype)
    }
}

impl fmt::Debug for IntvarEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IntvarEvent")
            .field(
                "subtype",
                &match self.get_type() {
                    Ok(subtype) => format!("{:?}", subtype),
                    Err(raw) => format!("Unknown event subtype {}", raw),
                },
            )
            .field("value", &self.value)
            .finish()
    }
}

impl BinlogStruct for IntvarEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::INTVAR_EVENT);

    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let post_header_len = fde.get_event_type_header_length(Self::EVENT_TYPE.unwrap());

        for _ in 0..post_header_len {
            input.read_u8()?;
        }

        let subtype = input.read_u8()?;
        let value = input.read_u64::<LittleEndian>()?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self { subtype, value })
    }

    fn write<T: Write>(&self, _version: BinlogVersion, mut output: T) -> io::Result<()> {
        output.write_u8(self.subtype)?;
        output.write_u64::<LittleEndian>(self.value)?;
        Ok(())
    }

    fn len(&self, _version: BinlogVersion) -> usize {
        9
    }
}

bitflags! {
    /// Flags of a user variable.
    pub struct UserVarFlags: u8 {
        const UNSIGNED = 0x01;
    }
}

/// User variable event.
///
/// Written every time a statement uses a user variable; precedes other events for the statement.
/// Indicates the value to use for the user variable in the next statement.
/// This is written only before a `QUERY_EVENT` and is not used with row-based logging.
///
/// # Notes on `BinlogEvent` implementation
///
/// * it won't try to read/write anything except `name` and `is_null` if `is_null` is `true`
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct UserVarEvent {
    /// User variable name.
    ///
    /// This field contains raw value. Use `Self::get_name` to parse it as UTF8.
    pub name: Vec<u8>,
    /// `true` if value is `NULL`.
    pub is_null: bool,
    /// Type of a value.
    ///
    /// Contains raw value. Use `Self::get_type` to parse it.
    pub value_type: i8,
    /// Character set of a value. Will be `0` if `is_null` is `true`.
    pub charset: u32,
    /// Value of a user variable. Will be empty if `is_null` is `true`.
    pub value: Vec<u8>,
    /// Flags of a user variable. Will be `0` if `is_null` is `true`.
    ///
    /// This field contains raw value. Use `Self::get_flags` to parse it.
    pub flags: u8,
}

impl UserVarEvent {
    /// Returns variable name as string.
    pub fn get_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.name[..])
    }

    /// Returns type of a value, or raw value if type is invalid
    /// (not in {`STRING_RESULT`, `REAL_RESULT`, `DECIMAL_RESULT`, `INT_RESULT`}).
    pub fn get_type(&self) -> Result<ItemResult, i8> {
        match ItemResult::try_from(self.value_type) {
            Ok(x @ ItemResult::STRING_RESULT) => Ok(x),
            Ok(x @ ItemResult::REAL_RESULT) => Ok(x),
            Ok(x @ ItemResult::INT_RESULT) => Ok(x),
            Ok(x @ ItemResult::DECIMAL_RESULT) => Ok(x),
            _ => Err(self.value_type),
        }
    }

    /// Returns parsed flags of a value, unknown bits will be dropped.
    pub fn get_flags(&self) -> UserVarFlags {
        UserVarFlags::from_bits_truncate(self.flags)
    }
}

impl fmt::Debug for UserVarEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UserVarEvent")
            .field("name", &self.get_name())
            .field("is_null", &self.is_null)
            .field(
                "value_type",
                &match self.get_type() {
                    Ok(value_type) => format!("{:?}", value_type),
                    Err(raw) => format!("Invalid value type {}", raw),
                },
            )
            .field("charset", &self.charset)
            .field("value", &self.value)
            .field("flags", &self.flags)
            .finish()
    }
}

impl BinlogStruct for UserVarEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::USER_VAR_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let name_len = input.read_u32::<LittleEndian>()? as usize;
        let mut name = vec![0_u8; name_len];
        input.read_exact(&mut name)?;
        let is_null = input.read_u8()? != 0;

        if is_null {
            return Ok(Self {
                name,
                is_null,
                value_type: ItemResult::STRING_RESULT as i8,
                charset: 63,
                value: Vec::new(),
                flags: UserVarFlags::empty().bits(),
            });
        }

        let value_type = input.read_i8()?;
        let charset = input.read_u32::<LittleEndian>()?;
        let value_len = input.read_u32::<LittleEndian>()? as usize;
        let mut value = vec![0_u8; value_len];
        input.read_exact(&mut value)?;

        // Old servers may not pack flags here.
        let flags = if input.limit() > 0 {
            input.read_u8()?
        } else {
            0
        };

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            name,
            is_null,
            value_type,
            charset,
            value,
            flags,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u32::<LittleEndian>(self.name.len() as u32)?;
        output.write_all(&self.name)?;
        output.write_u8(self.is_null as u8)?;
        if !self.is_null {
            output.write_i8(self.value_type)?;
            output.write_u32::<LittleEndian>(self.charset)?;
            output.write_u32::<LittleEndian>(self.value.len() as u32)?;
            output.write_all(&self.value)?;
            output.write_u8(self.flags)?;
        }
        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(4);
        len += S(min(self.name.len(), MAX_U32));
        len += S(1);

        if !self.is_null {
            len += S(1);
            len += S(4);
            len += S(4);
            len += S(min(self.value.len(), MAX_U32));
            len += S(1);
        }

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

/// Type of an incident event.
#[repr(u16)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum IncidentType {
    /// No incident.
    INCIDENT_NONE = 0,
    /// There are possibly lost events in the replication stream.
    INCIDENT_LOST_EVENTS = 1,
}

impl TryFrom<u16> for IncidentType {
    type Error = u16;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::INCIDENT_NONE),
            1 => Ok(Self::INCIDENT_LOST_EVENTS),
            x => Err(x),
        }
    }
}

/// Used to log an out of the ordinary event that occurred on the master.
///
/// It notifies the slave that something happened on the master that might cause data
/// to be in an inconsistent state.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct IncidentEvent {
    pub incident_type: u16,
    pub message: Vec<u8>,
}

impl IncidentEvent {
    /// Returns parsed incident type, or raw value if type is unknown.
    pub fn get_type(&self) -> Result<IncidentType, u16> {
        IncidentType::try_from(self.incident_type)
    }

    /// Returns event messages as string (using lossy conversion).
    pub fn get_message(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.message)
    }
}

impl fmt::Debug for IncidentEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IncidentEvent")
            .field(
                "incident_type",
                &match self.get_type() {
                    Ok(incident_type) => format!("{:?}", incident_type),
                    Err(raw) => format!("Unknown incident type {}", raw),
                },
            )
            .field("message", &self.get_message())
            .finish()
    }
}

impl BinlogStruct for IncidentEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::INCIDENT_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let incident_type = input.read_u16::<LittleEndian>()?;
        let message_len = input.read_u8()? as usize;
        let mut message = vec![0_u8; message_len];
        input.read_exact(&mut message)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self {
            incident_type,
            message,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));
        output.write_u16::<LittleEndian>(self.incident_type)?;
        output.write_u8(min(self.message.len(), MAX_U8) as u8)?;
        output.nest(S(MAX_U8)).write_all(&self.message)?;
        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(2);
        len += S(min(self.message.len(), MAX_U8));
        len += S(self.message.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

impl ColumnType {
    /// Returns type-specific metadata length for this column type.
    fn get_metadata_len(&self) -> usize {
        match self {
            Self::MYSQL_TYPE_STRING => 2,
            Self::MYSQL_TYPE_VAR_STRING => 2,
            Self::MYSQL_TYPE_VARCHAR => 2,
            Self::MYSQL_TYPE_BLOB => 1,
            Self::MYSQL_TYPE_DECIMAL => 2,
            Self::MYSQL_TYPE_NEWDECIMAL => 2,
            Self::MYSQL_TYPE_DOUBLE => 1,
            Self::MYSQL_TYPE_FLOAT => 1,
            Self::MYSQL_TYPE_SET | Self::MYSQL_TYPE_ENUM => 2,
            _ => 0,
        }
    }
}

/// Table map event.
///
/// In row-based mode, every row operation event is preceded by a Table_map_event which maps
/// a table definition to a number.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct TableMapEvent {
    // post-header
    /// The number that identifies the table.
    ///
    /// It's 6 bytes long, so valid range is [0, 1<<48).
    pub table_id: u64,
    /// Reserved for future use; currently always 0.
    pub flags: u16,

    // payload
    /// The name of the database in which the table resides.
    ///
    /// Length must be <= 64 bytes.
    pub database_name: Vec<u8>,
    /// The name of the table.
    ///
    /// Length must be <= 64 bytes.
    pub table_name: Vec<u8>,
    /// The type of each column in the table, listed from left to right.
    pub columns_type: Vec<u8>,
    /// For each column from left to right, a chunk of data who's length and semantics depends
    /// on the type of the column.
    pub columns_metadata: Vec<u8>,
    /// For each column, a bit indicating whether data in the column can be NULL or not.
    ///
    /// The number of bytes needed for this is int((column_count + 7) / 8).
    /// The flag for the first column from the left is in the least-significant bit
    /// of the first byte, the second is in the second least significant bit of the first byte,
    /// the ninth is in the least significant bit of the second byte, and so on.
    pub null_bitmask: Vec<u8>,
    /// Optional metadata.
    pub optional_metadata: Vec<u8>,
}

impl TableMapEvent {
    /// Returns database name as string (lossy converted).
    pub fn get_database_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.database_name)
    }

    /// Returns table name as string (lossy converted).
    pub fn get_table_name(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.table_name)
    }

    /// Returns columns count in this event.
    pub fn get_columns_count(&self) -> usize {
        self.columns_type.len()
    }

    /// Returns parsed type for the given column, or raw value if type is unknown.
    ///
    /// Returns `None` if column index is out of bounds.
    pub fn get_column_type(&self, col_idx: usize) -> Option<Result<ColumnType, UnknownColumnType>> {
        self.columns_type
            .get(col_idx)
            .map(|col_type| ColumnType::try_from(*col_type))
    }

    /// Returns metadata for the given column.
    ///
    /// Returns `None` if column index is out of bounds or if offset couldn't be calculated
    /// (e.g. because of unknown column type between `0` and `col_idx`).
    pub fn get_column_metadata(&self, col_idx: usize) -> Option<&[u8]> {
        let col_type = self.get_column_type(col_idx)?.ok()?;
        let metadata_len = col_type.get_metadata_len();

        let mut offset = 0;

        for _ in 0..col_idx {
            let ty = self.get_column_type(col_idx)?.ok()?;
            offset += ty.get_metadata_len();
        }

        self.columns_metadata.get(offset..(offset + metadata_len))
    }
}

impl fmt::Debug for TableMapEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TableMapEvent")
            .field("table_id", &self.table_id)
            .field("flags", &self.flags)
            .field("database_name", &self.get_database_name())
            .field("table_name", &self.get_table_name())
            .field(
                "columns_type",
                &self
                    .columns_type
                    .iter()
                    .map(|ty| match ColumnType::try_from(*ty) {
                        Ok(column_type) => format!("{:?}", column_type),
                        Err(raw) => format!("Unknown column type {:?}", raw),
                    }),
            )
            .field("columns_metadata", &self.columns_metadata)
            .field("null_bitmask", &{
                let mut out = String::with_capacity(8 * self.null_bitmask.len());
                'outer: for byte in &self.null_bitmask {
                    for bit in 0..8 {
                        if out.len() == self.columns_type.len() {
                            break 'outer;
                        }
                        if (byte >> bit) & 1_u8 == 1_u8 {
                            out.push('1');
                        } else {
                            out.push('0');
                        }
                    }
                }
                out
            })
            .field("optional_metadata", &self.optional_metadata)
            .finish()
    }
}

impl BinlogStruct for TableMapEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::TABLE_MAP_EVENT);

    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        let table_id = if 6 == fde.get_event_type_header_length(Self::EVENT_TYPE.unwrap()) {
            input.read_u32::<LittleEndian>()? as u64
        } else {
            input.read_u48::<LittleEndian>()?
        };

        let flags = input.read_u16::<LittleEndian>()?;

        let database_name_len = input.read_u8()? as usize;
        let mut database_name = vec![0_u8; database_name_len];
        input.read_exact(&mut database_name)?;
        input.read_u8()?; // skip null

        let table_name_len = input.read_u8()? as usize;
        let mut table_name = vec![0_u8; table_name_len];
        input.read_exact(&mut table_name)?;
        input.read_u8()?; // skip null

        let columns_count = input.read_lenenc_int()?;
        let mut columns_type = vec![0_u8; columns_count as usize];
        input.read_exact(&mut columns_type)?;

        let metadata_len = input.read_lenenc_int()? as usize;
        let mut columns_metadata = vec![0_u8; metadata_len];
        input.read_exact(&mut columns_metadata)?;

        let bitmask_len = (columns_count + 7) / 8;
        let mut null_bitmask = vec![0_u8; bitmask_len as usize];
        input.read_exact(&mut null_bitmask)?;

        let mut optional_metadata = vec![0_u8; input.limit()];
        input.read_exact(&mut optional_metadata)?;

        Ok(Self {
            table_id,
            flags,
            database_name,
            table_name,
            columns_type,
            columns_metadata,
            null_bitmask,
            optional_metadata,
        })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u48::<LittleEndian>(self.table_id)?;
        output.write_u16::<LittleEndian>(self.flags)?;
        output.write_u8(min(self.database_name.len(), MAX_U8) as u8)?;
        output.nest(S(MAX_U8)).write_all(&self.database_name)?;
        output.write_u8(0)?;
        output.write_u8(min(self.table_name.len(), MAX_U8) as u8)?;
        output.nest(S(MAX_U8)).write_all(&self.table_name)?;
        output.write_u8(0)?;
        output.write_lenenc_int(self.get_columns_count() as u64)?;
        output.write_all(&self.columns_type)?;
        output.write_lenenc_str(&self.columns_metadata)?;
        output.write_all(&self.null_bitmask)?;
        output.write_all(&self.optional_metadata)?;

        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(6);
        len += S(2);
        len += S(1);
        len += S(min(self.database_name.len(), MAX_U8));
        len += S(1);
        len += S(1);
        len += S(min(self.table_name.len(), MAX_U8));
        len += S(1);
        len += S(crate::misc::lenenc_int_len(self.get_columns_count() as u64) as usize);
        len += S(self.get_columns_count());
        len += S(crate::misc::lenenc_str_len(&self.columns_metadata) as usize);
        len += S((self.get_columns_count() + 8) / 7);
        len += S(self.optional_metadata.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}
/// Query that caused the following `ROWS_EVENT`.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct RowsQueryEvent {
    pub query: Vec<u8>,
}

impl RowsQueryEvent {
    /// Returns query as string (lossy converted).
    pub fn get_query(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.query)
    }
}

impl fmt::Debug for RowsQueryEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RowsQueryEvent")
            .field("query", &self.get_query())
            .finish()
    }
}

impl BinlogStruct for RowsQueryEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::ROWS_QUERY_EVENT);

    fn read<T: Read>(
        event_size: usize,
        _fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));

        input.read_u8()?; // ignore length
        let mut query = vec![0_u8; input.limit()];
        input.read_exact(&mut query)?;

        if input.limit() > 0 {
            return Err(Error::new(Other, "bytes remaining on stream"));
        }

        Ok(Self { query })
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u8(min(self.query.len(), MAX_U8) as u8)?;
        output.write_all(&self.query)?;

        Ok(())
    }

    fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(1);
        len += S(self.query.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

bitflags! {
    /// Rows event flags.
    pub struct RowsEventFalgs: u16 {
        /// Last event of a statement.
        const STMT_END = 0x0001;
        /// No foreign key checks.
        const NO_FOREIGN_KEY_CHECKS   = 0x0002;
        /// No unique key checks.
        const RELAXED_UNIQUE_CHECKS  = 0x0004;
        /// Indicates that rows in this event are complete,
        /// that is contain values for all columns of the table.
        const COMPLETE_ROWS = 0x0008;
    }
}

/// Common base structure for all row-containing binary log events.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct RowsEvent {
    /// Table identifier.
    ///
    /// If the table id is `0x00ffffff` it is a dummy event that should have
    /// the end of statement flag set that declares that all table maps can be freed.
    /// Otherwise it refers to a table defined by `TABLE_MAP_EVENT`.
    pub table_id: u64,
    /// Raw rows event flags (see `RowsEventFalgs`).
    pub flags: u16,
    /// Raw extra data.
    pub extra_data: Vec<u8>,
    /// Number of columns.
    pub num_columns: u64,
    /// For DELETE and UPDATE only. Bit-field indicating whether each column is used one bit
    /// per column.
    ///
    /// Will be empty for WRITE events.
    pub columns_before_image: Option<Vec<u8>>,
    /// For WRITE and UPDATE only. Bit-field indicating whether each column is used
    /// in the `UPDATE_ROWS_EVENT` and `WRITE_ROWS_EVENT` after-image; one bit per column.
    ///
    /// Will be empty for DELETE events.
    pub columns_after_image: Option<Vec<u8>>,
    /// A sequence of zero or more rows. The end is determined by the size of the event.
    ///
    /// Each row has the following format:
    ///
    /// *   A Bit-field indicating whether each field in the row is NULL. Only columns that
    ///     are "used" according to the second field in the variable data part are listed here.
    ///     If the second field in the variable data part has N one-bits, the amount of storage
    ///     required for this field is INT((N + 7) / 8) bytes.
    /// *   The row-image, containing values of all table fields. This only lists table fields
    ///     that are used (according to the second field of the variable data part) and non-NULL
    ///     (according to the previous field). In other words, the number of values listed here
    ///     is equal to the number of zero bits in the previous field. (not counting padding
    ///     bits in the last byte).
    pub rows_data: Vec<u8>,
}

impl RowsEvent {
    /// Returns parsed flags of this event. Unknown bits will be dropped.
    pub fn get_flags(&self) -> RowsEventFalgs {
        RowsEventFalgs::from_bits_truncate(self.flags)
    }

    /// Reads an event from the given stream.
    ///
    /// This function will be used in `BinlogStruct` implementations for derived events.
    pub fn read<T: Read>(
        event_type: EventType,
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self> {
        let mut input = LimitedRead::new(input, S(event_size) - S(BinlogEventHeader::len(version)));
        let post_header_len = fde.get_event_type_header_length(event_type);

        let is_delete_event = event_type == EventType::DELETE_ROWS_EVENT
            || event_type == EventType::DELETE_ROWS_EVENT_V1;

        let is_update_event = event_type == EventType::UPDATE_ROWS_EVENT
            || event_type == EventType::UPDATE_ROWS_EVENT_V1
            || event_type == EventType::PARTIAL_UPDATE_ROWS_EVENT;

        let table_id = if post_header_len == 6 {
            input.read_u32::<LittleEndian>()? as u64
        } else {
            input.read_u48::<LittleEndian>()?
        };

        let flags = input.read_u16::<LittleEndian>()?;

        let extra_data =
            if post_header_len == fde.get_event_type_header_length(EventType::WRITE_ROWS_EVENT) {
                // variable-length post header containing extra data
                let extra_data_len = input.read_u16::<LittleEndian>()? as usize;
                let mut extra_data = vec![0_u8; extra_data_len.saturating_sub(2)];
                input.read_exact(&mut extra_data)?;
                extra_data
            } else {
                Vec::new()
            };

        let num_columns = input.read_lenenc_int()?;
        let bitmap_len = (num_columns as usize + 7) / 8;

        let mut columns_image_1 = vec![0_u8; bitmap_len];
        input.read_exact(&mut columns_image_1)?;

        let columns_image_2 = if is_update_event {
            let mut columns_image_2 = vec![0_u8; bitmap_len];
            input.read_exact(&mut columns_image_2)?;
            Some(columns_image_2)
        } else {
            None
        };

        let mut rows_data = vec![0_u8; input.limit()];
        input.read_exact(&mut rows_data)?;

        let (columns_before_image, columns_after_image) = if is_update_event {
            (Some(columns_image_1), columns_image_2)
        } else if is_delete_event {
            (Some(columns_image_1), None)
        } else {
            (None, Some(columns_image_1))
        };

        Ok(Self {
            table_id,
            flags,
            extra_data,
            num_columns,
            columns_before_image,
            columns_after_image,
            rows_data,
        })
    }

    /// Writes this event into the given stream.
    ///
    /// This function will be used in `BinlogStruct` implementations for derived events.
    pub fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        let mut output = LimitedWrite::new(output, S(self.len(version)));

        output.write_u48::<LittleEndian>(self.table_id)?;
        output.write_u16::<LittleEndian>(self.flags)?;
        output.write_u16::<LittleEndian>(
            min(self.extra_data.len().saturating_add(2), MAX_U16) as u16
        )?;
        output.nest(S(MAX_U16 - 2)).write_all(&self.extra_data)?;
        output.write_lenenc_int(self.num_columns)?;
        let bitmap_len = (self.num_columns as usize + 7) / 8;
        {
            let num_bitmaps = self.columns_before_image.is_some() as usize
                + self.columns_after_image.is_some() as usize;
            let mut output = LimitedWrite::new(&mut output, S(bitmap_len * num_bitmaps));
            output.write_all(
                self.columns_before_image
                    .as_ref()
                    .map(|x| x.as_ref())
                    .unwrap_or_default(),
            )?;
            output.write_all(
                self.columns_after_image
                    .as_ref()
                    .map(|x| x.as_ref())
                    .unwrap_or_default(),
            )?;

            if output.limit() > 0 {
                return Err(Error::new(UnexpectedEof, "failed to fill whole buffer"));
            }
        }
        output.write_all(&self.rows_data)?;

        Ok(())
    }

    /// Returns length of this event in bytes.
    ///
    /// This function will be used in `BinlogStruct` implementations for derived events.
    pub fn len(&self, version: BinlogVersion) -> usize {
        let mut len = S(0);

        len += S(6); // table_id
        len += S(2); // flags
        len += S(2); // extra-data len
        len += S(min(self.extra_data.len(), MAX_U16 - 2)); // extra data
        len += S(crate::misc::lenenc_int_len(self.num_columns) as usize); // number of columns
        let bitmap_len = (self.num_columns as usize + 7) / 8;
        if self.columns_before_image.is_some() {
            len += S(bitmap_len); // columns present bitmap 1
        }
        if self.columns_after_image.is_some() {
            len += S(bitmap_len); // columns present bitmap 2
        }
        len += S(self.rows_data.len());

        min(len.0, MAX_U32 - BinlogEventHeader::len(version))
    }
}

impl fmt::Debug for RowsEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RowsEvent")
            .field("table_id", &self.table_id)
            .field(
                "flags",
                &format!(
                    "{:?} (Unknown flags: {:016b})",
                    self.get_flags(),
                    self.flags & (std::u16::MAX ^ RowsEventFalgs::all().bits())
                ),
            )
            .field("extra_data", &self.extra_data)
            .field("num_columns", &self.num_columns)
            .field("columns_before_image", &self.columns_before_image)
            .field("columns_after_image", &self.columns_after_image)
            .field("rows_data", &self.rows_data)
            .finish()
    }
}

/// Write rows event.
///
/// Used for row-based binary logging. Contains the row data to insert.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WriteRowsEvent(RowsEvent);

impl WriteRowsEvent {
    /// Returns parsed flags of this event. Unknown bits will be dropped.
    pub fn get_flags(&self) -> RowsEventFalgs {
        self.0.get_flags()
    }
}

impl BinlogStruct for WriteRowsEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::WRITE_ROWS_EVENT);

    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(RowsEvent::read(
            Self::EVENT_TYPE.unwrap(),
            event_size,
            fde,
            version,
            input,
        )?))
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        self.0.write(version, output)
    }

    fn len(&self, version: BinlogVersion) -> usize {
        self.0.len(version)
    }
}

/// Update rows event.
///
/// Used for row-based binary logging. Contains as much data as needed to identify
/// a row + the data to change.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct UpdateRowsEvent(RowsEvent);

impl UpdateRowsEvent {
    /// Returns parsed flags of this event. Unknown bits will be dropped.
    pub fn get_flags(&self) -> RowsEventFalgs {
        self.0.get_flags()
    }
}

impl BinlogStruct for UpdateRowsEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::UPDATE_ROWS_EVENT);

    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(RowsEvent::read(
            Self::EVENT_TYPE.unwrap(),
            event_size,
            fde,
            version,
            input,
        )?))
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        self.0.write(version, output)
    }

    fn len(&self, version: BinlogVersion) -> usize {
        self.0.len(version)
    }
}

/// Delete rows event.
///
/// Used for row-based binary logging. Contains as much data as needed to identify a row.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DeleteRowsEvent(RowsEvent);

impl DeleteRowsEvent {
    /// Returns parsed flags of this event. Unknown bits will be dropped.
    pub fn get_flags(&self) -> RowsEventFalgs {
        self.0.get_flags()
    }
}

impl BinlogStruct for DeleteRowsEvent {
    const EVENT_TYPE: Option<EventType> = Some(EventType::DELETE_ROWS_EVENT);

    fn read<T: Read>(
        event_size: usize,
        fde: &FormatDescriptionEvent,
        version: BinlogVersion,
        input: T,
    ) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(RowsEvent::read(
            Self::EVENT_TYPE.unwrap(),
            event_size,
            fde,
            version,
            input,
        )?))
    }

    fn write<T: Write>(&self, version: BinlogVersion, output: T) -> io::Result<()> {
        self.0.write(version, output)
    }

    fn len(&self, version: BinlogVersion) -> usize {
        self.0.len(version)
    }
}

#[cfg(test)]
mod tests {
    use std::io;

    use super::*;

    const BINLOG_FILE: &[u8] = &[
        0xfe, 0x62, 0x69, 0x6e, 0xfc, 0x35, 0xbb, 0x4a, 0x0f, 0x01, 0x00, 0x00, 0x00, 0x5e, 0x00,
        0x00, 0x00, 0x62, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x35, 0x2e, 0x30, 0x2e, 0x38,
        0x36, 0x2d, 0x64, 0x65, 0x62, 0x75, 0x67, 0x2d, 0x6c, 0x6f, 0x67, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xfc, 0x35, 0xbb, 0x4a, 0x13, 0x38, 0x0d, 0x00, 0x08, 0x00, 0x12, 0x00, 0x04, 0x04, 0x04,
        0x04, 0x12, 0x00, 0x00, 0x4b, 0x00, 0x04, 0x1a, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00,
        0x00, 0x00, 0x64, 0x00, 0x00, 0x00, 0xc6, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04,
        0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x74, 0x65, 0x73, 0x74, 0x00, 0x63, 0x72, 0x65, 0x61,
        0x74, 0x65, 0x20, 0x74, 0x61, 0x62, 0x6c, 0x65, 0x20, 0x74, 0x31, 0x28, 0x61, 0x20, 0x69,
        0x6e, 0x74, 0x29, 0x20, 0x65, 0x6e, 0x67, 0x69, 0x6e, 0x65, 0x3d, 0x20, 0x69, 0x6e, 0x6e,
        0x6f, 0x64, 0x62, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x65, 0x00, 0x00,
        0x00, 0x2b, 0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x05, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08,
        0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c, 0x00, 0x63, 0x72, 0x65, 0x61, 0x74, 0x65, 0x20, 0x74,
        0x61, 0x62, 0x6c, 0x65, 0x20, 0x74, 0x32, 0x28, 0x61, 0x20, 0x69, 0x6e, 0x74, 0x29, 0x20,
        0x65, 0x6e, 0x67, 0x69, 0x6e, 0x65, 0x3d, 0x20, 0x69, 0x6e, 0x6e, 0x6f, 0x64, 0x62, 0xfd,
        0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x45, 0x00, 0x00, 0x00, 0x70, 0x01, 0x00,
        0x00, 0x08, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x1a,
        0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x06, 0x03, 0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d, 0x79, 0x73,
        0x71, 0x6c, 0x00, 0x42, 0x45, 0x47, 0x49, 0x4e, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00,
        0x00, 0x00, 0x5c, 0x00, 0x00, 0x00, 0xcc, 0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04,
        0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x74, 0x65, 0x73, 0x74, 0x00, 0x69, 0x6e, 0x73, 0x65,
        0x72, 0x74, 0x20, 0x69, 0x6e, 0x74, 0x6f, 0x20, 0x74, 0x31, 0x20, 0x28, 0x61, 0x29, 0x20,
        0x76, 0x61, 0x6c, 0x75, 0x65, 0x73, 0x20, 0x28, 0x31, 0x29, 0xfd, 0x35, 0xbb, 0x4a, 0x02,
        0x01, 0x00, 0x00, 0x00, 0x5d, 0x00, 0x00, 0x00, 0x29, 0x02, 0x00, 0x00, 0x00, 0x00, 0x01,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40,
        0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74,
        0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c, 0x00, 0x69,
        0x6e, 0x73, 0x65, 0x72, 0x74, 0x20, 0x69, 0x6e, 0x74, 0x6f, 0x20, 0x74, 0x32, 0x20, 0x28,
        0x61, 0x29, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x73, 0x20, 0x28, 0x31, 0x29, 0xfd, 0x35,
        0xbb, 0x4a, 0x10, 0x01, 0x00, 0x00, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x44, 0x02, 0x00, 0x00,
        0x00, 0x00, 0x0b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfd, 0x35, 0xbb, 0x4a, 0x02,
        0x01, 0x00, 0x00, 0x00, 0x64, 0x00, 0x00, 0x00, 0xa8, 0x02, 0x00, 0x00, 0x00, 0x00, 0x01,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40,
        0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74,
        0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x74, 0x65, 0x73, 0x74, 0x00, 0x63, 0x72,
        0x65, 0x61, 0x74, 0x65, 0x20, 0x74, 0x61, 0x62, 0x6c, 0x65, 0x20, 0x74, 0x33, 0x28, 0x61,
        0x20, 0x69, 0x6e, 0x74, 0x29, 0x20, 0x65, 0x6e, 0x67, 0x69, 0x6e, 0x65, 0x3d, 0x20, 0x69,
        0x6e, 0x6e, 0x6f, 0x64, 0x62, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x65,
        0x00, 0x00, 0x00, 0x0d, 0x03, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08,
        0x00, 0x08, 0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c, 0x00, 0x63, 0x72, 0x65, 0x61, 0x74, 0x65,
        0x20, 0x74, 0x61, 0x62, 0x6c, 0x65, 0x20, 0x74, 0x34, 0x28, 0x61, 0x20, 0x69, 0x6e, 0x74,
        0x29, 0x20, 0x65, 0x6e, 0x67, 0x69, 0x6e, 0x65, 0x3d, 0x20, 0x6d, 0x79, 0x69, 0x73, 0x61,
        0x6d, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x45, 0x00, 0x00, 0x00, 0x52,
        0x03, 0x00, 0x00, 0x08, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00,
        0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d,
        0x79, 0x73, 0x71, 0x6c, 0x00, 0x42, 0x45, 0x47, 0x49, 0x4e, 0xfd, 0x35, 0xbb, 0x4a, 0x02,
        0x01, 0x00, 0x00, 0x00, 0x5c, 0x00, 0x00, 0x00, 0xae, 0x03, 0x00, 0x00, 0x00, 0x00, 0x01,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40,
        0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74,
        0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x74, 0x65, 0x73, 0x74, 0x00, 0x69, 0x6e,
        0x73, 0x65, 0x72, 0x74, 0x20, 0x69, 0x6e, 0x74, 0x6f, 0x20, 0x74, 0x33, 0x20, 0x28, 0x61,
        0x29, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x73, 0x20, 0x28, 0x32, 0x29, 0xfd, 0x35, 0xbb,
        0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x5d, 0x00, 0x00, 0x00, 0x0b, 0x04, 0x00, 0x00, 0x00,
        0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x1a, 0x00, 0x00,
        0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03,
        0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c,
        0x00, 0x69, 0x6e, 0x73, 0x65, 0x72, 0x74, 0x20, 0x69, 0x6e, 0x74, 0x6f, 0x20, 0x74, 0x34,
        0x20, 0x28, 0x61, 0x29, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x73, 0x20, 0x28, 0x32, 0x29,
        0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x48, 0x00, 0x00, 0x00, 0x53, 0x04,
        0x00, 0x00, 0x08, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00,
        0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d, 0x79,
        0x73, 0x71, 0x6c, 0x00, 0x52, 0x4f, 0x4c, 0x4c, 0x42, 0x41, 0x43, 0x4b, 0xfd, 0x35, 0xbb,
        0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x61, 0x00, 0x00, 0x00, 0xb4, 0x04, 0x00, 0x00, 0x00,
        0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x1a, 0x00, 0x00,
        0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03,
        0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x74, 0x65, 0x73, 0x74, 0x00,
        0x63, 0x72, 0x65, 0x61, 0x74, 0x65, 0x20, 0x74, 0x61, 0x62, 0x6c, 0x65, 0x20, 0x74, 0x35,
        0x28, 0x61, 0x20, 0x69, 0x6e, 0x74, 0x29, 0x20, 0x65, 0x6e, 0x67, 0x69, 0x6e, 0x65, 0x3d,
        0x20, 0x4e, 0x44, 0x42, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x62, 0x00,
        0x00, 0x00, 0x16, 0x05, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x05, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00,
        0x08, 0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c, 0x00, 0x63, 0x72, 0x65, 0x61, 0x74, 0x65, 0x20,
        0x74, 0x61, 0x62, 0x6c, 0x65, 0x20, 0x74, 0x36, 0x28, 0x61, 0x20, 0x69, 0x6e, 0x74, 0x29,
        0x20, 0x65, 0x6e, 0x67, 0x69, 0x6e, 0x65, 0x3d, 0x20, 0x4e, 0x44, 0x42, 0xfd, 0x35, 0xbb,
        0x4a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x45, 0x00, 0x00, 0x00, 0x5b, 0x05, 0x00, 0x00, 0x08,
        0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x1a, 0x00, 0x00,
        0x00, 0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03,
        0x73, 0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c,
        0x00, 0x42, 0x45, 0x47, 0x49, 0x4e, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00, 0x00, 0x00,
        0x5c, 0x00, 0x00, 0x00, 0xb7, 0x05, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04, 0x08, 0x00,
        0x08, 0x00, 0x08, 0x00, 0x74, 0x65, 0x73, 0x74, 0x00, 0x69, 0x6e, 0x73, 0x65, 0x72, 0x74,
        0x20, 0x69, 0x6e, 0x74, 0x6f, 0x20, 0x74, 0x35, 0x20, 0x28, 0x61, 0x29, 0x20, 0x76, 0x61,
        0x6c, 0x75, 0x65, 0x73, 0x20, 0x28, 0x33, 0x29, 0xfd, 0x35, 0xbb, 0x4a, 0x02, 0x01, 0x00,
        0x00, 0x00, 0x5d, 0x00, 0x00, 0x00, 0x14, 0x06, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73, 0x74, 0x64, 0x04,
        0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c, 0x00, 0x69, 0x6e, 0x73,
        0x65, 0x72, 0x74, 0x20, 0x69, 0x6e, 0x74, 0x6f, 0x20, 0x74, 0x36, 0x20, 0x28, 0x61, 0x29,
        0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x73, 0x20, 0x28, 0x33, 0x29, 0xfd, 0x35, 0xbb, 0x4a,
        0x02, 0x01, 0x00, 0x00, 0x00, 0x46, 0x00, 0x00, 0x00, 0x5a, 0x06, 0x00, 0x00, 0x08, 0x00,
        0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x1a, 0x00, 0x00, 0x00,
        0x40, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x03, 0x73,
        0x74, 0x64, 0x04, 0x08, 0x00, 0x08, 0x00, 0x08, 0x00, 0x6d, 0x79, 0x73, 0x71, 0x6c, 0x00,
        0x43, 0x4f, 0x4d, 0x4d, 0x49, 0x54, 0xfd, 0x35, 0xbb, 0x4a, 0x04, 0x01, 0x00, 0x00, 0x00,
        0x2c, 0x00, 0x00, 0x00, 0x86, 0x06, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x6d, 0x61, 0x73, 0x74, 0x65, 0x72, 0x2d, 0x62, 0x69, 0x6e, 0x2e, 0x30,
        0x30, 0x30, 0x30, 0x30, 0x32,
    ];

    #[test]
    fn binlog_file_header_roundtrip() -> io::Result<()> {
        let mut output = Vec::new();

        let binlog_file_header = BinlogFileHeader::read(
            4,
            &FormatDescriptionEvent::new(BinlogVersion::Version4),
            BinlogVersion::Version4,
            BINLOG_FILE,
        )?;
        binlog_file_header.write(BinlogVersion::Version4, &mut output)?;

        assert_eq!(&output[..], &BINLOG_FILE[..BinlogFileHeader::LEN]);

        Ok(())
    }

    #[test]
    fn binlog_file_iterator() -> io::Result<()> {
        let binlog_file = BinlogFile::new(BinlogVersion::Version4, BINLOG_FILE)?;

        let mut total = 0;
        let mut ev_pos = 4;

        for (i, ev) in binlog_file.enumerate() {
            let data_start = ev_pos + BinlogEventHeader::LEN;
            let ev = ev?;
            match i {
                0 => {
                    assert_eq!(
                        ev.header,
                        BinlogEventHeader {
                            timestamp: 1253783036,
                            event_type: 15,
                            server_id: 1,
                            event_size: 94,
                            log_pos: 98,
                            flags: 0,
                        }
                    );
                }
                1 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 100,
                        log_pos: 198,
                        flags: 0,
                    }
                ),
                2 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 101,
                        log_pos: 299,
                        flags: 0,
                    }
                ),
                3 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 69,
                        log_pos: 368,
                        flags: 8,
                    }
                ),
                4 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 92,
                        log_pos: 460,
                        flags: 0,
                    }
                ),
                5 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 93,
                        log_pos: 553,
                        flags: 0,
                    }
                ),
                6 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 16,
                        server_id: 1,
                        event_size: 27,
                        log_pos: 580,
                        flags: 0,
                    }
                ),
                7 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 100,
                        log_pos: 680,
                        flags: 0,
                    }
                ),
                8 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 101,
                        log_pos: 781,
                        flags: 0,
                    }
                ),
                9 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 69,
                        log_pos: 850,
                        flags: 8,
                    }
                ),
                10 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 92,
                        log_pos: 942,
                        flags: 0,
                    }
                ),
                11 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 93,
                        log_pos: 1035,
                        flags: 0,
                    }
                ),
                12 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 72,
                        log_pos: 1107,
                        flags: 8,
                    }
                ),
                13 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 97,
                        log_pos: 1204,
                        flags: 0,
                    }
                ),
                14 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 98,
                        log_pos: 1302,
                        flags: 0,
                    }
                ),
                15 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 69,
                        log_pos: 1371,
                        flags: 8,
                    }
                ),
                16 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 92,
                        log_pos: 1463,
                        flags: 0,
                    }
                ),
                17 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 93,
                        log_pos: 1556,
                        flags: 0,
                    }
                ),
                18 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 2,
                        server_id: 1,
                        event_size: 70,
                        log_pos: 1626,
                        flags: 8,
                    }
                ),
                19 => assert_eq!(
                    ev.header,
                    BinlogEventHeader {
                        timestamp: 1253783037,
                        event_type: 4,
                        server_id: 1,
                        event_size: 44,
                        log_pos: 1670,
                        flags: 0,
                    }
                ),
                _ => panic!("too many"),
            }

            assert_eq!(
                ev.data,
                &BINLOG_FILE[data_start
                    ..(data_start + ev.header.event_size as usize - BinlogEventHeader::LEN)],
            );

            total += 1;
            ev_pos = ev.header.log_pos as usize;
        }

        assert_eq!(total, 20);
        Ok(())
    }

    #[test]
    fn binlog_event_roundtrip() -> io::Result<()> {
        let files = [
            "./test-data/binlogs/bug32407.001",
            "./test-data/binlogs/bug11747887-bin.000003",
            "./test-data/binlogs/update-full-row.binlog",
            "./test-data/binlogs/update-partial-row.binlog",
            "./test-data/binlogs/ver_5_1_23.001",
            "./test-data/binlogs/ver_5_1-wl2325_r.001",
            "./test-data/binlogs/ver_5_1-wl2325_s.001",
            "./test-data/binlogs/ver_trunk_row_v2.001",
            "./test-data/binlogs/write-full-row.binlog",
            "./test-data/binlogs/write-partial-row.binlog",
        ];

        'outer: for file_name in &files {
            let file_data = std::fs::read(file_name)?;
            let mut binlog_file = BinlogFile::new(BinlogVersion::Version4, &file_data[..])?;

            let mut ev_pos = 4;

            while let Some(ev) = binlog_file.next() {
                let ev = ev?;
                let ev_end = ev_pos + ev.header.event_size as usize;
                let binlog_version = binlog_file
                    .fde
                    .get_binlog_version()
                    .unwrap_or(BinlogVersion::Version4);

                let mut output = Vec::new();
                ev.write(binlog_version, &mut output)?;
                assert_eq!(output, &file_data[ev_pos..ev_end]);

                let event = match ev.read_data() {
                    Ok(event) => event.unwrap(),
                    Err(err)
                        if err.kind() == std::io::ErrorKind::Other
                            && ev.header.get_event_type() == Ok(EventType::XID_EVENT)
                            && ev.header.event_size == 0x26
                            && *file_name == "./test-data/binlogs/ver_5_1-wl2325_r.001" =>
                    {
                        // Testfile contains broken xid event.
                        continue 'outer;
                    }
                    other => other.transpose().unwrap()?,
                };

                output = Vec::new();
                event.write(binlog_version, &mut output)?;

                if matches!(event, EventData::UserVarEvent(_)) {
                    // Server may or may not write the flags field, but we will always write it.
                    assert_eq!(&output[..ev.data.len()], &ev.data[..]);
                    assert!(output.len() == ev.data.len() || output.len() == ev.data.len() + 1);
                } else {
                    assert_eq!(output, ev.data);
                }

                ev_pos = ev_end;
            }
        }

        Ok(())
    }
}

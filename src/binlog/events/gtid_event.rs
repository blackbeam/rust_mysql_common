// Copyright (c) 2021 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! GTID event — unified representation for both traditional `GTID_EVENT`
//! (MySQL 5.6+) and `GTID_TAGGED_LOG_EVENT` (MySQL 8.4+).
//!
//! The two wire formats are fundamentally different:
//!
//! * **Untagged** (`GTID_EVENT`): fixed-offset binary with bit-flag tricks
//!   for conditional fields.
//! * **Tagged** (`GTID_TAGGED_LOG_EVENT`): MySQL serialization library format
//!   with variable-length field IDs.
//!
//! This module presents a single public type [`GtidEvent`] with an
//! [`is_tagged`](GtidEvent::is_tagged) helper, matching MySQL's own
//! `Gtid_log_event` design.
//!
//! # References
//!
//! * MySQL Serialization Library format:
//!   <https://dev.mysql.com/doc/dev/mysql-server/latest/PageLibsMysqlSerialization.html>
//! * Gtid_event class reference:
//!   <https://dev.mysql.com/doc/dev/mysql-server/latest/classmysql_1_1binlog_1_1event_1_1Gtid__event.html>

use std::{borrow::Cow, cmp::min, io};

use saturating::Saturating as S;

use crate::{
    binlog::{
        BinlogCtx, BinlogEvent, BinlogStruct,
        consts::{BinlogVersion, EventType, Gno, GtidFlags},
    },
    io::ParseBuf,
    misc::{
        raw::{RawConst, RawFlags, int::*},
        read_varlen_uint, varlen_uint_size, write_varlen_uint,
    },
    packets::Tag,
    proto::{MyDeserialize, MySerialize},
};

use super::BinlogEventHeader;

define_const!(
    ConstU8,
    LogicalTimestampTypecode,
    InvalidLogicalTimestampTypecode("Invalid logical timestamp typecode value for GTID event"),
    2
);

/// Field IDs for the tagged (MySQL serialization library) wire format.
///
/// Refer to `libs/mysql/binlog/event/control_events.h` in the MySQL source.
mod field_id {
    pub const GTID_FLAGS: u64 = 0;
    pub const SID: u64 = 1;
    pub const GNO: u64 = 2;
    pub const TAG: u64 = 3;
    pub const LAST_COMMITTED: u64 = 4;
    pub const SEQUENCE_NUMBER: u64 = 5;
    pub const IMMEDIATE_COMMIT_TIMESTAMP: u64 = 6;
    pub const ORIGINAL_COMMIT_TIMESTAMP: u64 = 7;
    pub const TRANSACTION_LENGTH: u64 = 8;
    pub const IMMEDIATE_SERVER_VERSION: u64 = 9;
    pub const ORIGINAL_SERVER_VERSION: u64 = 10;
    pub const COMMIT_GROUP_TICKET: u64 = 11;
}

/// GTID stands for Global Transaction IDentifier.
///
/// Represents both traditional (`GTID_EVENT`, MySQL 5.6+) and tagged
/// (`GTID_TAGGED_LOG_EVENT`, MySQL 8.4+) GTID events through a single
/// unified type.
///
/// Use [`is_tagged`](Self::is_tagged) to distinguish between the two, or
/// simply check whether [`tag`](Self::tag) returns `Some`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct GtidEvent {
    /// Raw flags value.
    flags: RawFlags<GtidFlags, u8>,
    /// UUID representing the SID.
    sid: [u8; Self::ENCODED_SID_LENGTH],
    /// Group number, second component of GTID.
    ///
    /// Should be an integer between `MIN_GNO` and `MAX_GNO` for GtidEvent
    /// or `0` for AnonymousGtidEvent.
    gno: RawConst<LeU64, Gno>,
    /// GTID tag (MySQL 8.4+). `None` for traditional GTIDs.
    tag: Option<Tag<'static>>,
    /// If defined, then always equal to the constant [`GtidEvent::LOGICAL_TIMESTAMP_TYPECODE`].
    ///
    /// May be missing for 5.6. Will have different value on 5.7.4 and earlier (ignored).
    /// Only used for the untagged wire format.
    lc_typecode: Option<LogicalTimestampTypecode>,
    /// Store the transaction's commit parent `sequence_number`.
    last_committed: RawInt<LeU64>,
    /// The transaction's logical timestamp assigned at prepare phase.
    ///
    /// If it isn't `0` then it must be greater than `last_committed` timestamp.
    sequence_number: RawInt<LeU64>,
    /// Timestamp when the transaction was committed on the nearest master.
    immediate_commit_timestamp: RawInt<LeU56>,
    /// Timestamp when the transaction was committed on the originating master.
    original_commit_timestamp: RawInt<LeU56>,
    /// The packed transaction's length in bytes, including the Gtid.
    tx_length: RawInt<LenEnc>,
    /// The version of the server where the transaction was originally executed.
    original_server_version: RawInt<LeU32>,
    /// The version of the immediate server.
    immediate_server_version: RawInt<LeU32>,
    /// MySQL serialization library format version (tagged format only).
    serialization_version: u8,
    /// Binlog group commit ticket (tagged format only).
    commit_group_ticket: u64,
}

impl GtidEvent {
    pub const POST_HEADER_LENGTH: usize = 1 + Self::ENCODED_SID_LENGTH + 8 + 1 + 16;
    pub const ENCODED_SID_LENGTH: usize = 16;
    pub const LOGICAL_TIMESTAMP_TYPECODE: u8 = 2;
    pub const IMMEDIATE_COMMIT_TIMESTAMP_LENGTH: usize = 7;
    pub const ORIGINAL_COMMIT_TIMESTAMP_LENGTH: usize = 7;
    pub const UNDEFINED_SERVER_VERSION: u32 = 999_999;
    pub const IMMEDIATE_SERVER_VERSION_LENGTH: usize = 4;
    pub const COMMIT_GROUP_TICKET_UNSET: u64 = 0;
    /// MySQL serialization library format version 2, used by tagged GTID events (MySQL 8.4+).
    pub const TAGGED_SERIALIZATION_VERSION_V2: u8 = 2;

    /// Creates a new untagged GTID event.
    pub fn new(sid: [u8; Self::ENCODED_SID_LENGTH], gno: u64) -> Self {
        Self {
            flags: Default::default(),
            sid,
            gno: RawConst::new(gno),
            tag: None,
            lc_typecode: Some(LogicalTimestampTypecode::default()),
            last_committed: Default::default(),
            sequence_number: Default::default(),
            immediate_commit_timestamp: Default::default(),
            original_commit_timestamp: Default::default(),
            tx_length: Default::default(),
            original_server_version: Default::default(),
            immediate_server_version: Default::default(),
            serialization_version: 0,
            commit_group_ticket: Self::COMMIT_GROUP_TICKET_UNSET,
        }
    }

    /// Creates a new tagged GTID event (MySQL 8.4+).
    ///
    /// Tagged GTIDs have the format: `UUID:tag:transaction_id`
    pub fn new_tagged(sid: [u8; Self::ENCODED_SID_LENGTH], tag: Tag<'static>, gno: u64) -> Self {
        Self {
            flags: Default::default(),
            sid,
            gno: RawConst::new(gno),
            tag: Some(tag),
            lc_typecode: None,
            last_committed: Default::default(),
            sequence_number: Default::default(),
            immediate_commit_timestamp: Default::default(),
            original_commit_timestamp: Default::default(),
            tx_length: Default::default(),
            original_server_version: RawInt::new(Self::UNDEFINED_SERVER_VERSION),
            immediate_server_version: RawInt::new(Self::UNDEFINED_SERVER_VERSION),
            serialization_version: Self::TAGGED_SERIALIZATION_VERSION_V2,
            commit_group_ticket: Self::COMMIT_GROUP_TICKET_UNSET,
        }
    }

    /// Returns `true` if this is a tagged GTID event (MySQL 8.4+).
    pub fn is_tagged(&self) -> bool {
        self.tag.is_some()
    }

    /// Returns the correct `EventType` for this event.
    ///
    /// Returns [`EventType::GTID_TAGGED_LOG_EVENT`] for tagged events and
    /// [`EventType::GTID_EVENT`] for untagged events.
    ///
    /// Prefer this over [`BinlogEvent::EVENT_TYPE`], which is a compile-time
    /// constant and always returns [`EventType::GTID_EVENT`].
    pub fn event_type(&self) -> EventType {
        if self.tag.is_some() {
            EventType::GTID_TAGGED_LOG_EVENT
        } else {
            EventType::GTID_EVENT
        }
    }

    /// Defines the `flags` value.
    pub fn with_flags(mut self, flags: GtidFlags) -> Self {
        self.flags = RawFlags::new(flags.bits());
        self
    }

    /// Returns the raw `flags` value.
    pub fn flags_raw(&self) -> u8 {
        self.flags.0
    }

    /// Returns the `flags` value. Unknown bits will be truncated.
    ///
    /// `00000001` – Transaction may have changes logged with SBR.
    ///
    /// In 5.6, 5.7.0-5.7.18, and 8.0.0-8.0.1, this flag is always set. Starting in 5.7.19 and
    /// 8.0.2, this flag is cleared if the transaction only contains row events.
    /// It is set if any part of the transaction is written in statement format.
    pub fn flags(&self) -> GtidFlags {
        self.flags.get()
    }

    /// Defines the `sid` value.
    pub fn with_sid(mut self, sid: [u8; Self::ENCODED_SID_LENGTH]) -> Self {
        self.sid = sid;
        self
    }

    /// Returns the `sid` value.
    ///
    /// `sid` is the UUID representing the SID.
    pub fn sid(&self) -> [u8; Self::ENCODED_SID_LENGTH] {
        self.sid
    }

    /// Defines the `gno` value.
    pub fn with_gno(mut self, gno: u64) -> Self {
        self.gno = RawConst::new(gno);
        self
    }

    /// Returns the `gno` value.
    ///
    /// `gno` is a group number, second component of GTID.
    pub fn gno(&self) -> u64 {
        self.gno.0
    }

    /// Returns the GTID tag, if present (MySQL 8.4+).
    pub fn tag(&self) -> Option<&Tag<'static>> {
        self.tag.as_ref()
    }

    /// Sets the GTID tag, making this a tagged GTID event (MySQL 8.4+).
    pub fn with_tag(mut self, tag: Tag<'static>) -> Self {
        self.tag = Some(tag);
        self.serialization_version = Self::TAGGED_SERIALIZATION_VERSION_V2;
        self
    }

    /// Returns the `lc_typecode` value.
    ///
    /// `lc_typecode` is the type of logical timestamp used in the logical clock fields.
    /// Only meaningful for untagged events.
    pub fn lc_typecode(&self) -> Option<u8> {
        self.lc_typecode.as_ref().map(|x| x.value())
    }

    /// Sets the `lc_typecode` value to [`GtidEvent::LOGICAL_TIMESTAMP_TYPECODE`].
    ///
    /// This is already by default, but `lc_typecode` might be `None` if `Self` is obtained
    /// from an old MySql server via [`MyDeserialize::deserialize`].
    pub fn with_lc_typecode(mut self) -> Self {
        self.lc_typecode = Some(LogicalTimestampTypecode::default());
        self
    }

    /// Sets the `last_committed` value.
    pub fn with_last_committed(mut self, last_committed: u64) -> Self {
        self.last_committed = RawInt::new(last_committed);
        self
    }

    /// Returns the `last_committed` value.
    ///
    /// `last_committed` stores the transaction's commit parent `sequence_number`.
    pub fn last_committed(&self) -> u64 {
        self.last_committed.0
    }

    /// Sets the `sequence_number` value.
    pub fn with_sequence_number(mut self, sequence_number: u64) -> Self {
        self.sequence_number = RawInt::new(sequence_number);
        self
    }

    /// Returns the `sequence_number` value.
    ///
    /// `sequence_number` is the transaction's logical timestamp assigned at prepare phase.
    pub fn sequence_number(&self) -> u64 {
        self.sequence_number.0
    }

    /// Sets the `immediate_commit_timestamp` value.
    pub fn with_immediate_commit_timestamp(mut self, immediate_commit_timestamp: u64) -> Self {
        self.immediate_commit_timestamp = RawInt::new(immediate_commit_timestamp);
        self
    }

    /// Returns the `immediate_commit_timestamp` value.
    ///
    /// `immediate_commit_timestamp` is a timestamp of commit on the immediate master.
    pub fn immediate_commit_timestamp(&self) -> u64 {
        self.immediate_commit_timestamp.0
    }

    /// Sets the `original_commit_timestamp` value.
    pub fn with_original_commit_timestamp(mut self, original_commit_timestamp: u64) -> Self {
        self.original_commit_timestamp = RawInt::new(original_commit_timestamp);
        self
    }

    /// Returns the `original_commit_timestamp` value.
    ///
    /// `original_commit_timestamp` is the timestamp of commit on the originating master.
    pub fn original_commit_timestamp(&self) -> u64 {
        self.original_commit_timestamp.0
    }

    /// Sets the `tx_length` value.
    pub fn with_tx_length(mut self, tx_length: u64) -> Self {
        self.tx_length = RawInt::new(tx_length);
        self
    }

    /// Returns the `tx_length` value.
    ///
    /// `tx_length` is the packed transaction's length in bytes, including the Gtid.
    pub fn tx_length(&self) -> u64 {
        self.tx_length.0
    }

    /// Sets the `original_server_version` value.
    pub fn with_original_server_version(mut self, original_server_version: u32) -> Self {
        self.original_server_version = RawInt::new(original_server_version);
        self
    }

    /// Returns the `original_server_version` value.
    ///
    /// `original_server_version` is the version of the server where the transaction was originally
    /// executed.
    pub fn original_server_version(&self) -> u32 {
        self.original_server_version.0
    }

    /// Sets the `immediate_server_version` value.
    pub fn with_immediate_server_version(mut self, immediate_server_version: u32) -> Self {
        self.immediate_server_version = RawInt::new(immediate_server_version);
        self
    }

    /// Returns the `immediate_server_version` value.
    ///
    /// `immediate_server_version` is the server version of the immediate server.
    pub fn immediate_server_version(&self) -> u32 {
        self.immediate_server_version.0
    }

    /// Returns the `commit_group_ticket` value.
    ///
    /// Only meaningful for tagged events.
    pub fn commit_group_ticket(&self) -> u64 {
        self.commit_group_ticket
    }

    /// Sets the `commit_group_ticket` value.
    pub fn with_commit_group_ticket(mut self, ticket: u64) -> Self {
        self.commit_group_ticket = ticket;
        self
    }
}

// ---------------------------------------------------------------------------
// Variable-length integer encoding (MySQL serialization library format)
// ---------------------------------------------------------------------------

/// Computes the self-inclusive payload size for the MySQL serialization
/// library envelope.
///
/// In MySQL's format, `payload_size` covers all bytes of the serialized event
/// data: the `extra_overhead` bytes that precede `payload_size` on the wire
/// (e.g. the serialization version byte), the varlen encoding of
/// `payload_size` itself, the varlen encoding of `last_non_ignorable_field_id`,
/// and the raw field bytes.  Since `payload_size` appears in its own
/// definition, this function iterates until the value stabilises (at most 2
/// iterations).
fn compute_self_inclusive_payload_size(
    extra_overhead: usize,
    fields_size: usize,
    lnif: u64,
) -> u64 {
    let fixed = extra_overhead as u64 + varlen_uint_size(lnif) as u64 + fields_size as u64;
    // Seed with fields_size as a lower-bound proxy for payload_size when
    // computing the varlen encoding size of payload_size.
    let mut ps = fixed + varlen_uint_size(fields_size as u64) as u64;
    loop {
        let next = fixed + varlen_uint_size(ps) as u64;
        if next == ps {
            return ps;
        }
        ps = next;
    }
}

/// Reads a variable-length signed integer from the MySQL serialization format.
///
/// Signed integers use zig-zag encoding: positive `x` is stored as `x << 1`,
/// negative `x` is stored as `(-(x+1)) << 1 | 1`. The LSB is the sign bit.
///
/// Mirrors `read_varlen_bytes_signed()` from MySQL:
/// <https://github.com/mysql/mysql-server/blob/trunk/libs/mysql/serialization/variable_length_integers.h>
fn read_varlen_int(buf: &mut ParseBuf<'_>) -> io::Result<i64> {
    let unsigned = read_varlen_uint(buf)?;
    let sign = unsigned & 1;
    let magnitude = (unsigned >> 1) as i64;
    if sign != 0 {
        // Use -magnitude - 1 instead of -(magnitude + 1) to avoid
        // overflow when magnitude == i64::MAX (decoding i64::MIN).
        Ok(-magnitude - 1)
    } else {
        Ok(magnitude)
    }
}

/// Writes a variable-length signed integer in the MySQL serialization format.
///
/// Uses zig-zag encoding: positive `x` → `x << 1`, negative `x` → `(-(x+1)) << 1 | 1`.
///
/// Mirrors `write_varlen_bytes_signed()` from MySQL:
/// <https://github.com/mysql/mysql-server/blob/trunk/libs/mysql/serialization/variable_length_integers.h>
fn write_varlen_int(buf: &mut Vec<u8>, value: i64) {
    let unsigned = if value >= 0 {
        (value as u64) << 1
    } else {
        ((-(value + 1)) as u64) << 1 | 1
    };
    write_varlen_uint(buf, unsigned);
}

/// Reads a UUID encoded as 16 sequential varlen uint8 values.
///
/// In the MySQL serialization library, `Uuid` is a fixed-size container
/// (`std::array<unsigned char, 16>`). Each byte is varlen-encoded individually
/// without field IDs or a nested message envelope.
fn read_serialized_uuid(buf: &mut ParseBuf<'_>) -> io::Result<[u8; 16]> {
    let mut uuid = [0u8; 16];
    for (i, byte) in uuid.iter_mut().enumerate() {
        let val = read_varlen_uint(buf)?;
        if val > u8::MAX as u64 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("UUID byte {} out of range: {}", i, val),
            ));
        }
        *byte = val as u8;
    }
    Ok(uuid)
}

/// Writes a UUID as 16 sequential varlen uint8 values.
fn write_serialized_uuid(buf: &mut Vec<u8>, uuid: &[u8; 16]) {
    for &byte in uuid.iter() {
        write_varlen_uint(buf, byte as u64);
    }
}

/// Reads a length-prefixed string from the MySQL serialization format.
fn read_varlen_string<'a>(buf: &mut ParseBuf<'a>) -> io::Result<Cow<'a, str>> {
    let raw_len = read_varlen_uint(buf)?;
    let len: usize = raw_len.try_into().map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("varlen string length {} exceeds platform usize", raw_len),
        )
    })?;

    if buf.len() < len {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "unexpected end of buffer reading varlen string",
        ));
    }

    let bytes = &buf.0[..len];
    buf.0 = &buf.0[len..];

    let s = std::str::from_utf8(bytes)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("invalid UTF-8: {}", e)))?;

    Ok(Cow::Borrowed(s))
}

// ---------------------------------------------------------------------------
// MyDeserialize — dispatches based on event type in BinlogCtx
// ---------------------------------------------------------------------------

impl<'de> MyDeserialize<'de> for GtidEvent {
    const SIZE: Option<usize> = None;
    type Ctx = BinlogCtx<'de>;

    fn deserialize(ctx: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let event_type = EventType::try_from(ctx.event_type_raw)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        match event_type {
            EventType::GTID_EVENT | EventType::ANONYMOUS_GTID_EVENT => {
                Self::deserialize_untagged(buf)
            }
            EventType::GTID_TAGGED_LOG_EVENT => Self::deserialize_tagged(buf),
            _ => Err(io::Error::other("unexpected event type for GtidEvent")),
        }
    }
}

// ---------------------------------------------------------------------------
// Deserialization helpers
// ---------------------------------------------------------------------------

impl GtidEvent {
    /// Deserializes a `GTID_EVENT` / `ANONYMOUS_GTID_EVENT`
    /// (fixed-offset binary format, MySQL 5.6+).
    fn deserialize_untagged(buf: &mut ParseBuf<'_>) -> io::Result<Self> {
        let mut sbuf: ParseBuf<'_> = buf.parse(1 + Self::ENCODED_SID_LENGTH + 8)?;
        let flags = sbuf.parse_unchecked(())?;
        let sid: [u8; Self::ENCODED_SID_LENGTH] = sbuf.parse_unchecked(())?;
        let gno = sbuf.parse_unchecked(())?;

        let mut lc_typecode = None;
        let mut last_committed = RawInt::new(0);
        let mut sequence_number = RawInt::new(0);
        let mut immediate_commit_timestamp = RawInt::new(0);
        let mut original_commit_timestamp = RawInt::new(0);
        let mut tx_length = RawInt::new(0);

        let mut original_server_version = RawInt::new(Self::UNDEFINED_SERVER_VERSION);
        let mut immediate_server_version = RawInt::new(Self::UNDEFINED_SERVER_VERSION);

        // Buf will be empty for MySql 5.6. Condition will be false for MySql <= 5.7.4
        if !buf.is_empty() && buf.0[0] == Self::LOGICAL_TIMESTAMP_TYPECODE {
            lc_typecode = Some(buf.parse_unchecked(())?);

            let mut sbuf: ParseBuf<'_> = buf.parse(16)?;
            last_committed = sbuf.parse_unchecked(())?;
            sequence_number = sbuf.parse_unchecked(())?;

            if buf.len() >= Self::IMMEDIATE_COMMIT_TIMESTAMP_LENGTH {
                immediate_commit_timestamp = buf.parse_unchecked(())?;
                if immediate_commit_timestamp.0 & (1 << 55) != 0 {
                    immediate_commit_timestamp.0 &= !(1 << 55);
                    original_commit_timestamp = buf.parse(())?;
                } else {
                    // The transaction originated in the previous server
                    original_commit_timestamp = immediate_commit_timestamp;
                }
            }

            if !buf.is_empty() {
                tx_length = buf.parse_unchecked(())?;
            }

            if buf.len() >= Self::IMMEDIATE_SERVER_VERSION_LENGTH {
                immediate_server_version = buf.parse_unchecked(())?;
                if immediate_server_version.0 & (1 << 31) != 0 {
                    immediate_server_version.0 &= !(1 << 31);
                    original_server_version = buf.parse(())?;
                } else {
                    original_server_version = immediate_server_version;
                }
            }
        }

        Ok(Self {
            flags,
            sid,
            gno,
            tag: None,
            lc_typecode,
            last_committed,
            sequence_number,
            immediate_commit_timestamp,
            original_commit_timestamp,
            tx_length,
            original_server_version,
            immediate_server_version,
            serialization_version: 0,
            commit_group_ticket: Self::COMMIT_GROUP_TICKET_UNSET,
        })
    }

    /// Deserializes a `GTID_TAGGED_LOG_EVENT`
    /// (MySQL serialization library format, MySQL 8.4+).
    fn deserialize_tagged(buf: &mut ParseBuf<'_>) -> io::Result<Self> {
        if buf.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected end reading serialization version",
            ));
        }
        let serialization_version = buf.0[0];
        buf.0 = &buf.0[1..];

        match serialization_version {
            Self::TAGGED_SERIALIZATION_VERSION_V2 => {
                Self::deserialize_tagged_v2(serialization_version, buf)
            }
            v => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unsupported tagged GTID serialization version {v}"),
            )),
        }
    }

    /// Deserializes the v2 tagged GTID format (MySQL 8.4+).
    ///
    /// Message envelope: `<payload_size> <last_non_ignorable_field_id> { <field_id> <field_data> }*`
    /// (the version byte has already been consumed by the caller).
    fn deserialize_tagged_v2(
        serialization_version: u8,
        buf: &mut ParseBuf<'_>,
    ) -> io::Result<Self> {
        // Track how many bytes the envelope consumes so we can derive the
        // field byte count from payload_size without hardcoded arithmetic.
        // Include the version byte already consumed by the caller.
        let buf_len_at_start = buf.len() + std::mem::size_of_val(&serialization_version);

        let payload_size = read_varlen_uint(buf)?;
        let last_non_ignorable_field_id = read_varlen_uint(buf)?;

        // payload_size covers all bytes of the serialized event data: the
        // version byte, its own varlen encoding, the lnif encoding, and the
        // field bytes.  Subtract the envelope bytes we already consumed to
        // get the field byte count.
        let envelope_consumed = (buf_len_at_start - buf.len()) as u64;
        if payload_size < envelope_consumed {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "payload_size ({}) is smaller than envelope overhead ({})",
                    payload_size, envelope_consumed,
                ),
            ));
        }
        let fields_len = (payload_size - envelope_consumed) as usize;
        if buf.len() < fields_len {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                format!(
                    "buffer has {} bytes but payload declares {} field bytes",
                    buf.len(),
                    fields_len,
                ),
            ));
        }
        let mut payload_buf = ParseBuf(&buf.0[..fields_len]);
        buf.0 = &buf.0[fields_len..];

        // Initialize fields with defaults
        let mut flags = RawFlags::new(0);
        let mut sid = [0u8; 16];
        let mut tag: Option<Tag<'static>> = None;
        let mut gno = 0u64;
        let mut last_committed = 0u64;
        let mut sequence_number = 0u64;
        let mut immediate_commit_timestamp = 0u64;
        let mut original_commit_timestamp = 0u64;
        let mut tx_length = 0u64;
        let mut original_server_version = Self::UNDEFINED_SERVER_VERSION;
        let mut immediate_server_version = Self::UNDEFINED_SERVER_VERSION;
        let mut commit_group_ticket = Self::COMMIT_GROUP_TICKET_UNSET;
        let mut seen_original_commit_timestamp = false;
        let mut seen_original_server_version = false;

        // Read field pairs until payload is consumed
        while !payload_buf.is_empty() {
            let fid = read_varlen_uint(&mut payload_buf)?;

            match fid {
                field_id::GTID_FLAGS => {
                    let val = read_varlen_uint(&mut payload_buf)?;
                    if val > u8::MAX as u64 {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("gtid_flags value out of u8 range: {}", val),
                        ));
                    }
                    flags = RawFlags::new(val as u8);
                }
                field_id::SID => {
                    sid = read_serialized_uuid(&mut payload_buf)?;
                }
                field_id::GNO => {
                    // GNO is int64_t in MySQL (rpl_gno), uses signed varlen encoding
                    let val = read_varlen_int(&mut payload_buf)?;
                    if val < 0 {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("negative gno value: {}", val),
                        ));
                    }
                    gno = val as u64;
                }
                field_id::TAG => {
                    let tag_str = read_varlen_string(&mut payload_buf)?;
                    tag = Some(
                        Tag::new(tag_str.into_owned())
                            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
                    );
                }
                field_id::LAST_COMMITTED => {
                    // last_committed is int64_t in MySQL, uses signed varlen encoding
                    let val = read_varlen_int(&mut payload_buf)?;
                    if val < 0 {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("negative last_committed value: {}", val),
                        ));
                    }
                    last_committed = val as u64;
                }
                field_id::SEQUENCE_NUMBER => {
                    // sequence_number is int64_t in MySQL, uses signed varlen encoding
                    let val = read_varlen_int(&mut payload_buf)?;
                    if val < 0 {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("negative sequence_number value: {}", val),
                        ));
                    }
                    sequence_number = val as u64;
                }
                field_id::IMMEDIATE_COMMIT_TIMESTAMP => {
                    immediate_commit_timestamp = read_varlen_uint(&mut payload_buf)?;
                }
                field_id::ORIGINAL_COMMIT_TIMESTAMP => {
                    original_commit_timestamp = read_varlen_uint(&mut payload_buf)?;
                    seen_original_commit_timestamp = true;
                }
                field_id::TRANSACTION_LENGTH => {
                    tx_length = read_varlen_uint(&mut payload_buf)?;
                }
                field_id::IMMEDIATE_SERVER_VERSION => {
                    let val = read_varlen_uint(&mut payload_buf)?;
                    if val > u32::MAX as u64 {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("immediate_server_version out of u32 range: {}", val),
                        ));
                    }
                    immediate_server_version = val as u32;
                }
                field_id::ORIGINAL_SERVER_VERSION => {
                    let val = read_varlen_uint(&mut payload_buf)?;
                    if val > u32::MAX as u64 {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("original_server_version out of u32 range: {}", val),
                        ));
                    }
                    original_server_version = val as u32;
                    seen_original_server_version = true;
                }
                field_id::COMMIT_GROUP_TICKET => {
                    commit_group_ticket = read_varlen_uint(&mut payload_buf)?;
                }
                _ => {
                    // Unknown field — cannot skip reliably without knowing the
                    // field type.
                    if fid <= last_non_ignorable_field_id {
                        // This field is non-ignorable: the producer considers it
                        // essential for correct interpretation of the event.
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!(
                                "unknown non-ignorable field {} in GTID_TAGGED_LOG_EVENT \
                                 (last_non_ignorable_field_id = {})",
                                fid, last_non_ignorable_field_id,
                            ),
                        ));
                    }
                    // Ignorable unknown field — stop parsing since we cannot
                    // determine its length to skip over it.
                    break;
                }
            }
        }

        // Default original values to immediate when not explicitly encoded
        if !seen_original_commit_timestamp {
            original_commit_timestamp = immediate_commit_timestamp;
        }
        if !seen_original_server_version {
            original_server_version = immediate_server_version;
        }

        // Tag is required for this event type
        let tag = tag.ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "GTID_TAGGED_LOG_EVENT missing tag field",
            )
        })?;

        Ok(Self {
            flags,
            sid,
            gno: RawConst::new(gno),
            tag: Some(tag),
            lc_typecode: None,
            last_committed: RawInt::new(last_committed),
            sequence_number: RawInt::new(sequence_number),
            immediate_commit_timestamp: RawInt::new(immediate_commit_timestamp),
            original_commit_timestamp: RawInt::new(original_commit_timestamp),
            tx_length: RawInt::new(tx_length),
            original_server_version: RawInt::new(original_server_version),
            immediate_server_version: RawInt::new(immediate_server_version),
            serialization_version,
            commit_group_ticket,
        })
    }
}

// ---------------------------------------------------------------------------
// MySerialize — dispatches between untagged and tagged wire formats
// ---------------------------------------------------------------------------

impl MySerialize for GtidEvent {
    fn serialize(&self, buf: &mut Vec<u8>) {
        if self.tag.is_some() {
            self.serialize_tagged(buf);
        } else {
            self.serialize_untagged(buf);
        }
    }
}

impl GtidEvent {
    fn serialize_untagged(&self, buf: &mut Vec<u8>) {
        self.flags.serialize(&mut *buf);
        self.sid.serialize(&mut *buf);
        self.gno.serialize(&mut *buf);
        match self.lc_typecode {
            Some(lc_typecode) => lc_typecode.serialize(&mut *buf),
            None => return,
        };
        self.last_committed.serialize(&mut *buf);
        self.sequence_number.serialize(&mut *buf);

        let mut immediate_commit_timestamp_with_flag = *self.immediate_commit_timestamp;
        if self.immediate_commit_timestamp != self.original_commit_timestamp {
            immediate_commit_timestamp_with_flag |= 1 << 55;
        } else {
            immediate_commit_timestamp_with_flag &= !(1 << 55);
        }
        RawInt::<LeU56>::new(immediate_commit_timestamp_with_flag).serialize(&mut *buf);

        if self.immediate_commit_timestamp != self.original_commit_timestamp {
            self.original_commit_timestamp.serialize(&mut *buf);
        }

        self.tx_length.serialize(&mut *buf);

        let mut immediate_server_version_with_flag = *self.immediate_server_version;
        if self.immediate_server_version != self.original_server_version {
            immediate_server_version_with_flag |= 1 << 31;
        } else {
            immediate_server_version_with_flag &= !(1 << 31);
        }
        RawInt::<LeU32>::new(immediate_server_version_with_flag).serialize(&mut *buf);

        if self.immediate_server_version != self.original_server_version {
            self.original_server_version.serialize(&mut *buf);
        }
    }

    fn serialize_tagged(&self, buf: &mut Vec<u8>) {
        match self.serialization_version {
            Self::TAGGED_SERIALIZATION_VERSION_V2 => self.serialize_tagged_v2(buf),
            // Unreachable: deserialize_tagged rejects unknown versions, and
            // constructors/with_tag always set TAGGED_SERIALIZATION_VERSION_V2.
            _ => unreachable!(
                "unsupported tagged GTID serialization version {}",
                self.serialization_version
            ),
        }
    }

    /// Writes all tagged v2 field pairs (field_id + field_data) into `fields`.
    fn write_tagged_fields_v2(&self, fields: &mut Vec<u8>) {
        // Field 0: gtid_flags (always encoded, uint8_t)
        write_varlen_uint(fields, field_id::GTID_FLAGS);
        write_varlen_uint(fields, self.flags.0 as u64);

        // Field 1: UUID/SID (16 sequential varlen uint8 values)
        write_varlen_uint(fields, field_id::SID);
        write_serialized_uuid(fields, &self.sid);

        // Field 2: GNO (int64_t — uses signed varlen encoding)
        assert!(self.gno.0 <= i64::MAX as u64, "gno exceeds i64::MAX");
        write_varlen_uint(fields, field_id::GNO);
        write_varlen_int(fields, self.gno.0 as i64);

        // Field 3: tag (string — length-prefixed)
        write_varlen_uint(fields, field_id::TAG);
        let tag_bytes = self
            .tag
            .as_ref()
            .map(|t| t.as_str().as_bytes())
            .unwrap_or(b"");
        write_varlen_uint(fields, tag_bytes.len() as u64);
        fields.extend_from_slice(tag_bytes);

        // Field 4: last_committed (int64_t — uses signed varlen encoding)
        assert!(
            self.last_committed.0 <= i64::MAX as u64,
            "last_committed exceeds i64::MAX"
        );
        write_varlen_uint(fields, field_id::LAST_COMMITTED);
        write_varlen_int(fields, self.last_committed.0 as i64);

        // Field 5: sequence_number (int64_t — uses signed varlen encoding)
        assert!(
            self.sequence_number.0 <= i64::MAX as u64,
            "sequence_number exceeds i64::MAX"
        );
        write_varlen_uint(fields, field_id::SEQUENCE_NUMBER);
        write_varlen_int(fields, self.sequence_number.0 as i64);

        // Field 6: immediate_commit_timestamp (uint64_t)
        write_varlen_uint(fields, field_id::IMMEDIATE_COMMIT_TIMESTAMP);
        write_varlen_uint(fields, self.immediate_commit_timestamp.0);

        // Field 7: original_commit_timestamp (uint64_t, only if different)
        if self.original_commit_timestamp != self.immediate_commit_timestamp {
            write_varlen_uint(fields, field_id::ORIGINAL_COMMIT_TIMESTAMP);
            write_varlen_uint(fields, self.original_commit_timestamp.0);
        }

        // Field 8: transaction_length (uint64_t)
        write_varlen_uint(fields, field_id::TRANSACTION_LENGTH);
        write_varlen_uint(fields, self.tx_length.0);

        // Field 9: immediate_server_version (uint32_t)
        write_varlen_uint(fields, field_id::IMMEDIATE_SERVER_VERSION);
        write_varlen_uint(fields, self.immediate_server_version.0 as u64);

        // Field 10: original_server_version (uint32_t, only if different)
        if self.original_server_version != self.immediate_server_version {
            write_varlen_uint(fields, field_id::ORIGINAL_SERVER_VERSION);
            write_varlen_uint(fields, self.original_server_version.0 as u64);
        }

        // Field 11: commit_group_ticket (uint64_t, only if set)
        if self.commit_group_ticket != Self::COMMIT_GROUP_TICKET_UNSET {
            write_varlen_uint(fields, field_id::COMMIT_GROUP_TICKET);
            write_varlen_uint(fields, self.commit_group_ticket);
        }
    }

    fn serialize_tagged_v2(&self, buf: &mut Vec<u8>) {
        let mut fields = Vec::new();
        self.write_tagged_fields_v2(&mut fields);

        // MySQL's serialization library marks all GTID fields as ignorable,
        // so last_non_ignorable_field_id is always 0.
        let last_non_ignorable_field_id: u64 = 0;
        let version_byte_size = std::mem::size_of_val(&self.serialization_version);
        let payload_size = compute_self_inclusive_payload_size(
            version_byte_size,
            fields.len(),
            last_non_ignorable_field_id,
        );

        // Write envelope
        buf.reserve(
            1 + varlen_uint_size(payload_size)
                + varlen_uint_size(last_non_ignorable_field_id)
                + fields.len(),
        );
        buf.push(self.serialization_version);
        write_varlen_uint(buf, payload_size);
        write_varlen_uint(buf, last_non_ignorable_field_id);
        buf.extend_from_slice(&fields);
    }
}

// ---------------------------------------------------------------------------
// BinlogStruct / BinlogEvent
// ---------------------------------------------------------------------------

impl<'a> BinlogStruct<'a> for GtidEvent {
    fn len(&self, _version: BinlogVersion) -> usize {
        if self.tag.is_some() {
            self.len_tagged()
        } else {
            self.len_untagged()
        }
    }
}

impl GtidEvent {
    fn len_untagged(&self) -> usize {
        let mut len = S(0);

        // post header
        len += S(1); // flags
        len += S(Self::ENCODED_SID_LENGTH); // sid
        len += S(8); // gno
        len += S(1); // lc_typecode
        len += S(8); // last_committed
        len += S(8); // sequence_number

        len += S(7); // immediate_commit_timestamp
        if self.immediate_commit_timestamp != self.original_commit_timestamp {
            len += S(7); // original_commit_timestamp
        }

        len += S(crate::misc::lenenc_int_len(*self.tx_length) as usize); // tx_length
        len += S(4); // immediate_server_version
        if self.immediate_server_version != self.original_server_version {
            len += S(4); // original_server_version
        }

        min(len.0, u32::MAX as usize - BinlogEventHeader::LEN)
    }

    fn len_tagged(&self) -> usize {
        match self.serialization_version {
            Self::TAGGED_SERIALIZATION_VERSION_V2 => self.len_tagged_v2(),
            // Unreachable: deserialize_tagged rejects unknown versions, and
            // constructors/with_tag always set TAGGED_SERIALIZATION_VERSION_V2.
            _ => unreachable!(
                "unsupported tagged GTID serialization version {}",
                self.serialization_version
            ),
        }
    }

    fn len_tagged_v2(&self) -> usize {
        let mut fields = Vec::new();
        self.write_tagged_fields_v2(&mut fields);

        let last_non_ignorable_field_id: u64 = 0;
        let version_byte_size = std::mem::size_of_val(&self.serialization_version);
        let total_size = compute_self_inclusive_payload_size(
            version_byte_size,
            fields.len(),
            last_non_ignorable_field_id,
        ) as usize;

        min(total_size, u32::MAX as usize - BinlogEventHeader::LEN)
    }
}

impl<'a> BinlogEvent<'a> for GtidEvent {
    /// Always returns `GTID_EVENT` (the untagged type) to satisfy the trait
    /// contract.  This constant is **not used** for dispatch — the binlog
    /// event reader selects the correct deserializer via
    /// [`BinlogEventHeader::event_type_raw`], which distinguishes
    /// `GTID_EVENT` (0x21) from `GTID_TAGGED_LOG_EVENT` (0x2a).
    ///
    /// For the per-instance event type use [`GtidEvent::event_type()`].
    const EVENT_TYPE: EventType = EventType::GTID_EVENT;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn varlen_uint_roundtrip() {
        let test_cases: &[u64] = &[
            0,
            1,
            63,
            64,
            127,
            128,
            255,
            256,
            0x3FFF,
            0x4000,
            0x1F_FFFF,
            0x20_0000,
            0x0FFF_FFFF,
            0x1000_0000,
            0x07_FFFF_FFFF,
            0x08_0000_0000,
            0x03FF_FFFF_FFFF,
            0x0400_0000_0000,
            0x01_FFFF_FFFF_FFFF,
            0x02_0000_0000_0000,
            0x00FF_FFFF_FFFF_FFFF,
            0x0100_0000_0000_0000,
            u64::MAX / 2,
            u64::MAX,
        ];

        for &value in test_cases {
            let mut buf = Vec::new();
            write_varlen_uint(&mut buf, value);

            let mut parse_buf = ParseBuf(&buf);
            let decoded = read_varlen_uint(&mut parse_buf).unwrap();

            assert_eq!(
                value, decoded,
                "roundtrip failed for {} (0x{:X}), encoded {:?}",
                value, value, buf
            );
            assert!(
                parse_buf.is_empty(),
                "leftover bytes for {} (0x{:X})",
                value,
                value
            );
        }
    }

    #[test]
    fn varlen_uint_byte_count() {
        // 1 byte: values 0-127
        let mut buf = Vec::new();
        write_varlen_uint(&mut buf, 0);
        assert_eq!(buf.len(), 1);

        buf.clear();
        write_varlen_uint(&mut buf, 127);
        assert_eq!(buf.len(), 1);

        // 2 bytes: values 128-16383
        buf.clear();
        write_varlen_uint(&mut buf, 128);
        assert_eq!(buf.len(), 2);

        // 9 bytes: very large values
        buf.clear();
        write_varlen_uint(&mut buf, u64::MAX);
        assert_eq!(buf.len(), 9);
    }

    #[test]
    fn varlen_int_roundtrip() {
        let test_cases: &[i64] = &[
            0,
            1,
            -1,
            127,
            -128,
            i64::MAX,
            i64::MIN,
            i64::MAX / 2,
            i64::MIN / 2,
            42,
            -42,
        ];

        for &value in test_cases {
            let mut buf = Vec::new();
            write_varlen_int(&mut buf, value);

            let mut parse_buf = ParseBuf(&buf);
            let decoded = read_varlen_int(&mut parse_buf).unwrap();

            assert_eq!(
                value, decoded,
                "signed roundtrip failed for {} (0x{:X})",
                value, value
            );
            assert!(parse_buf.is_empty(), "leftover bytes for signed {}", value);
        }
    }

    #[test]
    fn tagged_event_creation() {
        let sid = [1u8; 16];
        let tag = Tag::new("domain_1").unwrap();
        let event = GtidEvent::new_tagged(sid, tag, 42);

        assert!(event.is_tagged());
        assert_eq!(event.sid(), sid);
        assert_eq!(event.tag().unwrap().as_str(), "domain_1");
        assert_eq!(event.gno(), 42);
        assert_eq!(event.flags_raw(), 0);
    }

    #[test]
    fn untagged_event_creation() {
        let sid = [1u8; 16];
        let event = GtidEvent::new(sid, 42);

        assert!(!event.is_tagged());
        assert_eq!(event.sid(), sid);
        assert!(event.tag().is_none());
        assert_eq!(event.gno(), 42);
    }

    #[test]
    fn tagged_event_serialize_deserialize_roundtrip() {
        let sid = *uuid::Uuid::parse_str("3E11FA47-71CA-11E1-9E33-C80AA9429562")
            .unwrap()
            .as_bytes();

        let tag = Tag::new("domain_1").unwrap();
        let event = GtidEvent::new_tagged(sid, tag, 42)
            .with_last_committed(10)
            .with_sequence_number(11)
            .with_immediate_commit_timestamp(1234567890)
            .with_original_commit_timestamp(1234567890)
            .with_tx_length(256)
            .with_immediate_server_version(90200)
            .with_original_server_version(90200);

        // Serialize
        let mut buf = Vec::new();
        event.serialize(&mut buf);

        // Deserialize
        let mut parse_buf = ParseBuf(&buf);
        let decoded = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap();

        assert!(decoded.is_tagged());
        assert_eq!(decoded.sid(), event.sid());
        assert_eq!(
            decoded.tag().unwrap().as_str(),
            event.tag().unwrap().as_str()
        );
        assert_eq!(decoded.gno(), event.gno());
        assert_eq!(decoded.last_committed(), event.last_committed());
        assert_eq!(decoded.sequence_number(), event.sequence_number());
        assert_eq!(
            decoded.immediate_commit_timestamp(),
            event.immediate_commit_timestamp()
        );
        assert_eq!(
            decoded.original_commit_timestamp(),
            event.original_commit_timestamp()
        );
        assert_eq!(decoded.tx_length(), event.tx_length());
        assert_eq!(
            decoded.immediate_server_version(),
            event.immediate_server_version()
        );
        assert_eq!(
            decoded.original_server_version(),
            event.original_server_version()
        );
    }

    #[test]
    fn tagged_event_with_different_timestamps() {
        let sid = [0xABu8; 16];
        let tag = Tag::new("backup").unwrap();
        let event = GtidEvent::new_tagged(sid, tag, 1)
            .with_immediate_commit_timestamp(2000)
            .with_original_commit_timestamp(1000)
            .with_immediate_server_version(90200)
            .with_original_server_version(90100);

        let mut buf = Vec::new();
        event.serialize(&mut buf);

        let mut parse_buf = ParseBuf(&buf);
        let decoded = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap();

        assert_eq!(decoded.immediate_commit_timestamp(), 2000);
        assert_eq!(decoded.original_commit_timestamp(), 1000);
        assert_eq!(decoded.immediate_server_version(), 90200);
        assert_eq!(decoded.original_server_version(), 90100);
    }

    #[test]
    fn tagged_event_roundtrip_preserves_explicit_zero_originals() {
        let sid = [0xCDu8; 16];
        let tag = Tag::new("roundtrip").unwrap();

        // Case 1: original_commit_timestamp explicitly 0 while immediate is non-zero
        let event = GtidEvent::new_tagged(sid, tag.clone(), 5)
            .with_immediate_commit_timestamp(99999)
            .with_original_commit_timestamp(0)
            .with_immediate_server_version(90200)
            .with_original_server_version(90200);

        let mut buf = Vec::new();
        event.serialize(&mut buf);

        let mut parse_buf = ParseBuf(&buf);
        let decoded = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap();

        assert_eq!(decoded.original_commit_timestamp(), 0);
        assert_eq!(decoded.immediate_commit_timestamp(), 99999);

        // Case 2: original_server_version explicitly UNDEFINED while immediate is defined
        let event = GtidEvent::new_tagged(sid, tag, 5)
            .with_immediate_commit_timestamp(50000)
            .with_original_commit_timestamp(50000)
            .with_immediate_server_version(90200)
            .with_original_server_version(GtidEvent::UNDEFINED_SERVER_VERSION);

        buf.clear();
        event.serialize(&mut buf);

        let mut parse_buf = ParseBuf(&buf);
        let decoded = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap();

        assert_eq!(
            decoded.original_server_version(),
            GtidEvent::UNDEFINED_SERVER_VERSION,
        );
        assert_eq!(decoded.immediate_server_version(), 90200);
    }

    #[test]
    fn deserialize_rejects_unknown_non_ignorable_field() {
        let mut fields = Vec::new();

        // Field 0: GTID_FLAGS = 0
        write_varlen_uint(&mut fields, field_id::GTID_FLAGS);
        write_varlen_uint(&mut fields, 0);

        // Insert an unknown field ID (e.g. 99) that is within the non-ignorable range
        write_varlen_uint(&mut fields, 99);
        write_varlen_uint(&mut fields, 0); // dummy value

        let last_non_ignorable: u64 = 100;
        let fields_size = fields.len() as u64;
        let payload_size = fields_size
            + varlen_uint_size(fields_size) as u64
            + varlen_uint_size(last_non_ignorable) as u64;

        let mut buf = Vec::new();
        buf.push(GtidEvent::TAGGED_SERIALIZATION_VERSION_V2);
        write_varlen_uint(&mut buf, payload_size);
        write_varlen_uint(&mut buf, last_non_ignorable);
        buf.extend_from_slice(&fields);

        let mut parse_buf = ParseBuf(&buf);
        let result = GtidEvent::deserialize_tagged(&mut parse_buf);

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("unknown non-ignorable field"));
    }

    #[test]
    fn deserialize_rejects_unknown_serialization_version() {
        let mut buf = Vec::new();
        buf.push(0u8); // unknown version
        write_varlen_uint(&mut buf, 10); // payload_size (arbitrary)
        write_varlen_uint(&mut buf, 0); // lnif

        let mut parse_buf = ParseBuf(&buf);
        let result = GtidEvent::deserialize_tagged(&mut parse_buf);

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(
            err.to_string()
                .contains("unsupported tagged GTID serialization version"),
        );
    }

    /// Helper to build a raw serialized tagged GTID event payload from
    /// individual field (id, data) pairs.
    fn build_raw_tagged_payload(field_pairs: &[(u64, Vec<u8>)]) -> Vec<u8> {
        build_raw_tagged_payload_with_lnif(field_pairs, field_id::IMMEDIATE_SERVER_VERSION)
    }

    fn build_raw_tagged_payload_with_lnif(
        field_pairs: &[(u64, Vec<u8>)],
        last_non_ignorable: u64,
    ) -> Vec<u8> {
        let mut fields = Vec::new();
        for (fid, data) in field_pairs {
            write_varlen_uint(&mut fields, *fid);
            fields.extend_from_slice(data);
        }

        let version_byte: u8 = GtidEvent::TAGGED_SERIALIZATION_VERSION_V2;
        let version_byte_size = std::mem::size_of_val(&version_byte);
        let payload_size = compute_self_inclusive_payload_size(
            version_byte_size,
            fields.len(),
            last_non_ignorable,
        );

        let mut buf = Vec::new();
        buf.push(version_byte); // serialization_version
        write_varlen_uint(&mut buf, payload_size);
        write_varlen_uint(&mut buf, last_non_ignorable);
        buf.extend_from_slice(&fields);
        buf
    }

    fn encode_varlen_uint(value: u64) -> Vec<u8> {
        let mut buf = Vec::new();
        write_varlen_uint(&mut buf, value);
        buf
    }

    fn encode_varlen_int(value: i64) -> Vec<u8> {
        let mut buf = Vec::new();
        write_varlen_int(&mut buf, value);
        buf
    }

    fn encode_uuid(uuid: &[u8; 16]) -> Vec<u8> {
        let mut buf = Vec::new();
        write_serialized_uuid(&mut buf, uuid);
        buf
    }

    fn encode_varlen_string(s: &str) -> Vec<u8> {
        let mut buf = Vec::new();
        write_varlen_uint(&mut buf, s.len() as u64);
        buf.extend_from_slice(s.as_bytes());
        buf
    }

    #[test]
    fn deserialize_rejects_negative_signed_fields() {
        let sid = [0u8; 16];

        // Build a valid baseline with all required fields, but negative gno
        let buf = build_raw_tagged_payload(&[
            (field_id::GTID_FLAGS, encode_varlen_uint(0)),
            (field_id::SID, encode_uuid(&sid)),
            (field_id::GNO, encode_varlen_int(-1)),
            (field_id::TAG, encode_varlen_string("test")),
            (field_id::LAST_COMMITTED, encode_varlen_int(0)),
            (field_id::SEQUENCE_NUMBER, encode_varlen_int(0)),
            (field_id::IMMEDIATE_COMMIT_TIMESTAMP, encode_varlen_uint(0)),
            (field_id::TRANSACTION_LENGTH, encode_varlen_uint(0)),
            (field_id::IMMEDIATE_SERVER_VERSION, encode_varlen_uint(0)),
        ]);

        let mut parse_buf = ParseBuf(&buf);
        let err = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap_err();
        assert!(err.to_string().contains("negative gno"), "{}", err);

        // Negative last_committed
        let buf = build_raw_tagged_payload(&[
            (field_id::GTID_FLAGS, encode_varlen_uint(0)),
            (field_id::SID, encode_uuid(&sid)),
            (field_id::GNO, encode_varlen_int(1)),
            (field_id::TAG, encode_varlen_string("test")),
            (field_id::LAST_COMMITTED, encode_varlen_int(-5)),
            (field_id::SEQUENCE_NUMBER, encode_varlen_int(0)),
            (field_id::IMMEDIATE_COMMIT_TIMESTAMP, encode_varlen_uint(0)),
            (field_id::TRANSACTION_LENGTH, encode_varlen_uint(0)),
            (field_id::IMMEDIATE_SERVER_VERSION, encode_varlen_uint(0)),
        ]);

        let mut parse_buf = ParseBuf(&buf);
        let err = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap_err();
        assert!(
            err.to_string().contains("negative last_committed"),
            "{}",
            err
        );

        // Negative sequence_number
        let buf = build_raw_tagged_payload(&[
            (field_id::GTID_FLAGS, encode_varlen_uint(0)),
            (field_id::SID, encode_uuid(&sid)),
            (field_id::GNO, encode_varlen_int(1)),
            (field_id::TAG, encode_varlen_string("test")),
            (field_id::LAST_COMMITTED, encode_varlen_int(0)),
            (field_id::SEQUENCE_NUMBER, encode_varlen_int(-100)),
            (field_id::IMMEDIATE_COMMIT_TIMESTAMP, encode_varlen_uint(0)),
            (field_id::TRANSACTION_LENGTH, encode_varlen_uint(0)),
            (field_id::IMMEDIATE_SERVER_VERSION, encode_varlen_uint(0)),
        ]);

        let mut parse_buf = ParseBuf(&buf);
        let err = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap_err();
        assert!(
            err.to_string().contains("negative sequence_number"),
            "{}",
            err
        );
    }

    #[test]
    fn deserialize_rejects_missing_tag() {
        let sid = [0u8; 16];
        // All required fields except TAG
        let buf = build_raw_tagged_payload(&[
            (field_id::GTID_FLAGS, encode_varlen_uint(0)),
            (field_id::SID, encode_uuid(&sid)),
            (field_id::GNO, encode_varlen_int(1)),
            // TAG intentionally omitted
            (field_id::LAST_COMMITTED, encode_varlen_int(0)),
            (field_id::SEQUENCE_NUMBER, encode_varlen_int(0)),
            (field_id::IMMEDIATE_COMMIT_TIMESTAMP, encode_varlen_uint(0)),
            (field_id::TRANSACTION_LENGTH, encode_varlen_uint(0)),
            (field_id::IMMEDIATE_SERVER_VERSION, encode_varlen_uint(0)),
        ]);

        let mut parse_buf = ParseBuf(&buf);
        let err = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap_err();
        assert!(err.to_string().contains("missing tag field"), "{}", err);
    }

    #[test]
    fn deserialize_rejects_empty_buffer() {
        let buf: &[u8] = &[];
        let mut parse_buf = ParseBuf(buf);
        let err = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);
    }

    #[test]
    fn deserialize_rejects_truncated_varlen() {
        // A buffer with just a version byte and a varlen that claims more bytes
        // than available. 0xFF marker means 9 bytes total needed but we only have 3.
        let buf: &[u8] = &[GtidEvent::TAGGED_SERIALIZATION_VERSION_V2, 0xFF, 0x01, 0x02];
        let mut parse_buf = ParseBuf(buf);
        let err = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);
    }

    #[test]
    fn tagged_event_roundtrip_with_max_gno() {
        let sid = [0xFFu8; 16];
        let tag = Tag::new("maxgno").unwrap();
        let event = GtidEvent::new_tagged(sid, tag, i64::MAX as u64)
            .with_last_committed(i64::MAX as u64)
            .with_sequence_number(i64::MAX as u64);

        let mut buf = Vec::new();
        event.serialize(&mut buf);

        let mut parse_buf = ParseBuf(&buf);
        let decoded = GtidEvent::deserialize_tagged(&mut parse_buf).unwrap();

        assert_eq!(decoded.gno(), i64::MAX as u64);
        assert_eq!(decoded.last_committed(), i64::MAX as u64);
        assert_eq!(decoded.sequence_number(), i64::MAX as u64);
    }
}

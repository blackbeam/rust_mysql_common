// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use byteorder::{ByteOrder, LittleEndian as LE, ReadBytesExt, WriteBytesExt};
use lexical::parse;
use regex::bytes::Regex;

use std::borrow::Cow;
use std::cmp::max;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::marker::PhantomData;
use std::ptr;

use crate::constants::{
    CapabilityFlags, ColumnFlags, ColumnType, Command, SessionStateType, StatusFlags,
    MAX_PAYLOAD_LEN, UTF8MB4_GENERAL_CI, UTF8_GENERAL_CI,
};
use crate::io::{ReadMysqlExt, WriteMysqlExt};
use crate::misc::lenenc_str_len;
use crate::value::{ClientSide, SerializationSide, Value};

macro_rules! get_offset_and_len {
    ($buffer:expr, $slice:expr) => {{
        let val = $slice;
        (val.as_ptr() as usize - $buffer.as_ptr() as usize, val.len())
    }};
}

lazy_static! {
    static ref MARIADB_VERSION_RE: Regex =
        { Regex::new(r"^5.5.5-(\d{1,2})\.(\d{1,2})\.(\d{1,3})-MariaDB").unwrap() };
    static ref VERSION_RE: Regex = { Regex::new(r"^(\d{1,2})\.(\d{1,2})\.(\d{1,3})(.*)").unwrap() };
}

/// Represents MySql Column (column packet).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Column {
    payload: Vec<u8>,
    schema: (usize, usize),
    table: (usize, usize),
    org_table: (usize, usize),
    name: (usize, usize),
    org_name: (usize, usize),
    column_length: u32,
    character_set: u16,
    flags: ColumnFlags,
    column_type: ColumnType,
    decimals: u8,
}

/// Converts column-packet payload to an instance of `Column` structure.
pub fn column_from_payload(payload: Vec<u8>) -> io::Result<Column> {
    Column::from_payload(payload)
}

impl Column {
    /// Converts column-packet payload to an instance of `Column` structure.
    fn from_payload(payload: Vec<u8>) -> io::Result<Column> {
        let schema;
        let table;
        let org_table;
        let name;
        let org_name;
        let character_set;
        let column_length;
        let column_type;
        let flags;
        let decimals;

        {
            // Skip "def"
            let mut reader = &payload[4..];
            schema = get_offset_and_len!(payload, read_lenenc_str!(&mut reader)?);
            table = get_offset_and_len!(payload, read_lenenc_str!(&mut reader)?);
            org_table = get_offset_and_len!(payload, read_lenenc_str!(&mut reader)?);
            name = get_offset_and_len!(payload, read_lenenc_str!(&mut reader)?);
            org_name = get_offset_and_len!(payload, read_lenenc_str!(&mut reader)?);
            reader = &reader[1..];
            character_set = reader.read_u16::<LE>()?;
            column_length = reader.read_u32::<LE>()?;
            column_type = reader.read_u8()?;
            flags = reader.read_u16::<LE>()?;
            decimals = reader.read_u8()?;
        }

        Ok(Column {
            schema,
            table,
            org_table,
            name,
            org_name,
            payload,
            column_length,
            character_set,
            flags: ColumnFlags::from_bits_truncate(flags),
            column_type: ColumnType::from(column_type),
            decimals,
        })
    }

    /// Returns value of the column_length field of a column packet.
    pub fn column_length(&self) -> u32 {
        self.column_length
    }

    /// Returns value of the column_type field of a column packet.
    pub fn column_type(&self) -> ColumnType {
        self.column_type
    }

    /// Returns value of the character_set field of a column packet.
    pub fn character_set(&self) -> u16 {
        self.character_set
    }

    /// Returns value of the flags field of a column packet.
    pub fn flags(&self) -> ColumnFlags {
        self.flags
    }

    /// Returns value of the decimals field of a column packet.
    pub fn decimals(&self) -> u8 {
        self.decimals
    }

    /// Returns value of the schema field of a column packet as a byte slice.
    pub fn schema_ref(&self) -> &[u8] {
        &self.payload[self.schema.0..self.schema.0 + self.schema.1]
    }

    /// Returns value of the schema field of a column packet as a string (lossy converted).
    pub fn schema_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.schema_ref())
    }

    /// Returns value of the table field of a column packet as a byte slice.
    pub fn table_ref(&self) -> &[u8] {
        &self.payload[self.table.0..self.table.0 + self.table.1]
    }

    /// Returns value of the table field of a column packet as a string (lossy converted).
    pub fn table_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.table_ref())
    }

    /// Returns value of the org_table field of a column packet as a byte slice.
    ///
    /// "org_table" is for original table name.
    pub fn org_table_ref(&self) -> &[u8] {
        &self.payload[self.org_table.0..self.org_table.0 + self.org_table.1]
    }

    /// Returns value of the org_table field of a column packet as a string (lossy converted).
    pub fn org_table_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.org_table_ref())
    }

    /// Returns value of the name field of a column packet as a byte slice.
    pub fn name_ref(&self) -> &[u8] {
        &self.payload[self.name.0..self.name.0 + self.name.1]
    }

    /// Returns value of the name field of a column packet as a string (lossy converted).
    pub fn name_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.name_ref())
    }

    /// Returns value of the org_name field of a column packet as a byte slice.
    ///
    /// "org_name" is for original column name.
    pub fn org_name_ref(&self) -> &[u8] {
        &self.payload[self.org_name.0..self.org_name.0 + self.org_name.1]
    }

    /// Returns value of the org_name field of a column packet as a string (lossy converted).
    pub fn org_name_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.org_name_ref())
    }
}

/// Represents parsed change in session state (part of MySql's Ok packet).
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum SessionStateChange<'a> {
    IsTracked(bool),
    Schema(Cow<'a, [u8]>),
    SystemVariable(Cow<'a, [u8]>, Cow<'a, [u8]>),
    UnknownLayout(Cow<'a, [u8]>),
}

impl<'a> SessionStateChange<'a> {
    pub fn into_owned(self) -> SessionStateChange<'static> {
        match self {
            SessionStateChange::SystemVariable(name, value) => SessionStateChange::SystemVariable(
                name.into_owned().into(),
                value.into_owned().into(),
            ),
            SessionStateChange::Schema(schema) => {
                SessionStateChange::Schema(schema.into_owned().into())
            }
            SessionStateChange::IsTracked(x) => SessionStateChange::IsTracked(x),
            SessionStateChange::UnknownLayout(data) => {
                SessionStateChange::UnknownLayout(data.into_owned().into())
            }
        }
    }
}

/// Represents change in session state (part of MySql's Ok packet).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SessionStateInfo<'a> {
    data_type: SessionStateType,
    data: Cow<'a, [u8]>,
}

impl<'a> SessionStateInfo<'a> {
    pub fn parse(mut payload: &[u8]) -> io::Result<SessionStateInfo<'_>> {
        let data_type = payload.read_u8()?;
        Ok(SessionStateInfo {
            data_type: data_type.into(),
            data: read_lenenc_str!(&mut payload)?.into(),
        })
    }

    pub fn into_owned(self) -> SessionStateInfo<'static> {
        let SessionStateInfo { data_type, data } = self;
        SessionStateInfo {
            data_type,
            data: data.into_owned().into(),
        }
    }

    pub fn data_type(&self) -> SessionStateType {
        self.data_type
    }

    pub fn decode(&self) -> io::Result<SessionStateChange<'_>> {
        let mut reader = self.data.as_ref();
        match self.data_type {
            SessionStateType::SESSION_TRACK_SYSTEM_VARIABLES => {
                let name = read_lenenc_str!(&mut reader)?;
                let value = read_lenenc_str!(&mut reader)?;
                Ok(SessionStateChange::SystemVariable(
                    name.into(),
                    value.into(),
                ))
            }
            SessionStateType::SESSION_TRACK_SCHEMA => {
                let schema = read_lenenc_str!(&mut reader)?;
                Ok(SessionStateChange::Schema(schema.into()))
            }
            SessionStateType::SESSION_TRACK_STATE_CHANGE => {
                let is_tracked = read_lenenc_str!(&mut reader)?;
                Ok(SessionStateChange::IsTracked(is_tracked == b"1"))
            }
            // Layout not specified in documentation
            SessionStateType::SESSION_TRACK_GTIDS
            | SessionStateType::SESSION_TRACK_TRANSACTION_CHARACTERISTICS
            | SessionStateType::SESSION_TRACK_TRANSACTION_STATE => {
                Ok(SessionStateChange::UnknownLayout(self.data.clone()))
            }
        }
    }
}

/// OK packet kind (see _OK packet identifier_ section of [WL#7766][1]).
///
/// [1]: https://dev.mysql.com/worklog/task/?id=7766
#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum OkPacketKind {
    /// This packet terminates a result set (text or binary).
    ResultSetTerminator,
    /// Ok packet that is not a result set terminator.
    Other,
}

/// Represents MySql's Ok packet.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OkPacket<'a> {
    affected_rows: u64,
    last_insert_id: Option<u64>,
    status_flags: StatusFlags,
    warnings: u16,
    info: Option<Cow<'a, [u8]>>,
    session_state_info: Option<SessionStateInfo<'a>>,
}

/// Parses Ok packet from `payload` assuming passed client-server `capabilities`.
pub fn parse_ok_packet(
    payload: &[u8],
    capabilities: CapabilityFlags,
    kind: OkPacketKind,
) -> io::Result<OkPacket<'_>> {
    OkPacket::parse(payload, capabilities, kind)
}

impl<'a> OkPacket<'a> {
    /// Parses Ok packet from `payload` assuming passed client-server `capabilities`.
    fn parse(
        mut payload: &[u8],
        capabilities: CapabilityFlags,
        kind: OkPacketKind,
    ) -> io::Result<OkPacket> {
        let header = payload.read_u8()?;
        let (affected_rows, last_insert_id, status_flags, warnings, info, session_state_info) =
            if kind == OkPacketKind::Other && header == 0x00 {
                let affected_rows = payload.read_lenenc_int()?;
                let last_insert_id = payload.read_lenenc_int()?;
                // We assume that CLIENT_PROTOCOL_41 was set
                let status_flags = StatusFlags::from_bits_truncate(payload.read_u16::<LE>()?);
                let warnings = payload.read_u16::<LE>()?;

                let (info, session_state_info) =
                    if capabilities.contains(CapabilityFlags::CLIENT_SESSION_TRACK) {
                        let info = read_lenenc_str!(&mut payload)?;
                        let session_state_info =
                            if status_flags.contains(StatusFlags::SERVER_SESSION_STATE_CHANGED) {
                                read_lenenc_str!(&mut payload)?
                            } else {
                                &[][..]
                            };
                        (info, session_state_info)
                    } else {
                        (payload, &[][..])
                    };
                (
                    affected_rows,
                    last_insert_id,
                    status_flags,
                    warnings,
                    info,
                    session_state_info,
                )
            } else if kind == OkPacketKind::ResultSetTerminator
                && header == 0xFE
                && payload.len() < 8
            {
                // We assume that CLIENT_PROTOCOL_41 was set
                let warnings = payload.read_u16::<LE>()?;
                let status_flags = StatusFlags::from_bits_truncate(payload.read_u16::<LE>()?);
                (0, 0, status_flags, warnings, &[][..], &[][..])
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid OK_Packet header or length",
                ));
            };

        Ok(OkPacket {
            affected_rows,
            last_insert_id: if last_insert_id == 0 {
                None
            } else {
                Some(last_insert_id)
            },
            status_flags,
            warnings,
            info: if !info.is_empty() {
                Some(info.into())
            } else {
                None
            },
            session_state_info: if !session_state_info.is_empty() {
                Some(SessionStateInfo::parse(session_state_info)?)
            } else {
                None
            },
        })
    }

    pub fn into_owned(self) -> OkPacket<'static> {
        let OkPacket {
            affected_rows,
            last_insert_id,
            status_flags,
            warnings,
            info,
            session_state_info,
        } = self;
        OkPacket {
            affected_rows,
            last_insert_id,
            status_flags,
            warnings,
            info: info.map(|x| x.into_owned().into()),
            session_state_info: session_state_info.map(SessionStateInfo::into_owned),
        }
    }

    /// Value of the affected_rows field of an Ok packet.
    pub fn affected_rows(&self) -> u64 {
        self.affected_rows
    }

    /// Value of the last_insert_id field of an Ok packet.
    pub fn last_insert_id(&self) -> Option<u64> {
        self.last_insert_id
    }

    /// Value of the status_flags field of an Ok packet.
    pub fn status_flags(&self) -> StatusFlags {
        self.status_flags
    }

    /// Value of the warnings field of an Ok packet.
    pub fn warnings(&self) -> u16 {
        self.warnings
    }

    /// Value of the info field of an Ok packet as a byte slice.
    pub fn info_ref(&self) -> Option<&[u8]> {
        self.info.as_ref().map(|x| x.as_ref())
    }

    /// Value of the info field of an Ok packet as a string (lossy converted).
    pub fn info_str(&self) -> Option<Cow<str>> {
        self.info
            .as_ref()
            .map(|x| String::from_utf8_lossy(x.as_ref()))
    }

    pub fn session_state_info(&self) -> Option<&SessionStateInfo<'_>> {
        self.session_state_info.as_ref()
    }
}

/// Progress report information (may be in an error packet of MariaDB server).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProgressReport<'a> {
    stage: u8,
    max_stage: u8,
    progress: u32,
    stage_info: Cow<'a, [u8]>,
}

impl<'a> ProgressReport<'a> {
    fn new(stage: u8, max_stage: u8, progress: u32, stage_info: &[u8]) -> ProgressReport {
        ProgressReport {
            stage,
            max_stage,
            progress,
            stage_info: stage_info.into(),
        }
    }

    /// 1 to max_stage
    pub fn stage(&self) -> u8 {
        self.stage
    }

    pub fn max_stage(&self) -> u8 {
        self.max_stage
    }

    /// Progress as '% * 1000'
    pub fn progress(&self) -> u32 {
        self.progress
    }

    /// Status or state name as a byte slice.
    pub fn stage_info_ref(&self) -> &[u8] {
        &self.stage_info.as_ref()
    }

    /// Status or state name as a string (lossy converted).
    pub fn stage_info_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.stage_info.as_ref())
    }

    pub fn into_owned(self) -> ProgressReport<'static> {
        let ProgressReport {
            stage,
            max_stage,
            progress,
            stage_info,
        } = self;
        ProgressReport {
            stage,
            max_stage,
            progress,
            stage_info: stage_info.into_owned().into(),
        }
    }
}

impl<'a> fmt::Display for ProgressReport<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Stage: {} of {} '{}'  {:.2}% of stage done",
            self.stage(),
            self.max_stage(),
            self.progress(),
            self.stage_info_str()
        )
    }
}

/// MySql error packet.
///
/// May hold an error or a progress report.
#[derive(Debug, Clone, PartialEq)]
pub enum ErrPacket<'a> {
    /// (<error code>, <sql state>, <error message>)
    Error(u16, [u8; 5], Cow<'a, [u8]>),
    Progress(ProgressReport<'a>),
}

/// Parses error packet from `payload` assuming passed client-server `capabilities`.
pub fn parse_err_packet(
    payload: &[u8],
    capabilities: CapabilityFlags,
) -> io::Result<ErrPacket<'_>> {
    ErrPacket::parse(payload, capabilities)
}

impl<'a> ErrPacket<'a> {
    /// Parses error packet from `payload` assuming passed client-server `capabilities`.
    fn parse(mut payload: &[u8], capabilities: CapabilityFlags) -> io::Result<ErrPacket<'_>> {
        if payload.read_u8()? != 0xFF {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid ERR_Packet header",
            ));
        }

        let code = payload.read_u16::<LE>()?;
        // We assume that CLIENT_PROTOCOL_41 was set
        if code == 0xFFFF && capabilities.contains(CapabilityFlags::CLIENT_PROGRESS_OBSOLETE) {
            payload.read_u8()?; // Ignore number of strings.
            let stage = payload.read_u8()?;
            let max_stage = payload.read_u8()?;
            let progress = payload.read_uint::<LE>(3)?;
            let progress_info = read_lenenc_str!(&mut payload)?;
            Ok(ErrPacket::Progress(ProgressReport::new(
                stage,
                max_stage,
                progress as u32,
                progress_info,
            )))
        } else {
            match payload.get(0) {
                Some(b'#') => {
                    let (state, msg) =
                        split_at_or_err!(payload, 6, "EOF while reading error state")?;
                    Ok(ErrPacket::Error(
                        code,
                        unsafe { ptr::read(state.as_ptr().offset(1) as *const [u8; 5]) },
                        msg.into(),
                    ))
                }
                _ => Ok(ErrPacket::Error(
                    code,
                    [b'H', b'Y', b'0', b'0', b'0'],
                    payload.into(),
                )),
            }
        }
    }

    /// Returns false if this error packet contains progress report.
    pub fn is_error(&self) -> bool {
        match *self {
            ErrPacket::Error(..) => true,
            _ => false,
        }
    }

    /// Returns true if this error packet contains progress report.
    pub fn is_progress_report(&self) -> bool {
        !self.is_error()
    }

    /// Will panic if ErrPacket does not contains progress report
    pub fn progress_report(&self) -> &ProgressReport<'_> {
        match *self {
            ErrPacket::Progress(ref progress_report) => progress_report,
            _ => panic!("This ErrPacket does not contains progress report"),
        }
    }

    /// Will panic if ErrPacket contains progress report
    pub fn error_code(&self) -> u16 {
        match *self {
            ErrPacket::Error(code, ..) => code,
            _ => panic!("This ErrPacket contains progress report"),
        }
    }

    /// Will panic if ErrPacket contains progress report
    pub fn sql_state_ref(&self) -> &[u8; 5] {
        match *self {
            ErrPacket::Error(_, ref state, _) => state,
            _ => panic!("This ErrPacket contains progress report"),
        }
    }

    /// Will panic if ErrPacket contains progress report
    pub fn sql_state_str(&self) -> Cow<'_, str> {
        match *self {
            ErrPacket::Error(_, ref state, _) => String::from_utf8_lossy(&state[..]),
            _ => panic!("This ErrPacket contains progress report"),
        }
    }

    /// Will panic if ErrPacket contains progress report
    pub fn message_ref(&self) -> &[u8] {
        match *self {
            ErrPacket::Error(_, _, ref message) => message.as_ref(),
            _ => panic!("This ErrPacket contains progress report"),
        }
    }

    /// Will panic if ErrPacket contains progress report
    pub fn message_str(&self) -> Cow<'_, str> {
        match *self {
            ErrPacket::Error(_, _, ref message) => String::from_utf8_lossy(message.as_ref()),
            _ => panic!("This ErrPacket contains progress report"),
        }
    }

    pub fn into_owned(self) -> ErrPacket<'static> {
        match self {
            ErrPacket::Error(code, state, message) => {
                ErrPacket::Error(code, state, message.into_owned().into())
            }
            ErrPacket::Progress(progress_report) => {
                ErrPacket::Progress(progress_report.into_owned())
            }
        }
    }
}

impl<'a> fmt::Display for ErrPacket<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ErrPacket::Error(..) => write!(
                f,
                "ERROR {} ({}): {}",
                self.error_code(),
                self.sql_state_str(),
                self.message_str()
            ),
            ErrPacket::Progress(ref progress_report) => write!(f, "{}", progress_report),
        }
    }
}

/// Represents MySql's local infile packet.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalInfilePacket<'a> {
    file_name: Cow<'a, [u8]>,
}

/// Will parse payload as a local infile packet.
pub fn parse_local_infile_packet(payload: &[u8]) -> io::Result<LocalInfilePacket<'_>> {
    LocalInfilePacket::parse(payload)
}

impl<'a> LocalInfilePacket<'a> {
    /// Will parse payload as a local infile packet.
    fn parse(mut payload: &[u8]) -> io::Result<LocalInfilePacket<'_>> {
        if payload.read_u8()? != 0xfb {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid LOCAL_INFILE packet header",
            ));
        }

        Ok(LocalInfilePacket {
            file_name: payload.into(),
        })
    }

    /// Value of the file_name field of a local infile packet as a byte slice.
    pub fn file_name_ref(&self) -> &[u8] {
        self.file_name.as_ref()
    }

    /// Value of the file_name field of a local infile packet as a string (lossy converted).
    pub fn file_name_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.file_name.as_ref())
    }

    pub fn into_owned(self) -> LocalInfilePacket<'static> {
        LocalInfilePacket {
            file_name: self.file_name.into_owned().into(),
        }
    }
}

const MYSQL_NATIVE_PASSWORD_PLUGIN_NAME: &[u8] = b"mysql_native_password";
const CACHING_SHA2_PASSWORD_PLUGIN_NAME: &[u8] = b"caching_sha2_password";

/// Authentication plugin
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum AuthPlugin<'a> {
    /// Legacy authentication plugin
    MysqlNativePassword,
    /// Default since MySql v8.0.4
    CachingSha2Password,
    Other(Cow<'a, [u8]>),
}

impl<'a> AuthPlugin<'a> {
    pub fn from_bytes(name: &'a [u8]) -> AuthPlugin<'a> {
        match name {
            CACHING_SHA2_PASSWORD_PLUGIN_NAME => AuthPlugin::CachingSha2Password,
            MYSQL_NATIVE_PASSWORD_PLUGIN_NAME => AuthPlugin::MysqlNativePassword,
            name => AuthPlugin::Other(name.into()),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        match self {
            AuthPlugin::MysqlNativePassword => MYSQL_NATIVE_PASSWORD_PLUGIN_NAME,
            AuthPlugin::CachingSha2Password => CACHING_SHA2_PASSWORD_PLUGIN_NAME,
            AuthPlugin::Other(name) => &*name,
        }
    }

    pub fn into_owned(self) -> AuthPlugin<'static> {
        match self {
            AuthPlugin::CachingSha2Password => AuthPlugin::CachingSha2Password,
            AuthPlugin::MysqlNativePassword => AuthPlugin::MysqlNativePassword,
            AuthPlugin::Other(name) => AuthPlugin::Other(name.into_owned().into()),
        }
    }

    /// Generates auth plugin data for this plugin.
    ///
    /// It'll generate `None` if password is `None` or empty.
    pub fn gen_data(&self, pass: Option<&str>, nonce: &[u8]) -> Option<Vec<u8>> {
        use super::scramble::{scramble_native, scramble_sha256};

        match pass {
            Some(pass) => match self {
                AuthPlugin::CachingSha2Password => {
                    scramble_sha256(nonce, pass.as_bytes()).map(|x| Vec::from(&x[..]))
                }
                AuthPlugin::MysqlNativePassword => {
                    scramble_native(nonce, pass.as_bytes()).map(|x| Vec::from(&x[..]))
                }
                AuthPlugin::Other(_) => None,
            },
            None => None,
        }
    }
}

/// Extra auth-data beyond the initial challenge.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AuthMoreData<'a> {
    data: Cow<'a, [u8]>,
}

impl<'a> AuthMoreData<'a> {
    fn parse(mut payload: &'a [u8]) -> io::Result<Self> {
        match payload.read_u8()? {
            0x01 => Ok(AuthMoreData {
                data: payload.into(),
            }),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid AuthMoreData header",
            )),
        }
    }

    pub fn data(&self) -> &[u8] {
        &*self.data
    }

    pub fn into_owned(self) -> AuthMoreData<'static> {
        AuthMoreData {
            data: self.data.into_owned().into(),
        }
    }
}

/// Parses payload as an auth more data packet.
pub fn parse_auth_more_data(payload: &[u8]) -> io::Result<AuthMoreData<'_>> {
    AuthMoreData::parse(payload)
}

/// Authentication Method Switch Request Packet.
///
/// If both server and client support `CLIENT_PLUGIN_AUTH` capability, server can send this packet
/// to ask client to use another authentication method.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AuthSwitchRequest<'a> {
    auth_plugin: AuthPlugin<'a>,
    plugin_data: Cow<'a, [u8]>,
}

impl<'a> AuthSwitchRequest<'a> {
    fn parse(mut payload: &'a [u8]) -> io::Result<Self> {
        match payload.read_u8()? {
            0xfe => {
                let mut null_offset = 0;
                for byte in payload.iter() {
                    if *byte == 0x00 {
                        break;
                    }
                    null_offset += 1;
                }
                let (auth_plugin, mut payload) =
                    split_at_or_err!(payload, null_offset, "Invalid AuthSwitchRequest packet")?;
                payload.read_u8()?;
                let plugin_data = if payload[payload.len() - 1] == 0 {
                    &payload[..payload.len() - 1]
                } else {
                    payload
                };
                Ok(Self {
                    auth_plugin: AuthPlugin::from_bytes(auth_plugin),
                    plugin_data: plugin_data.into(),
                })
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid AuthSwitchRequest header",
            )),
        }
    }

    pub fn auth_plugin(&self) -> &AuthPlugin<'_> {
        &self.auth_plugin
    }

    pub fn plugin_data(&self) -> &[u8] {
        &*self.plugin_data
    }

    pub fn into_owned(self) -> AuthSwitchRequest<'static> {
        AuthSwitchRequest {
            auth_plugin: self.auth_plugin.into_owned(),
            plugin_data: self.plugin_data.into_owned().into(),
        }
    }
}

/// Parses payload as an auth switch request packet.
pub fn parse_auth_switch_request(payload: &[u8]) -> io::Result<AuthSwitchRequest<'_>> {
    AuthSwitchRequest::parse(payload)
}

/// Represents MySql's initial handshake packet.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct HandshakePacket<'a> {
    protocol_version: u8,
    server_version: Cow<'a, [u8]>,
    connection_id: u32,
    scramble_1: Cow<'a, [u8]>,
    scramble_2: Option<Cow<'a, [u8]>>,
    capabilities: CapabilityFlags,
    default_collation: u8,
    status_flags: StatusFlags,
    auth_plugin: Option<AuthPlugin<'a>>,
}

/// Parses payload as an initial handshake packet.
pub fn parse_handshake_packet(payload: &[u8]) -> io::Result<HandshakePacket<'_>> {
    HandshakePacket::parse(payload)
}

impl<'a> HandshakePacket<'a> {
    /// Parses payload as an initial handshake packet.
    fn parse(mut payload: &[u8]) -> io::Result<HandshakePacket<'_>> {
        let protocol_version = payload.read_u8()?;
        let mut nul_byte_pos = 0;
        for (i, byte) in payload.iter().enumerate() {
            if *byte == 0x00 {
                nul_byte_pos = i;
                break;
            }
        }
        let (server_version, mut payload) =
            split_at_or_err!(payload, nul_byte_pos, "Invalid handshake packet")?;
        payload.read_u8()?;
        let connection_id = payload.read_u32::<LE>()?;
        let (scramble_1, mut payload) = split_at_or_err!(payload, 8, "Invalid handshake packet")?;
        payload.read_u8()?;
        let capabilities_1 = payload.read_u16::<LE>()?;
        let default_collation = payload.read_u8()?;
        let status_flags = payload.read_u16::<LE>()?;
        let capabilities_2 = payload.read_u16::<LE>()?;
        let capabilities = CapabilityFlags::from_bits_truncate(
            u32::from(capabilities_1) | (u32::from(capabilities_2) << 16),
        );
        let scramble_len = payload.read_u8()?;
        let (_, payload) = split_at_or_err!(payload, 10, "Invalid handshake packet")?;
        let (scramble_2, payload) =
            if capabilities.contains(CapabilityFlags::CLIENT_SECURE_CONNECTION) {
                let (scramble_2, mut payload) = split_at_or_err!(
                    payload,
                    max(12, scramble_len as i8 - 9) as usize,
                    "Invalid handshake packet"
                )?;
                payload.read_u8()?;
                (Some(scramble_2), payload)
            } else {
                (None, payload)
            };
        let auth_plugin_name = if capabilities.contains(CapabilityFlags::CLIENT_PLUGIN_AUTH) {
            if let Some(pos) = payload.iter().position(|&x| x == 0x00) {
                Some(&payload[..pos])
            } else {
                Some(payload)
            }
        } else {
            None
        };
        Ok(HandshakePacket {
            protocol_version,
            server_version: server_version.into(),
            connection_id,
            scramble_1: scramble_1.into(),
            scramble_2: scramble_2.map(Into::into),
            capabilities,
            default_collation,
            status_flags: StatusFlags::from_bits_truncate(status_flags),
            auth_plugin: auth_plugin_name.map(AuthPlugin::from_bytes),
        })
    }

    pub fn into_owned(self) -> HandshakePacket<'static> {
        HandshakePacket {
            protocol_version: self.protocol_version,
            server_version: self.server_version.into_owned().into(),
            connection_id: self.connection_id,
            scramble_1: self.scramble_1.into_owned().into(),
            scramble_2: self.scramble_2.map(Cow::into_owned).map(Into::into),
            capabilities: self.capabilities,
            default_collation: self.default_collation,
            status_flags: self.status_flags,
            auth_plugin: self.auth_plugin.map(AuthPlugin::into_owned),
        }
    }

    /// Value of the protocol_version field of an initial handshake packet.
    pub fn protocol_version(&self) -> u8 {
        self.protocol_version
    }

    /// Value of the server_version field of an initial handshake packet as a byte slice.
    pub fn server_version_ref(&self) -> &[u8] {
        self.server_version.as_ref()
    }

    /// Value of the server_version field of an initial handshake packet as a string
    /// (lossy converted).
    pub fn server_version_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.server_version_ref())
    }

    /// Parsed server version.
    ///
    /// Will parse first \d+.\d+.\d+ of a server version string (if any).
    pub fn server_version_parsed(&self) -> Option<(u16, u16, u16)> {
        VERSION_RE
            .captures(self.server_version_ref())
            .map(|captures| {
                // Should not panic because validated with regex
                (
                    parse::<u16, _>(captures.get(1).unwrap().as_bytes()).unwrap(),
                    parse::<u16, _>(captures.get(2).unwrap().as_bytes()).unwrap(),
                    parse::<u16, _>(captures.get(3).unwrap().as_bytes()).unwrap(),
                )
            })
    }

    /// Parsed mariadb server version.
    pub fn maria_db_server_version_parsed(&self) -> Option<(u16, u16, u16)> {
        MARIADB_VERSION_RE
            .captures(self.server_version_ref())
            .map(|captures| {
                // Should not panic because validated with regex
                (
                    parse::<u16, _>(captures.get(1).unwrap().as_bytes()).unwrap(),
                    parse::<u16, _>(captures.get(2).unwrap().as_bytes()).unwrap(),
                    parse::<u16, _>(captures.get(3).unwrap().as_bytes()).unwrap(),
                )
            })
    }

    /// Value of the connection_id field of an initial handshake packet.
    pub fn connection_id(&self) -> u32 {
        self.connection_id
    }

    /// Value of the scramble_1 field of an initial handshake packet as a byte slice.
    pub fn scramble_1_ref(&self) -> &[u8] {
        self.scramble_1.as_ref()
    }

    /// Value of the scramble_2 field of an initial handshake packet as a byte slice.
    pub fn scramble_2_ref(&self) -> Option<&[u8]> {
        self.scramble_2.as_ref().map(Cow::as_ref)
    }

    /// Returns concatenated auth plugin nonce.
    pub fn nonce(&self) -> Vec<u8> {
        let mut out = Vec::from(self.scramble_1_ref());
        out.extend_from_slice(self.scramble_2_ref().unwrap_or(&[][..]));
        out
    }

    /// Value of a server capabilities.
    pub fn capabilities(&self) -> CapabilityFlags {
        self.capabilities
    }

    /// Value of the default_collation field of an initial handshake packet.
    pub fn default_collation(&self) -> u8 {
        self.default_collation
    }

    /// Value of a status flags.
    pub fn status_flags(&self) -> StatusFlags {
        self.status_flags
    }

    /// Value of the auth_plugin_name field of an initial handshake packet as a byte slice.
    pub fn auth_plugin_name_ref(&self) -> Option<&[u8]> {
        self.auth_plugin.as_ref().map(AuthPlugin::as_bytes)
    }

    /// Value of the auth_plugin_name field of an initial handshake packet as a string
    /// (lossy converted).
    pub fn auth_plugin_name_str(&self) -> Option<Cow<'_, str>> {
        self.auth_plugin
            .as_ref()
            .map(AuthPlugin::as_bytes)
            .map(String::from_utf8_lossy)
    }

    /// Auth plugin of a handshake packet
    pub fn auth_plugin(&self) -> Option<&AuthPlugin<'_>> {
        self.auth_plugin.as_ref()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct HandshakeResponse {
    data: Vec<u8>,
}

impl HandshakeResponse {
    pub fn new(
        scramble_buf: &Option<impl AsRef<[u8]>>,
        server_version: (u16, u16, u16),
        user: Option<&str>,
        db_name: Option<&str>,
        auth_plugin: &AuthPlugin<'_>,
        client_flags: CapabilityFlags,
        connect_attributes: &HashMap<String, String>,
    ) -> HandshakeResponse {
        let scramble = scramble_buf.as_ref().map(|x| x.as_ref()).unwrap_or(&[]);
        let database = db_name.unwrap_or("");

        let collation = if server_version >= (5, 5, 3) {
            UTF8MB4_GENERAL_CI
        } else {
            UTF8_GENERAL_CI
        };

        let mut data = Vec::with_capacity(1024);
        data.write_u32::<LE>(client_flags.bits()).unwrap();
        data.resize(data.len() + 4, 0);
        data.push(collation as u8);
        data.resize(data.len() + 23, 0);
        data.extend_from_slice(user.unwrap_or("").as_bytes());
        data.push(0);
        data.push(scramble.len() as u8);
        data.extend_from_slice(scramble);
        data.extend_from_slice(database.as_bytes());
        data.push(0);
        if client_flags.contains(CapabilityFlags::CLIENT_PLUGIN_AUTH) {
            data.extend_from_slice(auth_plugin.as_bytes());
            data.push(0);
        }
        if client_flags.contains(CapabilityFlags::CLIENT_CONNECT_ATTRS) {
            let len = connect_attributes
                .iter()
                .map(|(k, v)| lenenc_str_len(k) + lenenc_str_len(v))
                .sum::<usize>();
            data.write_lenenc_int(len as u64).expect("out of memory");

            for (name, value) in connect_attributes {
                data.write_lenenc_str(name.as_bytes())
                    .expect("out of memory");
                data.write_lenenc_str(value.as_bytes())
                    .expect("out of memory");
            }
        }

        HandshakeResponse { data }
    }
}

impl AsRef<[u8]> for HandshakeResponse {
    fn as_ref(&self) -> &[u8] {
        &self.data[..]
    }
}

impl Into<Vec<u8>> for HandshakeResponse {
    fn into(self) -> Vec<u8> {
        self.data
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SslRequest {
    data: Vec<u8>,
}

impl SslRequest {
    pub fn new(capabilities: CapabilityFlags) -> SslRequest {
        let mut data = vec![0u8; 4 + 4 + 1 + 23];
        LE::write_u32(&mut data[0..], capabilities.bits());
        LE::write_u32(&mut data[4..], 1024 * 1024);
        data[8] = UTF8_GENERAL_CI as u8;
        SslRequest { data }
    }
}

impl AsRef<[u8]> for SslRequest {
    fn as_ref(&self) -> &[u8] {
        &self.data[..]
    }
}

impl Into<Vec<u8>> for SslRequest {
    fn into(self) -> Vec<u8> {
        self.data
    }
}

/// Represents MySql's statement packet.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StmtPacket {
    statement_id: u32,
    num_columns: u16,
    num_params: u16,
    warning_count: u16,
}

/// Parses payload as a statement packet.
pub fn parse_stmt_packet(payload: &[u8]) -> io::Result<StmtPacket> {
    StmtPacket::parse(payload)
}

impl StmtPacket {
    /// Parses payload as a statement packet.
    fn parse(mut payload: &[u8]) -> io::Result<StmtPacket> {
        if payload.read_u8()? != 0x00 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid statement packet status",
            ));
        }

        let statement_id = payload.read_u32::<LE>()?;
        let num_columns = payload.read_u16::<LE>()?;
        let num_params = payload.read_u16::<LE>()?;
        payload.read_u8()?;
        let warning_count = payload.read_u16::<LE>()?;

        Ok(StmtPacket {
            statement_id,
            num_columns,
            num_params,
            warning_count,
        })
    }

    /// Value of the statement_id field of a statement packet.
    pub fn statement_id(&self) -> u32 {
        self.statement_id
    }

    /// Value of the num_columns field of a statement packet.
    pub fn num_columns(&self) -> u16 {
        self.num_columns
    }

    /// Value of the num_params field of a statement packet.
    pub fn num_params(&self) -> u16 {
        self.num_params
    }

    /// Value of the warning_count field of a statement packet.
    pub fn warning_count(&self) -> u16 {
        self.warning_count
    }
}

/// Null-bitmap.
///
/// http://dev.mysql.com/doc/internals/en/null-bitmap.html
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NullBitmap<T, U: AsRef<[u8]> = Vec<u8>>(U, PhantomData<T>);

impl<T: SerializationSide> NullBitmap<T, Vec<u8>> {
    /// Creates new null-bitmap for a given number of columns.
    pub fn new(num_columns: usize) -> Self {
        Self::from_bytes(vec![0; Self::bitmap_len(num_columns)])
    }

    /// Will read null-bitmap for a given number of columns from `input`.
    pub fn read(input: &mut &[u8], num_columns: usize) -> Self {
        let bitmap_len = Self::bitmap_len(num_columns);
        assert!(input.len() >= bitmap_len);

        let bitmap = Self::from_bytes(input[..bitmap_len].to_vec());
        *input = &input[bitmap_len..];

        bitmap
    }
}

impl<T: SerializationSide, U: AsRef<[u8]>> NullBitmap<T, U> {
    pub fn bitmap_len(num_columns: usize) -> usize {
        (num_columns + 7 + T::BIT_OFFSET) / 8
    }

    fn byte_and_bit(&self, column_index: usize) -> (usize, u8) {
        let offset = column_index + T::BIT_OFFSET;
        let byte = offset / 8;
        let bit = 1 << (offset % 8) as u8;

        assert!(byte < self.0.as_ref().len());

        (byte, bit)
    }

    /// Creates new null-bitmap from given bytes.
    pub fn from_bytes(bytes: U) -> Self {
        Self(bytes, PhantomData)
    }

    /// Returns `true` if given column is `NULL` in this `NullBitmap`.
    pub fn is_null(&self, column_index: usize) -> bool {
        let (byte, bit) = self.byte_and_bit(column_index);
        self.0.as_ref()[byte] & bit > 0
    }
}

impl<T: SerializationSide, U: AsRef<[u8]> + AsMut<[u8]>> NullBitmap<T, U> {
    /// Sets flag value for given column.
    pub fn set(&mut self, column_index: usize, is_null: bool) {
        let (byte, bit) = self.byte_and_bit(column_index);
        if is_null {
            self.0.as_mut()[byte] |= bit
        } else {
            self.0.as_mut()[byte] &= !bit
        }
    }
}

impl<T, U: AsRef<[u8]>> AsRef<[u8]> for NullBitmap<T, U> {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ComStmtExecuteRequestBuilder {
    body: Vec<u8>,
    bitmap_len: usize,
    params_added: u16,
}

impl ComStmtExecuteRequestBuilder {
    pub const NULL_BITMAP_OFFSET: usize = 10;

    pub fn new(stmt_id: u32) -> Self {
        let mut body = Vec::with_capacity(1024);
        body.resize(10, 0);
        // command
        body[0] = Command::COM_STMT_EXECUTE as u8;
        // stmt-id
        LE::write_u32(&mut body[1..5], stmt_id);
        // iteration-count
        body[6] = 1;

        Self {
            body,
            bitmap_len: 0,
            params_added: 0,
        }
    }

    pub fn build(mut self, params: &[Value]) -> (Vec<u8>, bool) {
        if params.len() > 0 {
            self.bitmap_len = NullBitmap::<ClientSide>::bitmap_len(params.len());
            let meta_len = params.len() * 2;
            let data_len: usize = params.iter().map(Value::bin_len).sum();

            let total_len = self.body.len() + self.bitmap_len + 1 + meta_len + data_len;
            let as_long_data = total_len > MAX_PAYLOAD_LEN;

            self.body
                .resize(self.body.len() + self.bitmap_len + 1 + meta_len, 0);
            self.body[Self::NULL_BITMAP_OFFSET + self.bitmap_len] = 1;

            for value in params {
                self.add_param(value, as_long_data);
            }

            (self.body, as_long_data)
        } else {
            (self.body, false)
        }
    }

    fn add_param(&mut self, value: &Value, as_long_data: bool) -> u64 {
        let param_index = self.params_added as usize;
        self.params_added += 1;

        let mut write = true;

        match value {
            Value::NULL => {
                self.set_null_flag(param_index);
                self.set_type(param_index, ColumnType::MYSQL_TYPE_NULL);
                write = false;
            }
            Value::Bytes(_) => {
                self.set_type(param_index, ColumnType::MYSQL_TYPE_VAR_STRING);
                write = !as_long_data;
            }
            Value::Int(_) => {
                self.set_type(param_index, ColumnType::MYSQL_TYPE_LONGLONG);
            }
            Value::UInt(_) => {
                self.set_type(param_index, ColumnType::MYSQL_TYPE_LONGLONG);
                self.set_unsigned(param_index);
            }
            Value::Float(_) => {
                self.set_type(param_index, ColumnType::MYSQL_TYPE_DOUBLE);
            }
            Value::Date(..) => {
                self.set_type(param_index, ColumnType::MYSQL_TYPE_DATETIME);
            }
            Value::Time(..) => {
                self.set_type(param_index, ColumnType::MYSQL_TYPE_TIME);
            }
        }

        if write {
            self.body.write_bin_value(value).expect("out of memory")
        } else {
            0
        }
    }

    fn set_type(&mut self, param_index: usize, param_type: ColumnType) {
        let param_meta_offset = self.param_meta_index_offset(param_index);
        self.body[param_meta_offset] = param_type as u8;
    }

    fn set_unsigned(&mut self, param_index: usize) {
        let param_meta_offset = self.param_meta_index_offset(param_index);
        self.body[param_meta_offset + 1] = 0x80;
    }

    fn set_null_flag(&mut self, param_index: usize) {
        let end = Self::NULL_BITMAP_OFFSET + self.bitmap_len;
        let bitmap_bytes = &mut self.body[Self::NULL_BITMAP_OFFSET..end];

        NullBitmap::<ClientSide, _>::from_bytes(bitmap_bytes).set(param_index, true);
    }

    fn param_meta_index_offset(&self, param_index: usize) -> usize {
        Self::NULL_BITMAP_OFFSET + self.bitmap_len + 1 + 2 * param_index
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ComStmtSendLongData {
    body: Vec<u8>,
}

impl ComStmtSendLongData {
    pub fn new(stmt_id: u32, param_index: usize, data: &[u8]) -> Self {
        let mut body = Vec::with_capacity(1 + 4 + 2 + data.len());

        body.push(Command::COM_STMT_SEND_LONG_DATA as u8);
        body.write_u32::<LE>(stmt_id).expect("unreachable");
        body.write_u16::<LE>(param_index as u16)
            .expect("unreachable");
        body.extend_from_slice(data);

        Self { body }
    }
}

impl AsRef<[u8]> for ComStmtSendLongData {
    fn as_ref(&self) -> &[u8] {
        &*self.body
    }
}

impl Into<Vec<u8>> for ComStmtSendLongData {
    fn into(self) -> Vec<u8> {
        self.body
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ComStmtClose {
    body: Vec<u8>,
}

impl ComStmtClose {
    pub fn new(stmt_id: u32) -> Self {
        let mut body = Vec::with_capacity(1 + 4);
        body.push(Command::COM_STMT_CLOSE as u8);
        body.write_u32::<LE>(stmt_id).expect("unreachable");
        Self { body }
    }

    pub fn set_id(&mut self, stmt_id: u32) {
        LE::write_u32(&mut self.body[1..], stmt_id);
    }
}

impl AsRef<[u8]> for ComStmtClose {
    fn as_ref(&self) -> &[u8] {
        &*self.body
    }
}

impl Into<Vec<u8>> for ComStmtClose {
    fn into(self) -> Vec<u8> {
        self.body
    }
}

#[cfg(test)]
mod test {
    use super::{
        column_from_payload, parse_auth_more_data, parse_auth_switch_request, parse_err_packet,
        parse_handshake_packet, parse_local_infile_packet, parse_ok_packet, parse_stmt_packet,
        OkPacketKind, SessionStateChange,
    };
    use crate::constants::{
        CapabilityFlags, ColumnFlags, ColumnType, StatusFlags, UTF8_GENERAL_CI,
    };

    #[test]
    fn should_parse_local_infile_packet() {
        const LIP: &[u8] = b"\xfbfile_name";

        let lip = parse_local_infile_packet(LIP).unwrap();
        assert_eq!(lip.file_name_str(), "file_name");
    }

    #[test]
    fn should_parse_stmt_packet() {
        const SP: &[u8] = b"\x00\x01\x00\x00\x00\x01\x00\x02\x00\x00\x00\x00";
        const SP_2: &[u8] = b"\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";

        let sp = parse_stmt_packet(SP).unwrap();
        assert_eq!(sp.statement_id(), 0x01);
        assert_eq!(sp.num_columns(), 0x01);
        assert_eq!(sp.num_params(), 0x02);
        assert_eq!(sp.warning_count(), 0x00);

        let sp = parse_stmt_packet(SP_2).unwrap();
        assert_eq!(sp.statement_id(), 0x01);
        assert_eq!(sp.num_columns(), 0x00);
        assert_eq!(sp.num_params(), 0x00);
        assert_eq!(sp.warning_count(), 0x00);
    }

    #[test]
    fn should_parse_handshake_packet() {
        const HSP: &[u8] = b"\x0a5.5.5-10.0.17-MariaDB-log\x00\x0b\x00\
                             \x00\x00\x64\x76\x48\x40\x49\x2d\x43\x4a\x00\xff\xf7\x08\x02\x00\
                             \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x34\x64\
                             \x7c\x63\x5a\x77\x6b\x34\x5e\x5d\x3a\x00";

        const HSP_2: &[u8] = b"\x0a\x35\x2e\x36\x2e\x34\x2d\x6d\x37\x2d\x6c\x6f\
                               \x67\x00\x56\x0a\x00\x00\x52\x42\x33\x76\x7a\x26\x47\x72\x00\xff\
                               \xff\x08\x02\x00\x0f\xc0\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                               \x00\x2b\x79\x44\x26\x2f\x5a\x5a\x33\x30\x35\x5a\x47\x00\x6d\x79\
                               \x73\x71\x6c\x5f\x6e\x61\x74\x69\x76\x65\x5f\x70\x61\x73\x73\x77\
                               \x6f\x72\x64\x00\x00";

        let hsp = parse_handshake_packet(HSP).unwrap();
        assert_eq!(hsp.protocol_version, 0x0a);
        assert_eq!(hsp.server_version_str(), "5.5.5-10.0.17-MariaDB-log");
        assert_eq!(hsp.server_version_parsed(), Some((5, 5, 5)));
        assert_eq!(hsp.maria_db_server_version_parsed(), Some((10, 0, 17)));
        assert_eq!(hsp.connection_id(), 0x0b);
        assert_eq!(hsp.scramble_1_ref(), b"dvH@I-CJ");
        assert_eq!(
            hsp.capabilities(),
            CapabilityFlags::from_bits_truncate(0xf7ff)
        );
        assert_eq!(hsp.default_collation(), 0x08);
        assert_eq!(hsp.status_flags(), StatusFlags::from_bits_truncate(0x0002));
        assert_eq!(hsp.scramble_2_ref(), Some(&b"*4d|cZwk4^]:"[..]));
        assert_eq!(hsp.auth_plugin_name_ref(), None);

        let hsp = parse_handshake_packet(HSP_2).unwrap();
        assert_eq!(hsp.protocol_version, 0x0a);
        assert_eq!(hsp.server_version_str(), "5.6.4-m7-log");
        assert_eq!(hsp.server_version_parsed(), Some((5, 6, 4)));
        assert_eq!(hsp.maria_db_server_version_parsed(), None);
        assert_eq!(hsp.connection_id(), 0x0a56);
        assert_eq!(hsp.scramble_1_ref(), b"RB3vz&Gr");
        assert_eq!(
            hsp.capabilities(),
            CapabilityFlags::from_bits_truncate(0xc00fffff)
        );
        assert_eq!(hsp.default_collation(), 0x08);
        assert_eq!(hsp.status_flags(), StatusFlags::from_bits_truncate(0x0002));
        assert_eq!(hsp.scramble_2_ref(), Some(&b"+yD&/ZZ305ZG"[..]));
        assert_eq!(
            hsp.auth_plugin_name_ref(),
            Some(&b"mysql_native_password"[..])
        );
    }

    #[test]
    fn should_parse_err_packet() {
        const ERR_PACKET: &[u8] = b"\xff\x48\x04\x23\x48\x59\x30\x30\x30\x4e\x6f\x20\x74\x61\x62\
        \x6c\x65\x73\x20\x75\x73\x65\x64";
        const ERR_PACKET_NO_STATE: &[u8] = b"\xff\x10\x04\x54\x6f\x6f\x20\x6d\x61\x6e\x79\x20\x63\
        \x6f\x6e\x6e\x65\x63\x74\x69\x6f\x6e\x73";
        const PROGRESS_PACKET: &[u8] = b"\xff\xff\xff\x01\x01\x0a\xcc\x5b\x00\x0astage name";

        let err_packet = parse_err_packet(ERR_PACKET, CapabilityFlags::empty()).unwrap();
        assert!(err_packet.is_error());
        assert_eq!(err_packet.error_code(), 1096);
        assert_eq!(err_packet.sql_state_str(), "HY000");
        assert_eq!(err_packet.message_str(), "No tables used");

        let err_packet =
            parse_err_packet(ERR_PACKET_NO_STATE, CapabilityFlags::CLIENT_PROTOCOL_41).unwrap();
        assert!(err_packet.is_error());
        assert_eq!(err_packet.error_code(), 1040);
        assert_eq!(err_packet.sql_state_str(), "HY000");
        assert_eq!(err_packet.message_str(), "Too many connections");

        let err_packet =
            parse_err_packet(PROGRESS_PACKET, CapabilityFlags::CLIENT_PROGRESS_OBSOLETE).unwrap();
        assert!(err_packet.is_progress_report());
        let progress_report = err_packet.progress_report();
        assert_eq!(progress_report.stage(), 1);
        assert_eq!(progress_report.max_stage(), 10);
        assert_eq!(progress_report.progress(), 23500);
        assert_eq!(progress_report.stage_info_str(), "stage name");
    }

    #[test]
    fn should_parse_column_packet() {
        const COLUMN_PACKET: &[u8] = b"\x03def\x06schema\x05table\x09org_table\x04name\
              \x08org_name\x0c\x21\x00\x0F\x00\x00\x00\x00\x01\x00\x08\x00\x00";
        let column = column_from_payload(COLUMN_PACKET.into()).unwrap();
        assert_eq!(column.schema_str(), "schema");
        assert_eq!(column.table_str(), "table");
        assert_eq!(column.org_table_str(), "org_table");
        assert_eq!(column.name_str(), "name");
        assert_eq!(column.org_name_str(), "org_name");
        assert_eq!(column.character_set(), UTF8_GENERAL_CI);
        assert_eq!(column.column_length(), 15);
        assert_eq!(column.column_type(), ColumnType::MYSQL_TYPE_DECIMAL);
        assert_eq!(column.flags(), ColumnFlags::NOT_NULL_FLAG);
        assert_eq!(column.decimals(), 8);
    }

    #[test]
    fn should_parse_auth_switch_request() {
        const PAYLOAD: &[u8] = b"\xfe\x6d\x79\x73\x71\x6c\x5f\x6e\x61\x74\x69\x76\x65\x5f\x70\x61\
                                 \x73\x73\x77\x6f\x72\x64\x00\x7a\x51\x67\x34\x69\x36\x6f\x4e\x79\
                                 \x36\x3d\x72\x48\x4e\x2f\x3e\x2d\x62\x29\x41\x00";
        let packet = parse_auth_switch_request(PAYLOAD).unwrap();
        assert_eq!(packet.auth_plugin().as_bytes(), b"mysql_native_password",);
        assert_eq!(packet.plugin_data(), b"zQg4i6oNy6=rHN/>-b)A",)
    }

    #[test]
    fn should_parse_auth_more_data() {
        const PAYLOAD: &[u8] = b"\x01\x04";
        let packet = parse_auth_more_data(PAYLOAD).unwrap();
        assert_eq!(packet.data(), b"\x04",);
    }

    #[test]
    fn should_parse_ok_packet() {
        const PLAIN_OK: &[u8] = b"\x00\x00\x00\x02\x00\x00\x00";
        const SESS_STATE_SYS_VAR_OK: &[u8] =
            (b"\x00\x00\x00\x02\x40\x00\x00\x00\x11\x00\x0f\x0a\x61\
        \x75\x74\x6f\x63\x6f\x6d\x6d\x69\x74\x03\x4f\x46\x46");
        const SESS_STATE_SCHEMA_OK: &[u8] =
            b"\x00\x00\x00\x02\x40\x00\x00\x00\x07\x01\x05\x04\x74\x65\x73\x74";
        const SESS_STATE_TRACK_OK: &[u8] = b"\x00\x00\x00\x02\x40\x00\x00\x00\x04\x02\x02\x01\x31";
        const EOF: &[u8] = b"\xfe\x00\x00\x02\x00";

        // packet starting with 0x00 is not an ok packet if it terminates a result set
        parse_ok_packet(
            PLAIN_OK,
            CapabilityFlags::empty(),
            OkPacketKind::ResultSetTerminator,
        )
        .unwrap_err();

        let ok_packet =
            parse_ok_packet(PLAIN_OK, CapabilityFlags::empty(), OkPacketKind::Other).unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            StatusFlags::SERVER_STATUS_AUTOCOMMIT
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        assert_eq!(ok_packet.session_state_info(), None);

        let ok_packet = parse_ok_packet(
            SESS_STATE_SYS_VAR_OK,
            CapabilityFlags::CLIENT_SESSION_TRACK,
            OkPacketKind::Other,
        )
        .unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            StatusFlags::SERVER_STATUS_AUTOCOMMIT | StatusFlags::SERVER_SESSION_STATE_CHANGED
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        let sess_state_info = ok_packet.session_state_info().unwrap();
        assert_eq!(
            sess_state_info.decode().unwrap(),
            SessionStateChange::SystemVariable((&b"autocommit"[..]).into(), (&b"OFF"[..]).into())
        );

        let ok_packet = parse_ok_packet(
            SESS_STATE_SCHEMA_OK,
            CapabilityFlags::CLIENT_SESSION_TRACK,
            OkPacketKind::Other,
        )
        .unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            StatusFlags::SERVER_STATUS_AUTOCOMMIT | StatusFlags::SERVER_SESSION_STATE_CHANGED
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        let sess_state_info = ok_packet.session_state_info().unwrap();
        assert_eq!(
            sess_state_info.decode().unwrap(),
            SessionStateChange::Schema((&b"test"[..]).into())
        );

        let ok_packet = parse_ok_packet(
            SESS_STATE_TRACK_OK,
            CapabilityFlags::CLIENT_SESSION_TRACK,
            OkPacketKind::Other,
        )
        .unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            StatusFlags::SERVER_STATUS_AUTOCOMMIT | StatusFlags::SERVER_SESSION_STATE_CHANGED
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        let sess_state_info = ok_packet.session_state_info().unwrap();
        assert_eq!(
            sess_state_info.decode().unwrap(),
            SessionStateChange::IsTracked(true)
        );

        let ok_packet = parse_ok_packet(
            EOF,
            CapabilityFlags::CLIENT_SESSION_TRACK,
            OkPacketKind::ResultSetTerminator,
        )
        .unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            StatusFlags::SERVER_STATUS_AUTOCOMMIT
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        assert_eq!(ok_packet.session_state_info(), None);
    }
}

// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use bytes::BufMut;
use lexical::parse;
use regex::bytes::Regex;
use smallvec::SmallVec;

use std::{
    borrow::Cow,
    cmp::max,
    collections::HashMap,
    convert::{TryFrom, TryInto},
    fmt, io,
    marker::PhantomData,
    ops,
};

use crate::{
    constants::{
        CapabilityFlags, ColumnFlags, ColumnType, Command, SessionStateType, StatusFlags,
        MAX_PAYLOAD_LEN, UTF8MB4_GENERAL_CI, UTF8_GENERAL_CI,
    },
    io::{BufMutExt, ParseBuf},
    misc::{lenenc_str_len, unexpected_buf_eof},
    proto::{MyDeserialize, MySerialize},
    value::{ClientSide, SerializationSide, Value},
};

lazy_static! {
    static ref MARIADB_VERSION_RE: Regex =
        Regex::new(r"^5.5.5-(\d{1,2})\.(\d{1,2})\.(\d{1,3})-MariaDB").unwrap();
    static ref VERSION_RE: Regex = Regex::new(r"^(\d{1,2})\.(\d{1,2})\.(\d{1,3})(.*)").unwrap();
}

/// Represents MySql Column (column packet).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Column {
    schema: SmallVec<[u8; 16]>,
    table: SmallVec<[u8; 16]>,
    org_table: SmallVec<[u8; 16]>,
    name: SmallVec<[u8; 16]>,
    org_name: SmallVec<[u8; 16]>,
    column_length: u32,
    character_set: u16,
    flags: ColumnFlags,
    column_type: ColumnType,
    decimals: u8,
}

impl<'de> MyDeserialize<'de> for Column {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let mut schema = SmallVec::new();
        let mut table = SmallVec::new();
        let mut org_table = SmallVec::new();
        let mut name = SmallVec::new();
        let mut org_name = SmallVec::new();

        // Skip lenenc "def"
        if !buf.checked_skip(4) {
            return Err(unexpected_buf_eof());
        }

        macro_rules! copy {
            ($buf:ident, $container:ident) => {
                let s = buf
                    .checked_eat_lenenc_str()
                    .ok_or_else(unexpected_buf_eof)?;
                $container.extend_from_slice(s);
            };
        }

        copy!(buf, schema);
        copy!(buf, table);
        copy!(buf, org_table);
        copy!(buf, name);
        copy!(buf, org_name);

        let mut buf = buf.checked_eat_buf(11).ok_or_else(unexpected_buf_eof)?;

        buf.skip(1);

        let character_set = buf.eat_u16_le();
        let column_length = buf.eat_u32_le();
        let column_type = buf.eat_u8();
        let flags = buf.eat_u16_le();
        let decimals = buf.eat_u8();

        Ok(Column {
            schema,
            table,
            org_table,
            name,
            org_name,
            column_length,
            character_set,
            flags: ColumnFlags::from_bits_truncate(flags),
            column_type: column_type
                .try_into()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
            decimals,
        })
    }
}

impl Column {
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
        &*self.schema
    }

    /// Returns value of the schema field of a column packet as a string (lossy converted).
    pub fn schema_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.schema_ref())
    }

    /// Returns value of the table field of a column packet as a byte slice.
    pub fn table_ref(&self) -> &[u8] {
        &*self.table
    }

    /// Returns value of the table field of a column packet as a string (lossy converted).
    pub fn table_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.table_ref())
    }

    /// Returns value of the org_table field of a column packet as a byte slice.
    ///
    /// "org_table" is for original table name.
    pub fn org_table_ref(&self) -> &[u8] {
        &*self.org_table
    }

    /// Returns value of the org_table field of a column packet as a string (lossy converted).
    pub fn org_table_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.org_table_ref())
    }

    /// Returns value of the name field of a column packet as a byte slice.
    pub fn name_ref(&self) -> &[u8] {
        &*self.name
    }

    /// Returns value of the name field of a column packet as a string (lossy converted).
    pub fn name_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.name_ref())
    }

    /// Returns value of the org_name field of a column packet as a byte slice.
    ///
    /// "org_name" is for original column name.
    pub fn org_name_ref(&self) -> &[u8] {
        &*self.org_name
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
                Cow::Owned(name.into_owned()),
                Cow::Owned(value.into_owned()),
            ),
            SessionStateChange::Schema(schema) => {
                SessionStateChange::Schema(Cow::Owned(schema.into_owned()))
            }
            SessionStateChange::IsTracked(x) => SessionStateChange::IsTracked(x),
            SessionStateChange::UnknownLayout(data) => {
                SessionStateChange::UnknownLayout(Cow::Owned(data.into_owned()))
            }
        }
    }
}

impl<'de> MyDeserialize<'de> for SessionStateChange<'de> {
    type Ctx = SessionStateType;

    fn deserialize(ty: SessionStateType, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        match ty {
            SessionStateType::SESSION_TRACK_SYSTEM_VARIABLES => {
                let name = buf
                    .checked_eat_lenenc_str()
                    .ok_or_else(unexpected_buf_eof)?;
                let value = buf
                    .checked_eat_lenenc_str()
                    .ok_or_else(unexpected_buf_eof)?;
                Ok(SessionStateChange::SystemVariable(
                    Cow::Borrowed(name),
                    Cow::Borrowed(value),
                ))
            }
            SessionStateType::SESSION_TRACK_SCHEMA => {
                let schema = buf
                    .checked_eat_lenenc_str()
                    .ok_or_else(unexpected_buf_eof)?;
                Ok(SessionStateChange::Schema(Cow::Borrowed(schema)))
            }
            SessionStateType::SESSION_TRACK_STATE_CHANGE => {
                let is_tracked = buf
                    .checked_eat_lenenc_str()
                    .ok_or_else(unexpected_buf_eof)?;
                Ok(SessionStateChange::IsTracked(is_tracked == b"1"))
            }
            // Layout isn't specified in the documentation
            SessionStateType::SESSION_TRACK_GTIDS
            | SessionStateType::SESSION_TRACK_TRANSACTION_CHARACTERISTICS
            | SessionStateType::SESSION_TRACK_TRANSACTION_STATE => Ok(
                SessionStateChange::UnknownLayout(Cow::Borrowed(buf.eat_all())),
            ),
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
    pub fn into_owned(self) -> SessionStateInfo<'static> {
        let SessionStateInfo { data_type, data } = self;
        SessionStateInfo {
            data_type,
            data: Cow::Owned(data.into_owned()),
        }
    }

    pub fn data_type(&self) -> SessionStateType {
        self.data_type
    }

    pub fn decode(&self) -> io::Result<SessionStateChange<'_>> {
        let mut data = ParseBuf(self.data.as_ref());
        SessionStateChange::deserialize(self.data_type, &mut data)
    }
}

impl<'de> MyDeserialize<'de> for SessionStateInfo<'de> {
    type Ctx = ();

    fn deserialize(_ctx: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let data_type = buf
            .checked_eat_u8()
            .ok_or_else(unexpected_buf_eof)?
            .try_into()
            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;
        let data = buf
            .checked_eat_lenenc_str()
            .ok_or_else(unexpected_buf_eof)?;

        Ok(SessionStateInfo {
            data_type,
            data: Cow::Borrowed(data),
        })
    }
}

/// Represents MySql's Ok packet.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OkPacketBody<'a> {
    affected_rows: u64,
    last_insert_id: u64,
    status_flags: StatusFlags,
    warnings: u16,
    info: &'a [u8],
    session_state_info: &'a [u8],
}

/// OK packet kind (see _OK packet identifier_ section of [WL#7766][1]).
///
/// [1]: https://dev.mysql.com/worklog/task/?id=7766
pub trait OkPacketKind {
    const HEADER: u8;

    fn parse_body<'de>(
        capabilities: CapabilityFlags,
        buf: &mut ParseBuf<'de>,
    ) -> io::Result<OkPacketBody<'de>>;
}

/// Ok pakcet that terminates a result set (text or binary).
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ResultSetTerminator;

impl OkPacketKind for ResultSetTerminator {
    const HEADER: u8 = 0xFE;

    fn parse_body<'de>(
        _: CapabilityFlags,
        buf: &mut ParseBuf<'de>,
    ) -> io::Result<OkPacketBody<'de>> {
        // We assume that CLIENT_PROTOCOL_41 was set
        let mut sbuf = buf.checked_eat_buf(4).ok_or_else(unexpected_buf_eof)?;
        let warnings = sbuf.eat_u16_le();
        let status_flags = StatusFlags::from_bits_truncate(sbuf.eat_u16_le());

        Ok(OkPacketBody {
            affected_rows: 0,
            last_insert_id: 0,
            status_flags,
            warnings,
            info: &[],
            session_state_info: &[],
        })
    }
}

/// Ok packet that is not a result set terminator.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct CommonOkPacket;

impl OkPacketKind for CommonOkPacket {
    const HEADER: u8 = 0x00;

    fn parse_body<'de>(
        capabilities: CapabilityFlags,
        buf: &mut ParseBuf<'de>,
    ) -> io::Result<OkPacketBody<'de>> {
        let affected_rows = buf
            .checked_eat_lenenc_int()
            .ok_or_else(unexpected_buf_eof)?;
        let last_insert_id = buf
            .checked_eat_lenenc_int()
            .ok_or_else(unexpected_buf_eof)?;
        // We assume that CLIENT_PROTOCOL_41 was set
        let mut sbuf = buf.checked_eat_buf(4).ok_or_else(unexpected_buf_eof)?;
        let status_flags = StatusFlags::from_bits_truncate(sbuf.eat_u16_le());
        let warnings = sbuf.eat_u16_le();

        let (info, session_state_info) =
            if capabilities.contains(CapabilityFlags::CLIENT_SESSION_TRACK) {
                let info = buf
                    .checked_eat_lenenc_str()
                    .ok_or_else(unexpected_buf_eof)?;
                let session_state_info =
                    if status_flags.contains(StatusFlags::SERVER_SESSION_STATE_CHANGED) {
                        buf.checked_eat_lenenc_str()
                            .ok_or_else(unexpected_buf_eof)?
                    } else {
                        &[][..]
                    };
                (info, session_state_info)
            } else if !buf.is_empty() && buf.0[0] > 0 {
                // The `info` field is a `string<EOF>` according to the MySQL Internals
                // Manual, but actually it's a `string<lenenc>`.
                // SEE: sql/protocol_classics.cc `net_send_ok`
                let info = buf
                    .checked_eat_lenenc_str()
                    .ok_or_else(unexpected_buf_eof)?;
                (info, &[][..])
            } else {
                (&[][..], &[][..])
            };

        Ok(OkPacketBody {
            affected_rows,
            last_insert_id,
            status_flags,
            warnings,
            info,
            session_state_info,
        })
    }
}

impl<'a> TryFrom<OkPacketBody<'a>> for OkPacket<'a> {
    type Error = io::Error;

    fn try_from(body: OkPacketBody<'a>) -> io::Result<Self> {
        Ok(OkPacket {
            affected_rows: body.affected_rows,
            last_insert_id: if body.last_insert_id == 0 {
                None
            } else {
                Some(body.last_insert_id)
            },
            status_flags: body.status_flags,
            warnings: body.warnings,
            info: if !body.info.is_empty() {
                Some(Cow::Borrowed(body.info))
            } else {
                None
            },
            session_state_info: if !body.session_state_info.is_empty() {
                Some(SessionStateInfo::deserialize(
                    (),
                    &mut ParseBuf(body.session_state_info),
                )?)
            } else {
                None
            },
        })
    }
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

impl<'a> OkPacket<'a> {
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
            info: info.map(|x| Cow::Owned(x.into_owned())),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OkPacketDeserializer<'de, T>(OkPacket<'de>, PhantomData<T>);

impl<'de, T> From<OkPacketDeserializer<'de, T>> for OkPacket<'de> {
    fn from(x: OkPacketDeserializer<'de, T>) -> Self {
        x.0
    }
}

impl<'de, T: OkPacketKind> MyDeserialize<'de> for OkPacketDeserializer<'de, T> {
    type Ctx = CapabilityFlags;

    fn deserialize(capabilities: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        if buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)? == T::HEADER {
            let body = T::parse_body(capabilities, buf)?;
            let ok = OkPacket::try_from(body)?;
            Ok(Self(ok, PhantomData))
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid OK_Packet header or length",
            ));
        }
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
            stage_info: Cow::Borrowed(stage_info),
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
            stage_info: Cow::Owned(stage_info.into_owned()),
        }
    }
}

impl<'de> MyDeserialize<'de> for ProgressReport<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let mut sbuf = buf.checked_eat_buf(6).ok_or_else(unexpected_buf_eof)?;

        sbuf.eat_u8(); // Ignore number of strings.
        let stage = sbuf.eat_u8();
        let max_stage = sbuf.eat_u8();
        let progress = sbuf.eat_u24_le();

        let progress_info = buf
            .checked_eat_lenenc_str()
            .ok_or_else(unexpected_buf_eof)?;
        Ok(Self::new(stage, max_stage, progress, progress_info))
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

impl<'a> ErrPacket<'a> {
    /// Returns false if this error packet contains progress report.
    pub fn is_error(&self) -> bool {
        matches!(self, ErrPacket::Error(..))
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
                ErrPacket::Error(code, state, Cow::Owned(message.into_owned()))
            }
            ErrPacket::Progress(progress_report) => {
                ErrPacket::Progress(progress_report.into_owned())
            }
        }
    }
}

impl<'de> MyDeserialize<'de> for ErrPacket<'de> {
    type Ctx = CapabilityFlags;

    fn deserialize(capabilities: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let mut sbuf = buf.checked_eat_buf(3).ok_or_else(unexpected_buf_eof)?;
        if sbuf.eat_u8() != 0xFF {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid ERR_Packet header",
            ));
        }

        let code = sbuf.eat_u16_le();

        // We assume that CLIENT_PROTOCOL_41 was set
        if code == 0xFFFF && capabilities.contains(CapabilityFlags::CLIENT_PROGRESS_OBSOLETE) {
            Ok(ErrPacket::Progress(ProgressReport::deserialize((), buf)?))
        } else {
            match buf.0[0] {
                b'#' => {
                    let err_state = buf.checked_eat(6).ok_or_else(unexpected_buf_eof)?;
                    let msg = buf.eat_all();
                    let mut state = [0; 5];
                    state.copy_from_slice(&err_state[1..]);
                    Ok(ErrPacket::Error(code, state, Cow::Borrowed(msg)))
                }
                _ => Ok(ErrPacket::Error(
                    code,
                    [b'H', b'Y', b'0', b'0', b'0'],
                    Cow::Borrowed(buf.0),
                )),
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

impl<'a> LocalInfilePacket<'a> {
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
            file_name: Cow::Owned(self.file_name.into_owned()),
        }
    }
}

impl<'de> MyDeserialize<'de> for LocalInfilePacket<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        if buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)? != 0xfb {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid LOCAL_INFILE packet header",
            ));
        }

        Ok(LocalInfilePacket {
            file_name: Cow::Borrowed(buf.eat_all()),
        })
    }
}

const MYSQL_NATIVE_PASSWORD_PLUGIN_NAME: &[u8] = b"mysql_native_password";
const CACHING_SHA2_PASSWORD_PLUGIN_NAME: &[u8] = b"caching_sha2_password";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthPluginData {
    /// Auth data for the `mysql_native_password` plugin.
    Native([u8; 20]),
    /// Auth data for `sha2_password` and `caching_sha2_password` plugins.
    Sha2([u8; 32]),
}

impl ops::Deref for AuthPluginData {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Native(x) => &x[..],
            Self::Sha2(x) => &x[..],
        }
    }
}

/// Authentication plugin
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum AuthPlugin<'a> {
    /// Legacy authentication plugin
    MysqlNativePassword,
    /// Default since MySql v8.0.4
    CachingSha2Password,
    Other(Cow<'a, [u8]>),
}

impl<'de> MyDeserialize<'de> for AuthPlugin<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        Ok(Self::from_bytes(buf.eat_all()))
    }
}

impl<'a> AuthPlugin<'a> {
    pub fn from_bytes(name: &'a [u8]) -> AuthPlugin<'a> {
        let name = if let [name @ .., 0] = name {
            name
        } else {
            name
        };
        match name {
            CACHING_SHA2_PASSWORD_PLUGIN_NAME => AuthPlugin::CachingSha2Password,
            MYSQL_NATIVE_PASSWORD_PLUGIN_NAME => AuthPlugin::MysqlNativePassword,
            name => AuthPlugin::Other(Cow::Borrowed(name)),
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
            AuthPlugin::Other(name) => AuthPlugin::Other(Cow::Owned(name.into_owned())),
        }
    }

    /// Generates auth plugin data for this plugin.
    ///
    /// It'll generate `None` if password is `None` or empty.
    pub fn gen_data(&self, pass: Option<&str>, nonce: &[u8]) -> Option<AuthPluginData> {
        use super::scramble::{scramble_native, scramble_sha256};

        match pass {
            Some(pass) => match self {
                AuthPlugin::CachingSha2Password => {
                    scramble_sha256(nonce, pass.as_bytes()).map(AuthPluginData::Sha2)
                }
                AuthPlugin::MysqlNativePassword => {
                    scramble_native(nonce, pass.as_bytes()).map(AuthPluginData::Native)
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
    pub fn data(&self) -> &[u8] {
        &*self.data
    }

    pub fn into_owned(self) -> AuthMoreData<'static> {
        AuthMoreData {
            data: Cow::Owned(self.data.into_owned()),
        }
    }
}

impl<'de> MyDeserialize<'de> for AuthMoreData<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        match buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)? {
            0x01 => Ok(AuthMoreData {
                data: Cow::Borrowed(buf.eat_all()),
            }),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid AuthMoreData header",
            )),
        }
    }
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
    pub fn auth_plugin(&self) -> &AuthPlugin<'_> {
        &self.auth_plugin
    }

    pub fn plugin_data(&self) -> &[u8] {
        &*self.plugin_data
    }

    pub fn into_owned(self) -> AuthSwitchRequest<'static> {
        AuthSwitchRequest {
            auth_plugin: self.auth_plugin.into_owned(),
            plugin_data: Cow::Owned(self.plugin_data.into_owned()),
        }
    }
}

impl<'de> MyDeserialize<'de> for AuthSwitchRequest<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        if buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)? != 0xfe {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid AuthSwitchRequest header",
            ));
        }

        let auth_plugin = buf.eat_null_str();
        buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)?;
        let plugin_data = match buf.eat_all() {
            [head @ .., 0] => head,
            all => all,
        };
        Ok(Self {
            auth_plugin: AuthPlugin::from_bytes(auth_plugin),
            plugin_data: Cow::Borrowed(plugin_data),
        })
    }
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

impl<'de> MyDeserialize<'de> for HandshakePacket<'de> {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let protocol_version = buf.checked_eat_u8().ok_or_else(unexpected_buf_eof)?;
        let server_version = buf.eat_null_str();

        let mut sbuf = buf.checked_eat_buf(32).ok_or_else(unexpected_buf_eof)?;
        sbuf.skip(1);
        let connection_id = sbuf.eat_u32_le();
        let scramble_1 = sbuf.eat(8);
        sbuf.skip(1);
        let capabilities_1 = sbuf.eat_u16_le();
        let default_collation = sbuf.eat_u8();
        let status_flags = sbuf.eat_u16_le();
        let capabilities_2 = sbuf.eat_u16_le();

        let capabilities = CapabilityFlags::from_bits_truncate(
            (capabilities_1 as u32) | ((capabilities_2 as u32) << 16),
        );

        let scramble_len = sbuf.eat_u8();

        let scramble_2 = capabilities
            .contains(CapabilityFlags::CLIENT_SECURE_CONNECTION)
            .then(|| {
                let pos = max(12, scramble_len as i8 - 9) as usize;
                let mut sbuf = buf
                    .checked_eat_buf(pos + 1)
                    .ok_or_else(unexpected_buf_eof)?;
                io::Result::Ok(sbuf.eat(pos))
            })
            .transpose()?;

        let auth_plugin_name = capabilities
            .contains(CapabilityFlags::CLIENT_PLUGIN_AUTH)
            .then(|| match buf.eat_all() {
                [name @ .., 0] => name,
                all => all,
            });

        Ok(HandshakePacket {
            protocol_version,
            server_version: Cow::Borrowed(server_version),
            connection_id,
            scramble_1: Cow::Borrowed(scramble_1),
            scramble_2: scramble_2.map(Cow::Borrowed),
            capabilities,
            default_collation,
            status_flags: StatusFlags::from_bits_truncate(status_flags),
            auth_plugin: auth_plugin_name.map(AuthPlugin::from_bytes),
        })
    }
}

impl<'a> HandshakePacket<'a> {
    pub fn into_owned(self) -> HandshakePacket<'static> {
        HandshakePacket {
            protocol_version: self.protocol_version,
            server_version: Cow::Owned(self.server_version.into_owned()),
            connection_id: self.connection_id,
            scramble_1: Cow::Owned(self.scramble_1.into_owned()),
            scramble_2: self.scramble_2.map(Cow::into_owned).map(Cow::Owned),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandshakeResponse<'a> {
    pub scramble_buf: Option<Cow<'a, [u8]>>,
    pub server_version: (u16, u16, u16),
    pub user: Option<Cow<'a, str>>,
    pub db_name: Option<Cow<'a, str>>,
    pub auth_plugin: AuthPlugin<'a>,
    pub client_flags: CapabilityFlags,
    pub connect_attributes: HashMap<String, String>,
}

impl<'a> HandshakeResponse<'a> {
    pub fn new(
        scramble_buf: Option<Cow<'a, [u8]>>,
        server_version: (u16, u16, u16),
        user: Option<Cow<'a, str>>,
        db_name: Option<Cow<'a, str>>,
        auth_plugin: AuthPlugin<'a>,
        client_flags: CapabilityFlags,
        connect_attributes: HashMap<String, String>,
    ) -> Self {
        Self {
            scramble_buf,
            server_version,
            user,
            db_name,
            auth_plugin,
            client_flags,
            connect_attributes,
        }
    }
}

impl MySerialize for HandshakeResponse<'_> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        let mut client_flags = self.client_flags;
        let scramble = self
            .scramble_buf
            .as_ref()
            .map(|x| x.as_ref())
            .unwrap_or(&[]);

        let collation = if self.server_version >= (5, 5, 3) {
            UTF8MB4_GENERAL_CI
        } else {
            UTF8_GENERAL_CI
        };

        if self.db_name.is_some() {
            client_flags |= CapabilityFlags::CLIENT_CONNECT_WITH_DB;
        }

        let user = self.user.as_deref().unwrap_or("").as_bytes();

        buf.put_u32_le(client_flags.bits());
        buf.put_slice(&[0, 0, 0, 1]);
        buf.put_u8(collation as u8);
        buf.put_slice(&[0; 23]);
        buf.put_slice(user);
        buf.put_u8(0);

        if client_flags.contains(CapabilityFlags::CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA) {
            buf.put_lenenc_str(scramble);
        } else if client_flags.contains(CapabilityFlags::CLIENT_SECURE_CONNECTION) {
            buf.put_u8(scramble.len() as u8);
            buf.put_slice(scramble);
        } else {
            buf.put_slice(scramble);
            buf.put_u8(0);
        }

        if client_flags.contains(CapabilityFlags::CLIENT_CONNECT_WITH_DB) {
            let database = self.db_name.as_deref().unwrap_or("");
            buf.put_slice(database.as_bytes());
            buf.put_u8(0);
        }

        if client_flags.contains(CapabilityFlags::CLIENT_PLUGIN_AUTH) {
            buf.put_slice(self.auth_plugin.as_bytes());
            buf.put_u8(0);
        }
        if client_flags.contains(CapabilityFlags::CLIENT_CONNECT_ATTRS) {
            let len = self
                .connect_attributes
                .iter()
                .map(|(k, v)| lenenc_str_len(k.as_bytes()) + lenenc_str_len(v.as_bytes()))
                .sum::<u64>();
            buf.put_lenenc_int(len);

            for (name, value) in &self.connect_attributes {
                buf.put_lenenc_str(name.as_bytes());
                buf.put_lenenc_str(value.as_bytes());
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SslRequest {
    pub capabilities: CapabilityFlags,
}

impl SslRequest {
    pub fn new(capabilities: CapabilityFlags) -> Self {
        Self { capabilities }
    }
}

impl MySerialize for SslRequest {
    fn serialize(&self, buf: &mut Vec<u8>) {
        buf.put_u32_le(self.capabilities.bits());
        buf.put_u32_le(1024 * 1024);
        buf.put_u8(UTF8_GENERAL_CI as u8);
        buf.put_slice(&[0; 23]);
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

impl<'de> MyDeserialize<'de> for StmtPacket {
    type Ctx = ();

    fn deserialize((): Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let mut buf = buf.checked_eat_buf(12).ok_or_else(unexpected_buf_eof)?;

        if buf.eat_u8() != 0x00 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid statement packet status",
            ));
        }

        let statement_id = buf.eat_u32_le();
        let num_columns = buf.eat_u16_le();
        let num_params = buf.eat_u16_le();
        buf.skip(1);
        let warning_count = buf.eat_u16_le();

        Ok(StmtPacket {
            statement_id,
            num_columns,
            num_params,
            warning_count,
        })
    }
}

impl StmtPacket {
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

impl<'de, T: SerializationSide> MyDeserialize<'de> for NullBitmap<T, Cow<'de, [u8]>> {
    type Ctx = usize;

    fn deserialize(num_columns: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        let bitmap_len = Self::bitmap_len(num_columns);
        let bytes = buf.checked_eat(bitmap_len).ok_or_else(unexpected_buf_eof)?;
        Ok(Self::from_bytes(Cow::Borrowed(bytes)))
    }
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct ComStmtExecuteRequestBuilder {
    pub stmt_id: u32,
}

impl ComStmtExecuteRequestBuilder {
    pub const NULL_BITMAP_OFFSET: usize = 10;

    pub fn new(stmt_id: u32) -> Self {
        Self { stmt_id }
    }
}

impl ComStmtExecuteRequestBuilder {
    pub fn build<'a>(self, params: &'a [Value]) -> (ComStmtExecuteRequest<'a>, bool) {
        let bitmap_len = NullBitmap::<ClientSide>::bitmap_len(params.len());

        let mut bitmap_bytes = vec![0; bitmap_len];
        let mut bitmap = NullBitmap::<ClientSide, _>::from_bytes(&mut bitmap_bytes);
        let params = params.iter().collect::<Vec<_>>();

        let meta_len = params.len() * 2;

        let mut data_len = 0;
        for (i, param) in params.iter().enumerate() {
            match param.bin_len() as usize {
                0 => bitmap.set(i, true),
                x => data_len += x,
            }
        }

        let total_len = 10 + bitmap_len + 1 + meta_len + data_len;

        let as_long_data = total_len > MAX_PAYLOAD_LEN;

        (
            ComStmtExecuteRequest {
                stmt_id: self.stmt_id,
                bitmap: bitmap_bytes,
                params,
                as_long_data,
            },
            as_long_data,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComStmtExecuteRequest<'a> {
    stmt_id: u32,
    bitmap: Vec<u8>,
    params: Vec<&'a Value>,
    as_long_data: bool,
}

impl MySerialize for ComStmtExecuteRequest<'_> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        buf.put_u8(Command::COM_STMT_EXECUTE as u8);
        buf.put_u32_le(self.stmt_id);
        // 1b flags, 4b le iteration-count
        buf.put_slice(&[0, 1, 0, 0, 0]);

        if !self.params.is_empty() {
            buf.put_slice(&*self.bitmap);
            buf.put_u8(1);
        }

        for param in &self.params {
            let (column_type, flags) = match param {
                Value::NULL => (ColumnType::MYSQL_TYPE_NULL, 0),
                Value::Bytes(_) => (ColumnType::MYSQL_TYPE_VAR_STRING, 0),
                Value::Int(_) => (ColumnType::MYSQL_TYPE_LONGLONG, 0),
                Value::UInt(_) => {
                    (ColumnType::MYSQL_TYPE_LONGLONG, 0x80 /* unsigned */)
                }
                Value::Float(_) => (ColumnType::MYSQL_TYPE_FLOAT, 0),
                Value::Double(_) => (ColumnType::MYSQL_TYPE_DOUBLE, 0),
                Value::Date(..) => (ColumnType::MYSQL_TYPE_DATETIME, 0),
                Value::Time(..) => (ColumnType::MYSQL_TYPE_TIME, 0),
            };

            buf.put_slice(&[column_type as u8, flags]);
        }

        for param in &self.params {
            match **param {
                Value::Int(_)
                | Value::UInt(_)
                | Value::Float(_)
                | Value::Double(_)
                | Value::Date(..)
                | Value::Time(..) => {
                    param.serialize(buf);
                }
                Value::Bytes(_) if !self.as_long_data => {
                    param.serialize(buf);
                }
                Value::Bytes(_) | Value::NULL => {}
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ComStmtSendLongData<'a> {
    pub stmt_id: u32,
    pub param_index: u16,
    pub data: Cow<'a, [u8]>,
}

impl<'a> ComStmtSendLongData<'a> {
    pub fn new(stmt_id: u32, param_index: u16, data: Cow<'a, [u8]>) -> Self {
        Self {
            stmt_id,
            param_index,
            data,
        }
    }
}

impl MySerialize for ComStmtSendLongData<'_> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        buf.put_u8(Command::COM_STMT_SEND_LONG_DATA as u8);
        buf.put_u32_le(self.stmt_id);
        buf.put_u16_le(self.param_index as u16);
        buf.put_slice(self.data.as_ref());
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ComStmtClose {
    pub stmt_id: u32,
}

impl ComStmtClose {
    pub fn new(stmt_id: u32) -> Self {
        Self { stmt_id }
    }
}

impl MySerialize for ComStmtClose {
    fn serialize(&self, buf: &mut Vec<u8>) {
        buf.put_u8(Command::COM_STMT_CLOSE as u8);
        buf.put_u32_le(self.stmt_id);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        constants::{CapabilityFlags, ColumnFlags, ColumnType, StatusFlags, UTF8_GENERAL_CI},
        proto::{MyDeserialize, MySerialize},
    };
    use std::collections::HashMap;

    #[test]
    fn should_parse_local_infile_packet() {
        const LIP: &[u8] = b"\xfbfile_name";

        let lip = LocalInfilePacket::deserialize((), &mut ParseBuf(LIP)).unwrap();
        assert_eq!(lip.file_name_str(), "file_name");
    }

    #[test]
    fn should_parse_stmt_packet() {
        const SP: &[u8] = b"\x00\x01\x00\x00\x00\x01\x00\x02\x00\x00\x00\x00";
        const SP_2: &[u8] = b"\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";

        let sp = StmtPacket::deserialize((), &mut ParseBuf(SP)).unwrap();
        assert_eq!(sp.statement_id(), 0x01);
        assert_eq!(sp.num_columns(), 0x01);
        assert_eq!(sp.num_params(), 0x02);
        assert_eq!(sp.warning_count(), 0x00);

        let sp = StmtPacket::deserialize((), &mut ParseBuf(SP_2)).unwrap();
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

        let hsp = HandshakePacket::deserialize((), &mut ParseBuf(HSP)).unwrap();
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

        let hsp = HandshakePacket::deserialize((), &mut ParseBuf(HSP_2)).unwrap();
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

        let err_packet =
            ErrPacket::deserialize(CapabilityFlags::empty(), &mut ParseBuf(ERR_PACKET)).unwrap();
        assert!(err_packet.is_error());
        assert_eq!(err_packet.error_code(), 1096);
        assert_eq!(err_packet.sql_state_str(), "HY000");
        assert_eq!(err_packet.message_str(), "No tables used");

        let err_packet = ErrPacket::deserialize(
            CapabilityFlags::CLIENT_PROTOCOL_41,
            &mut ParseBuf(ERR_PACKET_NO_STATE),
        )
        .unwrap();
        assert!(err_packet.is_error());
        assert_eq!(err_packet.error_code(), 1040);
        assert_eq!(err_packet.sql_state_str(), "HY000");
        assert_eq!(err_packet.message_str(), "Too many connections");

        let err_packet = ErrPacket::deserialize(
            CapabilityFlags::CLIENT_PROGRESS_OBSOLETE,
            &mut ParseBuf(PROGRESS_PACKET),
        )
        .unwrap();
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
        let column = Column::deserialize((), &mut ParseBuf(COLUMN_PACKET)).unwrap();
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
        let packet = AuthSwitchRequest::deserialize((), &mut ParseBuf(PAYLOAD)).unwrap();
        assert_eq!(packet.auth_plugin().as_bytes(), b"mysql_native_password",);
        assert_eq!(packet.plugin_data(), b"zQg4i6oNy6=rHN/>-b)A",)
    }

    #[test]
    fn should_parse_auth_more_data() {
        const PAYLOAD: &[u8] = b"\x01\x04";
        let packet = AuthMoreData::deserialize((), &mut ParseBuf(PAYLOAD)).unwrap();
        assert_eq!(packet.data(), b"\x04",);
    }

    #[test]
    fn should_parse_ok_packet() {
        const PLAIN_OK: &[u8] = b"\x00\x00\x00\x02\x00\x00\x00";
        const SESS_STATE_SYS_VAR_OK: &[u8] =
            b"\x00\x00\x00\x02\x40\x00\x00\x00\x11\x00\x0f\x0a\x61\
              \x75\x74\x6f\x63\x6f\x6d\x6d\x69\x74\x03\x4f\x46\x46";
        const SESS_STATE_SCHEMA_OK: &[u8] =
            b"\x00\x00\x00\x02\x40\x00\x00\x00\x07\x01\x05\x04\x74\x65\x73\x74";
        const SESS_STATE_TRACK_OK: &[u8] = b"\x00\x00\x00\x02\x40\x00\x00\x00\x04\x02\x02\x01\x31";
        const EOF: &[u8] = b"\xfe\x00\x00\x02\x00";

        // packet starting with 0x00 is not an ok packet if it terminates a result set
        OkPacketDeserializer::<ResultSetTerminator>::deserialize(
            CapabilityFlags::empty(),
            &mut ParseBuf(PLAIN_OK),
        )
        .unwrap_err();

        let ok_packet: OkPacket = OkPacketDeserializer::<CommonOkPacket>::deserialize(
            CapabilityFlags::empty(),
            &mut ParseBuf(PLAIN_OK),
        )
        .unwrap()
        .into();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            StatusFlags::SERVER_STATUS_AUTOCOMMIT
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        assert_eq!(ok_packet.session_state_info(), None);

        let ok_packet: OkPacket = OkPacketDeserializer::<CommonOkPacket>::deserialize(
            CapabilityFlags::CLIENT_SESSION_TRACK,
            &mut ParseBuf(SESS_STATE_SYS_VAR_OK),
        )
        .unwrap()
        .into();
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

        let ok_packet: OkPacket = OkPacketDeserializer::<CommonOkPacket>::deserialize(
            CapabilityFlags::CLIENT_SESSION_TRACK,
            &mut ParseBuf(SESS_STATE_SCHEMA_OK),
        )
        .unwrap()
        .into();
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

        let ok_packet: OkPacket = OkPacketDeserializer::<CommonOkPacket>::deserialize(
            CapabilityFlags::CLIENT_SESSION_TRACK,
            &mut ParseBuf(SESS_STATE_TRACK_OK),
        )
        .unwrap()
        .into();
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

        let ok_packet: OkPacket = OkPacketDeserializer::<ResultSetTerminator>::deserialize(
            CapabilityFlags::CLIENT_SESSION_TRACK,
            &mut ParseBuf(EOF),
        )
        .unwrap()
        .into();
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

    #[test]
    fn should_build_handshake_response() {
        let flags_without_db_name = CapabilityFlags::from_bits_truncate(0x81bea205);
        let response = HandshakeResponse::new(
            Some((&[][..]).into()),
            (5u16, 5, 5),
            Some("root".into()),
            None,
            AuthPlugin::MysqlNativePassword,
            flags_without_db_name,
            HashMap::new(),
        );
        let mut actual = Vec::new();
        response.serialize(&mut actual);

        let expected: Vec<u8> = [
            0x05, 0xa2, 0xbe, 0x81, // client capabilities
            0x00, 0x00, 0x00, 0x01, // max packet
            0x2d, // charset
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // reserved
            0x72, 0x6f, 0x6f, 0x74, 0x00, // username=root
            0x00, // blank scramble
            0x6d, 0x79, 0x73, 0x71, 0x6c, 0x5f, 0x6e, 0x61, 0x74, 0x69, 0x76, 0x65, 0x5f, 0x70,
            0x61, 0x73, 0x73, 0x77, 0x6f, 0x72, 0x64, 0x00, // mysql_native_password
            0x00,
        ]
        .to_vec();

        assert_eq!(expected, actual);

        let flags_with_db_name = flags_without_db_name | CapabilityFlags::CLIENT_CONNECT_WITH_DB;
        let response = HandshakeResponse::new(
            Some((&[][..]).into()),
            (5u16, 5, 5),
            Some("root".into()),
            Some("mydb".into()),
            AuthPlugin::MysqlNativePassword,
            flags_with_db_name,
            HashMap::new(),
        );
        let mut actual = Vec::new();
        response.serialize(&mut actual);

        let expected: Vec<u8> = [
            0x0d, 0xa2, 0xbe, 0x81, // client capabilities
            0x00, 0x00, 0x00, 0x01, // max packet
            0x2d, // charset
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // reserved
            0x72, 0x6f, 0x6f, 0x74, 0x00, // username=root
            0x00, // blank scramble
            0x6d, 0x79, 0x64, 0x62, 0x00, // dbname
            0x6d, 0x79, 0x73, 0x71, 0x6c, 0x5f, 0x6e, 0x61, 0x74, 0x69, 0x76, 0x65, 0x5f, 0x70,
            0x61, 0x73, 0x73, 0x77, 0x6f, 0x72, 0x64, 0x00, // mysql_native_password
            0x00,
        ]
        .to_vec();

        assert_eq!(expected, actual);

        let response = HandshakeResponse::new(
            Some((&[][..]).into()),
            (5u16, 5, 5),
            Some("root".into()),
            Some("mydb".into()),
            AuthPlugin::MysqlNativePassword,
            flags_without_db_name,
            HashMap::new(),
        );
        let mut actual = Vec::new();
        response.serialize(&mut actual);
        assert_eq!(expected, actual);

        let response = HandshakeResponse::new(
            Some((&[][..]).into()),
            (5u16, 5, 5),
            Some("root".into()),
            None,
            AuthPlugin::MysqlNativePassword,
            flags_with_db_name,
            HashMap::new(),
        );
        let mut actual = Vec::new();
        response.serialize(&mut actual);

        let expected: Vec<u8> = [
            0x0d, 0xa2, 0xbe, 0x81, // client capabilities
            0x00, 0x00, 0x00, 0x01, // max packet
            0x2d, // charset
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // reserved
            0x72, 0x6f, 0x6f, 0x74, 0x00, // username=root
            0x00, // blank db_name
            0x00, // blank scramble
            0x6d, 0x79, 0x73, 0x71, 0x6c, 0x5f, 0x6e, 0x61, 0x74, 0x69, 0x76, 0x65, 0x5f, 0x70,
            0x61, 0x73, 0x73, 0x77, 0x6f, 0x72, 0x64, 0x00, // mysql_native_password
            0x00,
        ]
        .to_vec();
        assert_eq!(expected, actual);
    }
}

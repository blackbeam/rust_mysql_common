use io::ReadMysqlExt;
use byteorder::{LittleEndian as LE, ReadBytesExt};
use constants::{CLIENT_PROGRESS_OBSOLETE, CLIENT_SESSION_TRACK, SERVER_SESSION_STATE_CHANGED,
                CapabilityFlags, ColumnFlags, ColumnType, SessionStateType, StatusFlags};
use std::borrow::Cow;
use std::fmt;
use std::io;
use std::ptr;

macro_rules! split_at_or_err {
    ($reader:expr, $at:expr, $msg:expr) => {
        if $reader.len() >= $at {
            Ok($reader.split_at($at))
        } else {
            Err(io::Error::new(io::ErrorKind::UnexpectedEof, $msg))
        }
    };
}

macro_rules! read_lenenc_str {
    ($reader:expr) => {
        $reader.read_lenenc_int().and_then(|len| {
            split_at_or_err!($reader, len as usize, "EOF while reading length-encoded string")
        })
    };
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Column<'a> {
    schema: Cow<'a, [u8]>,
    table: Cow<'a, [u8]>,
    org_table: Cow<'a, [u8]>,
    name: Cow<'a, [u8]>,
    org_name: Cow<'a, [u8]>,
    column_length: u32,
    character_set: u16,
    flags: ColumnFlags,
    column_type: ColumnType,
    decimals: u8,
}

pub fn parse_column(payload: &[u8]) -> io::Result<Column> {
    Column::parse(payload)
}

impl<'a> Column<'a> {
    fn parse<'x>(payload: &'x [u8]) -> io::Result<Column<'x>> {
        // Skip "def"
        let mut reader = &payload[4..];
        let (schema, mut reader) = read_lenenc_str!(reader)?;
        let (table, mut reader) = read_lenenc_str!(reader)?;
        let (org_table, mut reader) = read_lenenc_str!(reader)?;
        let (name, mut reader) = read_lenenc_str!(reader)?;
        let (org_name, mut reader) = read_lenenc_str!(reader)?;
        reader = &reader[1..];
        let character_set = reader.read_u16::<LE>()?;
        let column_length = reader.read_u32::<LE>()?;
        let column_type = reader.read_u8()?;
        let flags = reader.read_u16::<LE>()?;
        let decimals = reader.read_u8()?;

        Ok(Column {
            schema: schema.into(),
            table: table.into(),
            org_table: org_table.into(),
            name: name.into(),
            org_name: org_name.into(),
            column_length,
            character_set,
            flags: ColumnFlags::from_bits_truncate(flags),
            column_type: ColumnType::from(column_type),
            decimals,
        })
    }

    pub fn into_owned(self) -> Column<'static> {
        let Column {
            schema,
            table,
            org_table,
            name,
            org_name,
            column_length,
            character_set,
            flags,
            column_type,
            decimals,
        } = self;
        Column {
            schema: schema.into_owned().into(),
            table: table.into_owned().into(),
            org_table: org_table.into_owned().into(),
            name: name.into_owned().into(),
            org_name: org_name.into_owned().into(),
            column_length,
            character_set,
            flags,
            column_type,
            decimals,
        }
    }

    pub fn column_length(&self) -> u32 {
        self.column_length
    }

    pub fn column_type(&self) -> ColumnType {
        self.column_type
    }

    pub fn character_set(&self) -> u16 {
        self.character_set
    }

    pub fn flags(&self) -> ColumnFlags {
        self.flags
    }

    pub fn decimals(&self) -> u8 {
        self.decimals
    }

    pub fn schema_ref(&self) -> &[u8] {
        self.schema.as_ref()
    }

    pub fn schema_str(&'a self) -> Cow<'a, str> {
        String::from_utf8_lossy(self.schema.as_ref())
    }

    pub fn table_ref(&self) -> &[u8] {
        self.table.as_ref()
    }

    pub fn table_str(&'a self) -> Cow<'a, str> {
        String::from_utf8_lossy(self.table.as_ref())
    }

    pub fn org_table_ref(&self) -> &[u8] {
        self.org_table.as_ref()
    }

    pub fn org_table_str(&'a self) -> Cow<'a, str> {
        String::from_utf8_lossy(self.org_table.as_ref())
    }

    pub fn name_ref(&self) -> &[u8] {
        self.name.as_ref()
    }

    pub fn name_str(&'a self) -> Cow<'a, str> {
        String::from_utf8_lossy(self.name.as_ref())
    }

    pub fn org_name_ref(&self) -> &[u8] {
        self.org_name.as_ref()
    }

    pub fn org_name_str(&'a self) -> Cow<'a, str> {
        String::from_utf8_lossy(self.org_name.as_ref())
    }
}

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
            SessionStateChange::SystemVariable(name, value) => {
                SessionStateChange::SystemVariable(
                    name.into_owned().into(),
                    value.into_owned().into(),
                )
            }
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

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SessionStateInfo<'a> {
    data_type: SessionStateType,
    data: Cow<'a, [u8]>,
}

impl<'a> SessionStateInfo<'a> {
    pub fn parse(mut payload: &[u8]) -> io::Result<SessionStateInfo> {
        let data_type = payload.read_u8()?;
        Ok(SessionStateInfo {
            data_type: data_type.into(),
            data: read_lenenc_str!(payload)?.0.into(),
        })
    }

    pub fn into_owned(self) -> SessionStateInfo<'static> {
        let SessionStateInfo { data_type, data } = self;
        SessionStateInfo { data_type, data: data.into_owned().into() }
    }

    pub fn data_type(&self) -> SessionStateType {
        self.data_type
    }

    pub fn decode(&self) -> io::Result<SessionStateChange> {
        let mut reader = self.data.as_ref();
        match self.data_type {
            SessionStateType::SESSION_TRACK_SYSTEM_VARIABLES => {
                let (name, mut reader) = read_lenenc_str!(reader)?;
                let (value, _) = read_lenenc_str!(reader)?;
                Ok(SessionStateChange::SystemVariable(
                    name.into(),
                    value.into(),
                ))
            }
            SessionStateType::SESSION_TRACK_SCHEMA => {
                let (schema, _) = read_lenenc_str!(reader)?;
                Ok(SessionStateChange::Schema(schema.into()))
            }
            SessionStateType::SESSION_TRACK_STATE_CHANGE => {
                let (is_tracked, _) = read_lenenc_str!(reader)?;
                Ok(SessionStateChange::IsTracked(is_tracked == b"1"))
            }
            // Layout not specified in documentation
            SessionStateType::SESSION_TRACK_GTIDS |
            SessionStateType::SESSION_TRACK_TRANSACTION_CHARACTERISTICS |
            SessionStateType::SESSION_TRACK_TRANSACTION_STATE => {
                Ok(SessionStateChange::UnknownLayout(self.data.clone()))
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct OkPacket<'a> {
    affected_rows: u64,
    last_insert_id: Option<u64>,
    status_flags: StatusFlags,
    warnings: u16,
    info: Option<Cow<'a, [u8]>>,
    session_state_info: Option<SessionStateInfo<'a>>,
}

pub fn parse_ok_packet(payload: &[u8], capabilities: CapabilityFlags) -> io::Result<OkPacket> {
    OkPacket::parse(payload, capabilities)
}

impl<'a> OkPacket<'a> {
    fn parse<'x>(mut payload: &'x [u8], capabilities: CapabilityFlags) -> io::Result<OkPacket<'x>> {
        let header = payload.read_u8()?;
        let (affected_rows, last_insert_id, status_flags, warnings, info, session_state_info) =
            if header == 0x00 {
                let affected_rows = payload.read_lenenc_int()?;
                let last_insert_id = payload.read_lenenc_int()?;
                // We assume that CLIENT_PROTOCOL_41 was set
                let status_flags = StatusFlags::from_bits_truncate(payload.read_u16::<LE>()?);
                let warnings = payload.read_u16::<LE>()?;

                let (info, session_state_info) = if capabilities.contains(CLIENT_SESSION_TRACK) {
                    let (info, mut payload) = read_lenenc_str!(payload)?;
                    let session_state_info =
                        if status_flags.contains(SERVER_SESSION_STATE_CHANGED) {
                            let (session_state_info, _) = read_lenenc_str!(payload)?;
                            session_state_info
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
            } else if header == 0xFE && payload.len() < 8 {
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
            info: if info.len() > 0 {
                Some(info.into())
            } else {
                None
            },
            session_state_info: if session_state_info.len() > 0 {
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

    pub fn affected_rows(&self) -> u64 {
        self.affected_rows
    }

    pub fn last_insert_id(&self) -> Option<u64> {
        self.last_insert_id
    }

    pub fn status_flags(&self) -> StatusFlags {
        self.status_flags
    }

    pub fn warnings(&self) -> u16 {
        self.warnings
    }

    pub fn info_ref(&self) -> Option<&[u8]> {
        self.info.as_ref().map(|x| x.as_ref())
    }

    pub fn info_str<'x>(&'x self) -> Option<Cow<'x, str>> {
        self.info.as_ref().map(
            |x| String::from_utf8_lossy(x.as_ref()),
        )
    }

    pub fn session_state_info(&self) -> Option<&SessionStateInfo> {
        self.session_state_info.as_ref()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProgressReport<'a> {
    stage: u8,
    max_stage: u8,
    progress: u32,
    stage_info: Cow<'a, [u8]>,
}

impl<'a> ProgressReport<'a> {
    fn new<'x>(
        stage: u8,
        max_stage: u8,
        progress: u32,
        stage_info: &'x [u8],
    ) -> ProgressReport<'x> {
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

    /// Status or state name
    pub fn stage_info_ref(&self) -> &[u8] {
        &self.stage_info.as_ref()
    }

    /// Status or state name
    pub fn stage_info_str(&self) -> Cow<str> {
        String::from_utf8_lossy(self.stage_info.as_ref())
    }

    pub fn into_owned(self) -> ProgressReport<'static> {
        let ProgressReport { stage, max_stage, progress, stage_info } = self;
        ProgressReport {
            stage,
            max_stage,
            progress,
            stage_info: stage_info.into_owned().into(),
        }
    }
}

impl<'a> fmt::Display for ProgressReport<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
#[derive(Clone, PartialEq, Debug)]
pub enum ErrPacket<'a> {
    /// (<error code>, <sql state>, <error message>)
    Error(u16, [u8; 5], Cow<'a, [u8]>),
    Progress(ProgressReport<'a>),
}

pub fn parse_err_packet(payload: &[u8], capabilities: CapabilityFlags) -> io::Result<ErrPacket> {
    ErrPacket::parse(payload, capabilities)
}

impl<'a> ErrPacket<'a> {
    fn parse(mut payload: &[u8], capabilities: CapabilityFlags) -> io::Result<ErrPacket> {
        if payload.read_u8()? != 0xFF {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid ERR_Packet header",
            ));
        }

        let code = payload.read_u16::<LE>()?;
        // We assume that CLIENT_PROTOCOL_41 was set
        if code == 0xFFFF && capabilities.contains(CLIENT_PROGRESS_OBSOLETE) {
            payload.read_u8()?; // Ignore number of strings.
            let stage = payload.read_u8()?;
            let max_stage = payload.read_u8()?;
            let progress = payload.read_uint::<LE>(3)?;
            let (progress_info, _) = read_lenenc_str!(payload)?;
            Ok(ErrPacket::Progress(ProgressReport::new(
                stage,
                max_stage,
                progress as u32,
                progress_info,
            )))
        } else {
            let marker = payload.read_u8()?;
            if marker == b'#' {
                let (state, msg) = split_at_or_err!(payload, 5, "EOF while reading error state")?;
                Ok(ErrPacket::Error(
                    code,
                    unsafe { ptr::read(state.as_ptr() as *const [u8; 5]) },
                    msg.into(),
                ))
            } else {
                Ok(ErrPacket::Error(
                    code,
                    [b'H', b'Y', b'0', b'0', b'0'],
                    payload.into(),
                ))
            }
        }
    }

    pub fn is_error(&self) -> bool {
        match *self {
            ErrPacket::Error(..) => true,
            _ => false,
        }
    }

    pub fn is_progress_report(&self) -> bool {
        !self.is_error()
    }

    /// Will panic if ErrPacket does not contains progress report
    pub fn progress_report(&self) -> &ProgressReport {
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
    pub fn sql_state_str(&self) -> Cow<str> {
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
    pub fn message_str(&self) -> Cow<str> {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrPacket::Error(..) => {
                write!(
                    f,
                    "ERROR {} ({}): {}",
                    self.error_code(),
                    self.sql_state_str(),
                    self.message_str()
                )
            }
            ErrPacket::Progress(ref progress_report) => write!(f, "{}", progress_report),
        }
    }
}

#[cfg(test)]
mod test {
    use constants::{CLIENT_PROGRESS_OBSOLETE, CLIENT_SESSION_TRACK, NOT_NULL_FLAG,
                    SERVER_SESSION_STATE_CHANGED, SERVER_STATUS_AUTOCOMMIT, UTF8_GENERAL_CI,
                    ColumnType, CapabilityFlags};
    use super::{parse_column, parse_err_packet, parse_ok_packet, SessionStateChange};

    #[test]
    fn should_parse_err_packet() {
        const ERR_PACKET: &[u8] = b"\xff\x48\x04\x23\x48\x59\x30\x30\x30\x4e\x6f\x20\x74\x61\x62\
        \x6c\x65\x73\x20\x75\x73\x65\x64";
        const PROGRESS_PACKET: &[u8] = b"\xff\xff\xff\x01\x01\x0a\xcc\x5b\x00\x0astage name";

        let err_packet = parse_err_packet(ERR_PACKET, CapabilityFlags::empty()).unwrap();
        assert!(err_packet.is_error());
        assert_eq!(err_packet.error_code(), 1096);
        assert_eq!(err_packet.message_str(), "No tables used");

        let err_packet = parse_err_packet(PROGRESS_PACKET, CLIENT_PROGRESS_OBSOLETE).unwrap();
        assert!(err_packet.is_progress_report());
        let progress_report = err_packet.progress_report();
        assert_eq!(progress_report.stage(), 1);
        assert_eq!(progress_report.max_stage(), 10);
        assert_eq!(progress_report.progress(), 23500);
        assert_eq!(progress_report.stage_info_str(), "stage name");
    }

    #[test]
    fn should_parse_column_packet() {
        const COLUMN_PACKET: &[u8] =
            b"\x03def\x06schema\x05table\x09org_table\x04name\
              \x08org_name\x0c\x21\x00\x0F\x00\x00\x00\x00\x01\x00\x08\x00\x00";
        let column = parse_column(COLUMN_PACKET).unwrap();
        assert_eq!(column.schema_str(), "schema");
        assert_eq!(column.table_str(), "table");
        assert_eq!(column.org_table_str(), "org_table");
        assert_eq!(column.name_str(), "name");
        assert_eq!(column.org_name_str(), "org_name");
        assert_eq!(column.character_set(), UTF8_GENERAL_CI);
        assert_eq!(column.column_length(), 15);
        assert_eq!(column.column_type(), ColumnType::MYSQL_TYPE_DECIMAL);
        assert_eq!(column.flags(), NOT_NULL_FLAG);
        assert_eq!(column.decimals(), 8);
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

        let ok_packet = parse_ok_packet(PLAIN_OK, CapabilityFlags::empty()).unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(ok_packet.status_flags(), SERVER_STATUS_AUTOCOMMIT);
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        assert_eq!(ok_packet.session_state_info(), None);

        let ok_packet = parse_ok_packet(SESS_STATE_SYS_VAR_OK, CLIENT_SESSION_TRACK).unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            SERVER_STATUS_AUTOCOMMIT | SERVER_SESSION_STATE_CHANGED
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        let sess_state_info = ok_packet.session_state_info().unwrap();
        assert_eq!(
            sess_state_info.decode().unwrap(),
            SessionStateChange::SystemVariable((&b"autocommit"[..]).into(), (&b"OFF"[..]).into())
        );

        let ok_packet = parse_ok_packet(SESS_STATE_SCHEMA_OK, CLIENT_SESSION_TRACK).unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            SERVER_STATUS_AUTOCOMMIT | SERVER_SESSION_STATE_CHANGED
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        let sess_state_info = ok_packet.session_state_info().unwrap();
        assert_eq!(
            sess_state_info.decode().unwrap(),
            SessionStateChange::Schema((&b"test"[..]).into())
        );

        let ok_packet = parse_ok_packet(SESS_STATE_TRACK_OK, CLIENT_SESSION_TRACK).unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(
            ok_packet.status_flags(),
            SERVER_STATUS_AUTOCOMMIT | SERVER_SESSION_STATE_CHANGED
        );
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        let sess_state_info = ok_packet.session_state_info().unwrap();
        assert_eq!(
            sess_state_info.decode().unwrap(),
            SessionStateChange::IsTracked(true)
        );

        let ok_packet = parse_ok_packet(EOF, CLIENT_SESSION_TRACK).unwrap();
        assert_eq!(ok_packet.affected_rows(), 0);
        assert_eq!(ok_packet.last_insert_id(), None);
        assert_eq!(ok_packet.status_flags(), SERVER_STATUS_AUTOCOMMIT);
        assert_eq!(ok_packet.warnings(), 0);
        assert_eq!(ok_packet.info_ref(), None);
        assert_eq!(ok_packet.session_state_info(), None);
    }
}

//! Previous GTIDs Event.
//!
//! This event contains the complete GTID set of all transactions committed
//! prior to the current binlog file. It appears near the start of each binlog
//! file.
//!
//! # Binary Format (from MySQL source: `Gtid_set::encode()`)
//!
//! ## Header (8 bytes LE u64)
//!
//! Encodes both format type and n_sids:
//! - Bits 56-63: format identifier (`0x00` = untagged, `0x01` = tagged)
//! - Untagged: n_sids = lower 56 bits
//! - Tagged: n_sids = bits 8-55 (format byte repeated at bit 0-7)
//!
//! ## Per TSID entry
//!
//! 1. UUID (16 bytes)
//! 2. Tag (only in tagged format): varlen uint length + tag bytes
//! 3. n_intervals (8 bytes LE u64)
//! 4. Intervals (16 bytes each): start (LE u64) + end (LE u64, exclusive)

use std::{cmp::min, io};

use crate::{
    binlog::{
        BinlogCtx, BinlogEvent, BinlogStruct,
        consts::{BinlogVersion, EventType},
    },
    io::ParseBuf,
    misc::{read_varlen_uint, write_varlen_uint},
    packets::{GnoInterval, Tag},
    proto::{MyDeserialize, MySerialize},
};

use super::BinlogEventHeader;

/// An entry in [`PreviousGtidsEvent`] representing one (UUID, tag, intervals) tuple.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PreviousGtidsSid<'a> {
    uuid: [u8; 16],
    tag: Option<Tag<'a>>,
    intervals: Vec<GnoInterval>,
}

impl<'a> PreviousGtidsSid<'a> {
    /// Creates a new entry.
    pub fn new(uuid: [u8; 16], tag: Option<Tag<'a>>, intervals: Vec<GnoInterval>) -> Self {
        Self {
            uuid,
            tag,
            intervals,
        }
    }

    /// Returns the UUID.
    pub fn uuid(&self) -> [u8; 16] {
        self.uuid
    }

    /// Returns the tag, if any.
    pub fn tag(&self) -> Option<&Tag<'a>> {
        self.tag.as_ref()
    }

    /// Returns the GNO intervals.
    pub fn intervals(&self) -> &[GnoInterval] {
        &self.intervals
    }

    /// Returns a `'static` version of `self`.
    pub fn into_owned(self) -> PreviousGtidsSid<'static> {
        PreviousGtidsSid {
            uuid: self.uuid,
            tag: self.tag.map(|t| t.into_owned()),
            intervals: self.intervals,
        }
    }
}

/// Previous GTIDs Event.
///
/// Contains the complete GTID set of all transactions committed prior to the
/// current binlog file. Appears near the start of each binlog file.
///
/// Supports both the legacy untagged format and the tagged format introduced
/// in MySQL 8.4+.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PreviousGtidsEvent<'a> {
    sids: Vec<PreviousGtidsSid<'a>>,
}

impl<'a> PreviousGtidsEvent<'a> {
    /// Creates a new event.
    pub fn new(sids: Vec<PreviousGtidsSid<'a>>) -> Self {
        Self { sids }
    }

    /// Returns the SID entries.
    pub fn sids(&self) -> &[PreviousGtidsSid<'a>] {
        &self.sids
    }

    /// Returns a `'static` version of `self`.
    pub fn into_owned(self) -> PreviousGtidsEvent<'static> {
        PreviousGtidsEvent {
            sids: self.sids.into_iter().map(|s| s.into_owned()).collect(),
        }
    }

    /// Returns true if any SID entry has a tag.
    fn is_tagged(&self) -> bool {
        self.sids.iter().any(|s| s.tag.is_some())
    }
}

impl<'de> MyDeserialize<'de> for PreviousGtidsEvent<'de> {
    const SIZE: Option<usize> = None;
    type Ctx = BinlogCtx<'de>;

    fn deserialize(_ctx: Self::Ctx, buf: &mut ParseBuf<'de>) -> io::Result<Self> {
        if buf.len() < 8 {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "PreviousGtidsEvent too short for header",
            ));
        }

        let mut header_bytes = [0u8; 8];
        header_bytes.copy_from_slice(&buf.0[..8]);
        buf.0 = &buf.0[8..];
        let header = u64::from_le_bytes(header_bytes);

        let format = (header >> 56) as u8;
        let tagged = format != 0;

        let n_sids = if tagged {
            (header >> 8) & ((1u64 << 48) - 1)
        } else {
            header & ((1u64 << 56) - 1)
        };

        let mut sids = Vec::with_capacity(n_sids as usize);

        for _ in 0..n_sids {
            // UUID (16 bytes)
            if buf.len() < 16 {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "PreviousGtidsEvent: unexpected EOF reading UUID",
                ));
            }
            let mut uuid = [0u8; 16];
            uuid.copy_from_slice(&buf.0[..16]);
            buf.0 = &buf.0[16..];

            // Tag (only in tagged format)
            let tag = if tagged {
                let tag_len = read_varlen_uint(buf)? as usize;
                if tag_len == 0 {
                    None
                } else {
                    if buf.len() < tag_len {
                        return Err(io::Error::new(
                            io::ErrorKind::UnexpectedEof,
                            "PreviousGtidsEvent: unexpected EOF reading tag",
                        ));
                    }
                    let tag_bytes = &buf.0[..tag_len];
                    buf.0 = &buf.0[tag_len..];
                    let tag_str = std::str::from_utf8(tag_bytes).map_err(|e| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("invalid UTF-8 tag: {}", e),
                        )
                    })?;
                    Some(Tag::new(tag_str).map_err(|e| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("invalid GTID tag: {}", e),
                        )
                    })?)
                }
            } else {
                None
            };

            // n_intervals (8 bytes LE u64)
            if buf.len() < 8 {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "PreviousGtidsEvent: unexpected EOF reading n_intervals",
                ));
            }
            let mut n_intervals_bytes = [0u8; 8];
            n_intervals_bytes.copy_from_slice(&buf.0[..8]);
            buf.0 = &buf.0[8..];
            let n_intervals = u64::from_le_bytes(n_intervals_bytes);

            // Intervals (16 bytes each)
            let mut intervals = Vec::with_capacity(n_intervals as usize);
            for _ in 0..n_intervals {
                let interval: GnoInterval = buf.parse(())?;
                intervals.push(interval);
            }

            sids.push(PreviousGtidsSid {
                uuid,
                tag,
                intervals,
            });
        }

        Ok(Self { sids })
    }
}

impl MySerialize for PreviousGtidsEvent<'_> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        let tagged = self.is_tagged();
        let n_sids = self.sids.len() as u64;

        let header = if tagged {
            let format: u64 = 1;
            (format << 56) | (n_sids << 8) | format
        } else {
            n_sids
        };

        buf.extend_from_slice(&header.to_le_bytes());

        for sid in &self.sids {
            // UUID
            buf.extend_from_slice(&sid.uuid);

            // Tag (only in tagged format)
            if tagged {
                match &sid.tag {
                    Some(tag) => {
                        let tag_bytes = tag.as_str().as_bytes();
                        write_varlen_uint(buf, tag_bytes.len() as u64);
                        buf.extend_from_slice(tag_bytes);
                    }
                    None => {
                        write_varlen_uint(buf, 0);
                    }
                }
            }

            // n_intervals
            buf.extend_from_slice(&(sid.intervals.len() as u64).to_le_bytes());

            // Intervals
            for interval in &sid.intervals {
                interval.serialize(buf);
            }
        }
    }
}

impl<'a> BinlogStruct<'a> for PreviousGtidsEvent<'a> {
    fn len(&self, _version: BinlogVersion) -> usize {
        let mut tmp = Vec::new();
        self.serialize(&mut tmp);
        min(tmp.len(), u32::MAX as usize - BinlogEventHeader::LEN)
    }
}

impl<'a> BinlogEvent<'a> for PreviousGtidsEvent<'a> {
    const EVENT_TYPE: EventType = EventType::PREVIOUS_GTIDS_EVENT;
}

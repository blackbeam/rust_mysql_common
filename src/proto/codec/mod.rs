// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! MySql protocol codec implementation.

pub use flate2::Compression;

use byteorder::{ByteOrder, LittleEndian};
use bytes::{Buf, BufMut, BytesMut};
use flate2::{Compress, Decompress, FlushCompress, FlushDecompress, Status};

use std::{
    cmp::{max, min},
    io, mem,
    num::NonZeroUsize,
    ptr::slice_from_raw_parts_mut,
};

use self::error::PacketCodecError;
use crate::constants::{DEFAULT_MAX_ALLOWED_PACKET, MAX_PAYLOAD_LEN, MIN_COMPRESS_LENGTH};

pub mod error;

/// Will split given `packet` to MySql packet chunks and write into `dst`.
///
/// Chunk ids will start with given `seq_id`.
///
/// Resulting sequence id will be returned.
pub fn packet_to_chunks<T: Buf>(mut seq_id: u8, packet: &mut T, dst: &mut BytesMut) -> u8 {
    let extra_packet = packet.remaining().is_multiple_of(MAX_PAYLOAD_LEN);
    dst.reserve(packet.remaining() + (packet.remaining() / MAX_PAYLOAD_LEN) * 4 + 4);

    while packet.has_remaining() {
        let mut chunk_len = min(packet.remaining(), MAX_PAYLOAD_LEN);
        dst.put_u32_le(chunk_len as u32 | (u32::from(seq_id) << 24));
        while chunk_len > 0 {
            let chunk = packet.chunk();
            let count = min(chunk.len(), chunk_len);
            dst.put(&chunk[..count]);
            chunk_len -= count;
            packet.advance(count);
        }
        seq_id = seq_id.wrapping_add(1);
    }

    if extra_packet {
        dst.put_u32_le(u32::from(seq_id) << 24);
        seq_id = seq_id.wrapping_add(1);
    }

    seq_id
}

/// Will compress all data from `src` to `dst`.
///
/// Compressed packets will start with given `seq_id`. Resulting sequence id will be returned.
pub fn compress(
    seq_id: u8,
    compression: Compression,
    max_allowed_packet: usize,
    src: &mut BytesMut,
    dst: &mut BytesMut,
) -> Result<u8, PacketCodecError> {
    compress_with(seq_id, compression, &mut None, max_allowed_packet, src, dst)
}

/// Same as [`compress`], but deflates with `deflater`, creating it on first use.
///
/// `deflater` must have been created with `compression`.
fn compress_with(
    mut seq_id: u8,
    compression: Compression,
    deflater: &mut Option<Compress>,
    max_allowed_packet: usize,
    src: &mut BytesMut,
    dst: &mut BytesMut,
) -> Result<u8, PacketCodecError> {
    if src.is_empty() {
        return Ok(0);
    }

    for chunk in src.chunks(min(MAX_PAYLOAD_LEN, max_allowed_packet)) {
        dst.reserve(7 + chunk.len());

        let compressed_len =
            if compression != Compression::none() && chunk.len() >= MIN_COMPRESS_LENGTH {
                let deflater = deflater.get_or_insert_with(|| Compress::new(compression, true));
                deflate_into(deflater, chunk, dst)?
            } else {
                None
            };

        match compressed_len {
            Some(compressed_len) => unsafe {
                dst.put_uint_le(compressed_len as u64, 3);
                dst.put_u8(seq_id);
                dst.put_uint_le(chunk.len() as u64, 3);
                dst.advance_mut(compressed_len);
            },
            None => {
                dst.put_uint_le(chunk.len() as u64, 3);
                dst.put_u8(seq_id);
                dst.put_uint_le(0, 3);
                dst.put_slice(chunk);
            }
        }

        seq_id = seq_id.wrapping_add(1);
    }

    src.clear();

    Ok(seq_id)
}

/// Deflates `chunk` into the spare capacity of `dst`, leaving room for a compressed packet
/// header, and returns the compressed length.
///
/// Returns `None` if `chunk` does not get smaller, so that it is sent uncompressed.
/// `dst` must have at least `7 + chunk.len()` bytes of spare capacity.
fn deflate_into(
    deflater: &mut Compress,
    chunk: &[u8],
    dst: &mut BytesMut,
) -> io::Result<Option<usize>> {
    deflater.reset();
    let dst_buf = &mut dst.chunk_mut()[7..7 + chunk.len()];
    let status = unsafe {
        deflater.compress(
            chunk,
            &mut *slice_from_raw_parts_mut(dst_buf.as_mut_ptr(), dst_buf.len()),
            FlushCompress::Finish,
        )
    }
    .map_err(io::Error::other)?;

    let compressed_len = deflater.total_out() as usize;
    if status == Status::StreamEnd && compressed_len < chunk.len() {
        Ok(Some(compressed_len))
    } else {
        Ok(None)
    }
}

/// Inflates `compressed` into `dst`, failing unless it inflates to exactly `plain_len` bytes.
fn inflate_into(
    inflater: &mut Decompress,
    compressed: &[u8],
    plain_len: usize,
    dst: &mut BytesMut,
) -> io::Result<()> {
    inflater.reset(true);
    dst.reserve(plain_len);
    let dst_buf = &mut dst.chunk_mut()[..plain_len];
    let status = unsafe {
        inflater.decompress(
            compressed,
            &mut *slice_from_raw_parts_mut(dst_buf.as_mut_ptr(), dst_buf.len()),
            FlushDecompress::Finish,
        )
    }
    .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;

    if status != Status::StreamEnd || inflater.total_out() != plain_len as u64 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "compressed packet does not inflate to its declared length",
        ));
    }
    unsafe { dst.advance_mut(plain_len) };
    Ok(())
}

/// Chunk info.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ChunkInfo {
    /// A packet chunk with given sequence id that isn't last in a packet.
    ///
    /// Only makes sense for plain MySql protocol.
    Middle(u8),
    /// Last chunk in a packet. Stores chunk sequence id.
    ///
    /// The only variant that `CompDecoder` will return.
    Last(u8),
}

impl ChunkInfo {
    fn seq_id(self) -> u8 {
        match self {
            ChunkInfo::Middle(x) | ChunkInfo::Last(x) => x,
        }
    }
}

/// Decoder for MySql protocol chunk.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum ChunkDecoder {
    /// Decoder is waiting for the first or subsequent packet chunk.
    ///
    /// It'll need at least 4 bytes to start decoding a chunk.
    #[default]
    Idle,
    /// Chunk is being decoded.
    Chunk {
        /// Sequence id of chunk being decoded.
        seq_id: u8,
        /// Number of bytes needed to finish this chunk.
        needed: NonZeroUsize,
    },
}

impl ChunkDecoder {
    /// Will try to decode MySql packet chunk from `src` to `dst`.
    ///
    /// If chunk is decoded, then `ChunkInfo` is returned.
    ///
    /// If the `dst` buffer isn't empty then it is expected that it contains previous chunks
    /// of the same packet, or this function may erroneously report
    /// [`PacketCodecError::PacketTooLarge`] error.
    pub fn decode<T>(
        &mut self,
        src: &mut BytesMut,
        dst: &mut T,
        max_allowed_packet: usize,
    ) -> Result<Option<ChunkInfo>, PacketCodecError>
    where
        T: AsRef<[u8]>,
        T: BufMut,
    {
        match *self {
            ChunkDecoder::Idle => {
                if src.len() < 4 {
                    // We need at least 4 bytes to read chunk length and sequence id.
                    Ok(None)
                } else {
                    let raw_chunk_len = LittleEndian::read_u24(&*src) as usize;
                    let seq_id = src[3];

                    match NonZeroUsize::new(raw_chunk_len) {
                        Some(chunk_len) => {
                            if dst.as_ref().len() + chunk_len.get() > max_allowed_packet {
                                return Err(PacketCodecError::PacketTooLarge);
                            }

                            *self = ChunkDecoder::Chunk {
                                seq_id,
                                needed: chunk_len,
                            };

                            if src.len() > 4 {
                                self.decode(src, dst, max_allowed_packet)
                            } else {
                                Ok(None)
                            }
                        }
                        None => {
                            src.advance(4);
                            Ok(Some(ChunkInfo::Last(seq_id)))
                        }
                    }
                }
            }
            ChunkDecoder::Chunk { seq_id, needed } => {
                if src.len() >= 4 + needed.get() {
                    src.advance(4);

                    dst.put_slice(&src[..needed.get()]);
                    src.advance(needed.get());

                    *self = ChunkDecoder::Idle;

                    if dst.as_ref().len() % MAX_PAYLOAD_LEN == 0 {
                        Ok(Some(ChunkInfo::Middle(seq_id)))
                    } else {
                        Ok(Some(ChunkInfo::Last(seq_id)))
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }
}

/// Stores information about compressed packet being decoded.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CompData {
    /// Compressed(&lt;needed&gt;, &lt;uncompressed len&gt;)
    Compressed(NonZeroUsize, NonZeroUsize),
    /// Uncompressed(&lt;needed&gt;)
    Uncompressed(NonZeroUsize),
}

impl CompData {
    /// Creates new `CompData` if given arguments are valid.
    fn new(
        compressed_len: usize,
        uncompressed_len: usize,
        max_allowed_packet: usize,
    ) -> Result<Option<Self>, PacketCodecError> {
        // max_allowed_packet will be an upper boundary
        if max(compressed_len, uncompressed_len) > max_allowed_packet {
            return Err(PacketCodecError::PacketTooLarge);
        }

        let compressed_len = NonZeroUsize::new(compressed_len);
        let uncompressed_len = NonZeroUsize::new(uncompressed_len);

        match (compressed_len, uncompressed_len) {
            (Some(needed), Some(plain_len)) => Ok(Some(CompData::Compressed(needed, plain_len))),
            (Some(needed), None) => Ok(Some(CompData::Uncompressed(needed))),
            (None, Some(_)) => {
                // Zero bytes of compressed data that stores
                // non-zero bytes of plain data? Absurd.
                Err(PacketCodecError::BadCompressedPacketHeader)
            }
            (None, None) => Ok(None),
        }
    }

    /// Returns number of bytes needed to decode packet.
    fn needed(&self) -> usize {
        match *self {
            CompData::Compressed(needed, _) | CompData::Uncompressed(needed) => needed.get(),
        }
    }

    /// Writes the plain data of `payload` into `dst`, inflating it with `inflater` if compressed.
    fn write_payload(
        self,
        payload: &[u8],
        dst: &mut BytesMut,
        inflater: &mut Option<Decompress>,
    ) -> io::Result<()> {
        match self {
            CompData::Uncompressed(_) => {
                dst.extend_from_slice(payload);
                Ok(())
            }
            CompData::Compressed(_, plain_len) => {
                let inflater = inflater.get_or_insert_with(|| Decompress::new(true));
                inflate_into(inflater, payload, plain_len.get(), dst)
            }
        }
    }
}

/// Decoder for MySql compressed packet.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CompDecoder {
    /// Decoder is waiting for compressed packet header.
    Idle,
    /// Decoder is decoding a packet.
    Packet {
        /// Compressed packet sequence id.
        seq_id: u8,
        /// Compressed packet size information.
        needed: CompData,
    },
}

impl CompDecoder {
    /// Will try to decode compressed packet from `src` into `dst`.
    ///
    /// If packet is decoded, then `ChunkInfo::Last` is returned.
    pub fn decode(
        &mut self,
        src: &mut BytesMut,
        dst: &mut BytesMut,
        max_allowed_packet: usize,
    ) -> Result<Option<ChunkInfo>, PacketCodecError> {
        self.decode_with(src, dst, max_allowed_packet, &mut None)
    }

    /// Same as [`CompDecoder::decode`], but inflates with `inflater`, creating it on first use.
    fn decode_with(
        &mut self,
        src: &mut BytesMut,
        dst: &mut BytesMut,
        max_allowed_packet: usize,
        inflater: &mut Option<Decompress>,
    ) -> Result<Option<ChunkInfo>, PacketCodecError> {
        match *self {
            CompDecoder::Idle => {
                if src.len() < 7 {
                    // We need at least 7 bytes to read compressed packet header.
                    Ok(None)
                } else {
                    let compressed_len = LittleEndian::read_u24(&*src) as usize;
                    let seq_id = src[3];
                    let uncompressed_len = LittleEndian::read_u24(&src[4..]) as usize;

                    match CompData::new(compressed_len, uncompressed_len, max_allowed_packet)? {
                        Some(needed) => {
                            *self = CompDecoder::Packet { seq_id, needed };
                            self.decode_with(src, dst, max_allowed_packet, inflater)
                        }
                        None => {
                            src.advance(7);
                            Ok(Some(ChunkInfo::Last(seq_id)))
                        }
                    }
                }
            }
            CompDecoder::Packet { seq_id, needed } => {
                if src.len() >= 7 + needed.needed() {
                    src.advance(7);
                    needed.write_payload(&src[..needed.needed()], dst, inflater)?;
                    src.advance(needed.needed());
                    *self = CompDecoder::Idle;
                    Ok(Some(ChunkInfo::Last(seq_id)))
                } else {
                    Ok(None)
                }
            }
        }
    }
}

/// Codec for MySql protocol packets.
///
/// Codec supports both plain and compressed protocols.
#[derive(Debug)]
pub struct PacketCodec {
    /// Maximum size of a packet for this codec.
    pub max_allowed_packet: usize,
    /// Actual implementation.
    inner: PacketCodecInner,
}

impl PacketCodec {
    /// Sets sequence id to `0`.
    pub fn reset_seq_id(&mut self) {
        self.inner.reset_seq_id();
    }

    /// Overwrites plain sequence id with compressed sequence id.
    pub fn sync_seq_id(&mut self) {
        self.inner.sync_seq_id();
    }

    /// Turns compression on.
    pub fn compress(&mut self, level: Compression) {
        self.inner.compress(level);
    }

    /// Will try to decode a packet from `src` into `dst`.
    ///
    /// Returns
    ///
    /// * `true` - decoded packet was written into the `dst`,
    /// * `false` - `src` did not contain a full packet.
    pub fn decode<T>(&mut self, src: &mut BytesMut, dst: &mut T) -> Result<bool, PacketCodecError>
    where
        T: AsRef<[u8]>,
        T: BufMut,
    {
        self.inner.decode(src, dst, self.max_allowed_packet)
    }

    /// Will encode packets into `dst`.
    pub fn encode<T: Buf>(
        &mut self,
        src: &mut T,
        dst: &mut BytesMut,
    ) -> Result<(), PacketCodecError> {
        self.inner.encode(src, dst, self.max_allowed_packet)
    }
}

impl Default for PacketCodec {
    fn default() -> Self {
        Self {
            max_allowed_packet: DEFAULT_MAX_ALLOWED_PACKET,
            inner: Default::default(),
        }
    }
}

/// Packet codec implementation.
#[derive(Debug)]
enum PacketCodecInner {
    /// Plain packet codec.
    Plain(PlainPacketCodec),
    /// Compressed packet codec.
    Comp(CompPacketCodec),
}

impl PacketCodecInner {
    /// Sets sequence id to `0`.
    fn reset_seq_id(&mut self) {
        match self {
            PacketCodecInner::Plain(c) => c.reset_seq_id(),
            PacketCodecInner::Comp(c) => c.reset_seq_id(),
        }
    }

    /// Overwrites plain sequence id with compressed sequence id.
    fn sync_seq_id(&mut self) {
        match self {
            PacketCodecInner::Plain(_) => (),
            PacketCodecInner::Comp(c) => c.sync_seq_id(),
        }
    }

    /// Turns compression on.
    fn compress(&mut self, level: Compression) {
        match self {
            PacketCodecInner::Plain(c) => {
                *self = PacketCodecInner::Comp(CompPacketCodec {
                    level,
                    comp_seq_id: 0,
                    in_buf: BytesMut::with_capacity(DEFAULT_MAX_ALLOWED_PACKET),
                    out_buf: BytesMut::with_capacity(DEFAULT_MAX_ALLOWED_PACKET),
                    comp_decoder: CompDecoder::Idle,
                    deflater: None,
                    inflater: None,
                    plain_codec: mem::take(c),
                })
            }
            PacketCodecInner::Comp(c) => {
                if c.level != level {
                    c.level = level;
                    c.deflater = None;
                }
            }
        }
    }

    /// Will try to decode packet from `src` into `dst`.
    ///
    /// If `true` is returned then `dst` contains full packet.
    fn decode<T>(
        &mut self,
        src: &mut BytesMut,
        dst: &mut T,
        max_allowed_packet: usize,
    ) -> Result<bool, PacketCodecError>
    where
        T: AsRef<[u8]>,
        T: BufMut,
    {
        match self {
            PacketCodecInner::Plain(codec) => codec.decode(src, dst, max_allowed_packet, None),
            PacketCodecInner::Comp(codec) => codec.decode(src, dst, max_allowed_packet),
        }
    }

    /// Will try to encode packets into `dst`.
    fn encode<T: Buf>(
        &mut self,
        packet: &mut T,
        dst: &mut BytesMut,
        max_allowed_packet: usize,
    ) -> Result<(), PacketCodecError> {
        match self {
            PacketCodecInner::Plain(codec) => codec.encode(packet, dst, max_allowed_packet),
            PacketCodecInner::Comp(codec) => codec.encode(packet, dst, max_allowed_packet),
        }
    }
}

impl Default for PacketCodecInner {
    fn default() -> Self {
        PacketCodecInner::Plain(Default::default())
    }
}

/// Codec for plain MySql protocol.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct PlainPacketCodec {
    /// Chunk sequence id.
    pub seq_id: u8,
    /// Chunk decoder.
    chunk_decoder: ChunkDecoder,
}

impl PlainPacketCodec {
    /// Sets sequence id to `0`.
    fn reset_seq_id(&mut self) {
        self.seq_id = 0;
    }

    /// Will try to decode packet from `src` into `dst`.
    ///
    /// * `comp_seq_id` - is the sequence id of the last compressed packet (if any).
    ///
    /// If `true` is returned then `dst` contains full packet.
    fn decode<T>(
        &mut self,
        src: &mut BytesMut,
        dst: &mut T,
        max_allowed_packet: usize,
        comp_seq_id: Option<u8>,
    ) -> Result<bool, PacketCodecError>
    where
        T: AsRef<[u8]>,
        T: BufMut,
    {
        match self.chunk_decoder.decode(src, dst, max_allowed_packet)? {
            Some(chunk_info) => {
                if self.seq_id != chunk_info.seq_id() {
                    match comp_seq_id {
                        Some(seq_id) if seq_id == chunk_info.seq_id() => {
                            // server synchronized pkt_nr (in `net_flush`)
                            self.seq_id = seq_id;
                        }
                        _ => {
                            return Err(PacketCodecError::PacketsOutOfSync);
                        }
                    }
                }

                self.seq_id = self.seq_id.wrapping_add(1);

                match chunk_info {
                    ChunkInfo::Middle(_) => {
                        if !src.is_empty() {
                            self.decode(src, dst, max_allowed_packet, comp_seq_id)
                        } else {
                            Ok(false)
                        }
                    }
                    ChunkInfo::Last(_) => Ok(true),
                }
            }
            None => Ok(false),
        }
    }

    /// Will try to encode packets into `dst`.
    fn encode<T: Buf>(
        &mut self,
        packet: &mut T,
        dst: &mut BytesMut,
        max_allowed_packet: usize,
    ) -> Result<(), PacketCodecError> {
        if packet.remaining() > max_allowed_packet {
            return Err(PacketCodecError::PacketTooLarge);
        }

        self.seq_id = packet_to_chunks(self.seq_id, packet, dst);

        Ok(())
    }
}

/// Compression codec buffers (`in_buf`, `out_buf`) grow to accommodate large packets
/// but never shrink back, effectively leaking memory for the lifetime of the connection.
/// With 20+ compressed connections this can waste 200-500 MB of RSS.
/// We reclaim buffers that have grown past this threshold once they are fully drained.
const COMP_CODEC_SHRINK_THRESHOLD: usize = 4 * 1024 * 1024;

/// Codec for compressed MySql protocol.
#[derive(Debug)]
struct CompPacketCodec {
    /// Compression level for this codec.
    level: Compression,
    /// Compressed packet sequence id.
    comp_seq_id: u8,
    /// Buffer for decompressed input data.
    in_buf: BytesMut,
    /// Buffer for compressed output data.
    out_buf: BytesMut,
    /// Compressed packet decoder.
    comp_decoder: CompDecoder,
    /// Deflate stream for outgoing packets, kept for the connection's lifetime.
    deflater: Option<Compress>,
    /// Inflate stream for incoming packets, kept for the connection's lifetime.
    inflater: Option<Decompress>,
    /// Wrapped codec for plain MySql protocol.
    plain_codec: PlainPacketCodec,
}

impl CompPacketCodec {
    /// Sets sequence id to `0`.
    fn reset_seq_id(&mut self) {
        self.comp_seq_id = 0;
        self.plain_codec.reset_seq_id();
    }

    /// Overwrites plain sequence id with compressed sequence id
    /// if on compressed packet boundary.
    fn sync_seq_id(&mut self) {
        if self.in_buf.is_empty() {
            self.plain_codec.seq_id = self.comp_seq_id;
        }
    }

    /// Will try to decode packet from `src` into `dst`.
    ///
    /// If `true` is returned then `dst` contains full packet.
    fn decode<T>(
        &mut self,
        src: &mut BytesMut,
        dst: &mut T,
        max_allowed_packet: usize,
    ) -> Result<bool, PacketCodecError>
    where
        T: AsRef<[u8]>,
        T: BufMut,
    {
        if !self.in_buf.is_empty()
            && self.plain_codec.decode(
                &mut self.in_buf,
                dst,
                max_allowed_packet,
                // the server could sync the sequence id of the plain packet
                // with the id of the last compressed packet
                Some(self.comp_seq_id.wrapping_sub(1)),
            )?
        {
            // After decoding, in_buf may be empty but still hold capacity
            // from previous large responses. Reclaim if over threshold.
            if self.in_buf.is_empty() && self.in_buf.capacity() > COMP_CODEC_SHRINK_THRESHOLD {
                self.in_buf = BytesMut::new();
            }
            return Ok(true);
        }

        match self.comp_decoder.decode_with(
            src,
            &mut self.in_buf,
            max_allowed_packet,
            &mut self.inflater,
        )? {
            Some(chunk_info) => {
                if self.comp_seq_id != chunk_info.seq_id() {
                    return Err(PacketCodecError::PacketsOutOfSync);
                }

                self.comp_seq_id = self.comp_seq_id.wrapping_add(1);

                self.decode(src, dst, max_allowed_packet)
            }
            None => Ok(false),
        }
    }

    /// Will try to encode packets into `dst`.
    fn encode<T: Buf>(
        &mut self,
        packet: &mut T,
        dst: &mut BytesMut,
        max_allowed_packet: usize,
    ) -> Result<(), PacketCodecError> {
        self.plain_codec
            .encode(packet, &mut self.out_buf, max_allowed_packet)?;

        self.comp_seq_id = compress_with(
            self.comp_seq_id,
            self.level,
            &mut self.deflater,
            max_allowed_packet,
            &mut self.out_buf,
            dst,
        )?;

        // compress() drains out_buf but leaves allocated capacity behind.
        // Reclaim if over threshold to avoid holding megabytes idle.
        if self.out_buf.capacity() > COMP_CODEC_SHRINK_THRESHOLD {
            self.out_buf = BytesMut::new();
        }

        /* Sync packet number if using compression (see net_serv.cc) */
        self.plain_codec.seq_id = self.comp_seq_id;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const COMPRESSED: &[u8] = &[
        0x22, 0x00, 0x00, 0x00, 0x32, 0x00, 0x00, 0x78, 0x9c, 0xd3, 0x63, 0x60, 0x60, 0x60, 0x2e,
        0x4e, 0xcd, 0x49, 0x4d, 0x2e, 0x51, 0x50, 0x32, 0x30, 0x34, 0x32, 0x36, 0x31, 0x35, 0x33,
        0xb7, 0xb0, 0xc4, 0xcd, 0x52, 0x02, 0x00, 0x0c, 0xd1, 0x0a, 0x6c,
    ];

    const PLAIN: [u8; 46] = [
        0x03, 0x73, 0x65, 0x6c, 0x65, 0x63, 0x74, 0x20, 0x22, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35,
        0x36, 0x37, 0x38, 0x39, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30,
        0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35,
        0x22,
    ];

    #[test]
    fn zero_len_packet() -> Result<(), error::PacketCodecError> {
        let mut encoder = PacketCodec::default();
        let mut empty: &[u8] = &[];
        let mut src = BytesMut::new();
        encoder.encode(&mut empty, &mut src)?;

        let mut dst = vec![];
        let mut decoder = PacketCodec::default();
        let result = decoder.decode(&mut src, &mut dst)?;
        assert!(result);
        assert_eq!(dst, vec![0_u8; 0]);

        Ok(())
    }

    #[test]
    fn regular_packet() -> Result<(), error::PacketCodecError> {
        let mut encoder = PacketCodec::default();
        let mut src = BytesMut::new();
        encoder.encode(&mut &[0x31_u8, 0x32, 0x33][..], &mut src)?;

        let mut dst = vec![];
        let mut decoder = PacketCodec::default();
        let result = decoder.decode(&mut src, &mut dst)?;
        assert!(result);
        assert_eq!(dst, vec![0x31, 0x32, 0x33]);

        Ok(())
    }

    #[test]
    fn packet_sequence() -> Result<(), error::PacketCodecError> {
        let mut encoder = PacketCodec::default();
        let mut decoder = PacketCodec::default();
        let mut src = BytesMut::new();

        for i in 0..1024_usize {
            encoder.encode(&mut &*vec![0; i], &mut src)?;
            let mut dst = vec![];
            let result = decoder.decode(&mut src, &mut dst)?;
            assert!(result);
            assert_eq!(dst, vec![0; i]);
        }

        Ok(())
    }

    #[test]
    fn large_packets() -> Result<(), error::PacketCodecError> {
        let lengths = vec![MAX_PAYLOAD_LEN, MAX_PAYLOAD_LEN + 1, MAX_PAYLOAD_LEN * 2];
        let mut encoder = PacketCodec::default();
        let mut decoder = PacketCodec::default();
        let mut src = BytesMut::new();

        decoder.max_allowed_packet = *lengths.iter().max().unwrap();
        encoder.max_allowed_packet = *lengths.iter().max().unwrap();

        for &len in &lengths {
            encoder.encode(&mut &*vec![0x42_u8; len], &mut src)?;
        }

        for &len in &lengths {
            let mut dst = vec![];
            let result = decoder.decode(&mut src, &mut dst)?;
            assert!(result);
            assert_eq!(dst, vec![0x42; len]);
        }

        Ok(())
    }

    #[test]
    fn compressed_roundtrip() {
        let mut encoder = PacketCodec::default();
        let mut decoder = PacketCodec::default();
        let mut src = BytesMut::from(COMPRESSED);

        encoder.compress(Compression::best());
        decoder.compress(Compression::best());

        let mut dst = vec![];
        let result = decoder.decode(&mut src, &mut dst).unwrap();
        assert!(result);
        assert_eq!(&*dst, PLAIN);
        encoder.encode(&mut &*dst, &mut src).unwrap();

        let mut dst = vec![];
        decoder.reset_seq_id();
        let result = decoder.decode(&mut src, &mut dst).unwrap();
        assert!(result);
        assert_eq!(&*dst, PLAIN);
    }

    #[test]
    fn compression_none() {
        let mut encoder = PacketCodec::default();
        let mut decoder = PacketCodec::default();
        let mut src = BytesMut::new();

        encoder.compress(Compression::none());
        decoder.compress(Compression::none());

        encoder.encode(&mut (&PLAIN[..]), &mut src).unwrap();
        let mut dst = vec![];
        let result = decoder.decode(&mut src, &mut dst).unwrap();
        assert!(result);
        assert_eq!(&*dst, PLAIN);
    }

    fn payload(len: usize, seed: u64) -> Vec<u8> {
        let mut state = seed;
        (0..len)
            .map(|i| {
                if i % 64 < 48 {
                    b"SELECT id, name FROM users WHERE id = 1"[i % 39]
                } else {
                    state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
                    (state >> 56) as u8
                }
            })
            .collect()
    }

    fn incompressible(len: usize) -> Vec<u8> {
        let mut state = 0x9e3779b97f4a7c15u64;
        (0..len)
            .map(|_| {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                state as u8
            })
            .collect()
    }

    fn deflates_to_its_own_length(len: usize) -> Vec<u8> {
        (0..len)
            .map(|text_len| {
                let mut packet: Vec<u8> = b"SELECT id, name FROM users WHERE id = 1"
                    .iter()
                    .cycle()
                    .take(text_len)
                    .copied()
                    .collect();
                packet.extend(incompressible(len - text_len));
                packet
            })
            .find(|packet| {
                let mut deflated = Vec::with_capacity(len + 64);
                Compress::new(Compression::default(), true)
                    .compress_vec(packet, &mut deflated, FlushCompress::Finish)
                    .unwrap();
                deflated.len() == len
            })
            .unwrap()
    }

    fn compressed_packet(seq_id: u8, plain_len: usize, plain: &[u8]) -> BytesMut {
        let mut deflater = Compress::new(Compression::default(), true);
        let mut deflated = Vec::with_capacity(plain.len() + 64);
        deflater
            .compress_vec(plain, &mut deflated, FlushCompress::Finish)
            .unwrap();
        let mut packet = BytesMut::new();
        packet.put_uint_le(deflated.len() as u64, 3);
        packet.put_u8(seq_id);
        packet.put_uint_le(plain_len as u64, 3);
        packet.put_slice(&deflated);
        packet
    }

    #[test]
    fn compressed_roundtrip_reuses_streams() {
        let mut encoder = PacketCodec::default();
        let mut decoder = PacketCodec::default();
        encoder.max_allowed_packet = 64 * 1024 * 1024;
        decoder.max_allowed_packet = 64 * 1024 * 1024;
        encoder.compress(Compression::default());
        decoder.compress(Compression::default());

        let lengths = [
            0,
            1,
            MIN_COMPRESS_LENGTH - 5,
            MIN_COMPRESS_LENGTH - 4,
            MIN_COMPRESS_LENGTH,
            4096,
            100_000,
            MAX_PAYLOAD_LEN - 4,
            MAX_PAYLOAD_LEN + 100,
        ];
        for (i, len) in lengths.into_iter().enumerate() {
            if i == 4 {
                encoder.compress(Compression::fast());
            }
            for packet in [payload(len, i as u64), incompressible(len)] {
                let mut wire = BytesMut::new();
                encoder.reset_seq_id();
                encoder.encode(&mut &*packet, &mut wire).unwrap();
                let mut dst = vec![];
                decoder.reset_seq_id();
                assert!(decoder.decode(&mut wire, &mut dst).unwrap(), "{len}");
                assert_eq!(dst, packet, "{len}");
                assert!(wire.is_empty());
            }
        }
    }

    #[test]
    fn small_and_incompressible_packets_are_sent_uncompressed() {
        for packet in [
            payload(MIN_COMPRESS_LENGTH - 5, 1),
            incompressible(MIN_COMPRESS_LENGTH * 4),
            deflates_to_its_own_length(MIN_COMPRESS_LENGTH * 2),
        ] {
            let mut src = BytesMut::from(&packet[..]);
            let mut wire = BytesMut::new();
            compress(
                0,
                Compression::default(),
                DEFAULT_MAX_ALLOWED_PACKET,
                &mut src,
                &mut wire,
            )
            .unwrap();
            assert_eq!(LittleEndian::read_u24(&wire[4..]), 0);
            assert_eq!(&wire[7..], &packet[..]);
        }

        let packet = payload(MIN_COMPRESS_LENGTH, 2);
        let mut src = BytesMut::from(&packet[..]);
        let mut wire = BytesMut::new();
        compress(
            0,
            Compression::default(),
            DEFAULT_MAX_ALLOWED_PACKET,
            &mut src,
            &mut wire,
        )
        .unwrap();
        assert_eq!(LittleEndian::read_u24(&wire[4..]) as usize, packet.len());
        assert!(wire.len() < packet.len());
    }

    #[test]
    fn changing_compression_level_takes_effect() {
        let packet = payload(4096, 4);
        let mut codec = PacketCodec::default();
        codec.compress(Compression::none());
        codec.compress(Compression::default());
        let mut wire = BytesMut::new();
        codec.encode(&mut &*packet, &mut wire).unwrap();
        assert_eq!(
            LittleEndian::read_u24(&wire[4..]) as usize,
            packet.len() + 4
        );
        assert!(wire.len() < packet.len());
    }

    #[test]
    fn rejects_packet_that_inflates_to_another_length() {
        let mut plain = BytesMut::new();
        plain.put_uint_le(1000, 3);
        plain.put_u8(0);
        plain.put_slice(&payload(1000, 3));

        for declared in [plain.len() + 1, plain.len() - 1] {
            let mut decoder = PacketCodec::default();
            decoder.compress(Compression::default());
            let mut src = compressed_packet(0, declared, &plain);
            let mut dst = vec![];
            assert!(decoder.decode(&mut src, &mut dst).is_err(), "{declared}");
        }

        let mut decoder = PacketCodec::default();
        decoder.compress(Compression::default());
        let mut src = compressed_packet(0, plain.len(), &plain);
        let mut dst = vec![];
        assert!(decoder.decode(&mut src, &mut dst).unwrap());
        assert_eq!(dst, &plain[4..]);
    }

    #[test]
    #[should_panic(expected = "PacketsOutOfSync")]
    fn out_of_sync() {
        let mut src = BytesMut::from(&b"\x00\x00\x00\x01"[..]);
        let mut codec = PacketCodec::default();
        let mut dst = vec![];
        codec.decode(&mut src, &mut dst).unwrap();
    }

    #[test]
    #[should_panic(expected = "PacketTooLarge")]
    fn packet_too_large() {
        let mut encoder = PacketCodec::default();
        let mut decoder = PacketCodec::default();
        let mut src = BytesMut::new();

        encoder
            .encode(&mut &*vec![0; encoder.max_allowed_packet + 1], &mut src)
            .unwrap();
        let mut dst = vec![];
        decoder.decode(&mut src, &mut dst).unwrap();
    }
}

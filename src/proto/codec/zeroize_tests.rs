// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Tests for the `zeroize` feature of the packet codec.

use super::*;

use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::{
        Mutex,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};

/// Delegates to [`System`] and, for one watched allocation, counts the non-zero bytes among
/// its initialized prefix right before it is released.
struct WatchingAllocator;

static WATCHED: AtomicUsize = AtomicUsize::new(0);
static WATCHED_LEN: AtomicUsize = AtomicUsize::new(0);
static SAW_FREE: AtomicBool = AtomicBool::new(false);
static NONZERO: AtomicUsize = AtomicUsize::new(0);
static WATCH_LOCK: Mutex<()> = Mutex::new(());

unsafe impl GlobalAlloc for WatchingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        if WATCHED
            .compare_exchange(ptr as usize, 0, Ordering::SeqCst, Ordering::SeqCst)
            .is_ok()
        {
            let len = WATCHED_LEN.load(Ordering::SeqCst).min(layout.size());
            // Only the initialized prefix of a still-live allocation is read.
            let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
            NONZERO.store(bytes.iter().filter(|&&b| b != 0).count(), Ordering::SeqCst);
            SAW_FREE.store(true, Ordering::SeqCst);
        }
        unsafe { System.dealloc(ptr, layout) }
    }
}

#[global_allocator]
static ALLOCATOR: WatchingAllocator = WatchingAllocator;

fn watch(buf: &BytesMut) {
    assert_eq!(WATCHED.load(Ordering::SeqCst), 0);
    SAW_FREE.store(false, Ordering::SeqCst);
    NONZERO.store(usize::MAX, Ordering::SeqCst);
    WATCHED_LEN.store(buf.len(), Ordering::SeqCst);
    WATCHED.store(buf.as_ptr() as usize, Ordering::SeqCst);
}

fn assert_wiped_before_free() {
    assert!(
        SAW_FREE.load(Ordering::SeqCst),
        "the watched allocation was not released"
    );
    assert_eq!(
        NONZERO.load(Ordering::SeqCst),
        0,
        "data remained in the allocation when it was released"
    );
}

fn all_zero(ptr: *const u8, len: usize) -> bool {
    // The callers pass a prefix of an allocation that is still alive and initialized.
    unsafe { std::slice::from_raw_parts(ptr, len) }
        .iter()
        .all(|&b| b == 0)
}

#[test]
fn chunk_decoder_wipes_consumed_input() {
    let payload = b"first-packet-payload";
    let mut src = BytesMut::new();
    src.put_u32_le(payload.len() as u32);
    src.extend_from_slice(payload);
    src.extend_from_slice(b"next-packet");
    let ptr = src.as_ptr();
    let mut dst = Vec::new();
    assert_eq!(
        ChunkDecoder::default()
            .decode(&mut src, &mut dst, 1024)
            .unwrap(),
        Some(ChunkInfo::Last(0))
    );
    assert_eq!(dst, payload);
    assert_eq!(&src[..], b"next-packet");
    // `src` still owns the allocation, so the consumed prefix is still readable.
    assert!(all_zero(ptr, 4 + payload.len()));
}

#[test]
fn comp_decoder_wipes_consumed_input() {
    let payload = b"uncompressed-payload-in-a-compressed-envelope";
    let mut src = BytesMut::new();
    src.put_uint_le(payload.len() as u64, 3);
    src.put_u8(0);
    src.put_uint_le(0, 3);
    src.extend_from_slice(payload);
    src.extend_from_slice(b"next-envelope");
    let ptr = src.as_ptr();
    let mut dst = BytesMut::new();
    assert_eq!(
        CompDecoder::Idle.decode(&mut src, &mut dst, 1024).unwrap(),
        Some(ChunkInfo::Last(0))
    );
    assert_eq!(&dst[..], payload);
    assert_eq!(&src[..], b"next-envelope");
    assert!(all_zero(ptr, 7 + payload.len()));
}

#[test]
fn compress_wipes_its_input() {
    let mut src = BytesMut::from(&b"compression-input-payload"[..]);
    let ptr = src.as_ptr();
    let len = src.len();
    let mut dst = BytesMut::new();
    compress(0, Compression::none(), 1024, &mut src, &mut dst).unwrap();
    assert!(src.is_empty());
    // `BytesMut::clear` keeps the allocation, so the cleared bytes are still readable.
    assert!(all_zero(ptr, len));
    assert_eq!(&dst[7..], b"compression-input-payload");
}

#[test]
fn growth_wipes_the_released_allocation() {
    let _guard = WATCH_LOCK.lock().unwrap();
    let mut dst = BytesMut::with_capacity(32);
    dst.extend_from_slice(&[0x73; 32]);
    watch(&dst);
    let payload = [0x42; 1024];
    assert_eq!(packet_to_chunks(0, &mut &payload[..], &mut dst), 1);
    assert_wiped_before_free();
    assert_eq!(&dst[..32], &[0x73; 32]);
    assert_eq!(&dst[36..], &payload);
}

#[test]
fn drop_wipes_the_compressed_codec_buffers() {
    let _guard = WATCH_LOCK.lock().unwrap();
    for watch_input in [true, false] {
        let codec = CompPacketCodec {
            level: Compression::default(),
            comp_seq_id: 0,
            in_buf: BytesMut::from(&b"pending-input"[..]),
            out_buf: BytesMut::from(&b"pending-output"[..]),
            comp_decoder: CompDecoder::Idle,
            deflater: None,
            inflater: None,
            plain_codec: PlainPacketCodec::default(),
        };
        watch(if watch_input {
            &codec.in_buf
        } else {
            &codec.out_buf
        });
        drop(codec);
        assert_wiped_before_free();
    }
}

#[test]
fn failed_inflate_wipes_partial_output() {
    let mut src = BytesMut::new();
    compress(
        0,
        Compression::default(),
        1024,
        &mut BytesMut::from(&[0x43_u8; 128][..]),
        &mut src,
    )
    .unwrap();
    // Declare one byte more than the payload inflates to, so inflating fails after it has
    // written the whole payload into the spare capacity of `dst`.
    let declared = LittleEndian::read_u24(&src[4..7]);
    LittleEndian::write_u24(&mut src[4..7], declared + 1);
    let mut dst = BytesMut::from(&[0_u8; 256][..]);
    let ptr = dst.as_ptr();
    dst.clear();
    assert!(CompDecoder::Idle.decode(&mut src, &mut dst, 1024).is_err());
    assert!(dst.is_empty());
    assert!(all_zero(ptr, 256));
}

#[test]
fn compressed_round_trip_is_unchanged() {
    for level in [
        Compression::none(),
        Compression::fast(),
        Compression::default(),
        Compression::best(),
    ] {
        let mut encoder = PacketCodec::default();
        let mut decoder = PacketCodec::default();
        encoder.compress(level);
        decoder.compress(level);
        for size in [0, 3, 61, 128 * 1024] {
            let payload: Vec<u8> = (0..size).map(|i| (i % 251) as u8).collect();
            let mut encoded = BytesMut::new();
            encoder.encode(&mut &payload[..], &mut encoded).unwrap();
            let mut decoded = Vec::new();
            assert!(decoder.decode(&mut encoded, &mut decoded).unwrap());
            assert_eq!(decoded, payload);
            assert!(encoded.is_empty());
        }
    }
}

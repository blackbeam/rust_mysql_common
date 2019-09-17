// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use bytes::{BufMut, BytesMut};
use tokio_codec::{Decoder, Encoder};

use crate::constants::DEFAULT_MAX_ALLOWED_PACKET;
use std::io::{self, Read, Write};

/// This type provides synchronous alternative to the `tokio_codec::Framed`.
#[derive(Debug)]
pub struct SyncFramed<T, U> {
    eof: bool,
    in_buf: BytesMut,
    out_buf: BytesMut,
    codec: U,
    stream: T,
}

impl<T, U> SyncFramed<T, U> {
    /// Creates new instance with given `stream` and `codec`.
    pub fn new(stream: T, codec: U) -> Self {
        SyncFramed {
            eof: false,
            in_buf: BytesMut::with_capacity(DEFAULT_MAX_ALLOWED_PACKET),
            out_buf: BytesMut::with_capacity(DEFAULT_MAX_ALLOWED_PACKET),
            codec,
            stream,
        }
    }

    /// Returns reference to a stream.
    pub fn get_ref(&self) -> &T {
        &self.stream
    }

    /// Returns mutable reference to a stream.
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.stream
    }

    /// Returns reference to a codec.
    pub fn codec(&self) -> &U {
        &self.codec
    }

    /// Returns mutable reference to a codec.
    pub fn codec_mut(&mut self) -> &mut U {
        &mut self.codec
    }

    /// Consumes self and returns wrapped buffers, codec and stream.
    pub fn destruct(self) -> (BytesMut, BytesMut, U, T) {
        (self.in_buf, self.out_buf, self.codec, self.stream)
    }

    /// Creates new instance from given buffers, `codec` and `stream`.
    pub fn construct(in_buf: BytesMut, out_buf: BytesMut, codec: U, stream: T) -> Self {
        Self {
            eof: false,
            in_buf,
            out_buf,
            codec,
            stream,
        }
    }
}

impl<T, U> SyncFramed<T, U>
where
    T: Write,
    U: Encoder,
    U::Error: From<io::Error>,
{
    /// Will write item into the stream. Stream may not be flushed.
    pub fn write(&mut self, item: U::Item) -> Result<(), U::Error> {
        self.codec.encode(item, &mut self.out_buf)?;
        self.stream.write_all(&*self.out_buf)?;
        self.out_buf.clear();
        Ok(())
    }

    /// Will flush wrapped stream.
    pub fn flush(&mut self) -> Result<(), U::Error> {
        self.stream.flush()?;
        Ok(())
    }

    /// Will send `item` into the stream. Stream will be flushed.
    pub fn send(&mut self, item: U::Item) -> Result<(), U::Error> {
        self.write(item)?;
        self.flush()
    }
}

impl<T, U> Iterator for SyncFramed<T, U>
where
    T: Read,
    U: Decoder,
    U::Error: From<io::Error>,
{
    type Item = Result<U::Item, U::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.eof {
                return self.codec.decode_eof(&mut self.in_buf).transpose();
            } else {
                match self.codec.decode(&mut self.in_buf).transpose() {
                    Some(item) => return Some(item),
                    None => unsafe {
                        self.in_buf.reserve(1);
                        match self.stream.read(self.in_buf.bytes_mut()) {
                            Ok(0) => self.eof = true,
                            Ok(x) => {
                                self.in_buf.advance_mut(x);
                                continue;
                            }
                            Err(err) => return Some(Err(From::from(err))),
                        }
                    },
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::constants::MAX_PAYLOAD_LEN;
    use crate::proto::codec::PacketCodec;
    use crate::proto::sync_framed::SyncFramed;

    #[test]
    fn iter_packets() {
        let mut buf = Vec::new();
        {
            let mut codec = PacketCodec::default();
            codec.max_allowed_packet = MAX_PAYLOAD_LEN;
            let mut framed = SyncFramed::new(&mut buf, codec);
            framed.send(vec![vec![0_u8; 0]]).unwrap();
            framed.send(vec![vec![0_u8; 1]]).unwrap();
            framed.send(vec![vec![0_u8; MAX_PAYLOAD_LEN]]).unwrap();
        }
        let mut codec = PacketCodec::default();
        codec.max_allowed_packet = MAX_PAYLOAD_LEN;
        let mut buf = &buf[..];
        let mut framed = SyncFramed::new(&mut buf, codec);
        assert_eq!(framed.next().unwrap().unwrap(), vec![0_u8; 0]);
        assert_eq!(framed.next().unwrap().unwrap(), vec![0_u8; 1]);
        assert_eq!(framed.next().unwrap().unwrap(), vec![0_u8; MAX_PAYLOAD_LEN]);
        assert!(framed.next().is_none());
    }

    #[test]
    #[should_panic(expected = "bytes remaining on stream")]
    fn incomplete_packet() {
        let buf = vec![2, 0, 0, 0];
        let mut buf = &buf[..];
        let codec = PacketCodec::default();
        let mut framed = SyncFramed::new(&mut buf, codec);
        framed.next().unwrap().unwrap();
    }
}

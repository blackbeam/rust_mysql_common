// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use bytes::{BufMut, BytesMut};

use crate::{
    constants::DEFAULT_MAX_ALLOWED_PACKET,
    proto::codec::{error::PacketCodecError, PacketCodec},
};

use std::{
    io::{
        Error,
        ErrorKind::{Interrupted, Other},
        Read, Write,
    },
    ptr::slice_from_raw_parts_mut,
};

// stolen from futures-rs
macro_rules! with_interrupt {
    ($e:expr) => {
        loop {
            match $e {
                Ok(x) => {
                    break Ok(x);
                }
                Err(ref e) if e.kind() == Interrupted => {
                    continue;
                }
                Err(e) => {
                    break Err(e);
                }
            }
        }
    };
}

/// Synchronous framed stream for MySql protocol.
///
/// This type is a synchronous alternative to `tokio_codec::Framed`.
#[derive(Debug)]
pub struct MySyncFramed<T> {
    eof: bool,
    in_buf: BytesMut,
    out_buf: BytesMut,
    codec: PacketCodec,
    stream: T,
}

impl<T> MySyncFramed<T> {
    /// Creates new instance with given `stream`.
    pub fn new(stream: T) -> Self {
        MySyncFramed {
            eof: false,
            in_buf: BytesMut::with_capacity(DEFAULT_MAX_ALLOWED_PACKET),
            out_buf: BytesMut::with_capacity(DEFAULT_MAX_ALLOWED_PACKET),
            codec: PacketCodec::default(),
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
    pub fn codec(&self) -> &PacketCodec {
        &self.codec
    }

    /// Returns mutable reference to a codec.
    pub fn codec_mut(&mut self) -> &mut PacketCodec {
        &mut self.codec
    }

    /// Consumes self and returns wrapped buffers, codec and stream.
    pub fn destruct(self) -> (BytesMut, BytesMut, PacketCodec, T) {
        (self.in_buf, self.out_buf, self.codec, self.stream)
    }

    /// Creates new instance from given buffers, `codec` and `stream`.
    pub fn construct(in_buf: BytesMut, out_buf: BytesMut, codec: PacketCodec, stream: T) -> Self {
        Self {
            eof: false,
            in_buf,
            out_buf,
            codec,
            stream,
        }
    }
}

impl<T> MySyncFramed<T>
where
    T: Write,
{
    /// Will write packets into the stream. Stream may not be flushed.
    pub fn write(&mut self, item: Vec<u8>) -> Result<(), PacketCodecError> {
        self.codec.encode(item, &mut self.out_buf)?;
        with_interrupt!(self.stream.write_all(&*self.out_buf))?;
        self.out_buf.clear();
        Ok(())
    }

    /// Will flush wrapped stream.
    pub fn flush(&mut self) -> Result<(), PacketCodecError> {
        with_interrupt!(self.stream.flush())?;
        Ok(())
    }

    /// Will send packets into the stream. Stream will be flushed.
    pub fn send(&mut self, item: Vec<u8>) -> Result<(), PacketCodecError> {
        self.write(item)?;
        self.flush()
    }
}

impl<T> Iterator for MySyncFramed<T>
where
    T: Read,
{
    type Item = Result<Vec<u8>, PacketCodecError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.eof {
                return match self.codec.decode(&mut self.in_buf).transpose() {
                    Some(frame) => Some(frame),
                    None => {
                        if self.in_buf.is_empty() {
                            None
                        } else {
                            Some(Err(Error::new(Other, "bytes remaining on stream").into()))
                        }
                    }
                };
            } else {
                match self.codec.decode(&mut self.in_buf).transpose() {
                    Some(item) => return Some(item),
                    None => unsafe {
                        self.in_buf.reserve(1);
                        match with_interrupt!(self.stream.read(&mut *slice_from_raw_parts_mut(
                            self.in_buf.bytes_mut().as_mut_ptr(),
                            self.in_buf.bytes_mut().len()
                        ))) {
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
    use crate::{constants::MAX_PAYLOAD_LEN, proto::sync_framed::MySyncFramed};

    #[test]
    fn iter_packets() {
        let mut buf = Vec::new();
        {
            let mut framed = MySyncFramed::new(&mut buf);
            framed.codec_mut().max_allowed_packet = MAX_PAYLOAD_LEN;
            framed.send(vec![0_u8; 0]).unwrap();
            framed.send(vec![0_u8; 1]).unwrap();
            framed.send(vec![0_u8; MAX_PAYLOAD_LEN]).unwrap();
        }
        let mut buf = &buf[..];
        let mut framed = MySyncFramed::new(&mut buf);
        framed.codec_mut().max_allowed_packet = MAX_PAYLOAD_LEN;
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
        let mut framed = MySyncFramed::new(&mut buf);
        framed.next().unwrap().unwrap();
    }
}

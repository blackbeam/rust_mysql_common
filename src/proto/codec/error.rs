// Copyright (c) 2017 Anatoly Ikorsky
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use failure::Fail;

use std::io;

#[derive(Debug, Fail)]
pub enum PacketCodecError {
    #[fail(display = "IO error: `{}'", _0)]
    Io(#[cause] io::Error),
    #[fail(display = "Packet is larger than max_allowed_packet")]
    PacketTooLarge,
    #[fail(display = "Packets out of sync")]
    PacketsOutOfSync,
    #[fail(display = "Bad compressed packet header")]
    BadCompressedPacketHeader,
}

impl From<io::Error> for PacketCodecError {
    fn from(io_err: io::Error) -> Self {
        Self::Io(io_err)
    }
}

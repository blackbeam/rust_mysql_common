use byteorder::{LittleEndian as LE, ReadBytesExt};
use std::io;

pub trait ReadMysqlExt: ReadBytesExt {
    fn read_lenenc_int(&mut self) -> io::Result<u64> {
        match self.read_u8()? {
            0xfc => self.read_uint::<LE>(2),
            0xfd => self.read_uint::<LE>(3),
            0xfe => self.read_uint::<LE>(8),
            0xff => Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid length-encoded integer value",
            )),
            x => Ok(x as u64),
        }
    }
}

impl<T> ReadMysqlExt for T where T: ReadBytesExt {}

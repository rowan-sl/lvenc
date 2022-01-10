use bytes::{Buf, BufMut};
use bincode::serialized_size;
use super::{EncodedImage};

const HEADER_SIZE: usize =
b"message-header".len()+
std::mem::size_of::<u64>()+
b"header-end".len();

#[derive(thiserror::Error, Debug)]
enum HeaderParserError {
    #[error("Invalid prefix!")]
    InvalidPrefix,
    #[error("Invalid suffix!")]
    InvalidSuffix,
    #[error("Invalid length!")]
    InvalidLength,
}

#[derive(Clone, Copy, Debug)]
struct Header {
    size: u64
}

impl Header {
    pub fn make_header(img: &EncodedImage) -> Result<Self, Box<bincode::ErrorKind>> {
        Ok(
            Self {
                size: serialized_size(img)?
            }
        )
    }

    pub fn to_bytes(self) -> bytes::Bytes {
        let mut res = bytes::BytesMut::new();
        res.put(&b"message-header"[..]);
        res.put(&self.size.to_be_bytes()[..]);
        res.put(&b"header-end"[..]);
        res.freeze()
    }

    pub fn from_bytes(header: &bytes::Bytes) -> Result<Self, HeaderParserError> {
        if header.len() == HEADER_SIZE {
            let header_start = header.slice(..b"message-header".len());
            let mut message_size = header.slice(b"message-header".len()..b"message-header".len() + std::mem::size_of::<u64>());
            let header_end = header.slice(b"message-header".len() + std::mem::size_of::<u64>()..);
            if &header_start[..] != b"message-header" {
                return Err(HeaderParserError::InvalidPrefix);
            }
            if &header_end[..] != b"header-end" {
                return Err(HeaderParserError::InvalidSuffix);
            }
            Ok(Self {
                size: message_size.get_u64(),
            })
        } else {
            Err(HeaderParserError::InvalidLength)
        }
    }
}
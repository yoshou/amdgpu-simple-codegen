
use crate::ir::*;

pub fn is_bitcode(bytes: &[u8]) -> bool {
    if bytes.len() < 4 {
        false
    } else {
        bytes[0] == 0x42 && bytes[1] == 0x43 && bytes[2] == 0xC0 && bytes[3] == 0xDE
    }
}
pub struct Bitcode {
    data: Vec<u8>,
}

pub struct DecodeError {
    pub message: String
}

impl Bitcode {
    pub fn new(data: &[u8]) -> Self {
        Bitcode { data: data.to_vec() }
    }

    pub fn decode(&self) -> Result<Module, DecodeError> {
        Ok(Module {})
    }
}
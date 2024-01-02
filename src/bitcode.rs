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

use std::collections::HashMap;

pub struct BitcodeReader<'a> {
    p: usize,
    bitcode: &'a Bitcode,
    abbrev_len: Vec<usize>,
    abbrevs: Vec<Vec<BitcodeDefineAbbrev>>,
    entries: Vec<BitcodeEntry>,
    scope: Vec<usize>,
    block_infos: HashMap<u64, Vec<BitcodeDefineAbbrev>>,
}

#[derive(Clone, Debug)]
pub struct BitcodeBlock {
    pub position: u64,
    pub blockid: u64,
    pub newabbrevlen: u64,
    pub blocklen: u64
}

#[derive(Clone, Debug)]
pub enum BitcodeValue {
    Value(u64),
    Array(Vec<BitcodeValue>),
    Char6(char),
    Blob(usize, usize),
}

#[derive(Clone, Debug)]
pub struct BitcodeRecord {
    code: u64,
    values: Vec<BitcodeValue>
}

#[derive(Clone, Debug)]
pub enum BitcodeOperand {
    Literal(u64),
    Fixed(usize),
    VBR(usize),
    Array,
    Char6,
    Blob,
}

#[derive(Clone, Debug)]
pub struct BitcodeDefineAbbrev {
    ops: Vec<BitcodeOperand>
}

#[derive(Clone, Debug)]
pub enum BitcodeEntry {
    EndBlock,
    Block(BitcodeBlock),
    DefineAbbrev(BitcodeDefineAbbrev),
    Record(BitcodeRecord),
}

impl <'a> BitcodeReader<'a> {
    pub fn new(data: &'a Bitcode) -> Self {
        BitcodeReader { p: 0, bitcode: data, abbrev_len: vec![2], abbrevs: vec![vec![]], entries: vec![], scope: vec![], block_infos: HashMap::new() }
    }

    pub fn skip_bytes(&mut self, n: usize) -> Result<usize, DecodeError> {
        if self.p >= self.bitcode.data.len() * 8 {
            Err(DecodeError { message: "Invalid size".to_string() })
        } else {
            self.p += n * 8;
            Ok(n)
        }
    }

    pub fn skip_bits(&mut self, n: usize) -> Result<usize, DecodeError> {
        if self.p >= self.bitcode.data.len() * 8 {
            Err(DecodeError { message: "Invalid size".to_string() })
        } else {
            self.p += n;
            Ok(n)
        }
    }

    pub fn read_bits(&mut self, n: usize) -> Result<u64, DecodeError> {
        if self.p >= self.bitcode.data.len() * 8 {
            Err(DecodeError { message: "Invalid size".to_string() })
        } else {
            let mut value: u64 = 0;
            let mut read_bits = 0;
            while read_bits < n {
                let bit_position = self.p % 8;
                let bits_to_read = (n - read_bits).min(8 - bit_position);
                value |= ((self.bitcode.data[self.p / 8] as u64 >> bit_position) & ((1u64 << bits_to_read) - 1)) << read_bits;
                read_bits += bits_to_read;
                self.p += bits_to_read;
            }

            Ok(value)
        }
    }

    pub fn read_abbreviation_id(&mut self) -> Result<u64, DecodeError> {
        if let Some(abbrev_len) = self.abbrev_len.last() {
            self.read_bits(*abbrev_len)
        } else {
            Err(DecodeError { message: "Invalid abbreviation length".to_string() })
        }
    }

    fn parse_blockinfo(&mut self, from_entry: usize, to_entry: usize) {
        let mut current_blockid = None;
        for entry in &self.entries[from_entry..to_entry] {
            match entry {
                BitcodeEntry::Record(record) => {
                    if record.code == 1 {
                        let blockid = if let BitcodeValue::Value(value) = record.values[0] {
                            value
                        } else {
                            panic!();
                        };
                        current_blockid = Some(blockid);
                    }
                }
                BitcodeEntry::DefineAbbrev(define_abbrev) => {
                    if let Some(current_blockid) = current_blockid {
                        self.block_infos.entry(current_blockid)
                            .or_insert_with(Vec::new)
                            .push(define_abbrev.clone());
                    }
                }
                _ => {}
            }
        }
    }

    fn set_abbrevs(&mut self, blockid: u64) {
        if let Some(abbrevs) = self.block_infos.get(&blockid) {
            self.abbrevs.last_mut().unwrap().extend(abbrevs.clone());
        }
    }

    pub fn read_vbr(&mut self, n: usize) -> Result<u64, DecodeError> {
        let mut is_last = false;
        let mut value = 0u64;
        let mut read_bits = 0;
        while !is_last {
            let bits = self.read_bits(n)?;
            value |= (bits & ((1u64 << (n - 1)) - 1)) << read_bits;
            is_last = ((bits >> (n - 1)) & 1) == 0;
            read_bits += n - 1;
        }
        Ok(value)
    }

    pub fn skip_to_align(&mut self, alignment: usize) -> Result<usize, DecodeError> {
        let new_p = ((self.p + alignment - 1) / alignment) * alignment;
        self.skip_bits(new_p - self.p)
    }

    pub fn read_values_recursive<'b, I: Iterator<Item = &'b BitcodeOperand>>(&mut self, op: &BitcodeOperand, operands: &mut I) -> Result<BitcodeValue, DecodeError> {
        match op {
            BitcodeOperand::Literal(lit) => {
                Ok(BitcodeValue::Value(*lit))
            }
            BitcodeOperand::Fixed(width) => {
                let value = self.read_bits(*width)?;
                Ok(BitcodeValue::Value(value))
            }
            BitcodeOperand::VBR(width) => {
                let value = self.read_vbr(*width)?;
                Ok(BitcodeValue::Value(value))
            }
            BitcodeOperand::Array => {
                let len = self.read_vbr(6)?;
                let encoding = if let Some(value) = operands.next() {
                    value
                } else {
                    return Err(DecodeError { message: "Missing encoding for array elements".to_string() });
                };
                let mut array_values = vec![];
                for _ in 0..len {
                    let value = self.read_values_recursive(&encoding, operands)?;
                    array_values.push(value);
                }
                Ok(BitcodeValue::Array(array_values))
            }
            BitcodeOperand::Char6 => {
                let ch = self.read_bits(6)?;
                Ok(BitcodeValue::Char6(char::from_u32(ch as u32).unwrap()))
            }
            BitcodeOperand::Blob => {
                let len = self.read_vbr(6)? as usize;
                let position = self.p;
                self.skip_to_align(32)?;
                self.skip_bytes(len)?;
                self.skip_to_align(32)?;
                Ok(BitcodeValue::Blob(position, len))
            }
        }
    }

    pub fn read_values(&mut self, operands: &[BitcodeOperand]) -> Result<Vec::<BitcodeValue>, DecodeError> {
        let mut values = Vec::<BitcodeValue>::new();
        let mut ops_iter = operands.iter();
        while let Some(op) = ops_iter.next() {
            let value = self.read_values_recursive(op, &mut ops_iter)?;
            values.push(value);
        }
        Ok(values)
    }

    pub fn read(&mut self) -> Result<BitcodeEntry, DecodeError> {
        let id = self.read_abbreviation_id();

        let result = match id {
            Ok(0) => {
                self.abbrev_len.pop();
                self.abbrevs.pop();
                self.skip_to_align(32)?;
                Ok(BitcodeEntry::EndBlock)
            }
            Ok(1) => {
                let position = self.p;
                let blockid = self.read_vbr(8)?;
                let newabbrevlen = self.read_vbr(4)?;
                self.skip_to_align(32)?;
                let blocklen = self.read_bits(32)?;

                self.abbrev_len.push(newabbrevlen as usize);
                self.abbrevs.push(vec![]);

                Ok(BitcodeEntry::Block(BitcodeBlock { position: position as u64, blockid: blockid, newabbrevlen: newabbrevlen, blocklen: blocklen}))
            }
            Ok(2) => {
                let numabbrevops = self.read_vbr(5)?;
                let mut ops = Vec::<BitcodeOperand>::new();
                for _ in 0..numabbrevops {
                    let is_literal_operand = self.read_bits(1)? == 1;
                    if is_literal_operand {
                        let lit = self.read_vbr(8)?;
                        ops.push(BitcodeOperand::Literal(lit));
                    } else {
                        let encoding = self.read_bits(3)?;
                        match encoding {
                            1 => {
                                let width = self.read_vbr(5)?;
                                ops.push(BitcodeOperand::Fixed(width as usize));
                            }
                            2 => {
                                let width = self.read_vbr(5)?;
                                ops.push(BitcodeOperand::VBR(width as usize));
                            }
                            3 => {
                                ops.push(BitcodeOperand::Array);
                            }
                            4 => {
                                ops.push(BitcodeOperand::Char6);
                            }
                            5 => {
                                ops.push(BitcodeOperand::Blob);
                            }
                            _ => {
                                return Err(DecodeError { message: "Invalid encoding".to_string() })
                            }
                        }
                    }
                }
                let abbrev = BitcodeDefineAbbrev { ops: ops };
                self.abbrevs.last_mut().unwrap().push(abbrev.clone());
                Ok(BitcodeEntry::DefineAbbrev(abbrev))
            }
            Ok(3) => {
                let code = self.read_vbr(6)?;
                let numops = self.read_vbr(6)?;
                let mut values = Vec::<BitcodeValue>::new();
                for _ in 0..numops {
                    let value = self.read_vbr(6)?;
                    values.push(BitcodeValue::Value(value));
                }
                Ok(BitcodeEntry::Record(BitcodeRecord { code: code, values: values }))
            }
            Ok(id) => {
                let abbrevs = self.abbrevs.last().unwrap();
                if id as usize >= abbrevs.len() + 4 {
                    return Err(DecodeError { message: "Invalid abbreviation id".to_string() })
                }

                let abbrev = abbrevs[id as usize - 4].clone();
                if abbrev.ops.len() == 0 {
                    return Err(DecodeError { message: "Invalid abbreviation".to_string() })
                }
                let code = if let BitcodeOperand::Literal(value) = abbrev.ops[0] {
                    value
                } else {
                    return Err(DecodeError { message: "Invalid code operand".to_string() })
                };
                let values = self.read_values(&abbrev.ops[1..])?;
                Ok(BitcodeEntry::Record(BitcodeRecord { code: code, values: values }))
            }
            _ => {
                Err(DecodeError { message: "Invalid abbreviation id".to_string() })
            }
        };

        match &result {
            Ok(entry) => {
                match entry {
                    BitcodeEntry::Block(block) => {
                        self.scope.push(self.entries.len());
                        self.set_abbrevs(block.blockid);
                    }
                    BitcodeEntry::EndBlock => {
                        let start = self.scope.pop().unwrap();
                        let block = &self.entries[start];
                        if let BitcodeEntry::Block(block) = block {
                            if block.blockid == 0 {
                                self.parse_blockinfo(start + 1, self.entries.len());
                            }
                        }
                    }
                    _ => {}
                }
                self.entries.push(entry.clone());
            }
            _ => {}
        }

        result
    }
}

impl Bitcode {
    pub fn new(data: &[u8]) -> Self {
        Bitcode { data: data.to_vec() }
    }

    pub fn decode(&self) -> Result<Module, DecodeError> {
        let mut reader = BitcodeReader::new(self);
        reader.skip_bytes(4)?;

        let mut expect_block_end = vec![];

        while reader.p < reader.bitcode.data.len() * 8 {
            let entry = reader.read()?;
            match entry {
                BitcodeEntry::Block(block) => {
                    expect_block_end.push(reader.p + block.blocklen as usize * 32);
                }
                BitcodeEntry::DefineAbbrev(_) => {
                }
                BitcodeEntry::EndBlock => {
                    if let Some(value) = expect_block_end.pop() {
                        if value as usize != reader.p {
                            panic!();
                        }
                    }
                }
                BitcodeEntry::Record(_) => {
                }
            }

        }
        Ok(Module {})
    }
}
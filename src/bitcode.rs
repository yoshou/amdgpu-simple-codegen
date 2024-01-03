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

use std::collections::{HashMap, HashSet};

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
        BitcodeReader { p: 32, bitcode: data, abbrev_len: vec![2], abbrevs: vec![vec![]], entries: vec![], scope: vec![], block_infos: HashMap::new() }
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

    pub fn read_char6(&mut self) -> Result<char, DecodeError> {
        let ch = self.read_bits(6)? as u8;
        match ch {
            0..=25 => {
                Ok(('a' as u8 + ch) as char)
            }
            26..=51 => {
                Ok(('A' as u8 + (ch - 26)) as char)
            }
            52..=61 => {
                Ok(('0' as u8 + (ch - 52)) as char)
            }
            62 => {
                Ok('.')
            }
            63 => {
                Ok('_')
            }
            _ => {
                panic!();
            }
        }
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
                let ch = self.read_char6()?;
                Ok(BitcodeValue::Char6(ch))
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

    pub fn read_to_end(&mut self) -> Result<Vec<BitcodeEntry>, DecodeError> {
        let mut expect_block_end = vec![];
        while self.p < self.bitcode.data.len() * 8 {
            let entry = self.read()?;
            match entry {
                BitcodeEntry::Block(block) => {
                    expect_block_end.push(self.p + block.blocklen as usize * 32);
                }
                BitcodeEntry::DefineAbbrev(_) => {
                }
                BitcodeEntry::EndBlock => {
                    let error = DecodeError { message: "Broken block structure".to_string() };
                    if let Some(value) = expect_block_end.pop() {
                        if value as usize != self.p {
                            return Err(error);
                        }
                    } else {
                        return Err(error);
                    }
                }
                BitcodeEntry::Record(_) => {
                }
            }
        }
        Ok(self.entries.clone())
    }
}

use num_derive::*;

#[derive(FromPrimitive, ToPrimitive)]
pub enum LLVMIRBlockID {
    Module = 8,
    Parameter,
    ParameterGroup,
    Constant,
    Function,
    ValueSymtab = 14,
    Metadata,
    MetadataAttachment,
    Type,
    Symtab = 23,
}

struct BitcodeModuleParser<'a> {
    version: i32,
    module: &'a mut Module,
    types: Vec<Type>,
    attributes: HashMap<u32, AttributeList>
}

use either::*;
use num_traits::FromPrimitive;

fn flatten_record_values<'a>(record: &'a BitcodeRecord) -> impl Iterator<Item = &BitcodeValue> + 'a {
    record.values.iter().flat_map(|x| {
        match x {
            BitcodeValue::Array(values) => {
                Left(values.iter())
            }
            value => Right(std::iter::once(value))
        }
    })
}

impl num::FromPrimitive for AttributeKind {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            1 => Some(Self::Alignment),
            2 => Some(Self::AlwaysInline),
            3 => Some(Self::ByVal),
            4 => Some(Self::InlineHint),
            5 => Some(Self::InReg),
            6 => Some(Self::MinSize),
            7 => Some(Self::Naked),
            8 => Some(Self::Nest),
            9 => Some(Self::NoAlias),
            10 => Some(Self::NoBuiltin),
            11 => Some(Self::NoCapture),
            12 => unimplemented!(),
            13 => unimplemented!(),
            14 => unimplemented!(),
            15 => unimplemented!(),
            16 => unimplemented!(),
            17 => unimplemented!(),
            18 => Some(Self::NoUnwind),
            19 => unimplemented!(),
            20 => unimplemented!(),
            21 => Some(Self::ReadOnly),
            22 => unimplemented!(),
            23 => unimplemented!(),
            24 => unimplemented!(),
            25 => unimplemented!(),
            26 => unimplemented!(),
            27 => unimplemented!(),
            28 => unimplemented!(),
            29 => unimplemented!(),
            30 => unimplemented!(),
            31 => unimplemented!(),
            32 => unimplemented!(),
            33 => unimplemented!(),
            34 => unimplemented!(),
            35 => unimplemented!(),
            36 => unimplemented!(),
            37 => unimplemented!(),
            38 => unimplemented!(),
            39 => unimplemented!(),
            40 => unimplemented!(),
            41 => unimplemented!(),
            42 => unimplemented!(),
            43 => unimplemented!(),
            44 => unimplemented!(),
            45 => unimplemented!(),
            46 => unimplemented!(),
            47 => unimplemented!(),
            48 => Some(Self::NoRecurse),
            49 => unimplemented!(),
            50 => unimplemented!(),
            51 => unimplemented!(),
            52 => Some(Self::WriteOnly),
            53 => Some(Self::Speculatable),
            54 => unimplemented!(),
            55 => unimplemented!(),
            56 => unimplemented!(),
            57 => unimplemented!(),
            58 => unimplemented!(),
            59 => unimplemented!(),
            60 => unimplemented!(),
            61 => Some(Self::WillReturn),
            62 => Some(Self::NoFree),
            63 => Some(Self::NoSync),
            64 => unimplemented!(),
            65 => unimplemented!(),
            66 => unimplemented!(),
            67 => unimplemented!(),
            68 => unimplemented!(),
            69 => unimplemented!(),
            70 => Some(Self::MustProgress),
            71 => Some(Self::NoCallback),
            72 => unimplemented!(),
            73 => unimplemented!(),
            74 => unimplemented!(),
            75 => unimplemented!(),
            76 => unimplemented!(),
            77 => unimplemented!(),
            78 => unimplemented!(),
            79 => unimplemented!(),
            80 => unimplemented!(),
            81 => unimplemented!(),
            82 => unimplemented!(),
            83 => unimplemented!(),
            84 => unimplemented!(),
            85 => unimplemented!(),
            86 => Some(Self::Memory),
            87 => unimplemented!(),
            88 => unimplemented!(),
            _ => None
        }
    }
}

fn parse_null_terminated_string<'a, I: Iterator<Item = &'a BitcodeValue>>(iter: &mut I) -> Result<String, DecodeError> {
    String::from_utf8(iter.map_while(|value| match value {
        BitcodeValue::Value(0) => None,
        BitcodeValue::Value(x) => Some(x.clone() as u8),
        _ => None,
    }).collect::<Vec<_>>()).map_err(|_| DecodeError { message: "Invalid attribute string".to_string() })
}

impl Bitcode {
    pub fn new(data: &[u8]) -> Self {
        Bitcode { data: data.to_vec() }
    }

    fn skip_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(iter: &mut I) -> Result<(), DecodeError> {
        let mut depth = 1;
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Block(_) => {
                    depth += 1;
                }
                BitcodeEntry::EndBlock => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(())
                    }
                }
                _ => {}
            }
        }
        Err(DecodeError { message: "Bloken block structure".to_string() })
    }

    fn parse_version_record(record: &BitcodeRecord) -> Result<i32, DecodeError> {
        match record.values.get(0) {
            Some(BitcodeValue::Value(value)) => {
                Ok(*value as i32)
            }
            _ => Err(DecodeError { message: "Missing value".to_string() })
        }
    }

    fn parse_num_entry_record(record: &BitcodeRecord) -> Result<usize, DecodeError> {
        match record.values.get(0) {
            Some(BitcodeValue::Value(value)) => {
                Ok(*value as usize)
            }
            _ => Err(DecodeError { message: "Missing value".to_string() })
        }
    }

    fn parse_type_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(parser: &mut BitcodeModuleParser, iter: &mut I) -> Result<(), DecodeError> {
        let entries = &mut parser.types;
        let mut num_entry = entries.len();
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => {
                    match record.code {
                        1 => {
                            num_entry += Bitcode::parse_num_entry_record(record)?;
                        }
                        2 => {
                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::VoidType);
                        }
                        3 => {
                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::FloatType);
                        }
                        4 => {
                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::DoubleType);
                        }
                        5 => {
                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::LabelType);
                        }
                        7 => {
                            let width = match record.values.get(0) {
                                Some(BitcodeValue::Value(value)) => {
                                    *value as u32
                                }
                                _ => return Err(DecodeError { message: "Missing value".to_string() })
                            };

                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::IntegerType(IntegerType::new(width)));
                        }
                        16 => {
                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::MetadataType);
                        }
                        19 => {
                            let bytes = record.values.iter().map(|x| {
                                match x {
                                    BitcodeValue::Value(value) => {
                                        Ok(*value as u8)
                                    }
                                    _ => Err(DecodeError { message: "Invalid value".to_string() })
                                }
                            }).collect::<Result<Vec<_>, _>>()?;
                            let name = String::from_utf8(bytes).unwrap();

                            let ty = if let Some(BitcodeEntry::Record(record)) = iter.next() {
                                match record.code {
                                    6 => {
                                        unimplemented!();
                                    }
                                    20 => {
                                        let values_iter = flatten_record_values(record).skip(1);
                                        let elements = values_iter.map(|x: &BitcodeValue| {
                                            match x {
                                                BitcodeValue::Value(value) => {
                                                    Ok(Box::new(entries.get(*value as usize).unwrap().clone()))
                                                }
                                                _ => Err(DecodeError { message: "Invalid value".to_string() })
                                            }
                                        }).collect::<Result<Vec<_>, DecodeError>>()?;
                                        let struct_type = StructType::new_with_elements(name, elements);
                                        Ok(Type::StructType(struct_type))
                                    }
                                    _ => {
                                        Err(DecodeError { message: "Expect struct named or opaque".to_string() })
                                    }
                                }
                            } else {
                                Err(DecodeError { message: "Expect struct named or opaque".to_string() })
                            }?;
                            
                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(ty);
                        }
                        21 => {
                            let mut values_iter = flatten_record_values(record);
                            let is_vararg = if let Some(value) = values_iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        *value != 0
                                    }
                                    _ => {
                                        return Err(DecodeError { message: "Invalid value".to_string() });
                                    }
                                }
                            } else {
                                return Err(DecodeError { message: "Missing value".to_string() });
                            };
                            let return_type = if let Some(value) = values_iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        Box::new(entries.get(*value as usize).unwrap().clone())
                                    }
                                    _ => {
                                        return Err(DecodeError { message: "Invalid value".to_string() });
                                    }
                                }
                            } else {
                                return Err(DecodeError { message: "Missing value".to_string() });
                            };
                            let param_types = values_iter.map(|x: &BitcodeValue| {
                                match x {
                                    BitcodeValue::Value(value) => {
                                        Ok(Box::new(entries.get(*value as usize).unwrap().clone()))
                                    }
                                    _ => Err(DecodeError { message: "Invalid value".to_string() })
                                }
                            }).collect::<Result<Vec<_>, DecodeError>>()?;
                            let ty = FunctionType::new(return_type, param_types, is_vararg);
                            

                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::FunctionType(ty));
                        }
                        25 => {
                            let address_space = match record.values.get(0) {
                                Some(BitcodeValue::Value(value)) => {
                                    *value as u32
                                }
                                _ => return Err(DecodeError { message: "Missing value".to_string() })
                            };

                            if entries.len() >= num_entry {
                                return Err(DecodeError { message: "Invalid number of entries".to_string() })
                            }
                            entries.push(Type::PointerType(PointerType::new_with_address_space(address_space)));
                        }
                        _ => {
                            unimplemented!();
                        }
                    }
                }
                BitcodeEntry::Block(_) => {
                    return Err(DecodeError { message: "Unexpected block in type block".to_string() })
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }

        Ok(())
    }

    fn parse_parameter_group_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(parser: &mut BitcodeModuleParser, iter: &mut I) -> Result<(), DecodeError> {
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => {
                    match record.code {
                        3 => {
                            let mut list = AttributeList { attributes: HashSet::new(), function_attributes: HashSet::new() };
                            let mut values = record.values.iter();

                            let group_id = match values.next() {
                                Some(BitcodeValue::Value(value)) => {
                                    *value as u32
                                }
                                _ => return Err(DecodeError { message: "Missing value".to_string() })
                            };

                            let index = match values.next() {
                                Some(BitcodeValue::Value(value)) => {
                                    *value as u32
                                }
                                _ => return Err(DecodeError { message: "Missing value".to_string() })
                            };

                            while let Some(value) = values.next() {
                                let code = match value {
                                    BitcodeValue::Value(value) => {
                                        *value as u32
                                    }
                                    _ => return Err(DecodeError { message: "Invalid value".to_string() })
                                };
                                let attribute = match code {
                                    0 => {
                                        let kind = values.next().and_then(|value| match value {
                                            BitcodeValue::Value(value) => AttributeKind::from_u64(*value),
                                            _ => None
                                        }).ok_or(DecodeError { message: "Invalid attribute kind".to_string() })?;
                                        Attribute::Enum(kind)
                                    }
                                    1 => {
                                        let kind = values.next().and_then(|value| match value {
                                            BitcodeValue::Value(value) => AttributeKind::from_u64(*value),
                                            _ => None
                                        }).ok_or(DecodeError { message: "Invalid attribute kind".to_string() })?;
                                        let value = values.next().and_then(|value| match value {
                                            BitcodeValue::Value(value) => Some(*value),
                                            _ => None
                                        }).ok_or(DecodeError { message: "Invalid attribute kind".to_string() })?;
                                        Attribute::Int(kind, value)
                                    }
                                    4 => {
                                        let key = parse_null_terminated_string(&mut values)?;
                                        let value = parse_null_terminated_string(&mut values)?;
                                        Attribute::String(key, value)
                                    }
                                    _ => {
                                        return Err(DecodeError { message: "Invalid attribute".to_string() })
                                    }
                                };

                                if index == u32::MAX {
                                    list.function_attributes.insert(attribute);
                                } else {
                                    list.attributes.insert(attribute);
                                }
                            }

                            parser.attributes.insert(group_id, list);
                        }
                        _ => {
                            unimplemented!();
                        }
                    }
                }
                BitcodeEntry::Block(_) => {
                    return Err(DecodeError { message: "Unexpected block in parameter group block".to_string() })
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }
        Ok(())
    }

    fn parse_module_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(&self, parser: &mut BitcodeModuleParser, iter: &mut I) -> Result<(), DecodeError> {
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => {
                    match record.code {
                        1 => {
                            parser.version = Bitcode::parse_version_record(record)?;
                        }
                        _ => {
                            unimplemented!();
                        }
                    }
                }
                BitcodeEntry::Block(block) => {
                    if block.blockid == 0 {
                        Bitcode::skip_block(iter)?;
                    } else if let Some(id) = num::traits::FromPrimitive::from_u64(block.blockid) {
                        match id {
                            LLVMIRBlockID::Type => {
                                Bitcode::parse_type_block(parser, iter)?;
                            }
                            LLVMIRBlockID::ParameterGroup => {
                                Bitcode::parse_parameter_group_block(parser, iter)?;
                            }
                            _ => {
                                unimplemented!();
                            }
                        }
                    } else {
                        unimplemented!();
                    }
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }

        Ok(())
    }

    pub fn decode(&self) -> Result<Module, DecodeError> {
        let mut reader = BitcodeReader::new(self);
        let entries = reader.read_to_end()?;

        let mut iter = entries.iter();
        
        let mut module = Module {};
        let mut parser = BitcodeModuleParser{ version: 0, module: &mut module, types: vec![], attributes: HashMap::new() };

        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Block(block) => {
                    let id = num::traits::FromPrimitive::from_u64(block.blockid);
                    match id {
                        Some(LLVMIRBlockID::Module) => {
                            self.parse_module_block(&mut parser, &mut iter)?;
                            return Ok(module);
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        Err(DecodeError { message: "Missing module".to_string() })
    }
}
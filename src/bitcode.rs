use crate::ir::*;
use std::cell::*;
use std::rc::*;

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
    pub message: String,
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
    pub blocklen: u64,
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
    values: Vec<BitcodeValue>,
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
    ops: Vec<BitcodeOperand>,
}

#[derive(Clone, Debug)]
pub enum BitcodeEntry {
    EndBlock,
    Block(BitcodeBlock),
    DefineAbbrev(BitcodeDefineAbbrev),
    Record(BitcodeRecord),
}

impl<'a> BitcodeReader<'a> {
    pub fn new(data: &'a Bitcode) -> Self {
        BitcodeReader {
            p: 32,
            bitcode: data,
            abbrev_len: vec![2],
            abbrevs: vec![vec![]],
            entries: vec![],
            scope: vec![],
            block_infos: HashMap::new(),
        }
    }

    pub fn skip_bytes(&mut self, n: usize) -> Result<usize, DecodeError> {
        if self.p >= self.bitcode.data.len() * 8 {
            Err(DecodeError {
                message: "Invalid size".to_string(),
            })
        } else {
            self.p += n * 8;
            Ok(n)
        }
    }

    pub fn skip_bits(&mut self, n: usize) -> Result<usize, DecodeError> {
        if self.p >= self.bitcode.data.len() * 8 {
            Err(DecodeError {
                message: "Invalid size".to_string(),
            })
        } else {
            self.p += n;
            Ok(n)
        }
    }

    pub fn read_bits(&mut self, n: usize) -> Result<u64, DecodeError> {
        if self.p >= self.bitcode.data.len() * 8 {
            Err(DecodeError {
                message: "Invalid size".to_string(),
            })
        } else {
            let mut value: u64 = 0;
            let mut read_bits = 0;
            while read_bits < n {
                let bit_position = self.p % 8;
                let bits_to_read = (n - read_bits).min(8 - bit_position);
                value |= ((self.bitcode.data[self.p / 8] as u64 >> bit_position)
                    & ((1u64 << bits_to_read) - 1))
                    << read_bits;
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
            Err(DecodeError {
                message: "Invalid abbreviation length".to_string(),
            })
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
                        self.block_infos
                            .entry(current_blockid)
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
            0..=25 => Ok(('a' as u8 + ch) as char),
            26..=51 => Ok(('A' as u8 + (ch - 26)) as char),
            52..=61 => Ok(('0' as u8 + (ch - 52)) as char),
            62 => Ok('.'),
            63 => Ok('_'),
            _ => {
                panic!();
            }
        }
    }

    pub fn skip_to_align(&mut self, alignment: usize) -> Result<usize, DecodeError> {
        let new_p = ((self.p + alignment - 1) / alignment) * alignment;
        self.skip_bits(new_p - self.p)
    }

    pub fn read_values_recursive<'b, I: Iterator<Item = &'b BitcodeOperand>>(
        &mut self,
        op: &BitcodeOperand,
        operands: &mut I,
    ) -> Result<BitcodeValue, DecodeError> {
        match op {
            BitcodeOperand::Literal(lit) => Ok(BitcodeValue::Value(*lit)),
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
                    return Err(DecodeError {
                        message: "Missing encoding for array elements".to_string(),
                    });
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

    pub fn read_values(
        &mut self,
        operands: &[BitcodeOperand],
    ) -> Result<Vec<BitcodeValue>, DecodeError> {
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

                Ok(BitcodeEntry::Block(BitcodeBlock {
                    position: position as u64,
                    blockid: blockid,
                    newabbrevlen: newabbrevlen,
                    blocklen: blocklen,
                }))
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
                                return Err(DecodeError {
                                    message: "Invalid encoding".to_string(),
                                })
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
                Ok(BitcodeEntry::Record(BitcodeRecord {
                    code: code,
                    values: values,
                }))
            }
            Ok(id) => {
                let abbrevs = self.abbrevs.last().unwrap();
                if id as usize >= abbrevs.len() + 4 {
                    return Err(DecodeError {
                        message: "Invalid abbreviation id".to_string(),
                    });
                }

                let abbrev = abbrevs[id as usize - 4].clone();
                if abbrev.ops.len() == 0 {
                    return Err(DecodeError {
                        message: "Invalid abbreviation".to_string(),
                    });
                }
                let code = if let BitcodeOperand::Literal(value) = abbrev.ops[0] {
                    value
                } else {
                    return Err(DecodeError {
                        message: "Invalid code operand".to_string(),
                    });
                };
                let values = self.read_values(&abbrev.ops[1..])?;
                Ok(BitcodeEntry::Record(BitcodeRecord {
                    code: code,
                    values: values,
                }))
            }
            _ => Err(DecodeError {
                message: "Invalid abbreviation id".to_string(),
            }),
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
                BitcodeEntry::DefineAbbrev(_) => {}
                BitcodeEntry::EndBlock => {
                    let error = DecodeError {
                        message: "Broken block structure".to_string(),
                    };
                    if let Some(value) = expect_block_end.pop() {
                        if value as usize != self.p {
                            return Err(error);
                        }
                    } else {
                        return Err(error);
                    }
                }
                BitcodeEntry::Record(_) => {}
            }
        }
        Ok(self.entries.clone())
    }
}

use num_bigint::ToBigUint;
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
    Strtab = 23,
}

struct BitcodeModuleParser {
    strtab: Vec<u8>,
    version: i32,
    triple: String,
    data_layout: String,
    source_filename: String,
    types: Vec<Type>,
    attribute_groups: HashMap<u32, AttributeList>,
    attributes: Vec<AttributeList>,
    comdats: Vec<Comdat>,
    values: Vec<Rc<RefCell<Value>>>,
    global_inits: HashMap<usize, u64>,
    vst_offset: u64,
    function_demanding_body_index: Option<usize>,
}

impl BitcodeModuleParser {
    fn push_value(&mut self, value: Value) {
        let value = Rc::new(RefCell::new(value));
        self.values.push(value);
    }
}

use either::*;
use num_traits::FromPrimitive;

fn flatten_record_values<'a>(
    record: &'a BitcodeRecord,
) -> impl Iterator<Item = &BitcodeValue> + 'a {
    record.values.iter().flat_map(|x| match x {
        BitcodeValue::Array(values) => Left(values.iter()),
        value => Right(std::iter::once(value)),
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
            _ => None,
        }
    }
}

fn parse_null_terminated_string<'a, I: Iterator<Item = &'a BitcodeValue>>(
    iter: &mut I,
) -> Result<String, DecodeError> {
    String::from_utf8(
        iter.map_while(|value| match value {
            BitcodeValue::Value(0) => None,
            BitcodeValue::Value(x) => Some(x.clone() as u8),
            _ => None,
        })
        .collect::<Vec<_>>(),
    )
    .map_err(|_| DecodeError {
        message: "Invalid attribute string".to_string(),
    })
}

impl num::FromPrimitive for LinkageTypes {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::External),
            2 => Some(Self::Appending),
            3 => Some(Self::Internal),
            5 => Some(Self::External),
            6 => Some(Self::External),
            7 => Some(Self::ExternalWeak),
            8 => Some(Self::Common),
            9 => Some(Self::Private),
            12 => Some(Self::AvailableExternally),
            13 => Some(Self::Private),
            14 => Some(Self::Private),
            15 => Some(Self::External),
            1 | 16 => Some(Self::WeakAny),
            10 | 17 => Some(Self::WeakODR),
            4 | 18 => Some(Self::LinkOnceAny),
            11 | 19 => Some(Self::LinkOnceODR),
            _ => None,
        }
    }
}

impl num::FromPrimitive for VisibilityTypes {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::Default),
            1 => Some(Self::Hidden),
            2 => Some(Self::Protected),
            _ => None,
        }
    }
}

impl num::FromPrimitive for ThreadLocalMode {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::NotThreadLocal),
            1 => Some(Self::GeneralDynamicTLSModel),
            2 => Some(Self::LocalDynamicTLSModel),
            3 => Some(Self::InitialExecTLSModel),
            4 => Some(Self::LocalExecTLSModel),
            _ => None,
        }
    }
}

impl num::FromPrimitive for UnnamedAddr {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::None),
            1 => Some(Self::Global),
            2 => Some(Self::Local),
            _ => None,
        }
    }
}

impl num::FromPrimitive for DLLStorageClassTypes {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::Default),
            1 => Some(Self::DLLImport),
            2 => Some(Self::DLLExport),
            _ => None,
        }
    }
}

impl num::FromPrimitive for CastOpcode {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::Trunc),
            1 => Some(Self::ZExt),
            2 => Some(Self::SExt),
            3 => Some(Self::FPToUI),
            4 => Some(Self::FPToSI),
            5 => Some(Self::UIToFP),
            6 => Some(Self::SIToFP),
            7 => Some(Self::FPTrunc),
            8 => Some(Self::FPExt),
            9 => Some(Self::PtrToInt),
            10 => Some(Self::IntToPtr),
            11 => Some(Self::BitCast),
            12 => Some(Self::AddrSpaceCast),
            _ => None,
        }
    }
}

fn decode_binary_opcode(value: u64, ty: Type) -> Option<BinaryOpcode> {
    let is_fp = ty.is_floating_point();
    if is_fp {
        match value {
            0 => Some(BinaryOpcode::FAdd),
            1 => Some(BinaryOpcode::FSub),
            2 => Some(BinaryOpcode::FMul),
            3 => None,
            4 => Some(BinaryOpcode::FDiv),
            5 => None,
            6 => Some(BinaryOpcode::FRem),
            7 => None,
            8 => None,
            9 => None,
            10 => None,
            11 => None,
            12 => None,
            _ => None,
        }
    } else {
        match value {
            0 => Some(BinaryOpcode::Add),
            1 => Some(BinaryOpcode::Sub),
            2 => Some(BinaryOpcode::Mul),
            3 => Some(BinaryOpcode::UDiv),
            4 => Some(BinaryOpcode::SDiv),
            5 => Some(BinaryOpcode::URem),
            6 => Some(BinaryOpcode::SRem),
            7 => Some(BinaryOpcode::Shl),
            8 => Some(BinaryOpcode::LShr),
            9 => Some(BinaryOpcode::AShr),
            10 => Some(BinaryOpcode::And),
            11 => Some(BinaryOpcode::Or),
            12 => Some(BinaryOpcode::Xor),
            _ => None,
        }
    }
}

impl num::FromPrimitive for FCmpPredicate {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::FALSE),
            1 => Some(Self::OEQ),
            2 => Some(Self::OGT),
            3 => Some(Self::OGE),
            4 => Some(Self::OLT),
            5 => Some(Self::OLE),
            6 => Some(Self::ONE),
            7 => Some(Self::ORD),
            8 => Some(Self::UNO),
            9 => Some(Self::UEQ),
            10 => Some(Self::UGT),
            11 => Some(Self::UGE),
            12 => Some(Self::ULT),
            13 => Some(Self::ULE),
            14 => Some(Self::UNE),
            15 => Some(Self::TRUE),
            _ => None,
        }
    }
}

impl num::FromPrimitive for ICmpPredicate {
    fn from_i64(value: i64) -> Option<Self> {
        if value < 0 {
            None
        } else {
            Self::from_u64(value as u64)
        }
    }
    fn from_u64(value: u64) -> Option<Self> {
        match value {
            32 => Some(Self::EQ),
            33 => Some(Self::NE),
            34 => Some(Self::UGT),
            35 => Some(Self::UGE),
            36 => Some(Self::ULT),
            37 => Some(Self::ULE),
            38 => Some(Self::SGT),
            39 => Some(Self::SGE),
            40 => Some(Self::SLT),
            41 => Some(Self::SLE),
            _ => None,
        }
    }
}

struct ValueRefList {
    values: Vec<ValueRef>,
}

impl ValueRefList {
    fn get(&self, index: usize) -> Option<ValueRef> {
        self.values.get(index).map(|x| x.clone())
    }

    fn len(&self) -> usize {
        self.values.len()
    }

    fn push(&mut self, value: ValueRef) {
        self.values.push(value)
    }
}

impl Bitcode {
    pub fn new(data: &[u8]) -> Self {
        Bitcode {
            data: data.to_vec(),
        }
    }

    fn skip_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        iter: &mut I,
    ) -> Result<(), DecodeError> {
        let mut depth = 1;
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Block(_) => {
                    depth += 1;
                }
                BitcodeEntry::EndBlock => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(());
                    }
                }
                _ => {}
            }
        }
        Err(DecodeError {
            message: "Bloken block structure".to_string(),
        })
    }

    fn parse_version_record(record: &BitcodeRecord) -> Result<i32, DecodeError> {
        match record.values.get(0) {
            Some(BitcodeValue::Value(value)) => Ok(*value as i32),
            _ => Err(DecodeError {
                message: "Missing value".to_string(),
            }),
        }
    }

    fn parse_triple_record(record: &BitcodeRecord) -> Result<String, DecodeError> {
        let bytes = record
            .values
            .iter()
            .map(|x| match x {
                BitcodeValue::Value(value) => Ok(*value as u8),
                _ => Err(DecodeError {
                    message: "Invalid value".to_string(),
                }),
            })
            .collect::<Result<Vec<_>, _>>()?;
        String::from_utf8(bytes).map_err(|_| DecodeError {
            message: "Invalid triple string".to_string(),
        })
    }

    fn parse_data_layout_record(record: &BitcodeRecord) -> Result<String, DecodeError> {
        let bytes = record
            .values
            .iter()
            .map(|x| match x {
                BitcodeValue::Value(value) => Ok(*value as u8),
                _ => Err(DecodeError {
                    message: "Invalid value".to_string(),
                }),
            })
            .collect::<Result<Vec<_>, _>>()?;
        String::from_utf8(bytes).map_err(|_| DecodeError {
            message: "Invalid data layout string".to_string(),
        })
    }

    fn parse_source_filename_record(record: &BitcodeRecord) -> Result<String, DecodeError> {
        let values_iter = flatten_record_values(record);
        let bytes = values_iter
            .map(|x| match x {
                BitcodeValue::Value(value) => Ok(*value as u8),
                BitcodeValue::Char6(value) => Ok(*value as u8),
                _ => Err(DecodeError {
                    message: "Invalid value".to_string(),
                }),
            })
            .collect::<Result<Vec<_>, _>>()?;
        String::from_utf8(bytes).map_err(|_| DecodeError {
            message: "Invalid source filename string".to_string(),
        })
    }

    fn parse_num_entry_record(record: &BitcodeRecord) -> Result<usize, DecodeError> {
        match record.values.get(0) {
            Some(BitcodeValue::Value(value)) => Ok(*value as usize),
            _ => Err(DecodeError {
                message: "Missing value".to_string(),
            }),
        }
    }

    fn parse_vst_offset_record(record: &BitcodeRecord) -> Result<u64, DecodeError> {
        match record.values.get(0) {
            Some(BitcodeValue::Value(value)) => Ok(*value - 1),
            _ => Err(DecodeError {
                message: "Missing value".to_string(),
            }),
        }
    }

    fn parse_type_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        parser: &mut BitcodeModuleParser,
        iter: &mut I,
    ) -> Result<(), DecodeError> {
        let entries = &mut parser.types;
        let mut num_entry = entries.len();
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => match record.code {
                    1 => {
                        num_entry += Bitcode::parse_num_entry_record(record)?;
                    }
                    2 => {
                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::VoidType);
                    }
                    3 => {
                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::FloatType);
                    }
                    4 => {
                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::DoubleType);
                    }
                    5 => {
                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::LabelType);
                    }
                    7 => {
                        let width = match record.values.get(0) {
                            Some(BitcodeValue::Value(value)) => *value as u32,
                            _ => {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                })
                            }
                        };

                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::IntegerType(IntegerType::new(width)));
                    }
                    16 => {
                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::MetadataType);
                    }
                    19 => {
                        let bytes = record
                            .values
                            .iter()
                            .map(|x| match x {
                                BitcodeValue::Value(value) => Ok(*value as u8),
                                _ => Err(DecodeError {
                                    message: "Invalid value".to_string(),
                                }),
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        let name = String::from_utf8(bytes).unwrap();

                        let ty = if let Some(BitcodeEntry::Record(record)) = iter.next() {
                            match record.code {
                                6 => {
                                    unimplemented!();
                                }
                                20 => {
                                    let values_iter = flatten_record_values(record).skip(1);
                                    let elements = values_iter
                                        .map(|x: &BitcodeValue| match x {
                                            BitcodeValue::Value(value) => Ok(Box::new(
                                                entries.get(*value as usize).unwrap().clone(),
                                            )),
                                            _ => Err(DecodeError {
                                                message: "Invalid value".to_string(),
                                            }),
                                        })
                                        .collect::<Result<Vec<_>, DecodeError>>()?;
                                    let struct_type = StructType::new_with_elements(name, elements);
                                    Ok(Type::StructType(struct_type))
                                }
                                _ => Err(DecodeError {
                                    message: "Expect struct named or opaque".to_string(),
                                }),
                            }
                        } else {
                            Err(DecodeError {
                                message: "Expect struct named or opaque".to_string(),
                            })
                        }?;

                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(ty);
                    }
                    21 => {
                        let mut values_iter = flatten_record_values(record);
                        let is_vararg = if let Some(value) = values_iter.next() {
                            match value {
                                BitcodeValue::Value(value) => *value != 0,
                                _ => {
                                    return Err(DecodeError {
                                        message: "Invalid value".to_string(),
                                    });
                                }
                            }
                        } else {
                            return Err(DecodeError {
                                message: "Missing value".to_string(),
                            });
                        };
                        let return_type = if let Some(value) = values_iter.next() {
                            match value {
                                BitcodeValue::Value(value) => {
                                    Box::new(entries.get(*value as usize).unwrap().clone())
                                }
                                _ => {
                                    return Err(DecodeError {
                                        message: "Invalid value".to_string(),
                                    });
                                }
                            }
                        } else {
                            return Err(DecodeError {
                                message: "Missing value".to_string(),
                            });
                        };
                        let param_types = values_iter
                            .map(|x: &BitcodeValue| match x {
                                BitcodeValue::Value(value) => {
                                    Ok(Box::new(entries.get(*value as usize).unwrap().clone()))
                                }
                                _ => Err(DecodeError {
                                    message: "Invalid value".to_string(),
                                }),
                            })
                            .collect::<Result<Vec<_>, DecodeError>>()?;
                        let ty = FunctionType::new(return_type, param_types, is_vararg);

                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::FunctionType(ty));
                    }
                    25 => {
                        let address_space = match record.values.get(0) {
                            Some(BitcodeValue::Value(value)) => *value as u32,
                            _ => {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                })
                            }
                        };

                        if entries.len() >= num_entry {
                            return Err(DecodeError {
                                message: "Invalid number of entries".to_string(),
                            });
                        }
                        entries.push(Type::PointerType(PointerType::new_with_address_space(
                            address_space,
                        )));
                    }
                    _ => {
                        unimplemented!();
                    }
                },
                BitcodeEntry::Block(_) => {
                    return Err(DecodeError {
                        message: "Unexpected block in type block".to_string(),
                    })
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }

        Ok(())
    }

    fn parse_parameter_group_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        parser: &mut BitcodeModuleParser,
        iter: &mut I,
    ) -> Result<(), DecodeError> {
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => match record.code {
                    3 => {
                        let mut values = record.values.iter();

                        let group_id = match values.next() {
                            Some(BitcodeValue::Value(value)) => *value as u32,
                            _ => {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                })
                            }
                        };

                        let index = match values.next() {
                            Some(BitcodeValue::Value(value)) => *value as u32,
                            _ => {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                })
                            }
                        };

                        let mut attributes = HashSet::new();

                        while let Some(value) = values.next() {
                            let code = match value {
                                BitcodeValue::Value(value) => *value as u32,
                                _ => {
                                    return Err(DecodeError {
                                        message: "Invalid value".to_string(),
                                    })
                                }
                            };
                            let attribute = match code {
                                0 => {
                                    let kind = values
                                        .next()
                                        .and_then(|value| match value {
                                            BitcodeValue::Value(value) => {
                                                AttributeKind::from_u64(*value)
                                            }
                                            _ => None,
                                        })
                                        .ok_or(DecodeError {
                                            message: "Invalid attribute kind".to_string(),
                                        })?;
                                    Attribute::Enum(kind)
                                }
                                1 => {
                                    let kind = values
                                        .next()
                                        .and_then(|value| match value {
                                            BitcodeValue::Value(value) => {
                                                AttributeKind::from_u64(*value)
                                            }
                                            _ => None,
                                        })
                                        .ok_or(DecodeError {
                                            message: "Invalid attribute kind".to_string(),
                                        })?;
                                    let value = values
                                        .next()
                                        .and_then(|value| match value {
                                            BitcodeValue::Value(value) => Some(*value),
                                            _ => None,
                                        })
                                        .ok_or(DecodeError {
                                            message: "Invalid attribute kind".to_string(),
                                        })?;
                                    Attribute::Int(kind, value)
                                }
                                4 => {
                                    let key = parse_null_terminated_string(&mut values)?;
                                    let value = parse_null_terminated_string(&mut values)?;
                                    Attribute::String(key, value)
                                }
                                _ => {
                                    return Err(DecodeError {
                                        message: "Invalid attribute".to_string(),
                                    })
                                }
                            };

                            attributes.insert(attribute);
                        }

                        let mut list = AttributeList { attributes: vec![] };

                        if index != u32::MAX {
                            list.attributes.resize(index as usize + 2, HashSet::new());
                            list.attributes[index as usize] = attributes;
                        } else {
                            list.attributes.push(attributes);
                        }

                        parser.attribute_groups.insert(group_id, list);
                    }
                    _ => {
                        unimplemented!();
                    }
                },
                BitcodeEntry::Block(_) => {
                    return Err(DecodeError {
                        message: "Unexpected block in parameter group block".to_string(),
                    })
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }
        Ok(())
    }

    fn parse_parameter_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        parser: &mut BitcodeModuleParser,
        iter: &mut I,
    ) -> Result<(), DecodeError> {
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => match record.code {
                    1 => unimplemented!(),
                    2 => {
                        let mut groups = vec![];
                        for value in &record.values {
                            let group_id = match value {
                                BitcodeValue::Value(value) => *value as u32,
                                _ => {
                                    return Err(DecodeError {
                                        message: "Missing value".to_string(),
                                    })
                                }
                            };
                            let group =
                                parser.attribute_groups.get(&group_id).ok_or(DecodeError {
                                    message: "Invalid group id".to_string(),
                                })?;
                            groups.push(group.clone());
                        }

                        let attributes = AttributeList::merge(groups).ok_or(DecodeError {
                            message: "Invalid attributes".to_string(),
                        })?;

                        parser.attributes.push(attributes);
                    }
                    _ => {
                        unimplemented!();
                    }
                },
                BitcodeEntry::Block(_) => {
                    return Err(DecodeError {
                        message: "Unexpected block in parameter block".to_string(),
                    })
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }
        Ok(())
    }

    fn parse_constant_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        parser: &mut BitcodeModuleParser,
        iter: &mut I,
    ) -> Result<(), DecodeError> {
        let mut current_type = Type::VoidType;
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => {
                    let mut value = None;
                    match record.code {
                        1 => {
                            if let Some(value) = record.values.get(0) {
                                let type_id = match value {
                                    BitcodeValue::Value(value) => *value as usize,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid value type".to_string(),
                                        })
                                    }
                                };

                                current_type = parser
                                    .types
                                    .get(type_id)
                                    .ok_or(DecodeError {
                                        message: "Invalid value".to_string(),
                                    })?
                                    .clone();
                            } else {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                });
                            }
                            continue;
                        }
                        2 => {
                            let const_value = match current_type {
                                Type::IntegerType(_) => {
                                    Value::ConstantInt(Rc::new(RefCell::new(ConstantInt {
                                        ty: current_type.clone(),
                                        value: 0.to_biguint().unwrap(),
                                        name: ValueName::None,
                                    })))
                                }
                                _ => {
                                    return Err(DecodeError {
                                        message: "Type undefined null".to_string(),
                                    })
                                }
                            };
                            value = Some(const_value);
                        }
                        3 => {
                            value = Some(Value::Undef(Rc::new(RefCell::new(Undef {
                                ty: current_type.clone(),
                            }))));
                        }
                        4 => {
                            let const_value = if let Some(value) = record.values.get(0) {
                                match value {
                                    BitcodeValue::Value(value) => *value,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid value type".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                });
                            };

                            let const_value = if const_value & 1 == 0 {
                                const_value >> 1
                            } else if const_value != 1 {
                                -((const_value >> 1) as i64) as u64
                            } else {
                                1u64 << 63
                            };

                            value = Some(Value::ConstantInt(Rc::new(RefCell::new(ConstantInt {
                                ty: current_type.clone(),
                                value: const_value.to_biguint().unwrap(),
                                name: ValueName::None,
                            }))));
                        }
                        _ => {
                            unimplemented!();
                        }
                    }
                    parser.push_value(value.unwrap());
                }
                BitcodeEntry::Block(_) => {
                    return Err(DecodeError {
                        message: "Unexpected block in parameter block".to_string(),
                    })
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }
        Ok(())
    }

    fn get_function_demanding_body(
        parser: &BitcodeModuleParser,
    ) -> Result<Rc<RefCell<Function>>, DecodeError> {
        let function = parser
            .function_demanding_body_index
            .and_then(|index| parser.values.get(index))
            .ok_or(DecodeError {
                message: "Missing function".to_string(),
            })?;

        if let Value::Function(value) = function.borrow().clone() {
            Ok(value.clone())
        } else {
            panic!();
        }
    }

    fn parse_function_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        parser: &mut BitcodeModuleParser,
        iter: &mut I,
    ) -> Result<(), DecodeError> {
        let mut values = ValueRefList {
            values: parser.values.iter().map(|x| Rc::downgrade(&x)).collect(),
        };

        let function = Bitcode::get_function_demanding_body(parser)?;
        for arg in &function.borrow_mut().argument_values {
            values.push(Rc::downgrade(arg));
        }

        let mut lazy_assign = vec![];

        let mut bb_idx = 0;
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => {
                    match record.code {
                        1 => {
                            /* DECLAREBLOCKS */
                            let size = if let Some(value) = record.values.get(0) {
                                match value {
                                    BitcodeValue::Value(value) => *value as usize,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid value type".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                });
                            };

                            for _ in 0..size {
                                let bb = Rc::new(RefCell::new(BasicBlock {
                                    insts: vec![],
                                    inst_values: vec![],
                                }));

                                function.borrow_mut().bbs.push(Rc::downgrade(&bb));

                                let value = Rc::new(RefCell::new(Value::BasicBlock(bb)));

                                function.borrow_mut().bb_values.push(value);
                            }
                        }
                        2 => {
                            /* INST_BINOP */
                            let mut iter = record.values.iter();
                            let lhs = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid operand value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing operand".to_string(),
                                });
                            };

                            let rhs = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid operand value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing operand".to_string(),
                                });
                            };

                            let opcode = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => decode_binary_opcode(
                                        *value,
                                        lhs.upgrade().unwrap().borrow().ty(),
                                    )
                                    .unwrap()
                                    .clone(),
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid opcode value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing opcode".to_string(),
                                });
                            };

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let inst = Inst::BinOpInst(BinOpInst {
                                opcode,
                                lhs,
                                rhs,
                                name: ValueName::None,
                            });
                            let (value, _) =
                                bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                            values.push(value);
                        }
                        3 => {
                            /* INST_CAST */
                            let mut iter = record.values.iter();
                            let operand = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid operand value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing operand".to_string(),
                                });
                            };

                            let ty = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        parser.types.get(*value as usize).unwrap().clone()
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid type value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing type".to_string(),
                                });
                            };

                            let opcode = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        CastOpcode::from_u64(*value).unwrap().clone()
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid opcode value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing opcode".to_string(),
                                });
                            };

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let inst = Inst::CastInst(CastInst {
                                value: operand,
                                result_ty: ty,
                                opcode,
                                name: ValueName::None,
                            });
                            let (value, _) =
                                bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                            values.push(value);
                        }
                        10 => {
                            /* INST_RET */
                            if record.values.len() != 0 {
                                unimplemented!();
                            }
                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let inst = Inst::ReturnInst(ReturnInst {
                                return_value: None,
                                name: ValueName::None,
                            });

                            if inst.is_terminator() {
                                bb_idx += 1;
                            }

                            bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                        }
                        11 => {
                            /* INST_BR */
                            let mut iter = record.values.iter();
                            let true_cond = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            function.borrow().bbs.get(*value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid true cond value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing true cond".to_string(),
                                });
                            };
                            let false_cond = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            function.borrow().bbs.get(*value as usize)
                                        {
                                            Some(value.clone())
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid false cond value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                None
                            };
                            let cond = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            Some(value.clone())
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid cond value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                None
                            };

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let inst = Inst::BranchInst(BranchInst {
                                true_condition: true_cond,
                                false_condition: false_cond.zip(cond),
                                name: ValueName::None,
                            });

                            if inst.is_terminator() {
                                bb_idx += 1;
                            }

                            bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                        }
                        16 => {
                            /* INST_PHI */
                            let ty = if let Some(value) = record.values.get(0) {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        parser.types.get(*value as usize).unwrap().clone()
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid type value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing type".to_string(),
                                });
                            };

                            let inst = Inst::PhiNode(PhiNode {
                                ty,
                                incoming: vec![],
                                name: ValueName::None,
                            });

                            let reference_value_index = values.len();

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let (value, inst) = bb
                                .upgrade()
                                .unwrap()
                                .borrow_mut()
                                .push_instruction(inst.clone());
                            values.push(value);

                            let assign =
                                move |parser: &BitcodeModuleParser,
                                      values: &ValueRefList|
                                      -> Result<(), DecodeError> {
                                    let function = Bitcode::get_function_demanding_body(parser)?;
                                    let values_iter =
                                        flatten_record_values(record).skip(1).step_by(2);
                                    let bbs_iter = flatten_record_values(record).skip(2).step_by(2);
                                    let values = values_iter
                                        .zip(bbs_iter)
                                        .map(|(value, bb)| {
                                            let value = match value {
                                                BitcodeValue::Value(value) => {
                                                    let value = if (*value & 1) == 0 {
                                                        (*value >> 1) as i64
                                                    } else {
                                                        -((*value >> 1) as i64)
                                                    };
                                                    if let Some(value) = values.get(
                                                        (reference_value_index as i64 - value)
                                                            as usize,
                                                    ) {
                                                        value.clone()
                                                    } else {
                                                        unimplemented!();
                                                    }
                                                }
                                                _ => {
                                                    return Err(DecodeError {
                                                        message: "Invalid value".to_string(),
                                                    })
                                                }
                                            };
                                            let bb = match bb {
                                                BitcodeValue::Value(value) => {
                                                    if let Some(value) =
                                                        function.borrow().bbs.get(*value as usize)
                                                    {
                                                        value.clone()
                                                    } else {
                                                        unimplemented!();
                                                    }
                                                }
                                                _ => {
                                                    return Err(DecodeError {
                                                        message: "Invalid basic block".to_string(),
                                                    })
                                                }
                                            };
                                            Ok((bb, value))
                                        })
                                        .collect::<Result<Vec<_>, _>>()?;

                                    if let Inst::PhiNode(inst) =
                                        &mut *inst.upgrade().unwrap().borrow_mut()
                                    {
                                        inst.incoming = values;
                                    }

                                    Ok(())
                                };

                            lazy_assign.push(assign);
                        }
                        20 => {
                            /* INST_LOAD */
                            let mut value_index = 0;
                            let (operand, operand_type) = if let Some(value) =
                                record.values.get(value_index)
                            {
                                value_index += 1;
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            (value.clone(), value.upgrade().unwrap().borrow().ty())
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid operand value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing operand".to_string(),
                                });
                            };

                            if let Type::PointerType(_) = operand_type {
                            } else {
                                return Err(DecodeError {
                                    message: "Invalid operand type".to_string(),
                                });
                            }

                            let ty = if value_index + 3 == record.values.len() {
                                if let Some(value) = record.values.get(value_index) {
                                    value_index += 1;
                                    match value {
                                        BitcodeValue::Value(value) => {
                                            if let Some(value) = parser.types.get(*value as usize) {
                                                value.clone()
                                            } else {
                                                return Err(DecodeError {
                                                    message: "Invalid type".to_string(),
                                                });
                                            }
                                        }
                                        _ => {
                                            return Err(DecodeError {
                                                message: "Invalid type value".to_string(),
                                            })
                                        }
                                    }
                                } else {
                                    return Err(DecodeError {
                                        message: "Missing operand".to_string(),
                                    });
                                }
                            } else {
                                unimplemented!();
                            };

                            let alignment = if let Some(value) = record.values.get(value_index) {
                                match value {
                                    BitcodeValue::Value(value) => *value,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid alignment value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing alignment".to_string(),
                                });
                            } - 1;

                            let volatile = if let Some(value) = record.values.get(value_index + 1) {
                                match value {
                                    BitcodeValue::Value(value) => *value != 0,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid volatile value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing volatile".to_string(),
                                });
                            };

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let inst = Inst::LoadInst(LoadInst {
                                ty: ty,
                                ptr: operand,
                                is_volatile: volatile,
                                alignment: alignment as u32,
                                name: ValueName::None,
                            });
                            let (value, _) =
                                bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                            values.push(value);
                        }
                        28 => {
                            /* INST_CMP */
                            let mut iter = record.values.iter();
                            let lhs = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid operand value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing operand".to_string(),
                                });
                            };

                            let rhs = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid operand value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing operand".to_string(),
                                });
                            };

                            let inst = if lhs.upgrade().unwrap().borrow().ty().is_floating_point() {
                                let predicate = if let Some(value) = iter.next() {
                                    match value {
                                        BitcodeValue::Value(value) => {
                                            FCmpPredicate::from_u64(*value).unwrap().clone()
                                        }
                                        _ => {
                                            return Err(DecodeError {
                                                message: "Invalid predicate value".to_string(),
                                            })
                                        }
                                    }
                                } else {
                                    return Err(DecodeError {
                                        message: "Missing predicate".to_string(),
                                    });
                                };
                                Inst::FCmpInst(FCmpInst {
                                    predicate,
                                    lhs,
                                    rhs,
                                    name: ValueName::None,
                                })
                            } else {
                                let predicate = if let Some(value) = iter.next() {
                                    match value {
                                        BitcodeValue::Value(value) => {
                                            ICmpPredicate::from_u64(*value).unwrap().clone()
                                        }
                                        _ => {
                                            return Err(DecodeError {
                                                message: "Invalid predicate value".to_string(),
                                            })
                                        }
                                    }
                                } else {
                                    return Err(DecodeError {
                                        message: "Missing predicate".to_string(),
                                    });
                                };
                                Inst::ICmpInst(ICmpInst {
                                    predicate,
                                    lhs,
                                    rhs,
                                    name: ValueName::None,
                                })
                            };

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let (value, _) =
                                bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                            values.push(value);
                        }
                        34 => {
                            /* CALL */
                            let mut iter = record.values.iter();
                            let attributes = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if *value > 0 {
                                            parser
                                                .attributes
                                                .get(*value as usize - 1)
                                                .unwrap()
                                                .clone()
                                        } else {
                                            AttributeList { attributes: vec![] }
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid function attributes value"
                                                .to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing function attributes".to_string(),
                                });
                            };

                            let cc_info = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => *value,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid function cc info value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing function cc info".to_string(),
                                });
                            };

                            let fast_math_flags = if (cc_info >> 17) & 1 != 0 {
                                /* Fast math flag */
                                let mut fast_math_flags = FastMathFlags::empty();
                                let value = if let Some(value) = iter.next() {
                                    match value {
                                        BitcodeValue::Value(value) => *value,
                                        _ => {
                                            return Err(DecodeError {
                                                message: "Invalid fast math flags value"
                                                    .to_string(),
                                            })
                                        }
                                    }
                                } else {
                                    return Err(DecodeError {
                                        message: "Missing fast math flags".to_string(),
                                    });
                                };
                                if (value & (1 << 0)) != 0 {
                                    fast_math_flags = FastMathFlags::all();
                                } else if (value & (1 << 1)) != 0 {
                                    fast_math_flags |= FastMathFlags::NoNaNs;
                                } else if (value & (1 << 2)) != 0 {
                                    fast_math_flags |= FastMathFlags::NoInfs;
                                } else if (value & (1 << 3)) != 0 {
                                    fast_math_flags |= FastMathFlags::NoSignedZeros;
                                } else if (value & (1 << 4)) != 0 {
                                    fast_math_flags |= FastMathFlags::AllowReciprocal;
                                } else if (value & (1 << 5)) != 0 {
                                    fast_math_flags |= FastMathFlags::AllowContract;
                                } else if (value & (1 << 6)) != 0 {
                                    fast_math_flags |= FastMathFlags::ApproxFunc;
                                } else if (value & (1 << 7)) != 0 {
                                    fast_math_flags |= FastMathFlags::AllowReassoc;
                                }
                                fast_math_flags
                            } else {
                                FastMathFlags::empty()
                            };

                            let function_type = if (cc_info >> 15) & 1 != 0 {
                                /* Explicit type */
                                if let Some(value) = iter.next() {
                                    match value {
                                        BitcodeValue::Value(value) => {
                                            Some(parser.types.get(*value as usize).unwrap().clone())
                                        }
                                        _ => {
                                            return Err(DecodeError {
                                                message: "Invalid type value".to_string(),
                                            })
                                        }
                                    }
                                } else {
                                    return Err(DecodeError {
                                        message: "Missing type".to_string(),
                                    });
                                }
                            } else {
                                None
                            };

                            let callee = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        values.get(values.len() - *value as usize).unwrap().clone()
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid callee value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing callee".to_string(),
                                });
                            };

                            let function_type = if let Some(Type::FunctionType(ty)) = function_type
                            {
                                ty
                            } else {
                                return Err(DecodeError {
                                    message: "Missing function type".to_string(),
                                });
                            };

                            let args = function_type
                                .params
                                .iter()
                                .map(|param| match **param {
                                    Type::LabelType => {
                                        if let Some(value) = iter.next() {
                                            match value {
                                                BitcodeValue::Value(value) => Ok(Rc::downgrade(
                                                    &function.borrow_mut().bb_values
                                                        [*value as usize],
                                                )),
                                                _ => Err(DecodeError {
                                                    message: "Invalid value".to_string(),
                                                }),
                                            }
                                        } else {
                                            Err(DecodeError {
                                                message: "Missing value".to_string(),
                                            })
                                        }
                                    }
                                    _ => {
                                        unimplemented!();
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            if !function_type.is_vararg {
                                if !iter.next().is_none() {
                                    return Err(DecodeError {
                                        message: "Invalid number of function arguments".to_string(),
                                    });
                                }
                            } else {
                                unimplemented!();
                            }

                            let bb = &function.borrow_mut().bbs[bb_idx];

                            let inst = Inst::CallInst(CallInst {
                                function_type: Type::FunctionType(function_type),
                                callee: callee,
                                args: args,
                                attributes: attributes.clone(),
                                name: ValueName::None,
                            });

                            let (value, _) =
                                bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                            values.push(value);
                        }
                        43 => {
                            /* INST_GEP */
                            let mut iter = flatten_record_values(record);
                            let inbounds = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => *value != 0,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid inbounds value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing inbounds".to_string(),
                                });
                            };

                            let ty = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        parser.types.get(*value as usize).unwrap().clone()
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid type value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing type".to_string(),
                                });
                            };

                            let base_ptr = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid operand value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing operand".to_string(),
                                });
                            };

                            let indexes = iter
                                .map(|value| match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            Ok(value.clone())
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => Err(DecodeError {
                                        message: "Invalid operand value".to_string(),
                                    }),
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let inst = Inst::GetElementPtrInst(GetElementPtrInst {
                                ty: ty,
                                base_ptr: base_ptr,
                                indexes: indexes,
                                inbounds: inbounds,
                                name: ValueName::None,
                            });
                            let (value, _) =
                                bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                            values.push(value);
                        }
                        44 => {
                            /* INST_STORE */
                            let mut iter = flatten_record_values(record);
                            let ptr = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid pointer value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing pointer".to_string(),
                                });
                            };

                            let value = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => {
                                        if let Some(value) =
                                            values.get(values.len() - *value as usize)
                                        {
                                            value.clone()
                                        } else {
                                            unimplemented!();
                                        }
                                    }
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing value".to_string(),
                                });
                            };

                            let alignment = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => *value,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid alignment value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing alignment".to_string(),
                                });
                            } - 1;

                            let volatile = if let Some(value) = iter.next() {
                                match value {
                                    BitcodeValue::Value(value) => *value != 0,
                                    _ => {
                                        return Err(DecodeError {
                                            message: "Invalid volatile value".to_string(),
                                        })
                                    }
                                }
                            } else {
                                return Err(DecodeError {
                                    message: "Missing volatile".to_string(),
                                });
                            };

                            let bb = &function.borrow_mut().bbs[bb_idx];
                            let inst = Inst::StoreInst(StoreInst {
                                ptr,
                                value,
                                is_volatile: volatile,
                                alignment: alignment as u32,
                                name: ValueName::None,
                            });
                            let (value, _) =
                                bb.upgrade().unwrap().borrow_mut().push_instruction(inst);
                            values.push(value);
                        }
                        _ => {
                            unimplemented!();
                        }
                    }
                }
                BitcodeEntry::Block(block) => {
                    if let Some(id) = num::traits::FromPrimitive::from_u64(block.blockid) {
                        match id {
                            LLVMIRBlockID::Constant => {
                                Bitcode::parse_constant_block(parser, iter)?;
                            }
                            LLVMIRBlockID::Metadata => {
                                Bitcode::skip_block(iter)?;
                            }
                            LLVMIRBlockID::MetadataAttachment => {
                                Bitcode::skip_block(iter)?;
                            }
                            _ => {
                                unimplemented!();
                            }
                        }
                    } else if block.blockid == 18 {
                        /* USE_LIST */
                        Bitcode::skip_block(iter)?;
                    } else {
                        return Err(DecodeError {
                            message: "Unexpected block in function block".to_string(),
                        });
                    };
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }

        for assign in lazy_assign {
            assign(parser, &values)?;
        }

        Ok(())
    }

    fn get_string_from_strtab<'b, I: Iterator<Item = &'b BitcodeValue>>(
        parser: &BitcodeModuleParser,
        iter: &mut I,
    ) -> Result<String, DecodeError> {
        let name_offset = match iter.next() {
            Some(BitcodeValue::Value(value)) => *value as usize,
            _ => {
                return Err(DecodeError {
                    message: "Missing string offset in strtab".to_string(),
                })
            }
        };
        let name_size = match iter.next() {
            Some(BitcodeValue::Value(value)) => *value as usize,
            _ => {
                return Err(DecodeError {
                    message: "Missing string size".to_string(),
                })
            }
        };

        String::from_utf8(parser.strtab[name_offset..name_offset + name_size].to_vec()).map_err(
            |_| DecodeError {
                message: "Invalid name string encoding".to_string(),
            },
        )
    }

    fn parse_comdat_record(
        parser: &mut BitcodeModuleParser,
        record: &BitcodeRecord,
    ) -> Result<(), DecodeError> {
        let mut iter = record.values.iter();
        let name = Bitcode::get_string_from_strtab(parser, &mut iter)?;

        let selection_kind = match iter.next() {
            Some(BitcodeValue::Value(value)) => match *value {
                1 => ComdatSelectionKind::Any,
                2 => ComdatSelectionKind::ExactMatch,
                3 => ComdatSelectionKind::Largest,
                4 => ComdatSelectionKind::NoDeduplicate,
                5 => ComdatSelectionKind::SameSize,
                _ => {
                    return Err(DecodeError {
                        message: "Unknown comdat selection kind".to_string(),
                    })
                }
            },
            _ => {
                return Err(DecodeError {
                    message: "Missing comdat selection kind".to_string(),
                })
            }
        };

        parser.comdats.push(Comdat {
            name: name,
            selection_kind: selection_kind,
        });

        Ok(())
    }

    fn parse_globalvar_record(
        parser: &mut BitcodeModuleParser,
        record: &BitcodeRecord,
    ) -> Result<(), DecodeError> {
        let mut iter = record.values.iter();
        let name = Bitcode::get_string_from_strtab(parser, &mut iter)?;

        let ty = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => parser.types.get(*value as usize).unwrap().clone(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar type value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing globalvar type".to_string(),
            });
        };

        let (is_constant, address_space) = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => (
                    (*value & 1) != 0,
                    if (*value & 2) != 0 {
                        (*value >> 2) as u32
                    } else {
                        unimplemented!();
                    },
                ),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar is constant value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing globalvar is constant".to_string(),
            });
        };

        let init_id = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        Some(*value - 1)
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar initrd value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing globalvar initrd".to_string(),
            });
        };

        let linkage = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => LinkageTypes::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar linkage value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing globalvar linkage".to_string(),
            });
        };

        let alignment = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => *value,
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar alignment value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing globalvar alignment".to_string(),
            });
        } - 1;

        let section = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        Some(*value - 1)
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar section value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing globalvar section".to_string(),
            });
        };

        if let Some(_) = section {
            unimplemented!();
        }

        let visibility = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => VisibilityTypes::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar visibility value".to_string(),
                    })
                }
            }
        } else {
            VisibilityTypes::Default
        };

        let thread_local = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => ThreadLocalMode::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar thread local value".to_string(),
                    })
                }
            }
        } else {
            ThreadLocalMode::NotThreadLocal
        };

        let unnamed_addr = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => UnnamedAddr::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar unnamed addr value".to_string(),
                    })
                }
            }
        } else {
            UnnamedAddr::None
        };

        let is_externally_initialized = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => *value != 0,
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar externally initialized value".to_string(),
                    })
                }
            }
        } else {
            false
        };

        let dll_storage_class = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => DLLStorageClassTypes::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar dll storage class value".to_string(),
                    })
                }
            }
        } else {
            DLLStorageClassTypes::Default
        };

        let comdat = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => parser.comdats.get(*value as usize - 1),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid globalvar comdat value".to_string(),
                    })
                }
            }
        } else {
            None
        };

        let global_var = GlobalVariable {
            ty: ty,
            is_constant: is_constant,
            linkage: linkage,
            name: ValueName::String(name),
            initial_value: None,
            thread_local_mode: thread_local,
            address_space: Some(address_space),
            is_externally_initialized: is_externally_initialized,
            visibility: visibility,
            unnamed_address: unnamed_addr,
            dll_storage_class: dll_storage_class,
            alignment: alignment as u32,
            comdat: comdat.map(|x| x.clone()),
        };

        parser.push_value(Value::GlobalVariable(Rc::new(RefCell::new(global_var))));

        if let Some(init_id) = init_id {
            parser.global_inits.insert(parser.values.len() - 1, init_id);
        }

        Ok(())
    }

    fn parse_function_record(
        parser: &mut BitcodeModuleParser,
        record: &BitcodeRecord,
    ) -> Result<(), DecodeError> {
        let mut iter = record.values.iter();
        let name = Bitcode::get_string_from_strtab(parser, &mut iter)?;

        let ty = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if let Type::FunctionType(ty) = parser.types.get(*value as usize).unwrap() {
                        ty.clone()
                    } else {
                        return Err(DecodeError {
                            message: "Invalid function type".to_string(),
                        });
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function type value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing function type".to_string(),
            });
        };

        let calling_conv_id = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => *value as u32,
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function calling conv value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing function calling conv".to_string(),
            });
        };

        let is_proto = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => *value != 0,
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function calling conv value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing function calling conv".to_string(),
            });
        };

        let linkage = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => LinkageTypes::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function linkage value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing function linkage".to_string(),
            });
        };

        let attributes = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        parser.attributes.get(*value as usize - 1).unwrap().clone()
                    } else {
                        AttributeList { attributes: vec![] }
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function attributes value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing function attributes".to_string(),
            });
        };

        let alignment = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        Some(*value as u32 - 1)
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function alignment value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing function alignment".to_string(),
            });
        };

        let section = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        Some(*value - 1)
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function section value".to_string(),
                    })
                }
            }
        } else {
            return Err(DecodeError {
                message: "Missing function section".to_string(),
            });
        };

        if let Some(_) = section {
            unimplemented!();
        }

        let visibility = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => VisibilityTypes::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function visibility value".to_string(),
                    })
                }
            }
        } else {
            VisibilityTypes::Default
        };

        let gc = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        Some(*value - 1)
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function section value".to_string(),
                    })
                }
            }
        } else {
            None
        };

        if let Some(_) = gc {
            unimplemented!();
        }

        let unnamed_addr = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => UnnamedAddr::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function unnamed addr value".to_string(),
                    })
                }
            }
        } else {
            UnnamedAddr::None
        };

        let prologue = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        Some(*value - 1)
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function prologue value".to_string(),
                    })
                }
            }
        } else {
            None
        };

        if let Some(_) = prologue {
            unimplemented!();
        }

        let dll_storage_class = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => DLLStorageClassTypes::from_u64(*value).unwrap(),
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function dll storage class value".to_string(),
                    })
                }
            }
        } else {
            DLLStorageClassTypes::Default
        };

        let comdat = if let Some(value) = iter.next() {
            match value {
                BitcodeValue::Value(value) => {
                    if *value > 0 {
                        parser.comdats.get(*value as usize - 1).map(|x| x.clone())
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(DecodeError {
                        message: "Invalid function comdat value".to_string(),
                    })
                }
            }
        } else {
            None
        };

        let args = (0..ty.params.len())
            .map(|i| {
                Rc::new(RefCell::new(Argument {
                    ty: *ty.params[i].clone(),
                    name: ValueName::None,
                    position: i,
                }))
            })
            .collect::<Vec<Rc<RefCell<Argument>>>>();

        let func = Function {
            ty: ty,
            linkage: linkage,
            address_space: 0,
            calling_conv: calling_conv_id,
            name: ValueName::String(name),
            attributes: attributes,
            alignment: alignment,
            visibility: visibility,
            unnamed_address: unnamed_addr,
            dll_storage_class: dll_storage_class,
            comdat: comdat,
            bbs: vec![],
            bb_values: vec![],
            arguments: args.iter().map(|x| Rc::downgrade(x)).collect(),
            argument_values: args
                .iter()
                .map(|x| Rc::new(RefCell::new(Value::Argument(x.clone()))))
                .collect(),
        };

        parser.push_value(Value::Function(Rc::new(RefCell::new(func))));

        if !is_proto {
            parser.function_demanding_body_index = Some(parser.values.len() - 1);
        }

        Ok(())
    }

    fn parse_module_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        &self,
        parser: &mut BitcodeModuleParser,
        iter: &mut I,
    ) -> Result<(), DecodeError> {
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => match record.code {
                    1 => {
                        parser.version = Bitcode::parse_version_record(record)?;
                    }
                    2 => {
                        parser.triple = Bitcode::parse_triple_record(record)?;
                    }
                    3 => {
                        parser.data_layout = Bitcode::parse_data_layout_record(record)?;
                    }
                    7 => {
                        Bitcode::parse_globalvar_record(parser, record)?;
                    }
                    8 => {
                        Bitcode::parse_function_record(parser, record)?;
                    }
                    12 => {
                        Bitcode::parse_comdat_record(parser, record)?;
                    }
                    13 => {
                        parser.vst_offset = Bitcode::parse_vst_offset_record(record)?;
                    }
                    16 => {
                        parser.source_filename = Bitcode::parse_source_filename_record(record)?;
                    }
                    _ => {
                        unimplemented!();
                    }
                },
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
                            LLVMIRBlockID::Parameter => {
                                Bitcode::parse_parameter_block(parser, iter)?;
                            }
                            LLVMIRBlockID::Constant => {
                                Bitcode::parse_constant_block(parser, iter)?;
                            }
                            LLVMIRBlockID::Function => {
                                Bitcode::parse_function_block(parser, iter)?;
                            }
                            LLVMIRBlockID::ValueSymtab => {
                                Bitcode::skip_block(iter)?;
                            }
                            LLVMIRBlockID::Metadata => {
                                Bitcode::skip_block(iter)?;
                            }
                            _ => {
                                unimplemented!();
                            }
                        }
                    } else if block.blockid == 21 {
                        Bitcode::skip_block(iter)?;
                    } else if block.blockid == 22 {
                        Bitcode::skip_block(iter)?;
                    } else if block.blockid == 26 {
                        Bitcode::skip_block(iter)?;
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

    fn parse_strtab_block<'b, I: Iterator<Item = &'b BitcodeEntry>>(
        data: &Vec<u8>,
        iter: &mut I,
    ) -> Result<Vec<u8>, DecodeError> {
        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Record(record) => match record.code {
                    1 => {
                        let strtab =
                            if let Some(BitcodeValue::Blob(offset, size)) = record.values.get(0) {
                                let offset = *offset / 8 + 4;
                                data[offset..offset + *size].to_vec()
                            } else {
                                return Err(DecodeError {
                                    message: "Invalid strtab data".to_string(),
                                });
                            };
                        return Ok(strtab);
                    }
                    _ => {
                        unimplemented!();
                    }
                },
                BitcodeEntry::Block(_) => {
                    return Err(DecodeError {
                        message: "Unexpected block in parameter block".to_string(),
                    })
                }
                BitcodeEntry::EndBlock => {
                    break;
                }
                BitcodeEntry::DefineAbbrev(_) => {}
            }
        }
        Err(DecodeError {
            message: "Missing strtab data".to_string(),
        })
    }

    fn get_strtab(data: &Vec<u8>, entries: &Vec<BitcodeEntry>) -> Result<Vec<u8>, DecodeError> {
        let mut iter = entries.iter();

        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Block(block) => {
                    let id = num::traits::FromPrimitive::from_u64(block.blockid);
                    match id {
                        Some(LLVMIRBlockID::Strtab) => {
                            let strtab = Bitcode::parse_strtab_block(data, &mut iter)?;
                            return Ok(strtab);
                        }
                        _ => {}
                    }
                }
                _ => {
                    Bitcode::skip_block(&mut iter)?;
                }
            }
        }
        Err(DecodeError {
            message: "Missing strtab data".to_string(),
        })
    }

    pub fn decode(&self) -> Result<Module, DecodeError> {
        let mut reader = BitcodeReader::new(self);
        let entries = reader.read_to_end()?;

        let strtab = Bitcode::get_strtab(&self.data, &entries)?;

        let mut iter = entries.iter();

        let mut parser = BitcodeModuleParser {
            strtab: strtab,
            version: 0,
            triple: "".to_string(),
            data_layout: "".to_string(),
            source_filename: "".to_string(),
            types: vec![],
            attribute_groups: HashMap::new(),
            attributes: vec![],
            comdats: vec![],
            values: vec![],
            global_inits: HashMap::new(),
            vst_offset: 0,
            function_demanding_body_index: None,
        };

        while let Some(entry) = iter.next() {
            match entry {
                BitcodeEntry::Block(block) => {
                    let id = num::traits::FromPrimitive::from_u64(block.blockid);
                    match id {
                        Some(LLVMIRBlockID::Module) => {
                            self.parse_module_block(&mut parser, &mut iter)?;
                            let data_layout =
                                if let Some(value) = DataLayout::parse(&parser.data_layout) {
                                    value
                                } else {
                                    return Err(DecodeError {
                                        message: "Invalid data layout description".to_string(),
                                    });
                                };
                            return Ok(Module {
                                values: parser.values,
                                data_layout,
                            });
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        Err(DecodeError {
            message: "Missing module".to_string(),
        })
    }
}

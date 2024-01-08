use std::collections::HashSet;

pub struct Module {
    pub data_layout: DataLayout,
    pub values: Vec<Rc<RefCell<Value>>>,
}

impl Module {
    pub fn create_or_declar_intrinsic(
        &mut self,
        id: &str,
        types: &Vec<Type>,
    ) -> Option<ValueRef> {
        match id {
            "amdgcn_kernarg_segment_ptr" => {
                let ty = FunctionType {
                    result: Box::new(Type::PointerType(PointerType {
                        pointee_type: Some(Box::new(Type::IntegerType(IntegerType {
                            num_bits: 8,
                        }))),
                        address_space: 4,
                    })),
                    params: vec![],
                    is_vararg: false,
                };

                let func = Rc::new(RefCell::new(Function {
                    ty,
                    address_space: 0,
                    linkage: LinkageTypes::External,
                    name: "llvm.amdgcn.kernarg.segment.ptr".to_string(),
                    arguments: vec![],
                    argument_values: vec![],
                    bbs: vec![],
                    bb_values: vec![],
                    alignment: None,
                    calling_conv: 0,
                    attributes: AttributeList { attributes: vec![] },
                    visibility: VisibilityTypes::Default,
                    unnamed_address: UnnamedAddr::None,
                    dll_storage_class: DLLStorageClassTypes::Default,
                    comdat: None,
                }));
                let value = Rc::new(RefCell::new(Value::Function(func)));
                self.values.push(value.clone());
                Some(Rc::downgrade(&value))
            }
            _ => None,
        }
    }
}

pub struct PointerAlignment {
    pub abi_alignment: u32,
    pub preferred_alignment: u32,
    pub type_bit_width: u32,
    pub address_space: u32,
    pub index_bit_width: u32,
}

pub enum AlignType {
    Integer,
    Vector,
    Float,
    Aggrigate,
}

pub struct LayoutAlignment {
    pub type_bit_width: u32,
    pub abi_alignment: u32,
    pub preferred_alignment: u32,
}

pub struct DataLayout {
    pub big_endian: bool,
    pub pointers: Vec<PointerAlignment>,
    pub int_alignments: Vec<LayoutAlignment>,
    pub float_alignments: Vec<LayoutAlignment>,
    pub vector_alignments: Vec<LayoutAlignment>,
    pub struct_alignments: Vec<LayoutAlignment>,
    pub legal_int_widths: Vec<u32>,
    pub stack_natural_alignment: Option<u32>,
    pub alloca_address_space: u32,
    pub default_global_address_space: u32,
    pub non_integral_address_spaces: Vec<u32>,
}

impl DataLayout {
    pub fn parse(desc: &String) -> Option<DataLayout> {
        let mut big_endian = false;
        let mut pointers = vec![];
        let mut int_alignments = vec![];
        let mut float_alignments = vec![];
        let mut vector_alignments = vec![];
        let mut struct_alignments = vec![];
        let mut legal_int_widths = vec![];
        let mut stack_natural_alignment = None;
        let mut alloca_address_space = 0;
        let mut default_global_address_space = 0;
        let mut non_integral_address_spaces = vec![];

        for x in desc.split('-') {
            let (token, rest) = if let Some(values) = x.split_once(':') {
                values
            } else {
                (x, "")
            };

            match &token.chars().collect::<Vec<_>>()[..] {
                ['n', 'i'] => {
                    let values = rest
                        .splitn(2, ':')
                        .map(|x| x.parse::<u32>())
                        .collect::<Result<Vec<_>, _>>()
                        .ok()?;
                    
                    non_integral_address_spaces.extend(values);
                }
                ['e'] => big_endian = true,
                ['E'] => big_endian = false,
                ['p', token @ ..] => {
                    let address_space = if token.len() == 0 {
                        0
                    } else {
                        token.iter().collect::<String>().parse::<u32>().unwrap()
                    };

                    let values = rest
                        .splitn(2, ':')
                        .map(|x| x.parse::<u32>())
                        .collect::<Result<Vec<_>, _>>()
                        .ok()?;

                    let (mem_size, abi_alignment, pref_alignment, index_size) = match values[..] {
                        [mem_size, abi_alignment] => {
                            (mem_size, abi_alignment, abi_alignment, mem_size)
                        }
                        [mem_size, abi_alignment, pref_alignment] => {
                            (mem_size, abi_alignment, pref_alignment, mem_size)
                        }
                        [mem_size, abi_alignment, pref_alignment, index_size] => {
                            (mem_size, abi_alignment, pref_alignment, index_size)
                        }
                        _ => panic!(),
                    };
                    pointers.push(PointerAlignment {
                        address_space: address_space,
                        type_bit_width: mem_size,
                        abi_alignment: abi_alignment / 8,
                        preferred_alignment: pref_alignment / 8,
                        index_bit_width: index_size,
                    });
                }
                [ty @ ('i' | 'v' | 'f' | 'a'), token @ ..] => {
                    let bit_size = if token.len() == 0 {
                        0
                    } else {
                        token.iter().collect::<String>().parse::<u32>().unwrap()
                    };

                    let values = rest
                        .splitn(2, ':')
                        .map(|x| x.parse::<u32>())
                        .collect::<Result<Vec<_>, _>>()
                        .ok()?;

                    let (abi_alignment, pref_alignment) = match values[..] {
                        [abi_alignment] => (abi_alignment, abi_alignment),
                        [abi_alignment, pref_alignment] => (abi_alignment, pref_alignment),
                        _ => panic!(),
                    };

                    let alignments = match ty {
                        'i' => &mut int_alignments,
                        'v' => &mut vector_alignments,
                        'f' => &mut float_alignments,
                        'a' => &mut struct_alignments,
                        _ => panic!(),
                    };
                    alignments.push(LayoutAlignment {
                        type_bit_width: bit_size,
                        abi_alignment: abi_alignment / 8,
                        preferred_alignment: pref_alignment / 8,
                    });
                }
                ['n', token @ ..] => {
                    let value = token.iter().collect::<String>().parse::<u32>().ok()?;
                    
                    legal_int_widths.push(value);

                    let values = rest
                        .splitn(2, ':')
                        .map(|x| x.parse::<u32>())
                        .collect::<Result<Vec<_>, _>>()
                        .ok()?;
                    
                    legal_int_widths.extend(values);
                }
                ['S', token @ ..] => {
                    let value = token.iter().collect::<String>().parse::<u32>().ok()?;
                    
                    stack_natural_alignment = Some(value);
                }
                ['A', token @ ..] => {
                    alloca_address_space = token.iter().collect::<String>().parse::<u32>().ok()?;
                }
                ['G', token @ ..] => {
                    default_global_address_space = token.iter().collect::<String>().parse::<u32>().ok()?;
                }
                _ => unimplemented!(),
            }
        }
        Some(DataLayout {
            big_endian,
            pointers,
            int_alignments,
            vector_alignments,
            float_alignments,
            struct_alignments,
            legal_int_widths,
            stack_natural_alignment,
            alloca_address_space,
            default_global_address_space,
            non_integral_address_spaces,
        })
    }

    pub fn get_abi_alignment_by_type(&self, ty: &Type) -> Option<u32> {
        match ty {
            Type::PointerType(ty) => {
                let info = &self.pointers.iter().find(|x| x.address_space == ty.address_space)?;
                Some(info.abi_alignment)
            }
            _ => None,
        }
    }

    pub fn get_type_size_in_bits(&self, ty: &Type) -> Option<u32> {
        match ty {
            Type::PointerType(ty) => {
                let info = &self.pointers.iter().find(|x| x.address_space == ty.address_space)?;
                Some(info.type_bit_width)
            }
            _ => None,
        }
    }

    pub fn get_type_alloc_size(&self, ty: &Type) -> Option<u32> {
        let storage_size = (self.get_type_size_in_bits(ty)? + 7) / 8;
        let alignment = self.get_abi_alignment_by_type(ty)?;
        Some(((storage_size + alignment - 1) / alignment) * alignment)
    }
}

#[derive(Clone, Debug)]
pub struct PointerType {
    pub address_space: u32,
    pub pointee_type: Option<Box<Type>>,
}

impl PointerType {
    pub fn new(address_space: u32, pointee_type: Type) -> Self {
        PointerType {
            address_space: address_space,
            pointee_type: Some(Box::new(pointee_type)),
        }
    }
    pub fn new_with_address_space(address_space: u32) -> Self {
        PointerType {
            address_space: address_space,
            pointee_type: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct IntegerType {
    pub num_bits: u32,
}

impl IntegerType {
    pub fn new(num_bits: u32) -> Self {
        IntegerType { num_bits: num_bits }
    }
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub name: String,
    pub elements: Vec<Box<Type>>,
}

impl StructType {
    pub fn new(name: String) -> Self {
        StructType {
            name: name,
            elements: vec![],
        }
    }
    pub fn new_with_elements(name: String, elements: Vec<Box<Type>>) -> Self {
        StructType {
            name: name,
            elements: elements,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub result: Box<Type>,
    pub params: Vec<Box<Type>>,
    pub is_vararg: bool,
}

impl FunctionType {
    pub fn new(result: Box<Type>, params: Vec<Box<Type>>, is_vararg: bool) -> Self {
        FunctionType {
            result: result,
            params: params,
            is_vararg: is_vararg,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    VoidType,
    FloatType,
    DoubleType,
    LabelType,
    IntegerType(IntegerType),
    MetadataType,
    PointerType(PointerType),
    StructType(StructType),
    FunctionType(FunctionType),
}

impl Type {
    pub fn is_floating_point(&self) -> bool {
        match self {
            Self::FloatType | Self::DoubleType => true,
            _ => false,
        }
    }
    pub fn is_aggregate(&self) -> bool {
        match self {
            Self::StructType(_) => true,
            _ => false,
        }
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
#[repr(u32)]
pub enum AttributeKind {
    AllocAlign = 1,
    AllocatedPointer = 2,
    AlwaysInline = 3,
    Builtin = 4,
    Cold = 5,
    Convergent = 6,
    DisableSanitizerInstrumentation = 7,
    FnRetThunkExtern = 8,
    Hot = 9,
    ImmArg = 10,
    InReg = 11,
    InlineHint = 12,
    JumpTable = 13,
    MinSize = 14,
    MustProgress = 15,
    Naked = 16,
    Nest = 17,
    NoAlias = 18,
    NoBuiltin = 19,
    NoCallback = 20,
    NoCapture = 21,
    NoCfCheck = 22,
    NoDuplicate = 23,
    NoFree = 24,
    NoImplicitFloat = 25,
    NoInline = 26,
    NoMerge = 27,
    NoProfile = 28,
    NoRecurse = 29,
    NoRedZone = 30,
    NoReturn = 31,
    NoSanitizeBounds = 32,
    NoSanitizeCoverage = 33,
    NoSync = 34,
    NoUndef = 35,
    NoUnwind = 36,
    NonLazyBind = 37,
    NonNull = 38,
    NullPointerIsValid = 39,
    OptForFuzzing = 40,
    OptimizeForSize = 41,
    OptimizeNone = 42,
    PresplitCoroutine = 43,
    ReadNone = 44,
    ReadOnly = 45,
    Returned = 46,
    ReturnsTwice = 47,
    SExt = 48,
    SafeStack = 49,
    SanitizeAddress = 50,
    SanitizeHWAddress = 51,
    SanitizeMemTag = 52,
    SanitizeMemory = 53,
    SanitizeThread = 54,
    ShadowCallStack = 55,
    SkipProfile = 56,
    Speculatable = 57,
    SpeculativeLoadHardening = 58,
    StackProtect = 59,
    StackProtectReq = 60,
    StackProtectStrong = 61,
    StrictFP = 62,
    SwiftAsync = 63,
    SwiftError = 64,
    SwiftSelf = 65,
    WillReturn = 66,
    WriteOnly = 67,
    ZExt = 68,
    ByRef = 69,
    ByVal = 70,
    ElementType = 71,
    InAlloca = 72,
    Preallocated = 73,
    StructRet = 74,
    Alignment = 75,
    AllocKind = 76,
    AllocSize = 77,
    Dereferenceable = 78,
    DereferenceableOrNull = 79,
    Memory = 80,
    NoFPClass = 81,
    StackAlignment = 82,
    UWTable = 83,
    VScaleRange = 84,
}

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub enum Attribute {
    Enum(AttributeKind),
    Int(AttributeKind, u64),
    String(String, String),
}

#[derive(Clone, Debug)]
pub struct AttributeList {
    pub attributes: Vec<HashSet<Attribute>>,
}

impl AttributeList {
    pub fn merge(attributes_list: Vec<Self>) -> Option<Self> {
        let size = attributes_list.iter().map(|x| x.attributes.len()).max()?;

        let mut merged_attributes = AttributeList {
            attributes: vec![HashSet::new(); size],
        };

        for attributes in &attributes_list {
            for (merged_attributes, attributes) in merged_attributes
                .attributes
                .iter_mut()
                .zip(attributes.attributes[0..attributes.attributes.len() - 1].iter())
            {
                merged_attributes.extend(attributes.clone());
            }
            merged_attributes
                .attributes
                .last_mut()
                .and_then(|merged_attributes| {
                    attributes
                        .attributes
                        .last()
                        .map(|attributes| merged_attributes.extend(attributes.clone()))
                });
        }

        Some(merged_attributes)
    }
}

#[derive(Clone, Debug)]
pub enum ComdatSelectionKind {
    Any,
    ExactMatch,
    Largest,
    NoDeduplicate,
    SameSize,
}

#[derive(Clone, Debug)]
pub struct Comdat {
    pub name: String,
    pub selection_kind: ComdatSelectionKind,
}

#[derive(Clone, Debug)]
pub enum LinkageTypes {
    External,
    AvailableExternally,
    LinkOnceAny,
    LinkOnceODR,
    WeakAny,
    WeakODR,
    Appending,
    Internal,
    Private,
    ExternalWeak,
    Common,
}

#[derive(Clone, Debug)]
pub struct GlobalVariable {
    pub ty: Type,
    pub is_constant: bool,
    pub linkage: LinkageTypes,
    pub name: String,
    pub initial_value: Option<Constant>,
    pub thread_local_mode: ThreadLocalMode,
    pub address_space: Option<u32>,
    pub is_externally_initialized: bool,
    pub visibility: VisibilityTypes,
    pub unnamed_address: UnnamedAddr,
    pub dll_storage_class: DLLStorageClassTypes,
    pub alignment: u32,
    pub comdat: Option<Comdat>,
}

#[derive(Clone, Debug)]
pub enum VisibilityTypes {
    Default,
    Hidden,
    Protected,
}

#[derive(Clone, Debug)]
pub enum ThreadLocalMode {
    NotThreadLocal,
    GeneralDynamicTLSModel,
    LocalDynamicTLSModel,
    InitialExecTLSModel,
    LocalExecTLSModel,
}

#[derive(Clone, Debug)]
pub enum UnnamedAddr {
    None,
    Local,
    Global,
}

#[derive(Clone, Debug)]
pub enum DLLStorageClassTypes {
    Default,
    DLLImport,
    DLLExport,
}

#[derive(Clone, Debug)]
pub enum Constant {}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub insts: Vec<Weak<RefCell<Inst>>>,
    pub inst_values: Vec<Rc<RefCell<Value>>>,
}

impl BasicBlock {
    pub fn insert_instruction(&mut self, index: usize, inst: Inst) -> (ValueRef, Weak<RefCell<Inst>>) {
        let inst = Rc::new(RefCell::new(inst));
        let inst_ref = Rc::downgrade(&inst);
        self.insts.insert(index, Rc::downgrade(&inst));
        let value = Rc::new(RefCell::new(Value::Instruction(inst)));
        let value_ref = Rc::downgrade(&value);
        self.inst_values.insert(index, value);
        (value_ref, inst_ref)
    }
    
    pub fn push_instruction(&mut self, inst: Inst) -> (ValueRef, Weak<RefCell<Inst>>) {
        self.insert_instruction(self.insts.len(), inst)
    }

    pub fn position(&self, value: &ValueRef) -> Option<usize> {
        self.inst_values.iter().position(|x| x.as_ptr() == value.upgrade().unwrap().as_ptr())
    }

    pub fn create_call(
        &mut self,
        position: Option<&ValueRef>,
        func: ValueRef,
        args: Vec<ValueRef>,
        name: String,
    ) -> Option<ValueRef> {
        let function_type = func.upgrade().unwrap().borrow().ty().clone();

        let position = if let Some(position) = position {
            self.position(position)?
        } else {
            0
        };

        let (value, _) = self.insert_instruction(position, Inst::CallInst(CallInst {
            args,
            function_type,
            callee: func,
            attributes: AttributeList { attributes: vec![] },
        }));

        Some(value)
    }

    pub fn create_get_element_ptr_inbounds_index1(
        &mut self,
        position: Option<&ValueRef>,
        ty: Type,
        ptr: ValueRef,
        index: ValueRef,
        name: String,
    ) -> Option<ValueRef> {
        let position = if let Some(position) = position {
            self.position(position)?
        } else {
            0
        };

        let (value, _) = self.insert_instruction(position, Inst::GetElementPtrInst(GetElementPtrInst {
            inbounds: true,
            base_ptr: ptr,
            ty: ty,
            indexes: vec![index],
        }));

        Some(value)
    }

    pub fn create_aligned_load(
        &mut self,
        position: Option<&ValueRef>,
        ty: Type,
        ptr: ValueRef,
        alignment: u32,
        is_volatile: bool,
        name: String,
    ) -> Option<ValueRef> {
        let position = if let Some(position) = position {
            self.position(position)?
        } else {
            0
        };

        let (value, _) = self.insert_instruction(position, Inst::LoadInst(LoadInst {
            ptr,
            ty,
            alignment,
            is_volatile,
            name,
        }));

        Some(value)
    }
}

#[derive(Clone, Debug)]
pub struct Argument {
    pub ty: Type,
    pub name: String,
    pub position: usize,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub ty: FunctionType,
    pub linkage: LinkageTypes,
    pub address_space: u32,
    pub calling_conv: u32,
    pub name: String,
    pub attributes: AttributeList,
    pub alignment: Option<u32>,
    pub visibility: VisibilityTypes,
    pub unnamed_address: UnnamedAddr,
    pub dll_storage_class: DLLStorageClassTypes,
    pub comdat: Option<Comdat>,
    pub bbs: Vec<BasicBlockRef>,
    pub bb_values: Vec<Rc<RefCell<Value>>>,
    pub arguments: Vec<Weak<RefCell<Argument>>>,
    pub argument_values: Vec<Rc<RefCell<Value>>>,
}

use num_bigint::BigUint;

#[derive(Clone, Debug)]
pub struct ConstantInt {
    pub ty: Type,
    pub value: BigUint,
}

#[derive(Clone, Debug)]
pub struct Undef {
    pub ty: Type,
}

use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Clone, Debug)]
pub enum Value {
    GlobalVariable(Rc<RefCell<GlobalVariable>>),
    Function(Rc<RefCell<Function>>),
    Undef(Rc<RefCell<Undef>>),
    ConstantInt(Rc<RefCell<ConstantInt>>),
    Argument(Rc<RefCell<Argument>>),
    Instruction(Rc<RefCell<Inst>>),
    BasicBlock(Rc<RefCell<BasicBlock>>),
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Self::Instruction(value) => match &*value.borrow() {
                Inst::CallInst(inst) => match &inst.function_type {
                    Type::FunctionType(ty) => *ty.result.clone(),
                    _ => unimplemented!(),
                },
                Inst::GetElementPtrInst(inst) => inst.base_ptr.upgrade().unwrap().borrow().ty(),
                Inst::BinOpInst(inst) => inst.lhs.upgrade().unwrap().borrow().ty(),
                Inst::LoadInst(inst) => inst.ty.clone(),
                Inst::FCmpInst(_) => Type::IntegerType(IntegerType { num_bits: 1 }),
                Inst::ICmpInst(_) => Type::IntegerType(IntegerType { num_bits: 1 }),
                Inst::CastInst(inst) => inst.result_ty.clone(),
                Inst::BranchInst(_) => Type::VoidType,
                Inst::PhiInst(inst) => inst.ty.clone(),
                _ => unimplemented!(),
            },
            Self::GlobalVariable(value) => value.borrow().ty.clone(),
            Self::ConstantInt(value) => value.borrow().ty.clone(),
            Self::Argument(value) => value.borrow().ty.clone(),
            Self::Function(value) => (*value.borrow().ty.result).clone(),
            _ => unimplemented!(),
        }
    }

    pub fn name(&self) -> String {
        match self {
            Self::Instruction(value) => match &*value.borrow() {
                Inst::CallInst(inst) => todo!(),
                Inst::GetElementPtrInst(inst) => todo!(),
                Inst::BinOpInst(inst) => todo!(),
                Inst::LoadInst(inst) => inst.name.clone(),
                Inst::FCmpInst(_) => todo!(),
                Inst::ICmpInst(_) => todo!(),
                Inst::CastInst(inst) => todo!(),
                Inst::BranchInst(_) => todo!(),
                Inst::PhiInst(inst) => todo!(),
                _ => unimplemented!(),
            },
            Self::GlobalVariable(value) => value.borrow().name.clone(),
            Self::ConstantInt(value) => todo!(),
            Self::Argument(value) => value.borrow().name.clone(),
            _ => unimplemented!(),
        }
    }
}

pub type BasicBlockRef = Weak<RefCell<BasicBlock>>;
pub type ValueRef = Weak<RefCell<Value>>;

impl Inst {
    pub fn is_terminator(&self) -> bool {
        match self {
            Self::BranchInst(_) => true,
            Self::ReturnInst(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOpcode {
    Add,
    FAdd,
    Sub,
    FSub,
    Mul,
    FMul,
    UDiv,
    SDiv,
    FDiv,
    URem,
    SRem,
    FRem,
    Shl,
    LShr,
    AShr,
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug)]
pub struct BinOpInst {
    pub opcode: BinaryOpcode,
    pub lhs: ValueRef,
    pub rhs: ValueRef,
}

#[derive(Clone, Debug)]
pub enum ICmpPredicate {
    EQ,
    NE,
    UGT,
    UGE,
    ULT,
    ULE,
    SGT,
    SGE,
    SLT,
    SLE,
}

#[derive(Clone, Debug)]
pub enum FCmpPredicate {
    FALSE,
    OEQ,
    OGT,
    OGE,
    OLT,
    OLE,
    ONE,
    ORD,
    UNO,
    UEQ,
    UGT,
    UGE,
    ULT,
    ULE,
    UNE,
    TRUE,
}

#[derive(Clone, Debug)]
pub struct ICmpInst {
    pub predicate: ICmpPredicate,
    pub lhs: ValueRef,
    pub rhs: ValueRef,
}

#[derive(Clone, Debug)]
pub struct FCmpInst {
    pub predicate: FCmpPredicate,
    pub lhs: ValueRef,
    pub rhs: ValueRef,
}

#[derive(Clone, Debug)]
pub struct LoadInst {
    pub ty: Type,
    pub ptr: ValueRef,
    pub name: String,
    pub is_volatile: bool,
    pub alignment: u32,
}

#[derive(Clone, Debug)]
pub struct StoreInst {
    pub ptr: ValueRef,
    pub value: ValueRef,
    pub name: String,
    pub is_volatile: bool,
    pub alignment: u32,
}

#[derive(Clone, Debug)]
pub struct BranchInst {
    pub true_condition: BasicBlockRef,
    pub false_condition: Option<(BasicBlockRef, ValueRef)>,
}

#[derive(Clone, Debug)]
pub struct ReturnInst {
    pub return_value: Option<ValueRef>,
}

#[derive(Clone, Debug)]
pub struct PhiInst {
    pub ty: Type,
    pub incoming: Vec<(BasicBlockRef, ValueRef)>,
}

#[derive(Clone, Debug)]
pub enum CastOpcode {
    Trunc,
    ZExt,
    SExt,
    FPToUI,
    FPToSI,
    UIToFP,
    SIToFP,
    FPTrunc,
    FPExt,
    PtrToInt,
    IntToPtr,
    BitCast,
    AddrSpaceCast,
}

#[derive(Clone, Debug)]
pub struct CastInst {
    pub value: ValueRef,
    pub opcode: CastOpcode,
    pub result_ty: Type,
}

#[derive(Clone, Debug)]
pub struct CallInst {
    pub function_type: Type,
    pub callee: ValueRef,
    pub args: Vec<ValueRef>,
    pub attributes: AttributeList,
}

#[derive(Clone, Debug)]
pub struct GetElementPtrInst {
    pub ty: Type,
    pub base_ptr: ValueRef,
    pub indexes: Vec<ValueRef>,
    pub inbounds: bool,
}

#[derive(Clone, Debug)]
pub enum Inst {
    BinOpInst(BinOpInst),
    ICmpInst(ICmpInst),
    FCmpInst(FCmpInst),
    BranchInst(BranchInst),
    ReturnInst(ReturnInst),
    PhiInst(PhiInst),
    LoadInst(LoadInst),
    StoreInst(StoreInst),
    CastInst(CastInst),
    CallInst(CallInst),
    GetElementPtrInst(GetElementPtrInst),
}

use bitflags::bitflags;

bitflags! {
    pub struct FastMathFlags: u32 {
        const AllowReassoc = (1 << 0);
        const NoNaNs = (1 << 1);
        const NoInfs = (1 << 2);
        const NoSignedZeros = (1 << 3);
        const AllowReciprocal = (1 << 4);
        const AllowContract = (1 << 5);
        const ApproxFunc = (1 << 6);
    }
}

impl Function {
    pub fn arg_size(&self) -> usize {
        self.ty.params.len()
    }
}

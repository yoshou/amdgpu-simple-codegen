use std::collections::HashSet;

pub struct Module {
    pub values: Vec<Value>
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
    pub insts: Vec<Rc<RefCell<Inst>>>,
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
    pub name: String,
    pub attributes: AttributeList,
    pub alignment: Option<u32>,
    pub visibility: VisibilityTypes,
    pub unnamed_address: UnnamedAddr,
    pub dll_storage_class: DLLStorageClassTypes,
    pub comdat: Option<Comdat>,
    pub bbs: Vec<Rc<RefCell<BasicBlock>>>,
    pub arguments: Vec<Rc<RefCell<Argument>>>,
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
                Inst::GetElementPtrInst(inst) => inst.base_ptr.ty(),
                Inst::BinOpInst(inst) => inst.lhs.ty(),
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
            _ => unimplemented!(),
        }
    }
}

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
    pub lhs: Value,
    pub rhs: Value,
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
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Clone, Debug)]
pub struct FCmpInst {
    pub predicate: FCmpPredicate,
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Clone, Debug)]
pub struct LoadInst {
    pub ty: Type,
    pub ptr: Value,
    pub name: String,
    pub is_volatile: bool,
    pub alignment: u32,
}

#[derive(Clone, Debug)]
pub struct StoreInst {
    pub ptr: Value,
    pub val: Value,
    pub name: String,
    pub is_volatile: bool,
    pub alignment: u32,
}

#[derive(Clone, Debug)]
pub struct BranchInst {
    pub true_condition: Weak<RefCell<BasicBlock>>,
    pub false_condition: Option<(Weak<RefCell<BasicBlock>>, Value)>,
}

#[derive(Clone, Debug)]
pub struct ReturnInst {
    pub return_value: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct PhiInst {
    pub ty: Type,
    pub incoming: Vec<(Weak<RefCell<BasicBlock>>, Value)>,
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
    pub value: Value,
    pub opcode: CastOpcode,
    pub result_ty: Type,
}

#[derive(Clone, Debug)]
pub struct CallInst {
    pub function_type: Type,
    pub callee: Value,
    pub args: Vec<Value>,
    pub attributes: AttributeList,
}

#[derive(Clone, Debug)]
pub struct GetElementPtrInst {
    pub ty: Type,
    pub base_ptr: Value,
    pub indexes: Vec<Value>,
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

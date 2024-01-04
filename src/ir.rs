use std::collections::HashSet;

pub struct Module {
    
}

#[derive(Clone, Debug)]
pub struct PointerType {
    pub address_space: u32,
    pub pointee_type: Option<Box<Type>>
}

impl PointerType {
    pub fn new(address_space: u32, pointee_type: Type) -> Self {
        PointerType { address_space: address_space, pointee_type: Some(Box::new(pointee_type)) }
    }
    pub fn new_with_address_space(address_space: u32) -> Self {
        PointerType { address_space: address_space, pointee_type: None }
    }
}

#[derive(Clone, Debug)]
pub struct IntegerType {
    pub num_bits: u32
}

impl IntegerType {
    pub fn new(num_bits: u32) -> Self {
        IntegerType { num_bits: num_bits }
    }
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub name: String,
    pub elements: Vec<Box<Type>>
}

impl StructType {
    pub fn new(name: String) -> Self {
        StructType { name: name, elements: vec![] }
    }
    pub fn new_with_elements(name: String, elements: Vec<Box<Type>>) -> Self {
        StructType { name: name, elements: elements }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub result: Box<Type>,
    pub params: Vec<Box<Type>>,
    pub is_vararg: bool
}

impl FunctionType {
    pub fn new(result: Box<Type>, params: Vec<Box<Type>>, is_vararg: bool) -> Self {
        FunctionType { result: result, params: params, is_vararg: is_vararg }
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
    pub attributes: Vec<HashSet<Attribute>>
}

impl AttributeList {
    pub fn merge(attributes_list: Vec<Self>) -> Option<Self> {
        let size = attributes_list.iter().map(|x| x.attributes.len()).max()?;

        let mut merged_attributes = AttributeList { attributes: vec![HashSet::new(); size] };

        for attributes in &attributes_list {
            for (merged_attributes, attributes) in merged_attributes.attributes.iter_mut().zip(attributes.attributes[0..attributes.attributes.len() - 1].iter()) {
                merged_attributes.extend(attributes.clone());
            }
            merged_attributes.attributes.last_mut()
                .and_then(|merged_attributes| attributes.attributes.last().map(|attributes| merged_attributes.extend(attributes.clone())));
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
    Protected
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
pub enum Constant {
}

#[derive(Clone, Debug)]
pub enum Value {
    GlobalVariable(GlobalVariable)
}

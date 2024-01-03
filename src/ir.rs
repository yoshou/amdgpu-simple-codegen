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
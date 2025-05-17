use crate::expression::Env;
use crate::unique::Unique;
use crate::expression::Expression;
use std::sync::Arc;
use crate::types::Type;


//generally should be held in an arc
#[derive(Debug, PartialEq)]
pub struct Function {
    pub params: Arc<[usize]>,
    pub in_types: Arc<[Type]>,
    pub out_type : Arc<Type>,
    pub body:   Arc<Expression>,
    pub env:    Env,                     // captured bindings
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Func(Arc<Function>),
    Flag(Unique),
    Tuple(Arc<[Value]>),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::NUM,
            Value::Flag(f) => Type::Basic(*f),
            Value::Tuple(a) => Type::Tuple(a.iter().map(Value::get_type).collect()),
            Value::Func(f) => Type::Func(f.in_types.clone(),f.out_type.clone()),
        }
    }
}
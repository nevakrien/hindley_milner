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
    pub out_type : Type,
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
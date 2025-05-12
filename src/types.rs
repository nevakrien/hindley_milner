use std::sync::Arc;
use crate::unique::Unique;


#[derive(Debug,PartialEq,Clone)]
pub enum Type {
	Basic(Unique),
	Func(Arc<[Type]>,Arc<Type>),
	Tuple(Arc<[Type]>),
}

impl Type {
	pub const NUM: Type = Type::Basic(Unique::NUM);
}
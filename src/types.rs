use im::HashMap;
use std::sync::Arc;
use crate::unique::Unique;


#[derive(Debug,PartialEq,Clone)]
pub enum Type {
	Generic(usize),

	Basic(Unique),
	Func(Arc<[Type]>,Arc<Type>),
	Tuple(Arc<[Type]>),
}

pub type GenericMap = HashMap<usize,Type>;

/// resolves generic double refrences including cycles
/// by the end all generics in the map would not overlap
/// 
pub fn resolve_mappings(map:&GenericMap) -> (GenericMap,bool) {
	let mut changed = false;
	let mut ans =GenericMap::new();

	for k in map.keys() {
		if ans.get(k).is_some() {
			continue;
		}

		let mut keys = vec![*k];
		let mut cur = k;

		while let Some(v) = map.get(cur) {
			match v {
				Type::Generic(i) => {
					match ans.get(i) {
						None => {
							changed = true;
							keys.push(*i);
							cur = i;
							continue;
						}
						Some(_)=>{}
					}
				},
				_=> {}
			};

			break;
		}

		let value = &map[keys.last().unwrap()];

		for k in keys.into_iter() {
			ans.insert(k,value.clone());
		}
	}
	(ans,changed)
}

impl Type {
	pub const NUM: Type = Type::Basic(Unique::NUM);

	pub fn resolve_generics(&self,bounded_gens:&GenericMap) -> Self {
		match self {
			Type::Basic(b)=>Type::Basic(*b),

			Type::Generic(id) => {
				if let Some(ans) = bounded_gens.get(id) {
					ans.clone()
				} else {
					Type::Generic(*id)
				}
			},
			Type::Tuple(arr) => Type::Tuple(
				arr.iter()
				.map(|x| x.resolve_generics(bounded_gens))
				.collect()
			),

			Type::Func(arr,out) => Type::Func(
				arr.iter()
				.map(|x| x.resolve_generics(bounded_gens))
				.collect(),

				out.resolve_generics(bounded_gens)
				.into()
			),

		}
		
	}

	pub fn was_used_as(&self,req:&Type,bounded_gens:GenericMap)-> Result<(Self,GenericMap,bool),()>{

		todo!()
	}

}

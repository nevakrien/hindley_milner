use crate::expression::DefExp;
use crate::expression::Expression;
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

	pub fn was_used_as(&self,req:&Self,bounded_gens:GenericMap)-> Result<(Self,Self,GenericMap,bool),()>{

		todo!()
	}

}


pub type DefMap = HashMap<usize,Arc<DefExp>>;

pub fn update_expr(exp:Expression,bounded_gens:GenericMap,def_map:DefMap) -> Result<(Expression,GenericMap,DefMap,bool),()> {
	match exp {
		Expression::Ref(i, ref t) => {
			//limit the definined var by the refrence
			let mut defexp = def_map[&i].clone();
			let tdef = &defexp.var_annotation;
			let (tdef,t,bounded_gens,b) = tdef.was_used_as(t,bounded_gens)?;
			
			//update all our metadata
			Arc::make_mut(&mut defexp).var_annotation=tdef;
			let def_map = def_map.update(i,defexp);
			let me = Expression::Ref(i,t);
			Ok((me,bounded_gens,def_map,b))
		},
		Expression::Def(mut def) => {
			let mut changed = false;
			
			let (new_val,bounded_gens,_,b) = update_expr(def.var_val.clone(),bounded_gens.clone(),def_map.clone())?;
			
			changed |= b;	
			Arc::make_mut(&mut def).var_val = new_val;		

			let def_map = def_map.update(def.var,def.clone());
			let (new_ret,bounded_gens,_,b) = update_expr(def.ret.clone(),bounded_gens,def_map.clone())?;

			changed |= b;	
			Arc::make_mut(&mut def).ret = new_ret;		

			Ok((Expression::Def(def),bounded_gens,def_map,changed))

		}
		Expression::Lit(_) => Ok((exp,bounded_gens,def_map,false)),

	    _ => todo!(),
	}
}

pub fn find_typing(exp:Expression,bounded_gens:GenericMap) -> Result<Expression,()> {
	let mut changed = false;
	let (mut bounded_gens,b) = resolve_mappings(&bounded_gens);
	if b {
		changed = true;
		let temp = bounded_gens.clone();

		for (_k,v) in bounded_gens.iter_mut(){
			*v = v.resolve_generics(&temp);
		}
	}

	let (exp,bounded_gens,_,b) = update_expr(exp,bounded_gens,DefMap::new())?;
	changed |= b;
	if changed {
		find_typing(exp,bounded_gens)
	}else{
		Ok(exp)
	}
}



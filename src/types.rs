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

    /// Unify `self` with `req`, extending `bounded_gens`.
    ///
    /// Returns `(new_self, new_req, new_map, changed)`.
    /// *`changed`* is `true` iff the generic-binding map grew or either
    ///   returned type differs from its input.
    pub fn was_used_as(
        &self,
        req: &Self,
        bounded_gens: GenericMap,
    ) -> Result<(Self, Self, GenericMap, bool), ()> {
        use Type::*;

        match (self, req) {
            // ───── concrete basics ─────────────────────────────────────────
            (Basic(a), Basic(b)) => {
                if a == b {
                    Ok((self.clone(), req.clone(), bounded_gens, false))
                } else {
                    Err(())
                }
            }

            // ───── self is a generic ───────────────────────────────────────
            (Generic(id), other) => {
                if let Some(bound) = bounded_gens.clone().get(id) {
                    // The generic was already bound – unify the binding with `other`.
                    let (lhs, rhs, new_map, changed) =
                        bound.was_used_as(other, bounded_gens)?;
                    Ok((lhs, rhs, new_map, changed))
                } else if other == &Generic(*id) {
                    // Generic used as itself – nothing changes.
                    Ok((self.clone(), req.clone(), bounded_gens, false))
                } else {
                    // New binding:  id ↦ other
                    let new_map = bounded_gens.update(*id, other.clone());
                    Ok((other.clone(), other.clone(), new_map, true))
                }
            }

            // ───── req is a generic (mirror of previous arm) ───────────────
            (other, Generic(id)) => {
                if let Some(bound) = bounded_gens.clone().get(id) {
                    let (lhs, rhs, new_map, changed) =
                        other.was_used_as(bound, bounded_gens)?;
                    Ok((lhs, rhs, new_map, changed))
                } else {
                    let new_map = bounded_gens.update(*id, other.clone());
                    Ok((other.clone(), other.clone(), new_map, true))
                }
            }

            // ───── tuples – arity must match ───────────────────────────────
            (Tuple(xs), Tuple(ys)) => {
                if xs.len() != ys.len() {
                    return Err(());
                }

                let mut map   = bounded_gens;
                let mut chg   = false;
                let mut left  = Vec::with_capacity(xs.len());
                let mut right = Vec::with_capacity(ys.len());

                for (x, y) in xs.iter().zip(ys.iter()) {
                    let (nx, ny, m, c) = x.was_used_as(y, map)?;
                    map = m;
                    chg |= c;
                    left.push(nx);
                    right.push(ny);
                }

                Ok((
                    Tuple(left.into()),
                    Tuple(right.into()),
                    map,
                    chg,
                ))
            }

            // ───── functions – parameter-count must match ──────────────────
            (Func(px, rx), Func(py, ry)) => {
                if px.len() != py.len() {
                    return Err(());
                }

                let mut map   = bounded_gens;
                let mut chg   = false;
                let mut lp    = Vec::with_capacity(px.len());
                let mut rp    = Vec::with_capacity(py.len());

                // parameters
                for (a, b) in px.iter().zip(py.iter()) {
                    let (na, nb, m, c) = a.was_used_as(b, map)?;
                    map = m;
                    chg |= c;
                    lp.push(na);
                    rp.push(nb);
                }

                // result types
                let (nr1, nr2, map, c2) = rx.as_ref().was_used_as(ry.as_ref(), map)?;
                chg |= c2;

                Ok((
                    Func(lp.into(), Arc::new(nr1.clone())),
                    Func(rp.into(), Arc::new(nr2.clone())),
                    map,
                    chg,
                ))
            }

            // ───── any other shape combination is a hard mismatch ──────────
            _ => Err(()),
        }
    }


}


pub type DefMap = HashMap<usize,Arc<DefExp>>;

pub fn update_expr(exp:Expression,mut bounded_gens:GenericMap,mut def_map:DefMap) -> Result<(Expression,GenericMap,DefMap,bool),()> {
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
			
			let (_x,t,bounded_gens,b) = def.var_val.get_type().was_used_as(&def.var_annotation,bounded_gens)?;
			changed |= b;
			Arc::make_mut(&mut def).var_annotation = t;		


			//check on the val discarding the inner of the creator
			let (new_val,bounded_gens,_,b) = update_expr(def.var_val.clone(),bounded_gens.clone(),def_map.clone())?;
			
			changed |= b;	
			Arc::make_mut(&mut def).var_val = new_val;		

			let def_map = def_map.update(def.var,def.clone());
			let (new_ret,bounded_gens,_,b) = update_expr(def.ret.clone(),bounded_gens,def_map.clone())?;

			changed |= b;	
			Arc::make_mut(&mut def).ret = new_ret;		

			Ok((Expression::Def(def),bounded_gens,def_map,changed))

		}
		Expression::Lit(_) | Expression::Builtin(_) => Ok((exp,bounded_gens,def_map,false)),

		Expression::Tuple(arr) => {
			let mut changed = false;
			let mut v = Vec::with_capacity(arr.len());
			for e in arr.into_iter() {
				let (exp,gens,defs,b) = update_expr(e.clone(),bounded_gens,def_map)?;
				v.push(exp);
				bounded_gens = gens;
				def_map = defs;
				changed |= b;
			}

			Ok((Expression::Tuple(v.into()),bounded_gens,def_map,changed))
		},
		Expression::Lambda(_)  => todo!(),

		Expression::Call(out,args , out_anot) => {
			let mut changed = false;

			let ( out, out_args,out_anot) = match *out {
				Expression::Builtin(f) => {
					if f.in_types.len()!=args.len() {
						return Err(())
					}

					let (new_ty,_,bg,b) =out_anot.was_used_as(&f.out_type,bounded_gens)?;
					bounded_gens = bg;
					changed |= b;
					( out , f.in_types.clone(),new_ty)
				},
				Expression::Lambda(ref _l) => todo!(),
				_ => return  Err(())
			};

			let mut new_args = Vec::with_capacity(args.len());
			for (a,t) in args.iter().zip(out_args.iter()) {
				let (a,bg,_,b) = update_expr(a.clone(),bounded_gens,def_map.clone())?;
				changed |= b;

				let (_,_,bg,b) = t.was_used_as(&a.get_type(),bg)?;
				changed |= b;
				
				bounded_gens = bg;
				new_args.push(a);
			}


			let args : Arc<[Expression]> = new_args.into_iter().collect();

			let new_me = Expression::Call(out,args,out_anot);
			Ok((new_me,bounded_gens,def_map,changed))
		}

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




#[cfg(test)]
mod typing_tests {
    use crate::expression::PLUS;
use super::*;                // pull in everything we just defined
    use crate::expression::{Expression as E, DefExp};
    use crate::value::Value;
    use im::HashMap as PMap;     // only for readability in this module

    // ─── quick helpers ────────────────────────────────────────────────────
    fn lit(n: i64) -> E { E::Lit(Value::Int(n)) }

    // let-style reference with an explicit type
    fn var(id: usize, ty: Type) -> E { E::Ref(id, ty) }

    // ─── 1. single generic unified with an Int literal ───────────────────
    //
    //   let x : T = 5 in x
    //
    // Should produce    let x : NUM = 5 in x
    //
    #[test]
    fn find_typing_scalar_generic() {
        let expr = E::Def(Arc::new(DefExp {
            var:            0,
            var_val:        lit(5),
            var_annotation: Type::Generic(0),
            ret:            var(0, Type::Generic(1)),
        }));

        let typed = find_typing(expr, GenericMap::new()).expect("typing succeeds");

        if let E::Def(d) = typed {
            assert_eq!(d.var_annotation, Type::NUM);            // T ↦ NUM

            if let E::Ref(_, ref t) = d.ret {
                assert_eq!(t, &Type::NUM);                      // body updated
            } else {
                panic!("expected Ref in Def body");
            }
        } else {
            panic!("expected outer Def expression");
        }
    }

    // ─── 2. tuple with two generics unified to (Int, Int) ────────────────
    //
    //   let p : (A, B) = (1, 2) in p
    //
    // Should produce    let p : (NUM, NUM) = (1, 2) in p
    //
    #[test]
    fn find_typing_tuple_generics() {
        let tuple_gen = Type::Tuple(Arc::new([Type::Generic(0), Type::Generic(1)]));

        let expr = E::Def(Arc::new(DefExp {
            var:            1,
            var_val:        E::Tuple(Arc::new([lit(1), lit(2)])),
            var_annotation: tuple_gen.clone(),
            ret:            var(1, tuple_gen),
        }));

        let typed = find_typing(expr, GenericMap::new()).expect("typing succeeds");

        if let E::Def(d) = typed {
            let expected = Type::Tuple(Arc::new([Type::NUM, Type::NUM]));

            assert_eq!(d.var_annotation, expected);             // (A,B) ↦ (NUM,NUM)

            if let E::Ref(_, t) = &d.ret {
                assert_eq!(*t, expected);                       // body updated
            } else {
                panic!("expected Ref in Def body");
            }
        } else {
            panic!("expected outer Def expression");
        }
    }

     // ─── 3. annotation/value mismatch should error ─────────────────────────
    #[test]
    fn find_typing_annotation_value_mismatch() {
        // let x : (A, B) = 5 in x
        let tuple_gen = Type::Tuple(Arc::new([Type::Generic(0), Type::Generic(1)]));
        let expr = E::Def(Arc::new(DefExp {
            var:            0,
            var_val:        lit(5),
            var_annotation: tuple_gen,
            ret:            var(0, Type::Generic(0)),
        }));

        // we expect a type‐error because a single Int (NUM) cannot unify with a 2‐tuple
        find_typing(expr, GenericMap::new()).unwrap_err();
    }

    #[test]
    fn simple_call_check() {
    	let expr = Expression::Call(
    		 Expression::Builtin(&PLUS).into(),
    		Arc::new([Expression::Lit(Value::Int(1)),Expression::Lit(Value::Int(1))])
    		,Type::Generic(2)
    	);
    	let typed = find_typing(expr, GenericMap::new()).expect("typing succeeds");
    	assert_eq!(Type::NUM,typed.get_type());

    }
}

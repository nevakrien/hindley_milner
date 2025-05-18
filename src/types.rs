use std::ffi::c_void;
// use im::HashSet;
use crate::expression::LambdaExp;
use crate::expression::DefExp;
use crate::expression::Expression;
use im::HashMap;
use std::sync::Arc;
use crate::unique::Unique;


#[derive(Debug,PartialEq,Clone,Hash,Eq)]
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
	println!("calling resolve_mappings");
	

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
					println!("in resolve loop");
					if i==k {
						break;
					}
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
    ) -> Result<(Self, Self, GenericMap, bool), ()>{
		self._was_used_as(req,bounded_gens,&mut std::collections::HashSet::new())
    }

	


    fn _was_used_as(
        &self,
        req: &Self,
        bounded_gens: GenericMap,
        seen:&mut std::collections::HashSet<(Type,Type)>,
    ) -> Result<(Self, Self, GenericMap, bool), ()> {
        use Type::*;

		println!("calling _was_used_as");

		// note that this is only a half measure 
		// however it is nicer than it seems:
		// 
		// 		1. if these 2 types apear in multiple places 
		//         then the chagned from the first
		// 		   would trigger a rerun later
		//
		//      2. if the pair is wrong the error was returned earlier
		if !seen.insert((self.clone(),req.clone())) {
			return Ok((self.clone(),req.clone(),bounded_gens,false))
		}

        match (self, req) {
            // ───── concrete basics ─────────────────────────────────────────
            (Basic(a), Basic(b)) => {
				println!("basic basic");

                if a == b {
                    Ok((self.clone(), req.clone(), bounded_gens, false))
                } else {
                    Err(())
                }
            }

            // ───── self is a generic ───────────────────────────────────────
            (Generic(id), other) => {
				println!("left gen");

                if let Some(bound) = bounded_gens.clone().get(id) {
                    // The generic was already bound – unify the binding with `other`.
                    let (lhs, rhs, new_map, changed) =
                        bound._was_used_as(other, bounded_gens,seen)?;
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
            	println!("right gen");

                if let Some(bound) = bounded_gens.clone().get(id) {
                    let (lhs, rhs, new_map, changed) =
                        other._was_used_as(bound, bounded_gens,seen)?;
                    Ok((lhs, rhs, new_map, changed))
                } else {
                    let new_map = bounded_gens.update(*id, other.clone());
                    Ok((other.clone(), other.clone(), new_map, true))
                }
            }

            // ───── tuples – arity must match ───────────────────────────────
            (Tuple(xs), Tuple(ys)) => {
            	println!("tuples");
                

                if xs.len() != ys.len() {
                    return Err(());
                }

                let mut map   = bounded_gens;
                let mut chg   = false;
                let mut left  = Vec::with_capacity(xs.len());
                let mut right = Vec::with_capacity(ys.len());

                for (x, y) in xs.iter().zip(ys.iter()) {
                    let (nx, ny, m, c) = x._was_used_as(y, map,seen)?;
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
            	println!("funcs");

                if px.len() != py.len() {
                    return Err(());
                }

                let mut map   = bounded_gens;
                let mut chg   = false;
                let mut lp    = Vec::with_capacity(px.len());
                let mut rp    = Vec::with_capacity(py.len());

                // parameters
                for (a, b) in px.iter().zip(py.iter()) {
                    let (na, nb, m, c) = a._was_used_as(b, map,seen)?;
                    map = m;
                    chg |= c;
                    lp.push(na);
                    rp.push(nb);
                }

                // result types
                let (nr1, nr2, map, c2) = rx.as_ref()._was_used_as(ry.as_ref(), map,seen)?;
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


#[derive(Debug,Clone,PartialEq)]
pub enum Var {
	Defed(Arc<DefExp>),
	Arg(Type)
} 

impl Var {
	pub fn get_type_ref(&self) -> &Type {
		match self {
			Var::Defed(defexp) => &defexp.var_annotation,
			Var::Arg(t) => t
		}
	}

	pub fn set_type(&mut self,x:Type){
		match self {
			Var::Defed(defexp) => Arc::make_mut(defexp).var_annotation=x,
			Var::Arg(t) => *t=x
		}
	}
}

pub type DefMap = HashMap<usize,Var>;
// pub type Seen = std::collections::HashSet<*const c_void>;


pub fn update_expr(exp:Expression,mut bounded_gens:GenericMap,mut def_map:DefMap) -> Result<(Expression,GenericMap,DefMap,bool),()> {
	println!("calling update_expr");

	match exp {
		Expression::Ref(i, ref t) => {
			println!("ref update_expr");

			//limit the definined var by the refrence
			let mut var = def_map[&i].clone();
			let tdef = var.get_type_ref();
			let (tdef,t,bounded_gens,b) = tdef.was_used_as(t,bounded_gens)?;
			
			//update all our metadata
			var.set_type(tdef);
			let def_map = def_map.update(i,var);
			let me = Expression::Ref(i,t);
			Ok((me,bounded_gens,def_map,b))
		},
		Expression::Def(mut def) => {
			println!("def update_expr");

			let mut changed = false;
			
			let (_x,t,bounded_gens,b) = def.var_val.get_type().was_used_as(&def.var_annotation,bounded_gens)?;
			changed |= b;
			Arc::make_mut(&mut def).var_annotation = t;		


			//check on the val discarding the inner of the creator
			let (new_val,bounded_gens,_,b) = update_expr(def.var_val.clone(),bounded_gens,def_map.clone())?;
			
			changed |= b;	
			Arc::make_mut(&mut def).var_val = new_val;		

			let def_map = def_map.update(def.var,Var::Defed(def.clone()));
			let (new_ret,bounded_gens,_,b) = update_expr(def.ret.clone(),bounded_gens,def_map.clone())?;

			changed |= b;	
			Arc::make_mut(&mut def).ret = new_ret;		

			Ok((Expression::Def(def),bounded_gens,def_map,changed))

		}
		Expression::Lit(_) | Expression::Builtin(_) => {
			println!("terminal update_expr");
			
			Ok((exp,bounded_gens,def_map,false))
		},

		Expression::Tuple(arr) => {
			println!("tuple update_expr");


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
		Expression::Lambda(l)  => {
			println!("calling lambda resolve update_expr");

			let mut changed = false;

			let mut inner_map = def_map.clone();
			for (i,t) in l.params.iter().zip(l.in_types.iter()){
				inner_map=inner_map.update(*i,Var::Arg(t.clone()));
			}

			let (body,bounded_gens,inner_map,b) = update_expr((*l.body).clone(),bounded_gens,inner_map)?;
			changed |= b;

			let in_types = l.params.iter().map(|i| inner_map[i].get_type_ref().clone()).collect();

			let (out_type,_,bounded_gens,b) = l.out_type.was_used_as(&body.get_type(),bounded_gens)?;
			changed |= b;
			let new_me = Expression::Lambda(LambdaExp{
				params:l.params.clone(),
				out_type: out_type.into(),
				in_types,
				body: body.into()
			}.into());

			Ok((new_me,bounded_gens,def_map,changed))
		},

		Expression::Call(out,args , out_anot) => {
			println!("resolving call update_expr");
			
			let mut changed = false;

			let (out,mut bounded_gens,_,b) = update_expr((*out).clone(),bounded_gens,def_map.clone())?;
			changed|= b;


			let mut new_args = Vec::with_capacity(args.len());

			for a in args.iter() {
				let (a,bg,_,b) = update_expr(a.clone(),bounded_gens,def_map.clone())?;
				changed |= b;

				bounded_gens = bg;
				new_args.push(a);
			}

			let arg_types = new_args.iter().map(Expression::get_type).collect();
			let sig = Type::Func(arg_types,out_anot.into());

			let (sig,_,bounded_gens,b) = sig.was_used_as(&out.get_type(),bounded_gens)?;
			changed|=b;
			let out_anot = match sig {
				Type::Func(_,t) => (*t).clone(),
				_=> return Err(())
			};

			let args : Arc<[Expression]> = new_args.into_iter().collect();

			let new_me = Expression::Call(out.into(),args,out_anot);
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
		// println!("after run {:?}",exp);
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

        // ─── 4. creation of a stand-alone lambda should infer its out_type ───
    #[test]
    fn lambda_creation_type_inference() {
        // λx. 5  with param annotated G0 and out annotated G1
        let lam = E::Lambda(Arc::new(LambdaExp {
            params:   Arc::new([0]),
            in_types: Arc::new([Type::Generic(0)]),
            out_type: Type::Generic(1).into(),
            body:     Arc::new(lit(5)),
        }));

        let typed = find_typing(lam, GenericMap::new())
            .expect("lambda type‐checks");

        if let E::Lambda(le) = typed {
            // parameter stays G0 (no clue what it must be)
            assert_eq!(le.in_types[0], Type::Generic(0));
            // but the literal body forces out_type ↦ NUM
            assert_eq!(le.out_type, Type::NUM.into());
        } else {
            panic!("expected a Lambda node after typing");
        }
    }

    // ─── 5. nested defs forcing two rounds of inference ───────────────────
    #[test]
    fn nested_defs_unify_generics_in_two_passes() {
        // let x : G0 = 5 in
        //   let y : G1 = x in
        //     y
        let inner = E::Def(Arc::new(DefExp {
            var:            1,
            var_val:        var(0, Type::Generic(0)),    // x : G0
            var_annotation: Type::Generic(1),             // y : G1
            ret:            var(1, Type::Generic(1)),
        }));
        let outer = E::Def(Arc::new(DefExp {
            var:            0,
            var_val:        lit(5),
            var_annotation: Type::Generic(2),             // x : G0
            ret:            inner,
        }));

        let typed = find_typing(outer, GenericMap::new())
            .expect("nested defs type‐check");

        // Outer def’s annotation G0 ↦ NUM
        if let E::Def(d1) = typed {
            assert_eq!(d1.var_annotation, Type::NUM);

            // Inner def also unified: G1 ↦ NUM
            if let E::Def(d2) = &d1.ret {
                assert_eq!(d2.var_annotation, Type::NUM);

                // And the final "y" in the body is now Ref(_, NUM)
                if let E::Ref(_, t) = &d2.ret {
                    assert_eq!(t, &Type::NUM);
                } else {
                    panic!("expected Ref in inner Def body");
                }
            } else {
                panic!("expected inner Def expression");
            }
        } else {
            panic!("expected outer Def expression");
        }
    }


    use crate::expression::LambdaExp;  // make sure this is in scope
// …

    // ─── 6. bind a lambda to a var, return it, then call it ───────────────
    #[test]
    fn lambda_returned_as_value_and_called() {
        // let f : G0 = λx:G1. x in f(9)
        let lam = E::Lambda(Arc::new(LambdaExp {
            params:   Arc::new([0]),
            in_types: Arc::new([Type::Generic(1)]),
            out_type: Type::Generic(2).into(),
            body:     Arc::new(var(0, Type::Generic(1))),
        }));

        let expr = E::Def(Arc::new(DefExp {
            var:            0,
            var_val:        lam,
            var_annotation: Type::Generic(0),
            ret: E::Call(
                Arc::new(E::Ref(0, Type::Generic(0))),
                Arc::new([lit(9)]),
                Type::Generic(3),
            ),
        }));

        let typed = find_typing(expr, GenericMap::new()).expect("typing succeeds");

        // f’s annotation G0 → Func([NUM], NUM)
        if let E::Def(d) = typed {
            let func_ty = Type::Func(Arc::new([Type::NUM]), Arc::new(Type::NUM));
            assert_eq!(d.var_annotation, func_ty);

            // the Call’s result must unify to NUM
            if let E::Call(_, _, out_t) = &d.ret {
                assert_eq!(*	out_t, Type::NUM);
            } else {
                panic!("expected a Call node");
            }
        } else {
            panic!("expected outer Def");
        }
    }

    // ─── 7. higher-order lambda and nested calls ───────────────────────────
    #[test]
    fn higher_order_lambda_and_nested_call() {
        // let make_adder : G0 = λx:G1. λy:G2. x + y in
        //   let add_two : G3 = make_adder(2) in
        //     add_two(3)
        let make_adder = E::Lambda(Arc::new(LambdaExp {
            params:   Arc::new([0]),
            in_types: Arc::new([Type::Generic(1)]),
            out_type: Type::Generic(2).into(),
            body: Arc::new(E::Lambda(Arc::new(LambdaExp {
                params:   Arc::new([1]),
                in_types: Arc::new([Type::Generic(3)]),
                out_type: Type::Generic(4).into(),
                body:     Arc::new(E::Call(
                    Arc::new(E::Builtin(&PLUS)),
                    Arc::new([ var(0, Type::Generic(1)), var(1, Type::Generic(3)) ]),
                    Type::Generic(4),
                )),
            }))),
        }));

        // inner def: add_two = make_adder(2); result = add_two(3)
        let inner = E::Def(Arc::new(DefExp {
            var:            1,
            var_val:        E::Call(
                                Arc::new(E::Ref(0, Type::Generic(0))),
                                Arc::new([lit(2)]),
                                Type::Generic(5),
                            ),
            var_annotation: Type::Generic(6),
            ret:            E::Call(
                                Arc::new(E::Ref(1, Type::Generic(6))),
                                Arc::new([lit(3)]),
                                Type::Generic(7),
                            ),
        }));

        let outer = E::Def(Arc::new(DefExp {
            var:            0,
            var_val:        make_adder,
            var_annotation: Type::Generic(0),
            ret:            inner,
        }));

        let typed = find_typing(outer, GenericMap::new())
            .expect("nested higher-order call type-checks");

        // make_adder : G0 ↦ Func([NUM], Func([NUM], NUM))
        if let E::Def(d0) = typed {
            let inner_fn = Type::Func(Arc::new([Type::NUM]), Arc::new(Type::NUM));
            let expected = Type::Func(Arc::new([Type::NUM]), Arc::new(inner_fn.clone()));
            assert_eq!(d0.var_annotation, expected);

            // inner Def’s body should finally be a Call whose type is NUM
            if let E::Def(d1) = &d0.ret {
                assert_eq!(d1.ret.get_type(), Type::NUM);
            } else {
                panic!("expected inner Def");
            }
        } else {
            panic!("expected outer Def");
        }
    }

    #[test]
fn tuple_second_field_mismatch_is_silently_accepted() {
    use crate::expression::{Expression as E, DefExp};
    use crate::value::Value;

    // reusable helpers ----------------------------------------------------
    fn lit(n: i64) -> E { E::Lit(Value::Int(n)) }

    // (NUM , NUM)
    let inner_tuple_ty = Type::Tuple(Arc::new([Type::NUM, Type::NUM]));

    // value part ----------------------------------------------------------
    //
    //   v_val  = ( (1,2) , 3 )
    //                └──┬──┘   ^^^^^   *scalar* (NUM)  ←─ should not match
    //                   │                     (NUM , NUM)
    //                   └──────── first element is a 2-tuple
    //
    let v_val = E::Tuple(Arc::new([
        E::Tuple(Arc::new([lit(1), lit(2)])),   // (1,2)         : (NUM , NUM)
        lit(3),                                 // 3              : NUM  ❌
    ]));

    // annotation part -----------------------------------------------------
    //
    //   v_ann  = ( (NUM , NUM) , (NUM , NUM) )
    //                       ^^^^^^^^^^^^^^^^^   2-tuple expected – mismatch
    //
    let v_ann = Type::Tuple(Arc::new([
        inner_tuple_ty.clone(),      // ok – matches (1,2)
        inner_tuple_ty.clone(),      // ✘ – conflicts with the scalar ‘3’
    ]));

    // full:   let v : v_ann = v_val in v
    let expr = E::Def(Arc::new(DefExp {
        var:            0,
        var_val:        v_val,
        var_annotation: v_ann.clone(),
        ret:            E::Ref(0, v_ann),
    }));

    // A correct checker must *reject* this.
    //
    // Today, with the “seen-is-HashSet<Type>” guard, it is silently accepted
    // because:
    //   1. The first tuple field forces the type-checker to compare
    //      (NUM , NUM)   with   (NUM , NUM)            → both identical,
    //      hence **NUM** and the **inner tuple type** are *individually*
    //      inserted into `seen`.
    //   2. When the second field is reached the checker must compare
    //          self = NUM             with         req = (NUM , NUM)
    //      but *both* sides are now already in `seen`, so the short-circuit
    //      triggers and the mismatch is **skipped**.
    //
    // If the short-circuit were sound, this call would return Err(()).
    if find_typing(expr, GenericMap::new()).is_ok() {
        panic!("type-checker accepted a (NUM) ≠ (NUM,NUM) mismatch");
    }
}



}

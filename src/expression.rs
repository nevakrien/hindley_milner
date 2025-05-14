//! expression.rs  –  minimal, type-annotated AST + small interpreter
//! (Rust 2024 edition)

use im::HashMap as PMap;
use std::sync::Arc;

use crate::types::Type;
use crate::unique::Unique;

// ───── AST nodes  ──────────────────────────────────────────────────────────

#[derive(Debug, PartialEq)]
#[derive(Clone)]
pub struct DefExp {
    pub var:  usize,
    pub var_val: Expression,
    pub var_annotation: Type,
    pub ret:  Expression,
}

/// A λ-expression with explicit type annotations.
#[derive(Debug, PartialEq)]
pub struct LambdaExp {
    pub params:    Arc<[usize]>,
    pub in_types:  Arc<[Type]>,
    pub out_type:  Type,
    pub body:      Arc<Expression>,
}

/// A built-in primitive (e.g. “+”) that runs a Rust closure.
#[derive(Debug,PartialEq)]
pub struct Builtin {
    pub name:      &'static str,
    pub in_types:  Arc<[Type]>,
    pub out_type:  Type,
    pub exec:      fn(&[Value]) -> Option<Value>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    /// the usize here is both the ID of the var AND its the ID used in the generic
    Ref(usize,Type),
    Def(Arc<DefExp>),
    Lambda(Arc<LambdaExp>),
    Builtin(&'static Builtin),                    // behaves like a first-class fn
    Call(Arc<Expression>, Arc<[Expression]>),
    Lit(Value),
    Tuple(Arc<[Expression]>),
}

//AI slope used just as a testing thing for proof of concept

// ───── Persistent environment  ─────────────────────────────────────────────
pub type Env = PMap<usize, Value>;

fn extend(env: &Env, k: usize, v: Value) -> Env {
    env.update(k, v)          // O(log n) update, shared tails
}

pub fn empty_env() -> Env { Env::new() }

// ───── Values & Functions  ────────────────────────────────────────────────

#[derive(Debug, PartialEq)]
pub struct Function {
    pub params:    Arc<[usize]>,
    pub in_types:  Arc<[Type]>,
    pub out_type:  Type,
    pub body:      Arc<Expression>,
    pub env:       Env,                           // captured variables
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Func(Arc<Function>),
    Flag(Unique),
    Tuple(Arc<[Value]>),
}

// ───── Interpreter  ───────────────────────────────────────────────────────

impl Expression {
    pub fn eval(&self, env: &Env) -> Option<Value> {
        match self {
            // literals & variables ----------------------------------------------------------
            Expression::Lit(v)        => Some(v.clone()),
            Expression::Ref(i, _)        => env.get(i).cloned(),

            // tuple -------------------------------------------------------------------------
            Expression::Tuple(es) => {
                let mut out = Vec::with_capacity(es.len());
                for e in es.iter() { out.push(e.eval(env)?); }
                Some(Value::Tuple(out.into()))
            }

            // let / Def ---------------------------------------------------------------------
            Expression::Def(d) => {
                let val  = d.var_val.eval(env)?;
                let env1 = extend(env, d.var, val);
                d.ret.eval(&env1)
            }

            // λ capture ----------------------------------------------------------------------
            Expression::Lambda(le) => {
                Some(Value::Func(Arc::new(Function {
                    params:   le.params.clone(),
                    in_types: le.in_types.clone(),
                    out_type: le.out_type.clone(),
                    body:     le.body.clone(),
                    env:      env.clone(),               // O(1) share
                })))
            }

            // built-ins evaluate to themselves (so you can store them in env if you like)
            Expression::Builtin(b) => Some(Value::Func(Arc::new(Function {
                params:   (0..b.in_types.len()).map(|i| i).collect::<Vec<_>>().into(),
                in_types: b.in_types.clone(),
                out_type: b.out_type.clone(),
                body:     Arc::new(Expression::Builtin(b)), // sentinel, never executed
                env:      empty_env(),
            }))),

            // call --------------------------------------------------------------------------
            Expression::Call(f_expr, arg_exprs) => {
                // ---- Handle Builtin directly (fast-path, avoids alloc of Func wrapper) ----
                if let Expression::Builtin(b) = f_expr.as_ref() {
                    let mut args_v = Vec::with_capacity(arg_exprs.len());
                    for a in arg_exprs.iter() { args_v.push(a.eval(env)?); }
                    return (b.exec)(&args_v);
                }

                // ---- Otherwise normal user-defined function ------------------------------
                let fv = f_expr.eval(env)?;
                if let Value::Func(f) = fv {
                    if f.params.len() != arg_exprs.len() { return None; }
                    let mut call_env = f.env.clone();
                    for (p, a_expr) in f.params.iter().zip(arg_exprs.iter()) {
                        call_env = extend(&call_env, *p, a_expr.eval(env)?);
                    }
                    f.body.eval(&call_env)
                } else {
                    None
                }
            }
        }
    }
}

// ───── A tiny built-in “+” primitive for the test suite  ──────────────────
use once_cell::sync::Lazy;

fn plus_exec(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Int(a), Value::Int(b)] => Some(Value::Int(a + b)),
        _                              => None,
    }
}

pub static PLUS: Lazy<Builtin> = Lazy::new(|| Builtin {
    name:      "+",
    in_types:  Arc::new([Type::NUM, Type::NUM]),
    out_type:  Type::NUM,
    exec:      plus_exec,
});

// ───── Tests  ──────────────────────────────────────────────────────────────
#[cfg(test)]
mod tests {
    use super::*;
    use Expression as E;
    use Value::*;

    fn lit(n: i64) -> E { E::Lit(Int(n)) }
    fn var(i: usize) -> E { E::Ref(i, Type::NUM) }

    fn lambda1(param: usize, body: E) -> E {
        E::Lambda(Arc::new(LambdaExp {
            params: Arc::new([param]),
            in_types: Arc::new([Type::NUM]),
            out_type: Type::NUM,
            body: Arc::new(body),
        }))
    }

    fn call(fn_e: E, args: Arc<[E]>) -> E {
        E::Call(Arc::new(fn_e), args)
    }

    // ------------- individual expression kinds -------------------------------------------

    #[test] fn lit_works()  { assert_eq!(lit(7).eval(&empty_env()), Some(Int(7))); }

    #[test]
    fn ref_works() {
        let env = extend(&empty_env(), 1, Int(99));
        assert_eq!(var(1).eval(&env), Some(Int(99)));
    }

    #[test]
    fn def_works() {
        let expr = E::Def(Arc::new(DefExp {
            var: 0,
            var_val: lit(5),
            var_annotation: Type::Generic(100),
            ret: var(0),
        }));
        assert_eq!(expr.eval(&empty_env()), Some(Int(5)));
    }

    #[test]
    fn tuple_works() {
        let expr = E::Tuple(Arc::new([lit(1), lit(2)]));
        assert_eq!(
            expr.eval(&empty_env()),
            Some(Tuple(Arc::new([Int(1), Int(2)])))
        );
    }

    // ------------- lambda, builtin and closure capture -----------------------------------

    #[test]
    fn lambda_and_call() {
        let expr = call(lambda1(0, var(0)), vec![lit(42)].into());
        assert_eq!(expr.eval(&empty_env()), Some(Int(42)));
    }

    #[test]
    fn closure_capture_plus_builtin() {
        // let x = 10 in ((λy. +(x,y)) 5)
        let plus_node = Expression::Builtin(&PLUS);

        let add_x = lambda1(
            1,
            call(plus_node.clone(), vec![var(0), var(1)].into()), // x + y
        );

        let program = E::Def(Arc::new(DefExp {
            var: 0,                      // x
            var_val: lit(10),
            var_annotation: Type::Generic(100),
            ret: call(add_x, vec![lit(5)].into()),
        }));

        assert_eq!(program.eval(&empty_env()), Some(Int(15)));
    }
}

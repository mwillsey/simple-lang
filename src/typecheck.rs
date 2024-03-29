use crate::syntax::ast::*;

type Env = im::HashMap<Name, RcType>;

impl Type {
    pub fn subtype(&self, other: &Type) -> Result<(), String> {
        if self == other {
            Ok(())
        } else {
            Err("types not equal".into())
        }
    }
}

impl Expr {
    pub fn check_type(&self, prog: &Program, env: &Env, other: &Type) -> Result<(), String> {
        match self {
            Expr::Lambda { params, body } => match other {
                Type::Fn(param_types, ret_type) => {
                    let env = env.clone() + bind_multiple_types(params, &param_types)?;
                    body.check_type(prog, &env, ret_type)
                }
                _ => Err("can't check lambda against non function".into()),
            },
            _ => self.infer_type(prog, env)?.subtype(other),
        }
    }

    pub fn infer_type(&self, prog: &Program, env: &Env) -> Result<RcType, String> {
        match self {
            Expr::Var(name) => match env.get(name) {
                Some(t) => Ok(t.clone()),
                None => Err("can't find var".into()),
            },
            Expr::Literal(lit) => match lit {
                Literal::Int(_) => Ok(Type::Int.into()),
                Literal::Float(_) => Ok(Type::Float.into()),
            },
            Expr::Bop { bop, e1, e2 } => {
                let _ = bop;
                let t1 = e1.infer_type(prog, env)?;
                let t2 = e2.infer_type(prog, env)?;
                match (t1.as_ref(), t2.as_ref()) {
                    (Type::Int, Type::Int) => Ok(Type::Int.into()),
                    (Type::Float, Type::Float) => Ok(Type::Float.into()),
                    _ => Err("binop error".into()),
                }
            }
            Expr::Block(block) => block.infer_type(prog, env.clone()),
            Expr::Lambda { params, body } => {
                let mut param_types = Vec::with_capacity(params.len());
                for p in params {
                    match p.get_annotated_type() {
                        Some(t) => param_types.push(t),
                        None => return Err("need annotations on lambda".into()),
                    }
                }

                let env = env.clone() + bind_multiple_types(params, &param_types)?;
                let ret = body.infer_type(prog, &env)?;
                Ok(Type::Fn(param_types, ret).into())
            }
            Expr::Call { func, args } => match func.infer_type(prog, env)?.as_ref() {
                Type::Fn(param_types, ret) => {
                    if param_types.len() != args.len() {
                        return Err("arg len mismatch".into());
                    }
                    for (pt, a) in param_types.iter().zip(args) {
                        a.check_type(prog, env, pt)?
                    }
                    Ok(ret.clone())
                }
                _ => Err("calling a non function".into()),
            },
            Expr::Tuple(exprs) => {
                let mut types = Vec::with_capacity(exprs.len());
                for e in exprs {
                    types.push(e.infer_type(prog, env)?);
                }
                Ok(Type::Tuple(types).into())
            }
            Expr::Struct { name, fields } => {
                let st = prog.get_struct(name)?;

                for fname in fields.keys() {
                    if !st.fields.contains_key(fname) {
                        return Err("invalid key".into());
                    }
                }

                for fname in st.fields.keys() {
                    if !fields.contains_key(fname) {
                        return Err("missing key".into());
                    }
                }

                for (fname, fexpr) in fields.iter() {
                    let expected_type = st.fields.get(fname).unwrap();
                    fexpr.check_type(prog, env, &expected_type)?;
                }

                Ok(Type::Named(name.clone()).into())
            }
            Expr::FieldAccess { .. } => unimplemented!(),
        }
    }
}

impl Block {
    pub fn infer_type(&self, prog: &Program, mut env: Env) -> Result<RcType, String> {
        for stmt in &self.stmts {
            stmt.bind_type(prog, &mut env)?;
        }
        self.expr.infer_type(prog, &env)
    }
}

impl Stmt {
    pub fn bind_type(&self, prog: &Program, env: &mut Env) -> Result<(), String> {
        match self {
            Stmt::Let { pat, expr } => {
                let typ = match pat.get_annotated_type() {
                    None => expr.infer_type(prog, env)?,
                    Some(t) => {
                        // let was fully annotated, so we can check
                        // the expression instead of inferring it
                        expr.check_type(prog, env, &t)?;
                        t
                    }
                };
                pat.bind_type(&typ).map(|en| env.extend(en))
            }
        }
    }
}

fn bind_multiple_types(patterns: &[RcPattern], types: &[RcType]) -> Result<Env, String> {
    if patterns.len() != types.len() {
        return Err("binding of unequal length".into());
    }

    let mut env = Env::new();

    for (p, t) in patterns.iter().zip(types) {
        for (name, typ) in p.bind_type(t)? {
            if let Some(_) = env.insert(name.clone(), typ) {
                return Err(format!("duplicate binding of {:?}", name));
            }
        }
    }

    Ok(env)
}

impl Pattern {
    fn bind_type(&self, typ: &RcType) -> Result<Env, String> {
        match self {
            Pattern::Wildcard => Ok(Env::new()),
            Pattern::Binder(name) => Ok(Env::unit(name.clone(), typ.clone())),
            Pattern::Literal(lit) => match (lit, typ.as_ref()) {
                (Literal::Int(_), Type::Int) => Ok(Env::new()),
                (Literal::Float(_), Type::Float) => Ok(Env::new()),
                _ => Err("failed to bind to literal".into()),
            },
            Pattern::Annotated(pat, ann_type) => {
                ann_type.subtype(ann_type)?;
                pat.bind_type(ann_type)
            }
            Pattern::Tuple(pats) => match typ.as_ref() {
                Type::Tuple(types) => bind_multiple_types(pats, &types),
                _ => Err("can't bind non-tuple to tuple".into()),
            },
        }
    }

    fn get_annotated_type(&self) -> Option<RcType> {
        match self {
            Pattern::Wildcard => None,
            Pattern::Binder(_) => None,
            Pattern::Literal(lit) => match lit {
                Literal::Int(_) => Some(Type::Int.into()),
                Literal::Float(_) => Some(Type::Float.into()),
            },
            Pattern::Annotated(_pat, typ) => Some(typ.clone()),
            Pattern::Tuple(pats) => {
                let types: Option<Vec<RcType>> =
                    pats.iter().map(|p| p.get_annotated_type()).collect();
                types.map(|ts| Type::Tuple(ts).into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::{expr, pattern, program, typ};

    macro_rules! infer {
        ($prog:expr, $e:expr) => {{
            let e = expr!($e);
            let env = Env::new();
            e.infer_type($prog, &env)
        }};
        ($e:expr) => {{
            let prog = Program { decls: Vec::new() };
            infer!(&prog, $e)
        }};
    }

    #[test]
    fn test_infer_type() {
        assert_eq!(
            infer!({
                let x = 1;
                let square = |x: Int| x * x;
                square(x)
            }),
            Ok(typ!(Int))
        );

        assert_eq!(
            infer!(|x: Int| |y: Int| x * x),
            Ok(typ!(Fn (Int) -> Fn (Int) -> Int))
        );
    }

    #[test]
    fn test_infer_type_with_program() {
        let prog = program!(
            struct Foo {
                x: Int,
                y: Float,
            }
        );

        assert_eq!(infer!(&prog, Foo { x: 1, y: 1.0 }), Ok(typ!(Foo)));

        assert!(infer!(&prog, Foo { x: 1.0, y: 1.0 }).is_err());
    }

    #[test]
    fn test_infer_lambda() {
        // bare lambda should not be inferred
        assert!(infer!(|x| x + 1).is_err());

        // bound lambda without annotation should also not be inferred
        assert!(infer!({
            let inc = |x| x + 1;
            inc
        })
        .is_err());

        // bound lambda with annotation should be inferred
        assert_eq!(
            infer!({
                let inc: Fn(Int) -> Int = |x| x + 1;
                inc
            }),
            Ok(typ!(Fn(Int) -> Int))
        );

        // lambda at call site can also be inferred
        assert_eq!(
            infer!({
                let do_twice = |f: Fn(Int) -> Int, x: Int| f(f(x));
                do_twice(|x| x + 1, 1)
            }),
            Ok(typ!(Int))
        );
    }

    #[test]
    fn test_get_annotated_type() {
        let pat = pattern! { 1 };
        let typ = typ! { Int };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { x };
        assert_eq!(pat.get_annotated_type(), None);

        let pat = pattern! { ((x, y): (Int, Float)) };
        let typ = typ! { (Int, Float) };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { (x: Int, _: Float) };
        let typ = typ! { (Int, Float) };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { (3, y: Float) };
        let typ = typ! { (Int, Float) };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { (x: Int, y: Float, _) };
        assert_eq!(pat.get_annotated_type(), None);
    }
}

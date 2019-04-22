use std::rc::Rc;

use im::{HashMap as Map, HashSet as Set};

pub type RcType = Rc<Type>;
pub type RcPattern = Rc<Pattern>;
pub type RcExpr = Rc<Expr>;
pub type RcDecl = Rc<Decl>;

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Float,
    Tuple(Vec<RcType>),
    Fn(Vec<RcType>, RcType),
    Named(Name),
}

pub type Name = Rc<str>;

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f64),
}

/// Patterns

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Wildcard,
    Annotated(RcPattern, RcType),
    Literal(Literal),
    Binder(Name),
    Tuple(Vec<RcPattern>),
    // Record(Vec<(String, RcPattern)>),
    // Tag(String, RcPattern),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Bop {
    Add,
    Sub,
    Mul,
    Div,
}

/// Expressions

#[derive(Debug, PartialEq)]
pub enum Expr {
    Var(Name),
    // Ann(RcExpr, RcType), // Annotated expressions
    Literal(Literal),
    Bop {
        bop: Bop,
        e1: RcExpr,
        e2: RcExpr,
    },
    Block(Block),
    Lambda {
        params: Vec<RcPattern>,
        body: RcExpr,
    },
    Call {
        func: RcExpr,
        args: Vec<RcExpr>,
    },
    Tuple(Vec<RcExpr>),
    Struct {
        name: Name,
        fields: Map<Name, RcExpr>,
    },
    FieldAccess {
        expr: RcExpr,
        name: Name,
    },
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: RcExpr,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let { pat: RcPattern, expr: RcExpr },
}

/// Expressions

impl Expr {
    pub fn free_vars(&self) -> Set<Name> {
        self.free_vars_not_bound(&Set::new())
    }

    fn free_vars_not_bound(&self, bound: &Set<Name>) -> Set<Name> {
        match self {
            Expr::Var(name) => {
                if !bound.contains(name) {
                    Set::unit(name.clone())
                } else {
                    Set::new()
                }
            }
            // Ann(RcExpr, RcType), // Annotated expressions
            Expr::Literal(_) => Set::new(),
            Expr::Bop { e1, e2, .. } => {
                e1.free_vars_not_bound(bound) + e2.free_vars_not_bound(bound)
            }
            Expr::Lambda { params, body } => {
                let mut bound = bound.clone();
                for pat in params {
                    pat.add_bound_vars(&mut bound)
                }
                body.free_vars_not_bound(&bound)
            }
            Expr::Call { func, args } => {
                let func_fv = func.free_vars_not_bound(bound);
                let args_fv = Set::unions(args.iter().map(|a| a.free_vars_not_bound(bound)));
                func_fv + args_fv
            }
            Expr::Block(block) => block.free_vars_not_bound(bound),
            Expr::Tuple(exprs) => Set::unions(exprs.iter().map(|e| e.free_vars_not_bound(bound))),
            Expr::Struct { name: _, fields } => {
                Set::unions(fields.values().map(|e| e.free_vars_not_bound(bound)))
            }
            Expr::FieldAccess { expr, name: _ } => expr.free_vars_not_bound(bound),
        }
    }
}

impl Block {
    fn free_vars_not_bound(&self, bound: &Set<Name>) -> Set<Name> {
        let mut bound = bound.clone();
        let stmts_free = Set::unions(self.stmts.iter().map(|s| s.free_vars_not_bound(&mut bound)));
        stmts_free + self.expr.free_vars_not_bound(&bound)
    }
}

impl Stmt {
    fn free_vars_not_bound(&self, bound: &mut Set<Name>) -> Set<Name> {
        match self {
            Stmt::Let { pat, expr } => {
                let free = expr.free_vars();
                pat.add_bound_vars(bound);
                free
            }
        }
    }
}

impl Pattern {
    fn add_bound_vars(&self, bound: &mut Set<Name>) {
        match self {
            Pattern::Wildcard => (),
            Pattern::Annotated(pat, _typ) => pat.add_bound_vars(bound),
            Pattern::Literal(_) => (),
            Pattern::Binder(name) => {
                bound.insert(name.clone());
            }
            Pattern::Tuple(pats) => {
                for pat in pats {
                    pat.add_bound_vars(bound)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    Struct(Struct),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Struct {
    pub name: Name,
    pub fields: Map<Name, RcType>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
}

impl Program {
    pub fn get_struct(&self, name: &str) -> Result<Struct, String> {
        for d in &self.decls {
            match d {
                Decl::Struct(s) if s.name.as_ref() == name => return Ok(s.clone()),
                _ => (),
            }
        }
        Err("struct not found".into())
    }
}

pub fn name(s: &'static str) -> Name {
    s.into()
}

pub fn var(s: &'static str) -> RcExpr {
    Expr::Var(s.into()).into()
}

pub fn int(i: i32) -> RcExpr {
    Expr::Literal(Literal::Int(i)).into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::expr;

    fn set(strs: &[&str]) -> Set<Name> {
        strs.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn test_free_variables() {
        let e = expr!(|x| x + x);
        assert_eq!(e.free_vars(), set(&[]));

        let e = expr!(x + x);
        assert_eq!(e.free_vars(), set(&["x"]));

        let e = expr!(|x| y + x);
        assert_eq!(e.free_vars(), set(&["y"]));

        let e = expr!(|y| |x| x + y + z);
        assert_eq!(e.free_vars(), set(&["z"]));
    }

}

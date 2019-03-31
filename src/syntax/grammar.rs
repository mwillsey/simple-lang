include!(concat!(env!("OUT_DIR"), "/syntax/grammar.rs"));

pub fn parse_expr(s: &str) -> RcExpr {
    match ExprParser::new().parse(s) {
        Ok(e) => e,
        Err(err) => panic!("Failed to parse: {}\nerr: {}", s, err),
    }
}

pub fn parse_pattern(s: &str) -> RcPattern {
    match PatternParser::new().parse(s) {
        Ok(e) => e,
        Err(err) => panic!("Failed to parse: {}\nerr: {}", s, err),
    }
}

pub fn parse_type(s: &str) -> RcType {
    match TypeParser::new().parse(s) {
        Ok(e) => e,
        Err(err) => panic!("Failed to parse: {}\nerr: {}", s, err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple() {
        use crate::syntax::ast;
        use ast::Expr::*;

        fn binder(s: &str) -> RcPattern {
            ast::Pattern::Binder(s.into()).into()
        }

        assert_eq!(
            parse_expr("|x| |y| x * y"),
            Lambda {
                params: vec![binder("x")],
                body: Lambda {
                    params: vec![binder("y")],
                    body: Bop {
                        bop: ast::Bop::Mul,
                        e1: var("x"),
                        e2: var("y"),
                    }.into()
                }.into()
            }.into()
        );

        assert_eq!(
            parse_expr("1 + x()"),
            Bop {
                bop: ast::Bop::Add,
                e1: int(1),
                e2: Call {
                    func: var("x"),
                    args: vec![],
                }
                .into(),
            }
            .into()
        );

        assert_eq!(
            parse_expr("1 * x()"),
            Bop {
                bop: ast::Bop::Mul,
                e1: int(1),
                e2: Call {
                    func: var("x"),
                    args: vec![],
                }
                .into(),
            }
            .into()
        );

        assert_eq!(parse_expr("(x)"), var("x"));
        assert_eq!(parse_expr("(x,)"), Tuple(vec![var("x")]).into());
    }
}

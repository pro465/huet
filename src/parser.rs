use crate::{
    error::{Error, Loc},
    token::{Scanner, TokenTy},
};
use std::fmt::Display;
use std::path::PathBuf;

pub type Sym = Vec<String>;

pub fn to_str(s: &Sym) -> String {
    let mut res = s[0].clone();
    for i in &s[1..] {
        res.push('.');
        res.push_str(i);
    }
    res
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RuleKind {
    Theorem,
    Axiom,
}

#[derive(Clone, Debug)]
pub struct Rule {
    pub name: Sym,
    pub loc: Loc,
    pub ty: Expr,
    pub kind: RuleKind,
    pub proof: Option<Expr>,
}

impl Rule {
    pub fn is_theorem(&self) -> bool {
        self.kind == RuleKind::Theorem
    }
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Use {
        loc: Loc,
        root: bool,
        sym: Sym,
        star: bool,
        alias: Option<String>,
    },
    Rule(Rule),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Ty(Loc), // *
    Identifier {
        loc: Loc,
        name: Sym,
    },
    Call {
        loc: Loc,
        f: Box<Expr>,
        args: Vec<Expr>,
    },
    Lambda {
        loc: Loc,
        var: String,
        ty: Box<Expr>,
        body: Box<Expr>,
    },
}

impl Default for Expr {
    fn default() -> Self {
        Expr::Ty(Loc::new(PathBuf::new()))
    }
}

impl Expr {
    pub fn loc(&self) -> &Loc {
        match self {
            Expr::Ty(loc)
            | Expr::Identifier { loc, .. }
            | Expr::Call { loc, .. }
            | Expr::Lambda { loc, .. } => loc,
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ty(_) => write!(f, "*"),
            Expr::Identifier { name, .. } => {
                for i in 0..name.len() - 1 {
                    write!(f, "{}.", name[i])?;
                }
                write!(f, "{}", name.last().unwrap())
            }
            Expr::Lambda { var, ty, body, .. } => write!(f, "\\({}:{}). {}", var, ty, body),
            Expr::Call { f: func, args, .. } => {
                write!(f, "({}", func)?;
                for arg in args.iter() {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub struct Parser<'a> {
    sc: Scanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(sc: Scanner<'a>) -> Self {
        Self { sc }
    }
    pub fn parse_toplevel(&mut self) -> Result<Option<TopLevel>, Error> {
        if self.sc.peek()?.ty() == TokenTy::Eof {
            return Ok(None);
        }
        let tok = self
            .sc
            .expect_one(&[TokenTy::Use, TokenTy::Axiom, TokenTy::Theorem])?;
        match tok.ty {
            TokenTy::Axiom => self.parse_axiom(),
            TokenTy::Theorem => self.parse_theorem(),
            TokenTy::Use => self.parse_use(tok.loc),
            _ => unreachable!(),
        }
        .map(Some)
    }

    fn parse_use(&mut self, loc: Loc) -> Result<TopLevel, Error> {
        let root = self.sc.is_token(TokenTy::Period)?;
        let (_, name) = self.sc.expect_identifier()?;
        let sym = self.expect_sym(name)?;
        let star = self.sc.is_token(TokenTy::Star)?;
        let alias = if self.sc.is_token(TokenTy::As)? {
            Some(self.sc.expect_identifier()?.1)
        } else {
            None
        };
        Ok(TopLevel::Use {
            loc,
            root,
            star,
            sym,
            alias,
        })
    }
    fn parse_axiom(&mut self) -> Result<TopLevel, Error> {
        let rule = self.parse_rule_basic()?;
        let proof = if self.sc.is_token(TokenTy::Equal)? {
            Some(self.parse_expr()?)
        } else {
            None
        };
        Ok(TopLevel::Rule(Rule { proof, ..rule }))
    }

    fn parse_theorem(&mut self) -> Result<TopLevel, Error> {
        let rule = self.parse_rule_basic()?;
        self.sc.expect_token(TokenTy::Equal)?;
        let proof = Some(self.parse_expr()?);

        Ok(TopLevel::Rule(Rule {
            proof,
            kind: RuleKind::Theorem,
            ..rule
        }))
    }

    fn parse_rule_basic(&mut self) -> Result<Rule, Error> {
        let (loc, name) = self.sc.expect_identifier()?;

        self.sc.expect_token(TokenTy::Colon)?;

        let ty = self.parse_expr()?;

        Ok(Rule {
            name: vec![name],
            loc,
            ty,
            kind: RuleKind::Axiom,
            proof: None,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        if let Some((loc, name)) = self.sc.is_identifier()? {
            let name = self.expect_sym(name)?;
            return Ok(Expr::Identifier { loc, name });
        }
        let tok = self
            .sc
            .expect_one(&[TokenTy::Star, TokenTy::Lparen, TokenTy::Lambda])?;
        if tok.is(TokenTy::Star) {
            Ok(Expr::Ty(self.sc.loc()))
        } else if tok.is(TokenTy::Lparen) {
            let loc = self.sc.loc();
            let mut e = self.parse_expr()?;
            let mut args = Vec::new();
            while !self.sc.is_token(TokenTy::Rparen)? {
                args.push(self.parse_expr()?);
            }
            if !args.is_empty() {
                e = Expr::Call {
                    loc,
                    args,
                    f: Box::new(e),
                };
            }
            Ok(e)
        } else {
            assert!(tok.is(TokenTy::Lambda));
            self.sc.expect_token(TokenTy::Lparen)?;
            let (loc, var) = self.sc.expect_identifier()?;
            self.sc.expect_token(TokenTy::Colon)?;
            let ty = Box::new(self.parse_expr()?);
            self.sc.expect_token(TokenTy::Rparen)?;
            self.sc.expect_token(TokenTy::Period)?;
            let body = Box::new(self.parse_expr()?);
            Ok(Expr::Lambda { loc, var, ty, body })
        }
    }
    fn expect_sym(&mut self, first: String) -> Result<Sym, Error> {
        let mut res = vec![first];
        while self.sc.is_token(TokenTy::Period)? {
            let (_, s) = self.sc.expect_identifier()?;
            res.push(s);
        }
        Ok(res)
    }
}

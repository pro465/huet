use crate::error::Loc;
use crate::parser::{Expr, Rule, RuleKind, Sym};

use std::collections::HashMap;

pub struct NameResolver {
    globals: HashMap<Sym, Rule>,
    scopes: Vec<Rule>,
}

impl NameResolver {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            scopes: Vec::new(),
        }
    }
    pub fn get(&self, idx: &Sym) -> Option<&Rule> {
        if let Some(x) = self.scopes.iter().rfind(|x| &x.name == idx) {
            return Some(x);
        }
        self.globals.get(idx)
    }
    pub fn insert(&mut self, k: Sym, v: Rule) {
        self.globals.insert(k, v);
    }

    pub fn new_scope(&mut self, loc: Loc, k: String, v: Expr) {
        self.scopes.push(Rule {
            loc,
            name: vec![k],
            ty: v,
            kind: RuleKind::Axiom,
            proof: None,
        });
    }
    pub fn pop_scope(&mut self) -> Option<Rule> {
        self.scopes.pop()
    }
}

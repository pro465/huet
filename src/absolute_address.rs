use crate::{
    error::{Error, ErrorTy, Loc},
    parser::{to_str, Expr, Parser, Rule, Sym, TopLevel},
    token::Scanner,
};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;

pub struct Converter {
    name_resolver: HashMap<Sym, Sym>,
    stars: HashMap<Sym, Vec<Sym>>,
    defs: Vec<Rule>,
}

impl Converter {
    pub fn new() -> Self {
        Self {
            name_resolver: HashMap::new(),
            stars: HashMap::new(),
            defs: Vec::new(),
        }
    }
    pub fn convert(
        &mut self,
        mut scope: Sym,
        path: PathBuf,
        mut parser: Parser,
    ) -> Result<(), Error> {
        self.name_resolver.insert(scope.clone(), scope.clone());
        while let Some(toplevel) = parser.parse_toplevel()? {
            match toplevel {
                TopLevel::Rule(mut r) => {
                    self.convert_rule(&mut r, &mut scope);
                    self.defs.push(r);
                }
                TopLevel::Use {
                    loc,
                    root,
                    star,
                    sym,
                    alias,
                } => {
                    self.handle_use(loc, scope.clone(), sym, star, root, alias, path.clone())?;
                }
            }
        }
        Ok(())
    }
    pub fn defs(self) -> Vec<Rule> {
        self.defs
    }
    fn handle_use(
        &mut self,
        loc: Loc,
        mut scope: Sym,
        mut sym: Sym,
        star: bool,
        root: bool,
        alias: Option<String>,
        mut path: PathBuf,
    ) -> Result<(), Error> {
        let s = if !root {
            let l = scope.len();
            scope.append(&mut sym);
            let t = scope.clone();
            scope.truncate(l);
            t
        } else {
            sym
        };
        if star {
            self.stars.entry(scope.clone()).or_default().push(s.clone());
        }
        if let Some(s) = self.get(&s) {
            let a = alias.unwrap_or(s.last().unwrap().to_string());
            scope.push(a);
            self.name_resolver.insert(scope.clone(), s.clone());
            scope.pop();
            return Ok(());
        }

        if path.file_name() != Some(OsStr::new("mod.ht")) {
            path.pop();
            path.push(scope.last().unwrap());
        } else {
            path.pop();
        }
        let mut flag = true;
        let mut j = 0;
        for i in 0..scope.len() {
            flag &= i < s.len() && scope[i] == s[i];
            if flag {
                j = i + 1;
            } else {
                path.pop();
            }
        }

        assert!(j < s.len());

        self.import(loc.clone(), path, s[..j].to_vec(), &s[j])?;
        if let Some(s) = self.get(&s) {
            let a = alias.unwrap_or(s.last().unwrap().to_string());
            scope.push(a);
            self.name_resolver.insert(scope.clone(), s.clone());
            scope.pop();
        } else {
            return Err(Error {
                loc,
                ty: ErrorTy::ImportError,
                desc: format!("path `{}` doesn't exist", to_str(&s)),
            });
        }
        Ok(())
    }

    fn import(
        &mut self,
        loc: Loc,
        mut path: PathBuf,
        mut scope: Sym,
        module: &str,
    ) -> Result<(), Error> {
        scope.push(module.to_string());
        path.push(format!("{}.ht", module));
        let prog = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => {
                path.pop();
                path.push(format!("{}/mod.ht", module));
                match fs::read_to_string(&path) {
                    Ok(s) => s,
                    Err(_) => {
                        return Err(Error {
                            loc,
                            ty: ErrorTy::ImportError,
                            desc: format!("can't import module `{}`", module),
                        })
                    }
                }
            }
        };
        let scanner = Scanner::new(path.clone(), &prog);
        let parser = Parser::new(scanner);
        self.convert(scope, path, parser)
    }
    fn convert_rule(&mut self, rule: &mut Rule, scope: &mut Sym) {
        let l = scope.len();
        scope.append(&mut rule.name);
        self.name_resolver.insert(scope.clone(), scope.clone());
        rule.name = scope.clone();
        scope.truncate(l);
        let mut v = Vec::new();
        self.convert_expr(&mut rule.ty, &mut v, scope);
        if let Some(ref mut proof) = rule.proof {
            self.convert_expr(proof, &mut v, scope);
        }
    }

    fn convert_expr(&self, e: &mut Expr, v: &mut Vec<Sym>, scope: &mut Sym) {
        match e {
            Expr::Ty(_) => {}
            Expr::Call { f, args, .. } => {
                self.convert_expr(f, v, scope);
                for arg in args.iter_mut() {
                    self.convert_expr(arg, v, scope);
                }
            }
            Expr::Lambda { var, ty, body, .. } => {
                self.convert_expr(ty, v, scope);
                v.push(vec![var.clone()]);
                self.convert_expr(body, v, scope);
                v.pop();
            }
            Expr::Identifier { name, .. } => {
                if v.contains(&name) {
                    return;
                }
                let l = scope.len();
                scope.extend_from_slice(&name);
                *name = match self.get(scope) {
                    Some(x) => x.clone(),
                    None => scope.clone(),
                };
                scope.truncate(l);
            }
        }
    }
    fn get(&self, k: &Sym) -> Option<&Sym> {
        if let Some(x) = self.get_nonstar(k) {
            return Some(x);
        }
        for i in 1..k.len() {
            if let Some(stars) = self.stars.get(&k[..i]) {
                for s in stars {
                    let mut t = s.clone();
                    t.extend_from_slice(&k[i..]);
                    if let Some(x) = self.get_nonstar(&t) {
                        return Some(x);
                    }
                }
            }
        }
        None
    }

    fn get_nonstar(&self, k: &Sym) -> Option<&Sym> {
        let mut v = self.name_resolver.get(&k[..1])?;
        for i in &k[1..] {
            let mut t = v.clone();
            t.push(i.clone());
            v = self.name_resolver.get(&t)?;
        }
        Some(v)
    }
}

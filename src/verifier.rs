use crate::{
    error::{Error, ErrorTy, Loc},
    name_resolver::NameResolver,
    parser::{to_str, Expr, Rule, Sym},
};
use std::{collections::HashSet, vec::IntoIter};

pub struct Verifier {
    rules: NameResolver,
    rest: IntoIter<Rule>,
    stack: Vec<Expr>,
}

impl Verifier {
    pub fn new(v: Vec<Rule>) -> Self {
        Self {
            rules: NameResolver::new(),
            rest: v.into_iter(),
            stack: Vec::new(),
        }
    }

    pub fn verify_next(&mut self) -> Result<Option<()>, Error> {
        let a = match self.rest.next() {
            Some(x) => x,
            None => return Ok(None),
        };

        if a.is_theorem() {
            println!("verifying `{}`...", to_str(&a.name));
            let proof = a.proof.as_ref().expect("theorems always have proofs.");
            let actual_ty = self.get_ty(proof)?;
            let expected_ty = self.eval(a.ty.clone())?;

            if !is_eq(&mut vec![], &expected_ty, &actual_ty) {
                return Err(Error {
                    loc: a.loc,
                    ty: ErrorTy::VerifError,
                    desc: format!("type definition does not match type of proof:\n\texpected: `{}`,\n\tactual: `{}`", expected_ty, actual_ty),
                });
            }
        }
        self.rules.insert(a.name.clone(), a);
        Ok(Some(()))
    }

    fn get_ty(&mut self, p: &Expr) -> Result<Expr, Error> {
        let p = p.clone();
        let (loc, res) = match p {
            Expr::Ty(l) => (l.clone(), Expr::Ty(l)),
            Expr::Lambda { loc, var, ty, body } => {
                self.rules
                    .new_scope(loc.clone(), var.clone(), (&*ty).clone());
                let body = Box::new(self.get_ty(&body)?);
                self.rules.pop_scope();
                (loc.clone(), Expr::Lambda { loc, var, ty, body })
            }
            Expr::Identifier { loc, name, .. } => match self.rules.get(&name) {
                Some(x) => (loc, x.ty.clone()),
                None => {
                    return Err(Error {
                        loc,
                        ty: ErrorTy::VerifError,
                        desc: format!("can't find type for `{}`", to_str(&name)),
                    })
                }
            },
            Expr::Call { f, args, loc } => (
                loc.clone(),
                Expr::Call {
                    f: Box::new(self.get_ty(&f)?),
                    args,
                    loc,
                },
            ),
        };
        self.eval(res)
    }

    fn eval(&mut self, mut res: Expr) -> Result<Expr, Error> {
        if self.stack.iter().any(|e| is_eq(&mut vec![], e, &res)) {
            let mut s = String::new();
            for i in &self.stack {
                s.push('\n');
                s.push_str(&i.to_string());
            }
            panic!("loop detected:{}", s);
        }
        self.stack.push(res.clone());
        while self.reducible(&res) {
            //println!("{}: {}", self.stack.len(), &res);
            res = match res {
                Expr::Ty(_) => res,
                Expr::Identifier {
                    ref name, ref loc, ..
                } => match self.rules.get(name).and_then(|x| x.proof.clone()) {
                    Some(x) => {
                        let loc = loc.clone();
                        res = x.clone();
                        replace_locs(&mut res, &loc);
                        continue;
                    }
                    None => res,
                },
                Expr::Lambda { var, ty, body, loc } => {
                    let ty = self.eval(*ty)?;
                    self.rules.new_scope(loc, var, ty);
                    let body = Box::new(self.eval(*body)?);
                    let Rule { name, ty, loc, .. } = self.rules.pop_scope().unwrap();
                    let var = name.into_iter().next().unwrap();
                    Expr::Lambda {
                        var,
                        loc,
                        body,
                        ty: Box::new(ty),
                    }
                }
                Expr::Call { f, args, .. } => {
                    let f = self.eval((&*f).clone())?;
                    self.call(f, &args)?
                }
            };
        }
        self.stack.pop();
        Ok(res)
    }

    fn call(&mut self, mut f: Expr, args: &[Expr]) -> Result<Expr, Error> {
        for (i, arg) in args.into_iter().enumerate() {
            let argty = self.get_ty(&arg)?;
            if let Expr::Lambda { var, ty, body, .. } = f {
                let mut stacktrace = String::new();
                for i in self.stack.iter().rev() {
                    stacktrace.push_str("\n\t");
                    stacktrace.push_str(&i.to_string());
                }

                if !is_eq(&mut vec![], &ty, &argty) {
                    let mut desc = String::new();
                    desc.push_str(&format!("expected type of parameter `{}` in lambda does not match type of argument:", var));
                    desc.push_str(&format!(
                        "\n\texpected: `{}`\n\tactual: `{}`\n\tstack trace: {}",
                        ty, argty, stacktrace
                    ));
                    return Err(Error {
                        loc: arg.loc().clone(),
                        ty: ErrorTy::VerifError,
                        desc,
                    });
                }
                let mut free_vars = HashSet::new();
                get_free_vars(&mut vec![], &mut free_vars, &arg);
                //get_free_vars(&mut vec![], &mut free_vars, &body);
                //println!("{}\n{}={}", &body, &var, &arg);
                f = replace(*body, &mut vec![], &free_vars, &var, arg);
                //println!("{}", &f);
                //replace_locs(&mut f, loc);
                f = self.eval(f)?;
            } else {
                let mut new_args = Vec::new();
                for e in &args[i..] {
                    new_args.push(self.eval(e.clone())?);
                }
                return Ok(Expr::Call {
                    loc: f.loc().clone(),
                    f: Box::new(f),
                    args: new_args,
                });
            }
        }
        Ok(f)
    }
    fn reducible(&self, e: &Expr) -> bool {
        match e {
            Expr::Ty(_) => false,
            Expr::Identifier { name, .. } => {
                self.rules.get(&name).is_some_and(|r| r.proof.is_some())
            }
            Expr::Lambda { ty, body, .. } => self.reducible(ty) || self.reducible(body),
            Expr::Call { f, args, .. } => {
                if let Expr::Lambda { .. } = &**f {
                    true
                } else {
                    self.reducible(f) || args.iter().any(|x| self.reducible(x))
                }
            }
        }
    }
}

fn get_free_vars(bound_vars: &mut Vec<String>, free_vars: &mut HashSet<String>, e: &Expr) {
    match e {
        Expr::Ty(_) => {}
        Expr::Call { f, args, .. } => {
            get_free_vars(bound_vars, free_vars, &f);
            for arg in args {
                get_free_vars(bound_vars, free_vars, arg);
            }
        }
        Expr::Lambda { var, ty, body, .. } => {
            get_free_vars(bound_vars, free_vars, &ty);
            bound_vars.push(var.clone());
            get_free_vars(bound_vars, free_vars, &body);
            bound_vars.pop();
        }
        Expr::Identifier { name, .. } => {
            if name.len() == 1 && bound_vars.iter().find(|x| *x == &name[0]).is_none() {
                free_vars.insert(name[0].clone());
            }
        }
    }
}

fn replace(
    body: Expr,
    bound_vars: &mut Vec<(String, String)>,
    free_vars: &HashSet<String>,
    variable: &String,
    replacement: &Expr,
) -> Expr {
    match body {
        Expr::Ty(loc) => Expr::Ty(loc),
        Expr::Call { loc, f, args } => Expr::Call {
            loc,
            f: Box::new(replace(*f, bound_vars, free_vars, variable, replacement)),
            args: args
                .into_iter()
                .map(|arg| replace(arg, bound_vars, free_vars, variable, replacement))
                .collect(),
        },
        Expr::Lambda {
            loc,
            mut var,
            ty,
            body,
        } => {
            let orig = var.clone();
            while free_vars.contains(&var) || bound_vars.iter().any(|x| &x.1 == &var) {
                var.push('$');
            }
            let ty = Box::new(replace(*ty, bound_vars, free_vars, variable, replacement));
            bound_vars.push((orig, var.clone()));
            let res = Expr::Lambda {
                loc,
                var,
                ty,
                body: Box::new(replace(*body, bound_vars, free_vars, variable, replacement)),
            };
            bound_vars.pop();
            res
        }
        Expr::Identifier { loc, name } => {
            if name.len() == 1 {
                match bound_vars.iter().rfind(|x| &x.0 == &name[0]) {
                    Some((_, x)) => Expr::Identifier {
                        loc,
                        name: vec![x.clone()],
                    },
                    None => {
                        if variable == &name[0] {
                            replacement.clone()
                        } else {
                            Expr::Identifier { loc, name }
                        }
                    }
                }
            } else {
                Expr::Identifier { loc, name }
            }
        }
    }
}

fn is_eq(bindings: &mut Vec<(Sym, Sym)>, a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Ty(_), _) => true,
        (
            Expr::Call { f, args, .. },
            Expr::Call {
                f: f2, args: args2, ..
            },
        ) => {
            args.len() == args2.len()
                && is_eq(bindings, f, f2)
                && args
                    .iter()
                    .zip(args2.iter())
                    .all(|(x, y)| is_eq(bindings, x, y))
        }
        (Expr::Identifier { name, .. }, Expr::Identifier { name: name2, .. }) => {
            if let Some((_, n2)) = bindings.iter().rfind(|x| &x.0 == name) {
                n2 == name2
            } else {
                name == name2
            }
        }
        (
            Expr::Lambda { var, ty, body, .. },
            Expr::Lambda {
                var: var2,
                ty: ty2,
                body: body2,
                ..
            },
        ) => {
            if !is_eq(bindings, ty, ty2) {
                return false;
            }
            bindings.push((vec![var.to_string()], vec![var2.to_string()]));
            let cond = is_eq(bindings, body, body2);
            bindings.pop();
            cond
        }
        _ => false,
    }
}

fn replace_locs(f: &mut Expr, loc: &Loc) {
    match f {
        Expr::Ty(l) | Expr::Identifier { loc: l, .. } => *l = loc.clone(),
        Expr::Lambda {
            ty, body, loc: l, ..
        } => {
            *l = loc.clone();
            replace_locs(ty, loc);
            replace_locs(body, loc);
        }
        Expr::Call { f, args, loc: l } => {
            *l = loc.clone();
            replace_locs(f, loc);
            for i in args.iter_mut() {
                replace_locs(i, loc);
            }
        }
    }
}

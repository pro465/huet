use crate::{
    error::{Error, ErrorTy, Loc},
    name_resolver::NameResolver,
    parser::{to_str, Expr, Rule, Sym},
};
use std::{collections::HashSet, vec::IntoIter};

pub struct Verifier {
    rules: NameResolver,
    rest: IntoIter<Rule>,
}

impl Verifier {
    pub fn new(v: Vec<Rule>) -> Self {
        Self {
            rest: v.into_iter(),
            rules: NameResolver::new(),
        }
    }

    pub fn verify_next(&mut self) -> Result<Option<()>, Error> {
        let a = match self.rest.next() {
            Some(x) => x,
            None => return Ok(None),
        };

        if let Some(ref proof) = a.proof {
            let actual_ty = self.get_ty(proof)?;
            let mut expected_ty = a.ty.clone();
            while reducible(&expected_ty) {
                expected_ty = self.eval(expected_ty, &a.loc)?;
            }

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
        let (loc, mut res) = match p {
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
        while reducible(&res) {
            res = self.eval(res, &loc)?;
        }
        Ok(res)
    }

    fn eval(&mut self, res: Expr, loc2: &Loc) -> Result<Expr, Error> {
        match res {
            Expr::Identifier { .. } | Expr::Ty(_) => Ok(res),
            Expr::Lambda { var, ty, body, loc } => {
                let ty = self.eval(*ty, &loc)?;
                self.rules.new_scope(loc, var, ty);
                let body = Box::new(self.eval(*body, loc2)?);
                let Rule { name, ty, loc, .. } = self.rules.pop_scope().unwrap();
                let var = name.into_iter().next().unwrap();
                Ok(Expr::Lambda {
                    var,
                    loc,
                    body,
                    ty: Box::new(ty),
                })
            }
            Expr::Call { f, args, .. } => {
                let f = self.eval((&*f).clone(), loc2)?;
                self.call(f, &args, loc2)
            }
        }
    }

    fn call(&mut self, mut f: Expr, args: &[Expr], loc: &Loc) -> Result<Expr, Error> {
        for (i, arg) in args.into_iter().enumerate() {
            let argty = self.get_ty(&arg)?;
            if let Expr::Lambda { var, ty, body, .. } = f {
                if !is_eq(&mut vec![], &ty, &argty) {
                    return Err(Error {
                        loc: loc.clone(),
                        ty: ErrorTy::VerifError,
                        desc: format!("expected type of parameter `{}` in lambda does not match type of argument:\n\texpected: `{}`,\n\tactual: `{}`", var, ty, argty),
                    });
                }
                let mut free_vars = HashSet::new();
                get_free_vars(&mut vec![], &mut free_vars, &arg);
                get_free_vars(&mut vec![], &mut free_vars, &body);

                f = replace(*body, &mut vec![], &free_vars, &var, &arg);
            } else {
                return Ok(Expr::Call {
                    f: Box::new(f),
                    loc: loc.clone(),
                    args: args[i..].to_vec(),
                });
            }
        }
        Ok(f)
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
        Expr::Lambda { var, body, .. } => {
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
    bound_vars: &mut Vec<String>,
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
            bound_vars.push(var.clone());
            while free_vars.contains(&var) {
                var.push('$');
            }
            let res = Expr::Lambda {
                loc,
                var,
                ty: Box::new(replace(*ty, bound_vars, free_vars, variable, replacement)),
                body: Box::new(replace(*body, bound_vars, free_vars, variable, replacement)),
            };
            bound_vars.pop();
            res
        }
        Expr::Identifier { loc, mut name } => {
            if name.len() == 1 && bound_vars.contains(&name[0]) {
                while free_vars.contains(&name[0]) {
                    name[0].push('$');
                }
                Expr::Identifier { loc, name }
            } else if name.len() == 1 && variable == &name[0] {
                replacement.clone()
            } else {
                Expr::Identifier { loc, name }
            }
        }
    }
}

fn is_eq(bindings: &mut Vec<(Sym, Sym)>, a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Ty(_), Expr::Ty(_)) => true,
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

fn reducible(e: &Expr) -> bool {
    match e {
        Expr::Identifier { .. } | Expr::Ty(_) => false,
        Expr::Lambda { ty, body, .. } => reducible(ty) || reducible(body),
        Expr::Call { f, args, .. } => {
            if let Expr::Lambda { .. } = &**f {
                true
            } else {
                reducible(f) || args.iter().any(reducible)
            }
        }
    }
}

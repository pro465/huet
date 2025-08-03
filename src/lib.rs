mod absolute_address;
mod error;
mod name_resolver;
mod parser;
mod token;
mod verifier;

use std::path::{Path, PathBuf};

pub fn verify(path: &Path, prog: String) -> Result<(), error::Error> {
    let p = PathBuf::from(path);
    let scanner = token::Scanner::new(p.clone(), &prog);
    let mut name = path.file_stem().unwrap().to_str().unwrap();
    if name == "mod" {
        name = path
            .parent()
            .unwrap()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap();
    }
    let parser = parser::Parser::new(scanner);
    let mut converter = absolute_address::Converter::new();
    converter.convert(vec![String::from(name)], p, parser)?;
    let defs = converter.defs();
    //println!("{:#?}", defs);
    let mut verifier = verifier::Verifier::new(defs);
    while verifier.verify_next()?.is_some() {}
    Ok(())
}

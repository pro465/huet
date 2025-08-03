use huet::*;
use std::fs;

fn main() {
    let path = fs::canonicalize(std::env::args().nth(1).unwrap_or_else(|| help()))
        .expect("could not canonicalize argument");

    let prog = fs::read_to_string(&path).expect("could not read file");

    let result = verify(&path, prog);
    if let Err(e) = result {
        e.report();
        std::process::exit(-1);
    }
    println!("verified");
}

fn help() -> ! {
    println!(
        "usage: {} <filename>",
        std::env::current_exe()
            .unwrap_or_else(|_| "huet".into())
            .display()
    );
    std::process::exit(-1);
}

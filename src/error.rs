use std::fmt::*;
use std::path::PathBuf;
#[derive(Clone, Debug)]
pub struct Error {
    pub loc: Loc,
    pub ty: ErrorTy,
    pub desc: String,
}

impl Error {
    pub fn report(&self) {
        eprintln!("{} @ {}:\n{}", self.ty, self.loc, self.desc);
    }
}

#[derive(Clone, Debug)]
pub struct Loc {
    pub path: PathBuf,
    pub line: u64,
    pub col: u64,
}

impl Loc {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            line: 1,
            col: 1,
        }
    }
    pub fn new_line(&mut self) {
        self.line += 1;
        self.col = 1;
    }
    pub fn col(&mut self) {
        self.col += 1;
    }
}

impl Display for Loc {
    fn fmt<'a>(&self, fmt: &mut Formatter<'a>) -> Result {
        write!(
            fmt,
            "file {}, line {}, column {}",
            self.path.to_str().unwrap(),
            self.line,
            self.col
        )
    }
}

#[derive(Clone, Debug)]
pub enum ErrorTy {
    SyntaxError,
    ImportError,
    VerifError,
}

impl Display for ErrorTy {
    fn fmt<'a>(&self, fmt: &mut Formatter<'a>) -> Result {
        use ErrorTy::*;
        match self {
            SyntaxError => write!(fmt, "syntax error"),
            ImportError => write!(fmt, "import error"),
            VerifError => write!(fmt, "proof verification error"),
        }
    }
}

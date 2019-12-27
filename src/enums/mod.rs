use std::fmt;

#[derive(Debug)]
pub enum Token<'a> {
    Keyword(Keyword),
    Identifier(&'a str),
    OpenParens,
    CloseParens,
    OpenBrace,
    CloseBrace,
    Semicolon,
    IntLiteral(&'a str),
    Minus,
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Keyword(k) => match k {
                Keyword::Return => write!(f, "return"),
                Keyword::Int => write!(f, "int"),
            },
            Token::Identifier(s) => write!(f, "{}", s),
            Token::OpenParens => write!(f, "("),
            Token::CloseParens => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Semicolon => write!(f, ";"),
            Token::IntLiteral(s) => write!(f, "{}", s),
            Token::Minus => write!(f, "-"),
            Token::UnaryOp(op) => match op {
                UnaryOp::BitComplement => write!(f, "~"),
                UnaryOp::Negation => write!(f, "!"),
                UnaryOp::Minus => write!(f, "-"),
            },
            Token::BinaryOp(op) => match op {
                BinaryOp::Minus => write!(f, "-"),
                BinaryOp::Add => write!(f, "+"),
                BinaryOp::Mult => write!(f, "*"),
                BinaryOp::Div => write!(f, "/"),
                BinaryOp::LogAnd => write!(f, "&&"),
                BinaryOp::LogOr => write!(f, "||"),
                BinaryOp::Equal => write!(f, "=="),
                BinaryOp::NotEqual => write!(f, "!="),
                BinaryOp::LessThan => write!(f, "<"),
                BinaryOp::LessThanEqual => write!(f, "<="),
                BinaryOp::GreaterThan => write!(f, ">"),
                BinaryOp::GreaterThanEqual => write!(f, ">="),
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Int,
    Return,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    BitComplement,
    Negation,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Minus,
    Add,
    Mult,
    Div,
    LogAnd,
    LogOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Debug)]
pub enum Statement {
    Return,
}

#[derive(Debug)]
pub enum Syntax<'a> {
    Function(&'a str),
    Statement(Statement),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Constant(isize),
}

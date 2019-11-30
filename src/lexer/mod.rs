use std::fmt;

const KEYWORDS: [&str; 2] = ["int", "return"];

#[derive(Debug)]
pub enum Token<'a> {
    Keyword(&'a str),
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
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Keyword(s) => write!(f, "{}", s),
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
            },
        }
    }
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut out: Vec<Token> = Vec::new();
    let mut start = 0;
    let mut end = 0;

    for (i, ch) in input.chars().enumerate() {
        match ch {
            '{' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::OpenBrace);
                start = i + 1;
                end = i + 1;
            }
            '}' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::CloseBrace);
                start = i + 1;
                end = i + 1;
            }
            '(' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::OpenParens);
                start = i + 1;
                end = i + 1;
            }
            ')' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::CloseParens);
                start = i + 1;
                end = i + 1;
            }
            ';' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::Semicolon);
                start = i + 1;
                end = i + 1;
            }
            '-' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::Minus);
                start = i + 1;
                end = i + 1;
            }
            '~' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::UnaryOp(UnaryOp::BitComplement));
                start = i + 1;
                end = i + 1;
            }
            '!' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::UnaryOp(UnaryOp::Negation));
                start = i + 1;
                end = i + 1;
            }
            '+' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::BinaryOp(BinaryOp::Add));
                start = i + 1;
                end = i + 1;
            }
            '*' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::BinaryOp(BinaryOp::Mult));
                start = i + 1;
                end = i + 1;
            }
            '/' => {
                interpret_token(&input[start..end], &mut out);
                out.push(Token::BinaryOp(BinaryOp::Div));
                start = i + 1;
                end = i + 1;
            }
            _ => {
                if ch.is_whitespace() {
                    interpret_token(&input[start..end], &mut out);
                    start = i + 1;
                    end = i + 1;
                } else {
                    end += 1;
                }
            }
        }
    }

    fn interpret_token<'a>(token: &'a str, out: &mut Vec<Token<'a>>) {
        if token.is_empty() {
            return;
        }

        match KEYWORDS.iter().position(|&x| x == token) {
            Some(_) => out.push(Token::Keyword(token)),
            None => match token.parse::<isize>() {
                Ok(_) => {
                    out.push(Token::IntLiteral(token));
                }
                Err(_) => {
                    out.push(Token::Identifier(token));
                }
            },
        }
    }

    out
}

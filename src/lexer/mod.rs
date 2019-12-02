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

pub fn lex(input: &str) -> Vec<Token> {
    let mut out: Vec<Token> = Vec::new();
    let mut start = 0;
    let mut end = 0;

    let mut characters = input.chars().enumerate().peekable();
    while let Some((i, ch)) = characters.next() {
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
                match characters.peek() {
                    Some((_, '=')) => {
                        if let Some((j, _))  = characters.next() {
                            interpret_token(&input[start..end], &mut out);
                            out.push(Token::BinaryOp(BinaryOp::NotEqual));
                            start = j + 1;
                            end = j + 1;
                        }
                    }
                    _ => {
                        interpret_token(&input[start..end], &mut out);
                        out.push(Token::UnaryOp(UnaryOp::Negation));
                        start = i + 1;
                        end = i + 1;
                    }
                  }
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
            '&' => {
                match characters.peek() {
                    Some((_, '&')) => {
                        if let Some((j, _))  = characters.next() {
                            interpret_token(&input[start..end], &mut out);
                            out.push(Token::BinaryOp(BinaryOp::LogAnd));
                            start = j + 1;
                            end = j + 1;
                        }
                    }
                    _ => {}
                }
            }
            '|' => {
                match characters.peek() {
                    Some((_, '|')) => {
                        if let Some((j, _))  = characters.next() {
                            interpret_token(&input[start..end], &mut out);
                            out.push(Token::BinaryOp(BinaryOp::LogOr));
                            start = j + 1;
                            end = j + 1;
                        }
                    }
                    _ => {}
                }
            }
            '=' => {
                match characters.peek() {
                    Some((_, '=')) => {
                        if let Some((j, _)) = characters.next() {
                            interpret_token(&input[start..end], &mut out);
                            out.push(Token::BinaryOp(BinaryOp::Equal));
                            start = j + 1;
                            end = j + 1;
                        }
                    }
                    _ => {}
                }
            }
            '<' => {
                match characters.peek() {
                    Some((_, '=')) => {
                        if let Some((j, _)) = characters.next() {
                            interpret_token(&input[start..end], &mut out);
                            out.push(Token::BinaryOp(BinaryOp::LessThanEqual));
                            start = j + 1;
                            end = j + 1;
                        }
                    }
                    _ => {
                        interpret_token(&input[start..end], &mut out);
                        out.push(Token::BinaryOp(BinaryOp::LessThan));
                        start = i + 1;
                        end = i + 1;
                    }
                }
            }
            '>' => {
                match characters.peek() {
                    Some((_, '=')) => {
                        if let Some((j, _)) = characters.next() {
                            interpret_token(&input[start..end], &mut out);
                            out.push(Token::BinaryOp(BinaryOp::GreaterThanEqual));
                            start = j + 1;
                            end = j + 1;
                        }
                    }
                    _ => {
                        interpret_token(&input[start..end], &mut out);
                        out.push(Token::BinaryOp(BinaryOp::GreaterThan));
                        start = i + 1;
                        end = i + 1;
                    }
                }
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

        match token {
            "return" => out.push(Token::Keyword(Keyword::Return)),
            "int" => out.push(Token::Keyword(Keyword::Int)),
            _ => match token.parse::<isize>() {
                Ok(_) => out.push(Token::IntLiteral(token)),
                Err(_) => out.push(Token::Identifier(token)),
            },
        }
    }

    out
}

use crate::lexer::{BinaryOp, Keyword, Token, UnaryOp};
use smallvec::{smallvec, SmallVec};
use std::iter::Peekable;
use std::slice::Iter;

const SVEC_SIZE: usize = 5;

#[derive(Debug)]
enum Statement {
    Return,
}

#[derive(Debug)]
enum Syntax<'a> {
    Function(&'a str),
    Statement(Statement),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Constant(isize),
}

pub struct AST<'a> {
    nodes: Vec<Syntax<'a>>,
    children: Vec<SmallVec<[usize; SVEC_SIZE]>>,
    root_node: usize,
}

type PeekableTokens<'a> = Peekable<Iter<'a, Token<'a>>>;

impl<'a> AST<'a> {
    pub fn new(tokens: &'a [Token]) -> Result<Self, String> {
        let mut ast = AST {
            nodes: Vec::new(),
            children: Vec::new(),
            root_node: 0,
        };

        ast.nodes.reserve(tokens.len());
        ast.children.reserve(tokens.len());

        let mut token_iter = tokens.iter().peekable();
        let root = parse_function(&mut token_iter, &mut ast)?;
        ast.root_node = root;

        Ok(ast)
    }

    pub fn pretty_print(&self) {
        let curr_node = self.root_node;
        let recursion_level = 0;

        fn recurse(ast: &AST, node: usize, recursion_level: usize) {
            let mut indent = String::from("");
            for _ in 0..recursion_level {
                indent = format!("  {}", indent);
            }

            let value = &ast.nodes[node];
            let children = &ast.children[node];

            println!("{}{:?}", indent, value);

            for child in children.iter() {
                recurse(ast, *child, recursion_level + 1);
            }
        }

        recurse(self, curr_node, recursion_level);
    }

    pub fn generate_assembly(&self) -> String {
        let mut out: Vec<String> = Vec::new();
        let curr_node = self.root_node;

        fn recurse(ast: &AST, node: usize, out: &mut Vec<String>) {
            let value = &ast.nodes[node];
            let children = &ast.children[node];
            match value {
                Syntax::Function(s) => {
                    out.push(format!(".globl {}\n{}:\n", s, s));
                    for child in children.iter() {
                        recurse(ast, *child, out);
                    }
                }
                Syntax::Statement(st) => {
                    for child in children.iter() {
                        recurse(ast, *child, out);
                    }

                    match st {
                        Statement::Return => {
                            out.push("ret\n".to_string());
                        }
                    }
                }
                Syntax::Constant(i) => {
                    out.push(format!("movl  ${}, %eax\n", i));
                }
                Syntax::UnaryOp(op) => {
                    for child in children.iter() {
                        recurse(ast, *child, out);
                    }

                    match op {
                        UnaryOp::Minus => out.push("neg  %eax\n".to_string()),
                        UnaryOp::BitComplement => out.push("not  %eax\n".to_string()),
                        UnaryOp::Negation => {
                            out.push("cmpl  $0, %eax\n".to_string());
                            out.push("movl  $0, %eax\n".to_string());
                            out.push("sete  %al\n".to_string());
                        }
                    }
                }
                Syntax::BinaryOp(op) => match op {
                    BinaryOp::Add => {
                        recurse(ast, children[0], out);
                        out.push("push  %rax\n".to_string());
                        recurse(ast, children[1], out);
                        out.push("pop  %rcx\naddl  %ecx, %eax\n".to_string());
                    }
                    BinaryOp::Mult => {
                        recurse(ast, children[0], out);
                        out.push("push  %rax\n".to_string());
                        recurse(ast, children[1], out);
                        out.push("pop  %rcx\nimul  %ecx, %eax\n".to_string());
                    }
                    BinaryOp::Minus => {
                        recurse(ast, children[1], out);
                        out.push("push  %rax\n".to_string());
                        recurse(ast, children[0], out);
                        out.push("pop  %rcx\nsubl  %ecx, %eax\n".to_string());
                    }
                    BinaryOp::Div => {
                        recurse(ast, children[1], out);
                        out.push("movl  %eax, %r8d\n".to_string());
                        recurse(ast, children[0], out);
                        out.push("cdq\nidivl  %r8d\n".to_string());
                    }
                },
            }
        }

        recurse(self, curr_node, &mut out);

        out.push("\n".to_string());
        out.join("")
    }

    fn insert(
        &mut self,
        value: Syntax<'a>,
        children: Option<SmallVec<[usize; SVEC_SIZE]>>,
    ) -> usize {
        self.nodes.push(value);
        match children {
            Some(c) => self.children.push(c),
            None => self.children.push(SmallVec::<[usize; SVEC_SIZE]>::new()),
        }
        self.nodes.len() - 1
    }
}

// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function<'a>(tokens: &mut PeekableTokens<'a>, ast: &mut AST<'a>) -> Result<usize, String> {
    match tokens.next() {
        Some(Token::Keyword(Keyword::Int)) => {}
        Some(Token::Keyword(_)) => return Err("Unexpected keyword".to_string()),
        Some(t) => return Err(format!("Expected function return type but got: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    }

    let func_name = match tokens.next() {
        Some(Token::Identifier(s)) => s,
        Some(t) => return Err(format!("Unknown identifier: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    };

    match tokens.next() {
        Some(Token::OpenParens) => {}
        Some(t) => return Err(format!("Expected opening parens but got: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    }
    match tokens.next() {
        Some(Token::CloseParens) => {}
        Some(t) => return Err(format!("Expected closing parens but got: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    }
    match tokens.next() {
        Some(Token::OpenBrace) => {}
        Some(t) => return Err(format!("Expected opening braces but got: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    }

    let statement = parse_statement(tokens, ast)?;

    let function = ast.insert(Syntax::Function(func_name), Some(smallvec![statement]));

    match tokens.next() {
        Some(Token::CloseBrace) => {}
        Some(t) => return Err(format!("Expected closing brace but got: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    }

    Ok(function)
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(tokens: &mut PeekableTokens, ast: &mut AST) -> Result<usize, String> {
    match tokens.next() {
        Some(Token::Keyword(Keyword::Return)) => {}
        Some(t) => return Err(format!("Expected return keyword but got: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    }

    let exp = parse_expression(tokens, ast)?;

    let statement = ast.insert(Syntax::Statement(Statement::Return), Some(smallvec![exp]));

    match tokens.next() {
        Some(Token::Semicolon) => {}
        Some(t) => return Err(format!("Expected semicolon but got: {}", t)),
        None => return Err("Unexpected end of file".to_string()),
    }

    Ok(statement)
}

// <exp> ::= <term> { ("+" | "-") <term> }
fn parse_expression(tokens: &mut PeekableTokens, ast: &mut AST) -> Result<usize, String> {
    let mut term = parse_term(tokens, ast)?;

    loop {
        match tokens.peek() {
            Some(Token::BinaryOp(BinaryOp::Add)) => {
                tokens.next().expect("Unexpected end of file");

                let next_term = parse_term(tokens, ast)?;
                term = ast.insert(
                    Syntax::BinaryOp(BinaryOp::Add),
                    Some(smallvec![term, next_term]),
                );
            }
            Some(Token::Minus) => {
                tokens.next().expect("Unexpected end of file");

                let next_term = parse_term(tokens, ast)?;
                term = ast.insert(
                    Syntax::BinaryOp(BinaryOp::Minus),
                    Some(smallvec![term, next_term]),
                );
            }
            _ => break,
        }
    }

    Ok(term)
}

// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(tokens: &mut PeekableTokens, ast: &mut AST) -> Result<usize, String> {
    let mut factor = parse_factor(tokens, ast)?;

    loop {
        match tokens.peek() {
            Some(Token::BinaryOp(BinaryOp::Mult)) | Some(Token::BinaryOp(BinaryOp::Div)) => {
                if let Some(Token::BinaryOp(op)) = tokens.next() {
                    let next_factor = parse_factor(tokens, ast)?;
                    factor =
                        ast.insert(Syntax::BinaryOp(*op), Some(smallvec![factor, next_factor]));
                }
            }
            _ => break,
        }
    }

    Ok(factor)
}

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(tokens: &mut PeekableTokens, ast: &mut AST) -> Result<usize, String> {
    match tokens.next() {
        Some(Token::OpenParens) => {
            let exp = parse_expression(tokens, ast)?;
            match tokens.next() {
                Some(Token::CloseParens) => Ok(exp),
                _ => Err("Expected closing parens after expression".to_string()),
            }
        }
        Some(Token::UnaryOp(op)) => {
            let factor = parse_factor(tokens, ast)?;
            let op = ast.insert(Syntax::UnaryOp(*op), Some(smallvec![factor]));

            Ok(op)
        }
        Some(Token::Minus) => {
            let factor = parse_factor(tokens, ast)?;
            let op = ast.insert(Syntax::UnaryOp(UnaryOp::Minus), Some(smallvec![factor]));

            Ok(op)
        }
        Some(Token::IntLiteral(num)) => match num.parse::<isize>() {
            Ok(i) => {
                let index = ast.insert(Syntax::Constant(i), None);
                Ok(index)
            }
            Err(why) => Err(why.to_string()),
        },
        Some(t) => Err(format!("Unexpected token: {}", t)),
        None => Err("Unexpected end of file".to_string()),
    }
}

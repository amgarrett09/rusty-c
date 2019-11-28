use crate::lexer::{Token, UnaryOp};
use smallvec::{smallvec, SmallVec};
use std::slice::Iter;

const SVEC_SIZE: usize = 5;

#[derive(Debug)]
enum Expression {
    Constant(isize),
}

#[derive(Debug)]
enum Statement {
    Return,
}

#[derive(Debug)]
enum Syntax<'a> {
    Function(&'a str),
    Statement(Statement),
    Expression(Expression),
    UnaryOp(UnaryOp),
}

pub struct AST<'a> {
    nodes: Vec<Syntax<'a>>,
    children: Vec<SmallVec<[usize; SVEC_SIZE]>>,
    root_node: usize,
}

impl<'a> AST<'a> {
    pub fn new(tokens: &[Token<'a>]) -> Result<Self, String> {
        let mut ast = AST {
            nodes: Vec::new(),
            children: Vec::new(),
            root_node: 0,
        };

        ast.nodes.reserve(tokens.len());
        ast.children.reserve(tokens.len());

        let mut token_iter = tokens.iter();
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
                Syntax::Expression(exp) => match exp {
                    Expression::Constant(i) => {
                        out.push(format!("movl  ${}, %eax\n", i));
                    }
                },
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
fn parse_function<'a>(tokens: &mut Iter<Token<'a>>, ast: &mut AST<'a>) -> Result<usize, String> {
    match tokens.next() {
        Some(Token::Keyword(s)) => {
            if s != &"int" {
                return Err(format!("Unexpected keyword: {}", s));
            }
        }
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
fn parse_statement(tokens: &mut Iter<Token>, ast: &mut AST) -> Result<usize, String> {
    match tokens.next() {
        Some(Token::Keyword(s)) => {
            if s != &"return" {
                return Err(format!("Unknown keyword: {}", s));
            }
        }
        Some(t) => return Err(format!("Expected keyword but got: {}", t)),
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

// <exp> ::= <unary_op> <exp> | <int>
fn parse_expression(tokens: &mut Iter<Token>, ast: &mut AST) -> Result<usize, String> {
    match tokens.next() {
        Some(Token::IntLiteral(num)) => match num.parse::<isize>() {
            Ok(i) => {
                let index = ast.insert(Syntax::Expression(Expression::Constant(i)), None);

                Ok(index)
            }
            Err(why) => Err(why.to_string()),
        },
        Some(Token::UnaryOp(op)) => {
            let inner_expression = parse_expression(tokens, ast)?;

            let index = ast.insert(Syntax::UnaryOp(*op), Some(smallvec![inner_expression]));
            Ok(index)
        }
        Some(Token::Minus) => {
            let inner_expression = parse_expression(tokens, ast)?;

            let index = ast.insert(
                Syntax::UnaryOp(UnaryOp::Minus),
                Some(smallvec![inner_expression]),
            );
            Ok(index)
        }
        Some(t) => Err(format!("Unexpected token: {}", t)),
        None => Err("Unexpected end of file".to_string()),
    }
}

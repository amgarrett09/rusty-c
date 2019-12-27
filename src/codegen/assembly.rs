use crate::enums::{BinaryOp, Statement, Syntax, UnaryOp};
use crate::parser::AST;

pub fn generate_assembly(ast: &AST) -> String {
    let mut out: Vec<String> = Vec::new();
    let curr_node = ast.root_node;

    recurse(ast, curr_node, &mut out);

    out.push("\n".to_string());
    return out.join("");

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
                    out.push("pop  %r10\naddl  %r10d, %eax\n".to_string());
                }
                BinaryOp::Mult => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push("pop  %r10\nimul  %r10d, %eax\n".to_string());
                }
                BinaryOp::Minus => {
                    recurse(ast, children[1], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[0], out);
                    out.push("pop  %r10\nsubl  %r10d, %eax\n".to_string());
                }
                BinaryOp::Div => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push("movl  %eax, %r11d\npop  %rax\n".to_string());
                    out.push("cdq\nidivl  %r11d\n".to_string());
                }
                BinaryOp::Equal => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push(
                        "pop  %r10\ncmpl  %eax, %r10d\nmovl  $0, %eax\nsete %al\n".to_string(),
                    );
                }
                BinaryOp::NotEqual => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push(
                        "pop  %r10\ncmpl  %eax, %r10d\nmovl  $0, %eax\nsetne %al\n".to_string(),
                    );
                }
                BinaryOp::GreaterThan => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push(
                        "pop  %r10\ncmpl  %eax, %r10d\nmovl  $0, %eax\nsetg %al\n".to_string(),
                    );
                }
                BinaryOp::GreaterThanEqual => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push(
                        "pop  %r10\ncmpl  %eax, %r10d\nmovl  $0, %eax\nsetge %al\n".to_string(),
                    );
                }
                BinaryOp::LessThan => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push(
                        "pop  %r10\ncmpl  %eax, %r10d\nmovl  $0, %eax\nsetl %al\n".to_string(),
                    );
                }
                BinaryOp::LessThanEqual => {
                    recurse(ast, children[0], out);
                    out.push("push  %rax\n".to_string());
                    recurse(ast, children[1], out);
                    out.push(
                        "pop  %r10\ncmpl  %eax, %r10d\nmovl  $0, %eax\nsetle %al\n".to_string(),
                    );
                }
                BinaryOp::LogOr => {
                    let clause_label = format!("_clause{}", node);
                    let end_label = format!("_end{}", node);

                    recurse(ast, children[0], out);
                    out.push(format!("cmpl  $0, %eax\nje  {}\n", clause_label));
                    out.push(format!("movl  $1, %eax\njmp  {}\n", end_label));

                    out.push(format!("{}:\n", clause_label));
                    recurse(ast, children[1], out);
                    out.push("cmpl  $0, %eax\nmovl  $0, %eax\nsetne  %al\n".to_string());
                    out.push(format!("{}:\n", end_label));
                }
                BinaryOp::LogAnd => {
                    let clause_label = format!("_clause{}", node);
                    let end_label = format!("_end{}", node);

                    recurse(ast, children[0], out);
                    out.push(format!("cmpl  $0, %eax\njne {}\n", clause_label));
                    out.push(format!("jmp  {}\n", end_label));

                    out.push(format!("{}:\n", clause_label));
                    recurse(ast, children[1], out);
                    out.push("cmpl  $0, %eax\nmovl $0, %eax\nsetne  %al\n".to_string());
                    out.push(format!("{}:\n", end_label));
                }
            },
        }
    }
}

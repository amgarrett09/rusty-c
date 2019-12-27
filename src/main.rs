use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;

mod codegen;
mod enums;
mod lexer;
mod parser;

use codegen::assembly;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let path_string = match args.get(1) {
        Some(val) => val,
        None => panic!("File path expected but none provided"),
    };

    let path = Path::new(&path_string);
    let mut file = File::open(&path)?;
    let mut buffer = String::new();

    file.read_to_string(&mut buffer)?;

    let token_list = lexer::lex(&buffer);
    println!("{:?}", token_list);

    let ast = match parser::AST::new(&token_list) {
        Ok(t) => t,
        Err(why) => {
            println!("{}", why);
            return Ok(());
        }
    };

    ast.pretty_print();

    let asm = assembly::generate_assembly(&ast);

    let out_string = path_string
        .split('.')
        .next()
        .expect("Expected input file path to exist");
    let out_string = format!("{}.s", out_string);
    let out_path = Path::new(&out_string);
    let mut out_file = File::create(&out_path)?;
    out_file.write_all(asm.as_bytes())?;

    Ok(())
}

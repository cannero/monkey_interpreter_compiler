use std::io;

use libinterpreter::evaluator::Evaluator;
use libinterpreter::lexer::Lexer;
use libinterpreter::parser::Parser;

fn main() {
    start_repl();
}

fn start_repl() {
    println!("Enter code, empty line to quit.");
    loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");
        let input = input.trim();
        println!("input: <{}>", input);
        if input == "" {
            break;
        }
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        if !parser.errors().is_empty() {
            println!("ERRORS:");
            println!("{:?}", parser.errors());
            continue;
        }

        let evaluator = Evaluator::default();
        println!("{:?}", evaluator.eval(program));
    }
}

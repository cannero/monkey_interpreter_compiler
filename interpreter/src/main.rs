use std::io;

use libinterpreter::lexer::Lexer;

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
        for token in lexer {
            println!("{:?}", token);
        }
    }
}

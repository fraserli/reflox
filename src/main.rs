mod lexer;
mod parser;

use parser::Parser;

use anyhow::Result;

use std::io::Write;

fn main() -> Result<()> {
    loop {
        print!("\x1b[1;34m[reflox]>\x1b[0m ");
        std::io::stdout().flush()?;

        let mut line = String::new();
        if std::io::stdin().read_line(&mut line)? != 0 {
            let tokens = lexer::lex(&line)?;
            let mut parser = Parser::new(tokens);
            parser.parse()?;
        } else {
            break;
        }
    }

    Ok(())
}

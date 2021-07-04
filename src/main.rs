use std::io;
use calcr::engine::Calcr;

use ansi_term::Colour::{Red, Green, RGB};

fn main() {
    let mut calcr = Calcr::default();

    let _ = ansi_term::enable_ansi_support(); // enables ANSI support

    let gray = RGB(175, 175, 175);

    println!("{}", gray.italic().paint("Welcome back..."));
    
    let error = Red.bold().paint("error");
    
    loop {
        let mut buffer = String::new();
        
        io::stdin()
        .read_line(&mut buffer)
        .expect("An error occured while trying to read the input.");
        
        match calcr.evaluate(&buffer) {
            Ok(result) => println!("{} {}", Green.bold().paint(">"), result),
            Err(token_error) => {
                let colored_token_error = gray.paint(format!("{}", token_error));
                
                eprintln!("{}: {}", error, colored_token_error);
            },
        }
    }
}
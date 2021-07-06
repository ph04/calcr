use std::io;
use calcr::engine::Calcr;
use ansi_term::Colour::{Red, Green, RGB};

fn main() {
    let mut calcr = Calcr::new();

    if cfg!(windows) {
        let _ = ansi_term::enable_ansi_support(); // enables ANSI support for windows
    }

    let gray = RGB(175, 175, 175);

    clearscreen::clear().expect("An error occured while trying to clear the screen.");
    
    println!("{}", gray.italic().paint("Welcome back..."));

    let red_error = Red.bold().paint("error");
    
    let green_arrow = Green.bold().paint(">");

    loop {
        let mut buffer = String::new();
        
        io::stdin()
        .read_line(&mut buffer)
        .expect("An error occured while trying to read the input.");
        
        match calcr.evaluate(&buffer) {
            Ok(result) => {
                print!("{} ", green_arrow);

                if calcr.ratio_flag {
                    println!("{}", unsafe { result.float_unchecked() });
                } else {
                    println!("{}", result);
                }
            },
            Err(token_error) => {
                let colored_token_error = gray.paint(format!("{}", token_error));
                
                eprintln!("{}: {}", red_error, colored_token_error);
            },
        }
    }
}
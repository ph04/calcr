use std::io;
use calcr::engine::Calcr;
use ansi_term::Colour::{Red, Green, RGB};
use structopt::StructOpt;

// hex flag is not available from the command line
// because it can conflict with the ratio flag
#[derive(StructOpt, Debug)]
#[structopt(name = "calcr")]
struct Opt {
    /// Toggle debug flag.
    #[structopt(short, long)]
    debug: bool,

    /// Toggle ratio flag.
    #[structopt(short, long)]
    ratio: bool
}

fn main() {
    let opt = Opt::from_args();

    let mut calcr = Calcr::new();

    if opt.debug {
        calcr.debug_flag = true
    }

    if opt.ratio {
        calcr.ratio_flag = true
    }

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
                    println!("{}", result);
                } else if calcr.hex_flag {
                    let r = result.try_integer();

                    match r {
                        Ok(i) => println!("{:#X}", i),
                        Err(math_error) => {
                            let colored_math_error = gray.paint(format!("{}", math_error));
                            
                            eprintln!("{}: {}", red_error, colored_math_error);
                        },
                    }
                } else {
                    println!("{}", unsafe { result.float_unchecked() })
                }
            },
            Err(token_error) => {
                let colored_token_error = gray.paint(format!("{}", token_error));
                
                eprintln!("{}: {}", red_error, colored_token_error);
            },
        }
    }
}
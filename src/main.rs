use std::io;
use calcr::engine::Calcr;

fn main() {
    let mut calcr = Calcr::default();

    loop {
        let mut buffer = String::new();
        
        io::stdin()
        .read_line(&mut buffer)
        .expect("An error occured while trying to read the input.");
        
        // if let Some(result) = calcr.evaluate(&buffer) {
        //     println!("{:?}", result);
        // }
        match calcr.evaluate(&buffer) {
            Ok(result) => println!("> {}", result),
            Err(token_error) => eprintln!("Error while evaluating the input: {}", token_error),
        }

        // println!("{:?}", calcr.vars())
    }
}
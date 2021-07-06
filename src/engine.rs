use crate::{
    fraction::{Fraction, ZERO_FRACTION, PI_FRACTION},
    token::{Ops, Cmd, Token, TokenError}
};
// use std::{process, f32};
// use std::{convert::TryInto, f32};
use std::{collections::HashMap, f32, iter::FromIterator};
use ansi_term::Colour::RGB;
// use clearscreen::clear;
use std::fmt::Write;

// const HELP: String = build_help();

// fn build_help() -> String {
//     let gray = RGB(175, 175, 175);

//     let mut help_message = String::new();

//     writeln!(help_message, "Currently supported commands:");
//     writeln!(help_message, "{} {}\n", gray.italic().paint("Note:"), gray.paint("if a command takes more parameters than what it requires, the exceeding ones will be ignored"));
//     writeln!(help_message, "- {}: exits the program; takes no parameter", gray.paint("\\exit"));
//     writeln!(help_message, "- {}: prints this help message; takes no parameter", gray.paint("\\help"));
//     writeln!(help_message, "");

//     help_message
// }

/// The struct of the engine of the calculator. The calculator
/// takes the input, tokenizes it with the lexer, then it
/// parses the tokens returning a postfix expression of the
/// given input, and then evaluates the postfix expression.
// TODO: examples
#[derive(Debug, Clone, PartialEq)]
pub struct Calcr {
    pub ratio_flag: bool,
    debug_flag: bool,

    // vars: Vec<Token>,
    // vars: HashMap<Token, Option<Fraction>>,
    consts: [Token; 1],
    vars: HashMap<Token, Fraction>,
}

impl Calcr {
    // TODO: docs and examples
    pub fn new() -> Self {
        // let mut vars = HashMap::new();
        // let mut vars = Vec::new();
        let mut vars = HashMap::new();

        vars.insert(Token::Variable("pi".to_string()), PI_FRACTION);
        // vars.insert(Token::Variable("pi".to_string()), Some(PI_FRACTION));
        // vars.insert(Token::Variable("e".clone()), E_FRACTION);
        // vars.insert(Token::Variable("tau".clone()), TAU_FRACTION);


        // FIXME: find a better way to do this, it's absolutely terrible
        // let pi = "pi".to_string();
        // let e = "e".to_string();
        // let tau = "tau".to_string();

        // vars.insert(pi.clone(), Token::Variable(pi, Some(f32::consts::PI)));
        // vars.insert(e.clone(), Token::Variable(e, Some(f32::consts::E)));
        // vars.insert(tau.clone(), Token::Variable(tau, Some(f32::consts::TAU)));
        
        // vars.insert(Token::Variable(pi), Some(f32::consts::PI));

        Self {
            debug_flag: false,
            ratio_flag: false,
            vars,
        }
    }

    /// Returns the stored variables.
    // TODO: examples
    // pub fn vars(&self) -> Vec<Token> {
    //     self.vars.clone()
    // }
    // pub fn vars(&self) -> HashMap<String, Token> {
    //     self.vars.clone()
    // }

    /// Tokenizes the given input.
    /// - numbers are tokenized as `Token::Number(number)`
    /// - variables are tokenizes as `Token::Variable(variable)`
    /// - `(` is tokenized as `Token::LeftBracket`
    /// - `)` is tokenized as `Token::RightBracket`
    /// - `&` is tokenized as `Token::Operators(Ops::And)`
    /// - `|` is tokenized as `Token::Operators(Ops::Or)`
    /// - `+` is tokenized as `Token::Operators(Ops::Add)`
    /// - `-` is tokenized as `Token::Operators(Ops::Sub)`
    /// - `*` is tokenized as `Token::Operators(Ops::Mul)`
    /// - `/` is tokenized as `Token::Operators(Ops::Div)`
    /// - `^` is tokenized as `Token::Operators(Ops::Pow)`
    /// - `!` is tokenized as `Token::Operators(Ops::Fac)`
    /// - `\r`, `\n` and ` ` are ignored
    /// - every other character is tokenized as `Token::Unknown(character)`
    // TODO: examples
    fn lexer(&mut self, input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();

        let mut iter = input.char_indices();
        let mut to_skip = 0;

        while let Some((idx, ch)) = iter.by_ref().nth(to_skip) {
            to_skip = 0;

            match ch {
                '0'..='9' => {
                    if let Some(prev) = tokens.iter().last() {
                        if prev == &Token::RightBracket {
                            tokens.push(Token::Operators(Ops::Mul))
                        }
                    }

                    to_skip = input[idx..]
                        .bytes()
                        .take_while(|c| c.is_ascii_digit())
                        .count();

                    let number = input[idx..idx + to_skip]
                        .chars()
                        .map(|c| c.to_digit(10).unwrap() as f32) // this will never panic
                        .fold(0.0, |acc, d| acc * 10.0 + d);

                    tokens.push(Token::Number(Fraction::from(number)));

                    to_skip -= 1;
                },
                'a'..='z' | 'A'..='Z' => {
                    if let Some(prev) = tokens.iter().last() {
                        match prev {
                            Token::RightBracket | Token::Number(_) => tokens.push(Token::Operators(Ops::Mul)),
                            _ => {},
                        }
                    }

                    to_skip = input[idx..]
                        .bytes()
                        .take_while(|c| c.is_ascii_alphabetic())
                        .count();

                    let string: String = input[idx..idx + to_skip].into();

                    if let Some(Token::CommandToken) = tokens.iter().last() {
                        let command = match string.as_str() {
                            "exit" => Cmd::Exit,
                            "help" => Cmd::Help,
                            "clear" => Cmd::Clear,
                            "debug" => Cmd::Debug,
                            "ratio" => Cmd::Ratio,
                            _ => Cmd::Unknown(string),
                        };

                        tokens.pop().unwrap();

                        tokens.push(Token::Command(command))
                    } else {
                        // FIXME: HERE
                        // let name = Token::Variable(string);

                        // if let Some(value) = self.vars.get(&name) {
                        //     tokens.push(name)
                        // } else {

                        // }
                        tokens.push(Token::Variable(string));
                    }

                    to_skip -= 1;
                },
                '\\' => tokens.push(Token::CommandToken),
                '(' => {
                    if let Some(prev) = tokens.iter().last() {
                        match prev {
                            Token::Number(_) | Token::Variable(..) | Token::RightBracket => tokens.push(Token::Operators(Ops::Mul)),
                            _ => {},
                        }
                    }

                    tokens.push(Token::LeftBracket);
                },
                ')' => tokens.push(Token::RightBracket),
                '&' => tokens.push(Token::Operators(Ops::And)),
                '|' => tokens.push(Token::Operators(Ops::Or)),
                '+' => {
                    // TODO: maybe a macro?
                    if let Some(prev) = tokens.iter().last() {
                        if prev == &Token::LeftBracket {
                            tokens.push(Token::Number(ZERO_FRACTION))
                        }
                    } else {
                        tokens.push(Token::Number(ZERO_FRACTION))
                    }

                    tokens.push(Token::Operators(Ops::Add));
                },
                '-' => {
                    // TODO: maybe a macro?
                    if let Some(prev) = tokens.iter().last() {
                        if prev == &Token::LeftBracket {
                            tokens.push(Token::Number(ZERO_FRACTION));
                        }
                    } else {
                        tokens.push(Token::Number(ZERO_FRACTION));
                    }

                    tokens.push(Token::Operators(Ops::Sub));
                },
                '*' => tokens.push(Token::Operators(Ops::Mul)),
                '/' => tokens.push(Token::Operators(Ops::Div)),
                '^' => tokens.push(Token::Operators(Ops::Pow)),
                '!' => tokens.push(Token::Operators(Ops::Fac)),
                '=' => tokens.push(Token::Equal),
                '\r' | '\n' | ' ' => {},
                _ => tokens.push(Token::Unknown(ch)),
            }
        }

        tokens
    }

    /// Returns the postfix expression of the given input.
    /// Returns `Ok(Vec<Token>)` if there are no syntax errors in the input,
    /// otherwise it returns an `Err(TokenError)`.
    // explain when each error is returned
    // TODO: examples
    fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<Token>, TokenError> {
        let mut buffer: Vec<Token> = Vec::new();
        let mut stack: Vec<Token> = Vec::new();
        
        for token in tokens {
            match token {
                Token::LeftBracket => stack.push(token),
                Token::RightBracket => {
                    stack
                        .iter()    
                        .rev()
                        .take_while(|t| **t != Token::LeftBracket)
                        .for_each(|t| buffer.push(t.clone()));

                    if let Some(index) = stack.iter().rev().position(|t| *t == Token::LeftBracket) {
                        stack.truncate(stack.len() - 1 - index);
                    } else {
                        return Err(TokenError::MisplacedBrackets);
                    }
                },
                Token::Operators(operator) => {
                    while let Some(Token::Operators(last_operator)) = stack.iter().last() {
                        if last_operator.precedence() >= operator.precedence() {
                            let last = stack.pop().unwrap();

                            buffer.push(last);
                        } else {
                            break;
                        }
                    }
                    
                    stack.push(token.clone())
                },
                Token::Number(_) => buffer.push(token),
                Token::Variable(..) => buffer.push(token.clone()),
                // Token::Variable(..) => {
                //     buffer.push(token.clone());

                //     if !self.vars.contains(&token) {
                //         self.vars.push(token);
                //     }
                // }
                Token::Unknown(c) => return Err(TokenError::UnknownToken(c)),
                _ => return Err(TokenError::UnknownError(0)),
            }
            // TODO: remove these when everything works
            // println!("stack = {:?}", stack);
            // println!("buffer = {:?}", buffer);
        }

        while !stack.is_empty() {
            buffer.push(stack.pop().unwrap())
        }

        // in the case where there are no `RightBracket`s in
        // the given input, the parser has no way to know if
        // there are any unclosed `LeftBracket`s, thus this
        // check makes sure to handle this edge case
        if buffer.contains(&Token::LeftBracket) {
            return Err(TokenError::MisplacedBrackets)
        }

        Ok(buffer)
    }

    fn evaluate_postfix(&self, expression: Vec<Token>) -> Result<Fraction, TokenError> {
    // fn evaluate_postfix(&self, expression: &[&Token]) -> Result<Fraction, TokenError> {
        let mut stack = Vec::new();

        for token in expression {
            match token {
                Token::Number(number) => stack.push(number),
                Token::Variable(ref name) => {
                    if let Some(value) = self.vars.get(&token) {
                        stack.push(*value);
                    } else {
                        return Err(TokenError::UnknownVariable(name.clone()))
                    }
                }
                // Token::Variable(name, value) => {
                //     if let Some(v) = value {
                //         stack.push(Fraction::from(v))
                //     } else {
                //         return Err(TokenError::UnknownVariable(name))
                //     }
                // },
                Token::Operators(operator) => {
                    let first = stack.pop();
                    let second = stack.pop();

                    match (first, second) {
                        // note: they are inverted
                        (Some(second_number), Some(first_number)) => {
                            let result = match operator {
                                // FIXME: BITWISE OPERATORS ARE SUPER BROKEN
                                // Ops::And => {
                                //     if first_number.fract() == 0.0 && second_number.fract() == 0.0 {
                                //         let f = unsafe { first_number.to_int_unchecked::<isize>() };
                                //         let s = unsafe { first_number.to_int_unchecked::<isize>() };

                                //         (f & s) as f32
                                //     } else {
                                //         return Err(TokenError::InvalidBitwiseOperands);
                                //     }
                                // },
                                // Ops::Or => {
                                //     if first_number.fract() == 0.0 && second_number.fract() == 0.0 {
                                //         let f = unsafe { first_number.to_int_unchecked::<isize>() };
                                //         let s = unsafe { first_number.to_int_unchecked::<isize>() };

                                //         (f | s) as f32
                                //     } else {
                                //         return Err(TokenError::InvalidBitwiseOperands);
                                //     }
                                // },
                                Ops::Add => first_number + second_number,
                                Ops::Sub => first_number - second_number,
                                Ops::Mul => first_number * second_number,
                                Ops::Div => {
                                    if let Ok(q) = first_number / second_number {
                                        q
                                    } else {
                                        return Err(TokenError::DivisionByZero)
                                    }
                                },
                                // TODO: fix
                                // Ops::Pow => first_number.pow(second_number),
                                _ => return Err(TokenError::UnimplementedOperator(operator)),
                            };

                            stack.push(result);
                        },
                        _ => return Err(TokenError::MissingOperands),
                    }
                },
                _ => return Err(TokenError::UnknownError(1)),
            }
        }

        if stack.len() > 1 {
            return Err(TokenError::MissingOperators)
        }

        Ok(stack[0])
    }

    // TODO: documentation
    // TODO: examples
    // TODO: debug command to show vecs and stuff (probably it's a mess because it's a global flag, fuck)
    pub fn evaluate(&mut self, input: &str) -> Result<Fraction, TokenError> {
        let tokens = self.lexer(input);
        
        if tokens.is_empty() {
            return Ok(ZERO_FRACTION);
        }

        if tokens.len() > 1 {
            for window in tokens.windows(2) {
                if let (Token::Number(_) | Token::Variable(..), Token::Number(_) | Token::Variable(..)) = (&window[0], &window[1]) {
                    return Err(TokenError::MissingOperators)
                }
            }
        }

        // command check
        for (idx, token) in tokens.iter().enumerate() {
            match (idx, token) {
                (0, Token::Command(Cmd::Exit)) => {
                    // TODO: better way to exit the program, too rough and ugly message
                    println!("A nicer way to exit from the program is still WIP...");

                    // clearscreen::clear().expect("An error occured while trying to clear the screen.");
                    // process::exit(0x100);
                    quit::with_code(0x100);
                },
                (0, Token::Command(Cmd::Help)) => {
                    // TODO: make this help message a constant, this way is too dumb

                    println!("WIP going on here");
                    return Ok(ZERO_FRACTION);
                },
                (0, Token::Command(Cmd::Clear)) => {
                    clearscreen::clear().expect("An error occured while trying to clear the screen.");

                    return Ok(ZERO_FRACTION);
                },
                (0, Token::Command(Cmd::Debug)) => {
                    self.debug_flag = !self.debug_flag;

                    println!("Debug flag toggled.");

                    return Ok(ZERO_FRACTION);
                },
                (0, Token::Command(Cmd::Ratio)) => {
                    self.ratio_flag = !self.ratio_flag;

                    println!("Ratio flag toggled.");

                    return Ok(ZERO_FRACTION);
                },
                (0, Token::Command(Cmd::Unknown(name))) => return Err(TokenError::UnknownCommand(name.clone())),
                (_, Token::Command(cmd)) => return Err(TokenError::InvalidCommandSyntax(cmd.clone())),
                _ => {},
            }
        }
        // println!("{:?}", tokens.iter().take_while(|t| **t == Token::Equal).collect::<Vec<&Token>>());

        if tokens.iter().filter(|t| **t == Token::Equal).count() > 1 {
            return Err(TokenError::InvalidDefinitionSyntax)
        }
        // println!("{:?}", tokens);

        if tokens.contains(&Token::Equal) {
            if tokens.iter().take_while(|t| **t != Token::Equal).count() == 1 {
                // the statement above makes sure that `tokens[0]` always exists
                let first_token = tokens[0].clone();

                match first_token {
                    // Token::Variable(name, value) => {
                    Token::Variable(ref name) => {
                        // let rhs: Vec<(usize, &Token)> = tokens
                        let rhs: Vec<Token> = tokens
                            .clone()
                            .into_iter()
                            // .enumerate()
                            // .skip_while(|(_, t)| **t != Token::Equal)
                            .skip_while(|t| t != &Token::Equal)
                            .skip(1)
                            .collect();

                        println!("RHS = {:?}", rhs);

                        if rhs.is_empty() {
                            return Err(TokenError::InvalidDefinitionSyntax);
                        }

                        // let rhs_input = &input[rhs[0].0 + 1..];
                        // println!("RHS INPUT = {:?}", rhs_input);

                        let rhs_parsed = self.parse(rhs);

                        match rhs_parsed {
                            Ok(parsed) => {
                                let rhs_evaluated = self.evaluate_postfix(parsed);

                                match rhs_evaluated {
                                    Ok(result) => {
                                        println!("somehow it works lmao");

                                        println!("{:?}", result);

                                        if let Some(value) = self.vars.get_mut(&first_token) {
                                            *value = result;
                                        } else {
                                            // FIXME: some other stuff i still have to think
                                        }
                                    },
                                    Err(token_error) => {println!("here"); return Err(token_error)}
                                }
                            },
                            Err(token_error) => return Err(token_error),
                        }
                    },
                    _ => return Err(TokenError::InvalidDefinitionSyntax)
                }
            } else {
                return Err(TokenError::InvalidDefinitionSyntax)
            }
        }

        // if self.debug_flag {
        //     println!("Tokenized input: {:?}", tokens);
        // }
        
        // let postfix = self.parse(tokens);

        // if self.debug_flag {
        //     println!("Postfix expression: {:?}", postfix);
        // }

        let postfix = self.parse(tokens);

        match postfix {
            Ok(expression) => {
                return self.evaluate_postfix(expression)
            },
            Err(token_error) => Err(token_error), // cringe
        }
    }
}

// TODO: write tests for commands
// TODO: write tests for bit operators
// TODO: ^
// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn parse_tests() {
//         let mut calcr = Calcr::new();

//         let tokens1 = calcr.lexer("((1 + 2) * (3 - 4))");
                
//         assert_eq!(calcr.parse(tokens1), Ok(vec![
//             Token::Number(1.0),
//             Token::Number(2.0),
//             Token::Operators(Ops::Add),
//             Token::Number(3.0),
//             Token::Number(4.0),
//             Token::Operators(Ops::Sub),
//             Token::Operators(Ops::Mul),
//         ]));

//         let tokens2 = calcr.lexer("1 * 2 - 3 * 4");

//         assert_eq!(calcr.parse(tokens2), Ok(vec![
//             Token::Number(1.0),
//             Token::Number(2.0),
//             Token::Operators(Ops::Mul),
//             Token::Number(3.0),
//             Token::Number(4.0),
//             Token::Operators(Ops::Mul),
//             Token::Operators(Ops::Sub),
//         ]));

//         let tokens3 = calcr.lexer("1 + 2 - 3 + 4");

//         assert_eq!(calcr.parse(tokens3), Ok(vec![
//             Token::Number(1.0),
//             Token::Number(2.0),
//             Token::Operators(Ops::Add),
//             Token::Number(3.0),
//             Token::Operators(Ops::Sub),
//             Token::Number(4.0),
//             Token::Operators(Ops::Add),
//         ]));

//         let tokens4 = calcr.lexer("((((2+4)) ()) () (()3*5))");

//         assert_eq!(calcr.parse(tokens4), Ok(vec![
//             Token::Number(2.0),
//             Token::Number(4.0),
//             Token::Operators(Ops::Add),
//             Token::Operators(Ops::Mul),
//             Token::Operators(Ops::Mul),
//             Token::Number(3.0),
//             Token::Operators(Ops::Mul),
//             Token::Number(5.0),
//             Token::Operators(Ops::Mul),
//             Token::Operators(Ops::Mul),
//         ]));

//         let tokens5 = calcr.lexer("(1 + 4))");

//         assert_eq!(calcr.parse(tokens5), Err(TokenError::MisplacedBrackets));

//         let tokens6 = calcr.lexer("((3 + 2)");

//         assert_eq!(calcr.parse(tokens6), Err(TokenError::MisplacedBrackets));

//         let tokens7 = calcr.lexer("2 / e (3 + pi)");

//         assert_eq!(calcr.parse(tokens7), Ok(vec![
//             Token::Number(2.0),
//             Token::Variable("e".to_string(), Some(f32::consts::E)),
//             Token::Operators(Ops::Div),
//             Token::Number(3.0),
//             Token::Variable("pi".to_string(), Some(f32::consts::PI)),
//             Token::Operators(Ops::Add),
//             Token::Operators(Ops::Mul),
//         ]));

//         let tokens8 = calcr.lexer("(1 + 2) 3");
        
//         assert_eq!(calcr.parse(tokens8), Ok(vec![
//             Token::Number(1.0),
//             Token::Number(2.0),
//             Token::Operators(Ops::Add),
//             Token::Number(3.0),
//             Token::Operators(Ops::Mul),
//         ]));

//         let tokens9 = calcr.lexer("1 + 2 * 3 - 4");

//         assert_eq!(calcr.parse(tokens9), Ok(vec![
//             Token::Number(1.0),
//             Token::Number(2.0),
//             Token::Number(3.0),
//             Token::Operators(Ops::Mul),
//             Token::Operators(Ops::Add),
//             Token::Number(4.0),
//             Token::Operators(Ops::Sub),
//         ]));
        
//         // FIXME: this test right here, and it's not enough anyway
//         let tokens11 = calcr.lexer("3 (8)");

//         assert_eq!(calcr.parse(tokens11), Ok(vec![
//             Token::Number(3.0),
//             Token::Number(8.0),
//             Token::Operators(Ops::Mul),
//         ]));
//     }

//     #[test]
//     fn evaluation_tests() {
//         let mut calcr = Calcr::new();

//         assert_eq!(calcr.evaluate("-2 + 4"), Ok(2.0));

//         assert_eq!(calcr.evaluate("(1 + 5) * 8 - 2 * 3"), Ok(42.0));
        
//         assert_eq!(calcr.evaluate("1 + pi * 2"), Ok(1.0 + f32::consts::PI * 2.0));

//         assert_eq!(calcr.evaluate(" 5 (3 -5)"), Ok(-10.0));

//         assert_eq!(calcr.evaluate("1-"), Err(TokenError::MissingOperands));

//         assert_eq!(calcr.evaluate("14 25"), Err(TokenError::MissingOperators));
        
//         assert_eq!(calcr.evaluate("]"), Err(TokenError::UnknownToken(']')));

//         assert_eq!(calcr.evaluate("pi 3 *"), Err(TokenError::MissingOperators));
//     }

//     #[test]
//     fn lexer_tests() {
//         // TODO: custom variables need to be implemented
//         // let mut calcr = Calcr::default();
//         // 
//         // assert_eq!(calcr.lexer("- ( super/ test) * crazy %+224a-    13"), vec![
//         //     Token::Number(0.0),
//         //     Token::LeftBracket,
//         //     Token::Variable("super".to_string()),
//         //     Token::Operators(Ops::Div),
//         //     Token::Variable("test".to_string()),
//         //     Token::RightBracket,
//         //     Token::Operators(Ops::Mul),
//         //     Token::Variable("crazy".to_string()),
//         //     Token::Unknown('%'),
//         //     Token::Operators(Ops::Add),
//         //     Token::Number(224.0),
//         //     Token::Variable("a".to_string()),
//         //     Token::Operators(Ops::Sub),
//         //     Token::Number(13.0),
//         // ]);

//         // TODO: 3 () 3
//         // TODO: (5) 3
//         // TODO: (5) (3)
//         // TODO: -5 (-3) (+4)
//         // TODO: 2 pi
//         // let tokens10 = Calcr::lexer("5 6 +").unwrap();

//         // assert_eq!(calcr.parse(tokens10), Err(TokenError::MissingOperators));
//     }
// }
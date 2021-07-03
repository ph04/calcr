use crate::token::{Ops, Cmd, Token, TokenError};
use std::{process, f32};

/// The struct of the engine of the calculator. The calculator
/// takes the input, tokenizes it with the `lexer()`, then it
/// parses the tokens returning a postfix expression of the
/// given input, and then evaluates the postfix expression.
// TODO: examples
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Calcr {
    vars: Vec<Token>,
}

impl Calcr {
    /// Returns the stored variables.
    pub fn vars(&self) -> Vec<Token> {
        self.vars.clone()
    }

    /// Tokenizes the given input.
    /// - numbers are tokenized as `Token::Number(number)`
    /// - variables are tokenizes as `Token::Variable(variable)`
    /// - `(` is tokenized as `Token::LeftBracket`
    /// - `)` is tokenized as `Token::RightBracket`
    /// - `+` is tokenized as `Token::Operators(Ops::Add)`
    /// - `-` is tokenized as `Token::Operators(Ops::Sub)`
    /// - `*` is tokenized as `Token::Operators(Ops::Mul)`
    /// - `/` is tokenized as `Token::Operators(Ops::Div)`
    /// - `\r`, `\n` and ` ` are ignored
    /// - every other character is tokenized as `Token::Unknown(character)`
    /// 
    /// **Note**: if there is a `+` or a `-` at the beginning
    /// of the input, a `Token::Number(0.0)` is appended at
    /// the beginning of the `Vec<Token>`, and those are the only
    /// two operands that are allowed to be at the beginning
    /// of the input.
    // TODO: examples
    fn lexer(input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();

        let mut iter = input.char_indices();
        let mut to_skip = 0;

        while let Some((idx, ch)) = iter.by_ref().nth(to_skip) {
            to_skip = 0;

            match ch {
                '0'..='9' => {
                    to_skip = input[idx..]
                        .bytes()
                        .take_while(|c| c.is_ascii_digit())
                        .count();

                    let number = input[idx..idx + to_skip]
                        .chars()
                        .map(|c| c.to_digit(10).unwrap() as f32) // this will never panic
                        .fold(0.0, |acc, d| acc * 10.0 + d);

                    tokens.push(Token::Number(number));

                    to_skip -= 1;
                },
                'a'..='z' | 'A'..='Z' => {
                    to_skip = input[idx..]
                        .bytes()
                        .take_while(|c| c.is_ascii_alphabetic())
                        .count();

                    let string: String = input[idx..idx + to_skip].into();

                    if let Some(Token::CommandToken) = tokens.iter().last() {
                        let command = match string.as_str() {
                            "exit" => Cmd::Exit,
                            "help" => Cmd::Help,
                            _ => Cmd::UnknownCommand,
                        };

                        tokens.pop().unwrap();

                        tokens.push(Token::Command(command))
                    } else {
                        // TODO: implement custom variables
                        let value = match string.as_str() {
                            "pi" => Some(f32::consts::PI),
                            "e" => Some(f32::consts::E),
                            "tau" => Some(f32::consts::TAU),
                            _ => None
                        };

                        tokens.push(Token::Variable(string, value));
                    }

                    to_skip -= 1;
                },
                '\\' => tokens.push(Token::CommandToken),
                '(' => tokens.push(Token::LeftBracket),
                ')' => tokens.push(Token::RightBracket),
                '+' => {
                    if tokens.is_empty() {
                        tokens.push(Token::Number(0.0));
                    }

                    tokens.push(Token::Operators(Ops::Add));
                },
                '-' => {
                    if tokens.is_empty() {
                        tokens.push(Token::Number(0.0));
                    }

                    tokens.push(Token::Operators(Ops::Sub));
                },
                '*' => tokens.push(Token::Operators(Ops::Mul)),
                '/' => tokens.push(Token::Operators(Ops::Div)),
                '^' => tokens.push(Token::Operators(Ops::Pow)),
                '!' => tokens.push(Token::Operators(Ops::Fac)),
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
                Token::Variable(..) => {
                    buffer.push(token.clone());

                    if !self.vars.contains(&token) {
                        self.vars.push(token);
                    }
                }
                Token::Unknown(c) => return Err(TokenError::UnknownToken(c)),
                _ => return Err(TokenError::UnknownError),
            }
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

    // TODO: documentation
    // TODO: examples
    pub fn evaluate(&mut self, input: &str) -> Result<f32, TokenError> {
        let tokens = Self::lexer(input);
        
        for (idx, token) in tokens.iter().enumerate() {
            match (idx, token) {
                (0, Token::Command(Cmd::Exit)) => {
                    println!("bye )/");

                    process::exit(0x100);
                },
                (0, Token::Command(Cmd::Help)) => {
                    // TODO: explain that if the command does not have parameters,
                    // if there are any, they will be ignored
                    // TODO: write the help message
                    println!("* Insert help message here *");

                    return Ok(0.0);
                },
                (_, Token::Command(cmd)) => return Err(TokenError::InvalidCommandSyntax(*cmd)),
                _ => {},
            }
        }

        let postfix = self.parse(tokens);

        // println!("{:?}", postfix);

        match postfix {
            Ok(expression) => {
                let mut stack = Vec::new();

                for token in expression {
                    match token {
                        Token::Number(number) => stack.push(number),
                        Token::Variable(var, value) => {
                            if let Some(v) = value {
                                stack.push(v)
                            } else {
                                return Err(TokenError::UnknownVariable(var))
                            }
                        },
                        Token::Operators(operator) => {
                            let first = stack.pop();
                            let second = stack.pop();

                            match (first, second) {
                                // note: they are inverted
                                (Some(second_number), Some(first_number)) => {
                                    let result = match operator {
                                        Ops::Add => first_number + second_number,
                                        Ops::Sub => first_number - second_number,
                                        Ops::Mul => first_number * second_number,
                                        Ops::Div => first_number / second_number,
                                        _ => return Err(TokenError::UnimplementedOperator(operator)),
                                    };

                                    stack.push(result);
                                },
                                _ => return Err(TokenError::MissingOperands),
                            }
                        },
                        _ => return Err(TokenError::UnknownError),
                    }
                }
                // println!("stack at the end = {:?}", stack);

                // the given input was empty
                if stack.is_empty() {
                    return Ok(0.0)
                }

                if stack.len() > 1 {
                    return Err(TokenError::MissingOperators)
                }

                Ok(stack[0])
            },
            Err(token_error) => Err(token_error),
        }
    }
}


// TODO: write tests for command
// TODO: write tests for other things, once implemented
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_tests() {
        let mut calcr = Calcr::default();

        let tokens1 = Calcr::lexer("((1 + 2) * (3 - 4))");
                
        assert_eq!(calcr.parse(tokens1), Ok(vec![
            Token::Number(1.0),
            Token::Number(2.0),
            Token::Operators(Ops::Add),
            Token::Number(3.0),
            Token::Number(4.0),
            Token::Operators(Ops::Sub),
            Token::Operators(Ops::Mul),
        ]));

        let tokens2 = Calcr::lexer("1 * 2 - 3 * 4");

        assert_eq!(calcr.parse(tokens2), Ok(vec![
            Token::Number(1.0),
            Token::Number(2.0),
            Token::Operators(Ops::Mul),
            Token::Number(3.0),
            Token::Number(4.0),
            Token::Operators(Ops::Mul),
            Token::Operators(Ops::Sub),
        ]));

        let tokens3 = Calcr::lexer("1 + 2 - 3 + 4");

        assert_eq!(calcr.parse(tokens3), Ok(vec![
            Token::Number(1.0),
            Token::Number(2.0),
            Token::Operators(Ops::Add),
            Token::Number(3.0),
            Token::Operators(Ops::Sub),
            Token::Number(4.0),
            Token::Operators(Ops::Add),
        ]));

        let tokens4 = Calcr::lexer("((((2+4)) ()) () (()3*5))");

        assert_eq!(calcr.parse(tokens4), Ok(vec![
            Token::Number(2.0),
            Token::Number(4.0),
            Token::Operators(Ops::Add),
            Token::Number(3.0),
            Token::Number(5.0),
            Token::Operators(Ops::Mul),
        ]));

        let tokens5 = Calcr::lexer("(1 + 4))");

        assert_eq!(calcr.parse(tokens5), Err(TokenError::MisplacedBrackets));

        let tokens6 = Calcr::lexer("((3 + 2)");

        assert_eq!(calcr.parse(tokens6), Err(TokenError::MisplacedBrackets));

        let tokens7 = Calcr::lexer("2 / e * (3 + pi)");

        assert_eq!(calcr.parse(tokens7), Ok(vec![
            Token::Number(2.0),
            Token::Variable("e".to_string(), Some(f32::consts::E)),
            Token::Operators(Ops::Div),
            Token::Number(3.0),
            Token::Variable("pi".to_string(), Some(f32::consts::PI)),
            Token::Operators(Ops::Add),
            Token::Operators(Ops::Mul),
        ]));

        let tokens8 = Calcr::lexer("(1 + 2) * 3");
        
        assert_eq!(calcr.parse(tokens8), Ok(vec![
            Token::Number(1.0),
            Token::Number(2.0),
            Token::Operators(Ops::Add),
            Token::Number(3.0),
            Token::Operators(Ops::Mul),
        ]));

        let tokens9 = Calcr::lexer("1 + 2 * 3 - 4");

        assert_eq!(calcr.parse(tokens9), Ok(vec![
            Token::Number(1.0),
            Token::Number(2.0),
            Token::Number(3.0),
            Token::Operators(Ops::Mul),
            Token::Operators(Ops::Add),
            Token::Number(4.0),
            Token::Operators(Ops::Sub),
        ]));
    }

    #[test]
    fn evaluation_tests() {
        let mut calcr = Calcr::default();

        assert_eq!(calcr.evaluate("-2 + 4"), Ok(2.0));

        assert_eq!(calcr.evaluate("(1 + 5) * 8 - 2 * 3"), Ok(42.0));
        
        assert_eq!(calcr.evaluate("1 + pi * 2"), Ok(1.0 + f32::consts::PI * 2.0));

        assert_eq!(calcr.evaluate("1-"), Err(TokenError::MissingOperands));

        assert_eq!(calcr.evaluate("14 25"), Err(TokenError::MissingOperators));
        
        assert_eq!(calcr.evaluate("]"), Err(TokenError::UnknownToken(']')));
    }

    #[test]
    fn lexer_tests() {
        // TODO: custom variables need to be implemented
        // assert_eq!(Calcr::lexer("- ( super/ test) * crazy %+224a-    13"), vec![
        //     Token::Number(0.0),
        //     Token::LeftBracket,
        //     Token::Variable("super".to_string()),
        //     Token::Operators(Ops::Div),
        //     Token::Variable("test".to_string()),
        //     Token::RightBracket,
        //     Token::Operators(Ops::Mul),
        //     Token::Variable("crazy".to_string()),
        //     Token::Unknown('%'),
        //     Token::Operators(Ops::Add),
        //     Token::Number(224.0),
        //     Token::Variable("a".to_string()),
        //     Token::Operators(Ops::Sub),
        //     Token::Number(13.0),
        // ]);
    }
}
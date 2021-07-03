use std::{error::Error, fmt, f32};

/// An enum used to handle operator tokens, used in `Token::Operators`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ops {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Fac,
}

impl Ops {
    /// A method used to return the precedence level of
    /// any operator. The precedence levels are:
    /// - `Add` and `Sub`: level `0`
    /// - `Mul` and `Div`: level `1`
    /// - `Pow` and `Fac`: level `2`
    fn precedence(&self) -> u8 {
        match *self {
            Self::Add | Self::Sub => 0,
            Self::Mul | Self::Div => 1,
            Self::Pow | Self::Fac => 2,
        }
    }
}

impl fmt::Display for Ops {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Pow => write!(f, "^"),
            Self::Fac => write!(f, "!")
        }
    }
}

/// An enum used to handle tokens of the given input.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f32), // TODO: currently can't handle float numbers
    Variable(String, Option<f32>),
    LeftBracket,
    RightBracket,
    Operators(Ops),
    Unknown(char),
    // Commands(String), // TODO: implement commands
}

/// The struct of the engine of the calculator. The calculator
/// takes the input, tokenizes it with the `lexer()`, then it
/// parses the tokens returning a postfix expression of the
/// given input, and then TODO: finish documentation here
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

                    let var: String = input[idx..idx + to_skip].into();

                    // TODO: implement custom variables
                    let value = match var.as_str() {
                        "pi" => Some(f32::consts::PI),
                        "e" => Some(f32::consts::E),
                        "tau" => Some(f32::consts::TAU),
                        _ => None
                    };

                    tokens.push(Token::Variable(var, value));

                    to_skip -= 1;
                },
                '(' => tokens.push(Token::LeftBracket),
                ')' => tokens.push(Token::RightBracket),
                '+' => tokens.push(Token::Operators(Ops::Add)),
                '-' => tokens.push(Token::Operators(Ops::Sub)),
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

    // pub fn evaluate(&mut self, input: &str) -> Result<Option<isize>, TokenError> {
        // pub fn evaluate(&mut self, input: &str) -> Option<isize> {
    pub fn evaluate(&mut self, input: &str) -> Result<f32, TokenError> {
        let tokens = Self::lexer(input);
        
        let postfix = self.parse(tokens);

        println!("{:?}", postfix);

        match postfix {
            Ok(expression) => {
                let mut stack = Vec::new();
                // let mut idx: usize = 0; // can't use .enumerate() otherwise i'd had to .clone() everything

                // for (idx, token) in expression.iter().enumerate() {
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
                                (Some(first_number), Some(second_number)) => {
                                    let result = match operator {
                                        Ops::Add => first_number + second_number,
                                        Ops::Sub => first_number - second_number,
                                        Ops::Mul => first_number * second_number,
                                        Ops::Div => first_number / second_number,
                                        _ => return Err(TokenError::UnimplementedOperator(operator)),
                                    };

                                    stack.push(result);
                                },
                                (Some(number), None) => {
                                    // FIXME: fix this somehow
                                    // if idx != 0 {
                                    // if !stack.is_empty() || stack.len() == 1 {
                                    // if stack.len() > 1 {
                                    //     return Err(TokenError::MissingOperands)
                                    // }

                                    match operator {
                                        Ops::Add => stack.push(number),
                                        Ops::Sub => stack.push(-number),
                                        _ => return Err(TokenError::MissingOperands),
                                    }
                                }
                                _ => return Err(TokenError::MissingOperands),
                            }
                        },
                        _ => {},
                    }
                    // idx += 1;
                }

                println!("stack at the end = {:?}", stack);

                if stack.len() > 1 {
                    return Err(TokenError::UnknownError)
                }

                Ok(stack[0])
            },
            Err(token_error) => Err(token_error),
        }
    }
}

/// An enum to handle syntax errors in the given input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenError {
    UnknownToken(char),
    UnknownVariable(String),
    MisplacedBrackets,
    MissingOperands,
    UnimplementedOperator(Ops),
    UnknownError, // TODO: for debugging, to be deleted
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownToken(c) => write!(f, "Unknown token {:?}.", c),
            Self::UnknownVariable(v) => write!(f, "Unknown variable {}.", v),
            Self::MisplacedBrackets => write!(f, "Brackets are misplaced."),
            Self::MissingOperands => write!(f, "There are not enough operands."),
            Self::UnimplementedOperator(operator) => write!(f, "'{}' is currently unimplemented.", operator),
            Self::UnknownError => write!(f, "An unknown error occured while evaluating the input."),
        }
    }
}

impl Error for TokenError {}

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

        // TODO: write a specific test for custom variables, once implemented
    }

    #[test]
    fn evaluation_tests() {
        // TODO: write tests for evaluation
        let mut calcr = Calcr::default();

        assert_eq!(calcr.evaluate("-2 + 4"), Ok(2.0));
    }

    #[test]
    fn lexer_tests() {
        // TODO: custom variables need to be implemented
        // assert_eq!(Calcr::lexer(" ( super/ test) * crazy %+224a-    13"), vec![
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
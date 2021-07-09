use crate::{
    fraction::{Fraction, consts::*},
    token::{Ops, Cmd, Const, Token, TokenError}
};
use std::{fmt::Write, collections::HashMap};
use ansi_term::Colour::{Red, Green, RGB};
use lazy_static::*;

lazy_static! {
    /// Holds the help message, which can be shown by calling the `\help` command.
    pub static ref HELP: String = {
        let gray = RGB(175, 175, 175);

        let mut help_message = String::new();

        let msg = "An error occured while building the help message";

        writeln!(help_message, "Currently supported commands:").expect(msg);
        writeln!(help_message, "{} {}\n", gray.italic().paint("Note:"), gray.paint("if a command receives more parameters than what it requires, the exceeding ones will be ignored.")).expect(msg);
        writeln!(help_message, "- {}: exits the program; takes no parameters", gray.paint(r"\exit")).expect(msg);
        writeln!(help_message, "- {}: shows this help message; takes no parameters", gray.paint(r"\help")).expect(msg);
        writeln!(help_message, "- {}: clears the screen; takes no parameters", gray.paint(r"\clear")).expect(msg);
        writeln!(help_message, "- {}: shows some debug informations; takes no parameters", gray.paint(r"\debug")).expect(msg);
        writeln!(help_message, "- {}: shows the result as a fraction; takes no parameters", gray.paint(r"\ratio")).expect(msg);
        writeln!(help_message, "- {}: shows the result in hexadecimal; takes no parameters", gray.paint(r"\hex")).expect(msg);
        writeln!(help_message, "- {}: shows the current status of every flag; takes no parameters", gray.paint(r"\flags")).expect(msg);
        writeln!(help_message, "- {}: shows every custom variable stored in the calculator; takes no parameters", gray.paint(r"\vars")).expect(msg);
        writeln!(help_message, "- {}: removes the specified variable; takes one parameter", gray.paint(r"\remove")).expect(msg);

        help_message
    };
}

/// The struct of the engine of the calculator. The calculator
/// takes the input, tokenizes it with a lexer, then it
/// parses the tokens returning a postfix expression of the
/// given input, and lastly it evaluates the postfix expression.
/// 
/// # Examples
/// 
/// ```
/// # pub use calcr::engine::Calcr;
/// # pub use calcr::token::TokenError;
/// # pub use calcr::fraction::Fraction;
/// # pub use std::f32::consts::PI;
/// let mut calcr = Calcr::new();
/// 
/// // the calculator internally handles numbers as fraction
/// assert_eq!(calcr.evaluate("- 3 * 5"), Ok(Some(Fraction::new(-15, 1).unwrap()))); // -3 * 5 = -15
/// 
/// assert_eq!(calcr.evaluate("(3 - 5) / (4 - 4)"), Err(TokenError::DivisionByZero)); // can't divide by zero!
/// 
/// assert_eq!(calcr.evaluate(r"\flags"), Ok(None)); // shows the flags status
/// 
/// assert_eq!(calcr.evaluate("ans * 2").unwrap().unwrap().try_integer().unwrap(), -30); // stores the last valid result!
/// 
/// assert_eq!(calcr.evaluate("myvar = 5"), Ok(Some(Fraction::new(5, 1).unwrap()))); // supports custom variables!
/// 
/// assert_eq!(calcr.evaluate("2 pi").unwrap().unwrap().float().unwrap(), 2.0 * PI); // knows many constants!
/// ```
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Calcr {
    /// If `true`, debug details will be provided along with the result of the given input.
    pub debug_flag: bool,

    /// If `true`, the result will be printed as a fraction.
    pub ratio_flag: bool,

    /// If `true`, the result will be printed with the hexadecimal format.
    pub hex_flag: bool,

    ans: Fraction,
    vars: HashMap<Token, Fraction>,
}

impl Calcr {
    /// Returns a `Calcr` instance. By default, every flag
    /// is set to `false`, there are no stored variables,
    /// and `ans` (which stores the result of the previous input),
    /// is set to `ZERO_FRACTION`.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::engine::Calcr;
    /// let mut calcr = Calcr::new();
    /// 
    /// assert_eq!(calcr.debug_flag, false);
    /// ```
    pub fn new() -> Self {
        let vars = HashMap::new();

        Self {
            debug_flag: false,
            ratio_flag: false,
            hex_flag: false,
            
            ans: ZERO_FRACTION,
            vars,
        }
    }

    /// Tokenizes the given input.
    /// - numbers are tokenized as `Token::Number(number)`
    /// - variables are tokenizes as `Token::Variable(variable)`
    /// - constants are tokenized as `Token::Constant(constant)`
    /// - `\` is tokenized as `Token::CommandToken`
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
    /// - `=` is tokenized as `Token::Equal`
    /// - `\r`, `\n` are ignored
    /// - ` ` is ignored in most cases, except in some contexts
    /// where spacing is not allowed, and it's tokenized as `Token::Space`
    /// - every other character is tokenized as `Token::Unknown(character)`
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
                        if let Token::RightBracket | Token::Number(_) = prev {
                            tokens.push(Token::Operators(Ops::Mul))
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
                            "hex" => Cmd::Hex,
                            "flags" => Cmd::Flags,
                            "vars" => Cmd::Vars,
                            "remove" => Cmd::Remove,
                            _ => Cmd::Unknown(string),
                        };

                        tokens.pop().unwrap(); // removes the `\`

                        tokens.push(Token::Command(command));
                    } else {
                        tokens.push(match string.as_str() {
                            "ans" => Token::Ans,
                            "pi" => Token::Constant(Const::Pi),
                            "e" => Token::Constant(Const::E),
                            "tau" => Token::Constant(Const::Tau),
                            _ => Token::Variable(string),
                        });
                    }

                    to_skip -= 1;
                },
                '\\' => tokens.push(Token::CommandToken),
                '(' => {
                    if let Some(prev) = tokens.iter().last() {
                        if let Token::Number(_) | Token::Variable(..) | Token::Constant(_) | Token::RightBracket = prev {
                            tokens.push(Token::Operators(Ops::Mul))
                        }
                    }

                    tokens.push(Token::LeftBracket);
                },
                ')' => tokens.push(Token::RightBracket),
                '&' => tokens.push(Token::Operators(Ops::And)),
                '|' => tokens.push(Token::Operators(Ops::Or)),
                '+' => {
                    if let Some(prev) = tokens.iter().last() {
                        if let Token::LeftBracket | Token::Equal = prev {
                            tokens.push(Token::Number(ZERO_FRACTION));
                        }
                    } else {
                        tokens.push(Token::Number(ZERO_FRACTION));
                    }

                    tokens.push(Token::Operators(Ops::Add));
                },
                '-' => {
                    if let Some(prev) = tokens.iter().last() {
                        if let Token::LeftBracket | Token::Equal = prev {
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
                '\r' | '\n' => {},
                ' ' => {
                    if let Some(Token::CommandToken) = tokens.iter().last() {
                        tokens.pop().unwrap(); // removes the `\`

                        tokens.push(Token::Space)
                    }
                }
                _ => tokens.push(Token::Unknown(ch)),
            }
        }

        if self.debug_flag {
            println!("Tokenized input: {:?}", tokens);
        }

        tokens
    }

    /// Returns the postfix expression of the tokenized input.
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
                Token::Constant(c) => buffer.push(Token::Number(c.value())),
                Token::Ans => buffer.push(Token::Number(self.ans)),
                Token::Unknown(c) => return Err(TokenError::UnknownToken(c)),
                _ => return Err(TokenError::UnknownError(0x0001)),
            }
        }

        while !stack.is_empty() {
            buffer.push(stack.pop().unwrap())
        }

        // in the case where there are no `)` in
        // the given input, the parser has no way to know if
        // there are any unclosed `(`, thus this
        // check makes sure to handle this edge case
        if buffer.contains(&Token::LeftBracket) {
            return Err(TokenError::MisplacedBrackets)
        }

        if self.debug_flag {
            println!("Postfix expression: {:?}", buffer);
        }

        if buffer.is_empty() { // the input was `()`
            Err(TokenError::EmptyBrackets)
        } else {
            Ok(buffer)
        }
    }

    /// Evaluates the postfix expression returned from the parser.
    fn evaluate_postfix(&mut self, expression: Vec<Token>) -> Result<Fraction, TokenError> {
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
                                Ops::Pow => {
                                    if let Ok(result) = first_number.powf(second_number) {
                                        result
                                    } else {
                                        return Err(TokenError::IndeterminateForm)
                                    }
                                },
                                _ => return Err(TokenError::UnimplementedOperator(operator)),
                            };

                            stack.push(result);
                        },
                        _ => return Err(TokenError::MissingOperands),
                    }
                },
                _ => return Err(TokenError::UnknownError(0x0002)),
            }
        }

        if stack.len() > 1 {
            return Err(TokenError::MissingOperators)
        }

        let result = stack[0];

        self.ans = result;

        Ok(result)
    }

    /// Evaluates the given expression.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # pub use calcr::engine::Calcr;
    /// # pub use calcr::fraction::Fraction;
    /// let mut calcr = Calcr::new();
    /// 
    /// // the calculator internally handles numbers as fraction
    /// assert_eq!(calcr.evaluate("3 + 2"), Ok(Some(Fraction::new(5, 1).unwrap()))) // 3 + 2 = 5
    /// ```
    pub fn evaluate(&mut self, input: &str) -> Result<Option<Fraction>, TokenError> {
        let tokens = self.lexer(input);
        
        if tokens.is_empty() {
            return Ok(Some(ZERO_FRACTION));
        }

        if tokens.contains(&Token::CommandToken) {
            return Err(TokenError::MissingCommand)
        }

        if tokens.contains(&Token::Space) {
            return Err(TokenError::WrongSpacing)
        }

        match tokens.first().unwrap() {
            Token::Command(Cmd::Exit) => {
                // TODO: better way to exit the program, too rough and ugly message
                println!("A nicer way to exit from the program is still WIP...");

                quit::with_code(0x100);
            },
            Token::Command(Cmd::Help) => print!("{}", *HELP),
            Token::Command(Cmd::Clear) => clearscreen::clear().expect("An error occured while trying to clear the screen."),
            Token::Command(Cmd::Debug) => {
                self.debug_flag = !self.debug_flag;

                println!("Debug flag toggled.");
            },
            Token::Command(Cmd::Ratio) => {
                self.ratio_flag = !self.ratio_flag;

                println!("Ratio flag toggled.");
            },
            Token::Command(Cmd::Hex) => {
                self.hex_flag = !self.hex_flag;

                if self.ratio_flag {
                    self.ratio_flag = false;

                    println!("Ratio flag toggled, since it was conflicting with the hex flag.");
                }

                println!("Hex flag toggled.");
            },
            Token::Command(Cmd::Flags) => {
                let t = &Green.paint("true");
                let f = &Red.paint("false");

                println!("Current flags:");
                println!("Debug flag: {}", if self.debug_flag { t } else { f });
                println!("Ratio flag: {}", if self.ratio_flag { t } else { f });
                println!("Hex flag: {}", if self.hex_flag { t } else { f });
            },
            Token::Command(Cmd::Vars) => {
                if self.vars.is_empty() {
                    println!("There are no variables stored.")
                } else {
                    for var in self.vars.iter() {
                        if let Token::Variable(name) = var.0 {
                            print!("{}: ", name)
                        }

                        println!("{} = {}", var.1, unsafe { var.1.float_unchecked() })
                    }
                }
            },
            Token::Command(Cmd::Remove) => {
                if let Some(to_delete) = tokens.get(1) {
                    if self.vars.get(to_delete).is_some() {
                        self.vars.remove(to_delete);

                        println!("Variable '{}' removed.", to_delete);
                    } else if let Token::Variable(name) = to_delete {
                        return Err(TokenError::UnknownVariable(name.clone()));
                    } else {
                        return Err(TokenError::InvalidVariableName(to_delete.clone()))
                    }
                } else {
                    return Err(TokenError::NotEnoughArguments);
                }
            },
            Token::Command(Cmd::Unknown(name)) => return Err(TokenError::UnknownCommand(name.clone())),
            _ => {
                for token in tokens.iter() {
                    if token != tokens.first().unwrap() {
                        if let Token::Command(cmd) = token {
                            if let Cmd::Unknown(name) = cmd {
                                return Err(TokenError::UnknownCommand(name.clone()))
                            }

                            return Err(TokenError::InvalidCommandSyntax(cmd.clone()))
                        }
                    }
                }

                if tokens.len() > 1 {
                    for window in tokens.windows(2) {
                        if let (Token::Number(_) | Token::Variable(..) | Token::Constant(_), Token::Number(_) | Token::Variable(..) | Token::Constant(_)) = (&window[0], &window[1]) {
                            return Err(TokenError::MissingOperators)
                        }
                    }
                }

                if tokens.contains(&Token::Equal) {
                    if tokens.iter().filter(|t| **t == Token::Equal).count() > 1 {
                        return Err(TokenError::InvalidDefinitionSyntax)
                    }

                    if tokens.iter().take_while(|t| **t != Token::Equal).count() == 1 {
                        // the statement above makes sure that `tokens[0]` always exists
                        let first_token = tokens[0].clone();

                        match first_token {
                            Token::Variable(..) => {
                                let rhs: Vec<Token> = tokens
                                    .into_iter()
                                    .skip_while(|t| t != &Token::Equal)
                                    .skip(1)
                                    .collect();

                                if rhs.is_empty() {
                                    return Err(TokenError::InvalidDefinitionSyntax);
                                }

                                let rhs_parsed = self.parse(rhs);

                                match rhs_parsed {
                                    Ok(parsed) => {
                                        let rhs_evaluated = self.evaluate_postfix(parsed);

                                        match rhs_evaluated {
                                            Ok(result) => {
                                                if let Some(value) = self.vars.get_mut(&first_token) {
                                                    *value = result;
                                                } else {
                                                    self.vars.insert(first_token, result);
                                                }

                                                if self.debug_flag {
                                                    println!("Variables: {:?}", self.vars);
                                                }

                                                return Ok(Some(result));
                                            },
                                            Err(token_error) => return Err(token_error),
                                        }
                                    },
                                    Err(token_error) => return Err(token_error),
                                }
                            },
                            Token::Constant(c) => return Err(TokenError::ConstantName(c)),
                            _ => return Err(TokenError::InvalidVariableName(first_token)),
                        }
                    } else {
                        return Err(TokenError::InvalidDefinitionSyntax)
                    }
                }

                let postfix = self.parse(tokens);

                return match postfix {
                    Ok(expression) => {
                        let evaluated = self.evaluate_postfix(expression);

                        match evaluated {
                            Ok(result) => Ok(Some(result)),
                            Err(error) => Err(error), 
                        }
                    }
                    Err(token_error) => Err(token_error),
                }
            },
        }

        Ok(None)
    }
}
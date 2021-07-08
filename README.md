# calcr

A simple calculator made in Rust. The calculator internally handles
numbers as fractions, and it's able to handle constants and custom variables.
The input is first tokenized with a lexer, and the token stream
is then parsed and evaluated using a postfix expression.

## What it can

This calculator is able to handle:

- `&` is currently unimplemented
- `|` is currently unimplemented
- `+` addition
- `-` subtraction
- `*` multiplication
- `/` division
- `^` exponential
- `!` is currently unimplemented

It comes with many commands and features:

- `\exit` allows you to exit the program
- `\help` is currently WIP
- `\clear` clears the screen
- `\debug` shows some debug information, which can be toggled from the command line by passing `--debug`
- `\ratio` shows the result as a fraction, which can be toggled from the command line by passing `--ratio`
- `\hex` shows the result in hexadecimal
- `\flags` shows the current status of every flag
- `\vars` shows every custom variable stored in the calculator

There are some constants already defined in the calculator,
such as `pi` and `e`, but if you need more variables you can define custom ones!

```plain text
myvar = 4
> 4
myvar + 3
> 7
```

## WIP

- [x] impmlement powf to fix `^`
- [x] implement hashmap for variables
- [x] implement constants
- [x] implement custom variables
- [x] fix `a = -5`
- [x] fix `()`
- [x] implement `\hex` command
- [x] implement `\flags` command
- [x] fix `0^0`
- [x] `^` operator
- [x] implement structopt
- [x] implement ans
- [x] implement `\vars` command
- [x] docs and examples all around
- [x] implement `\remove` command
- [x] change how commands are implemented
- [x] fix fraction.rs doc tests
- [ ] handle floats
- [ ] fix `\help` command
- [ ] fix bitwise operators
- [ ] `!` operator
- [ ] history system

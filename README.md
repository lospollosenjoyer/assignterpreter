# assignterpreter

A simple interpreter for a basic assignment language with arithmetic expressions, built from scratch in Haskell using the Alex lexer and Happy parser generator tools as my study exercise.

## About The Project

This project is an educational exercise in building language interpreter. It scans a sequence of variable assignments, evaluates the arithmetic expressions and tracks the state of all variables. The interpreter is built with a classic pipeline:

`Lexer (Alex) -> Parser (Happy) -> Evaluator -> Map (identifier: value)`

The core logic is exposed via a single function `interpret`, which takes a source code string and returns either an error or the map of variables and their values.

### Features

- Variable assignments
    Assignment of values to variables
    
```
x = 10;
```

- Arithmetic expressions
    Addition (+), subtraction (-), multiplication (\*) and division (/) support with parentheses for grouping
    
```
a = b * (c - d / e) - (f + d);
```

- Mixed-type arithmetic
    Operations between integers and doubles, promoting the result to a double value. Promoting logic is incapsulated inside of `Number` type.
    
```
a = (3.5 / 7 - 5) * 2.13;
```
	
- Stateful evaluation
    Variables retain their values across statements. Reassigning a variable updates its value for subsequent calculations.
    
- Strict error handling
    The program will halt and report an error for:
    - lexical errors
    - syntax errors
    - undeclared variables usage
    - division by zero

### Technology stack

- Language: **Haskell**
    *GHC 9.8.4*
- Lexer generator: **Alex**
- Parser generator: **Happy**
- Build tool: **Stack**

`Data.ByteString.Lazy.Char8.ByteString` is used for efficient string handling
**unordered-containers** for the 
`Data.HashMap.Strict` used to store variables

## Getting Started

To build and run this project, you will need **GHC** and **Stack**

### Installation & Building

1. Clone the repository

```
$ git clone https://github.com/lospollosenjoyer/assignterpreter.git
$ cd assignterpreter
```
    
2. Build the project and its dependencies

```
$ stack build
```

### Usage

You can use the `interpret` function directly in your Haskell code or in a GHCi session.

- Start a GHCi session in your project's context
	
```
$ stack ghci
```
    
```
$ ghci> import qualified Data.ByteString.Lazy.Char8 as BS
$ ghci> :l Interpret
$ ghci> interpret $ BS.pack "a = 10 * (3 + 5); b = a - 100.5; a = b * a;"
Right (fromList [("a", Ndouble (-1640.0)),("b",Ndouble (-20.5))])
```

### Error handling examples

The interpreter will return a `Left String` value upon encountering an error

- Lexical error
	usage of undefined symbols, capitalized or starting with digit/underscore variables

```
ghci> interpret "A = 10;"
Left "lexical error at line 1, column 1"
```

- Syntax error
	statement is not assignment, incorrect expressions, lack of semicolon at the assignment end

```
ghci> interpret "a += 10;"
Left "syntax error at line 1, column 3"
```

- Using an uninitialized variable

```
ghci> interpret "a = b + 10;"
Left "error: undeclared variable b at line 1, column 5"
```

- Division by zero

```
ghci> interpret "a = 10 / 0;"
Left "error: division by 0 at line 1, column 8"
```

## Tooling

`scripts` is collection of handy scripts for autoformatting and autolinting of the code

- *Fourmolu* is a formatter for Haskell source code

```
stack install fourmolu
```

- *HLint* is a tool for suggesting possible improvements to Haskell code

```
stack install hlint
```

- `scripts/check.sh`
	checks for possible format fixes and linter hints

- `scripts/format.sh`
	just formats the code

- `scripts/lint.sh`
	just lints the code in interactive mode

- `scripts/tidy.sh`
	`format.sh` + `lint.sh`

- `scripts/fast_tidy.sh`
	as `tidy.sh` applies both format and lint fixes to the code, but in non-interactive mode

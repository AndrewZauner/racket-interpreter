# Racket Interpreter

Welcome to the Racket Interpreter repository! This interpreter, written in Racket, allows you to evaluate Racket expressions and supports a variety of features, including variable bindings, environments, higher-order functions, and a REPL (Read-Eval-Print Loop) for interactive use.


## Installation

To use this Racket interpreter, you need to have Racket installed on your system. You can download and install Racket from the official website.

Clone this repository to your local machine:

```
git clone https://github.com/AndrewZauner/racket-interpreter.git
cd racket-interpreter
```

## Usage

You can start the REPL (Read-Eval-Print Loop) to interact with the interpreter:

Open a terminal and navigate to the directory where you cloned the repository.
    
Run the interpreter with the following command:
    

    racket -f path/to/interpreter.rkt

    
  In the REPL, you can type Racket expressions to evaluate them. To exit the REPL, type exit.

### Example REPL session:

```
INTERPRETER> (define x 10)
x
INTERPRETER> (+ x 5)
15
INTERPRETER> exit
INTERPRETER done
```

## Features

The interpreter supports the following features:

* Variable Bindings: Define and look up variables.
* Frames and Environments: Manage scopes and variable bindings.
* Primitives: Built-in functions like +, -, *, /, =, cons, first, rest, null?, >, <, >=, <=, display, and newline.
* Conditionals: if and cond expressions.
* Lambda Expressions: Define anonymous functions.
* Higher-Order Functions: Support for map, filter, and fold.
* REPL: Interactive shell to evaluate expressions.

### Example Expressions

```
  (define x 10)
  (if (> x 5) 'greater 'lesser)
  (let ((y 20)) (+ x y))
  (map (lambda (n) (* n n)) '(1 2 3 4 5))
  (filter (lambda (n) (> n 2)) '(1 2 3 4 5))
  (fold (lambda (acc n) (+ acc n)) 0 '(1 2 3 4 5))
```

## Testing

Test cases for the interpreter are provided in the function_test.rkt file. You can run these tests to ensure the interpreter is functioning correctly.

To run the tests, execute the following command:

```
racket path/to/function_test.rkt
```

## Contributing

I welcome contributions to enhance the functionality and performance of this Racket interpreter. If you have any ideas, suggestions, or bug fixes, please open an issue or submit a pull request.

* Fork the repository.
* Create a new branch for your feature or bug fix.
* Commit your changes with descriptive messages.
* Push your branch to your forked repository.
* Open a pull request to the main repository.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.

Have fun exploring the Racket interpreter! If you have any questions or need further assistance, feel free to open an issue in this repository.

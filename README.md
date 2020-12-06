# 320-interpreter-tests
A collection of property based tests and test vectors for the interpreter project administered in BU CS 320 (Fall 2020).

## Motivations

- Learn about property-based testing
- Practice OCaml

## Usage

This project uses [dune](https://github.com/ocaml/dune) to manage the build process.

There are two types of tests in this project:
1) Property-based tests: Tests observable properties+invariants of the interpreter.
2) Vector tests: Runs a set of fixed inputs against the interpreter and asserts the returned outputs correctly match expected outputs.

In order to run these tests, two additional functions must be added to the interpreter implementation.

```ocaml
let interpret (input : string list) : string list
(** [interpret input] returns the final stack after running the interpreter on [input].

Each element in [input] represents a command (as defined in the Constants/Programs grammar found in the handout) to execute.

E.g. interpret [Push 1; Push 1; Add; Quit;] : [2]
*)

let interpret_file (inputFile : string) : string list
(** [interpret_file inputFile] behaves the same as [interpret] except the input on which the interpreter is run is contained in the file located at path [inputFile].
*)
```

### Running the tests

1) Place interpreter implementation at path `interpreter/interpreter.ml`
2) From the root directory, run `dune build`. You only need to run this once, or when new vectors are added in `vector_tests/vectors`.
3) From the root directory, run `dune runtest -f`.

## TODO

For a future iteration, I hope to finish the following tasks:

- [ ] Property-based tests for `BeginEnd`, `IfThenElse`, and all the commands in part 3.
 - I only managed to write property-based tests up to the `BeginEnd` environment command. The generator for valid `BeginEnd` commands became difficult to implement due to the following invariant described in the handout: "Begin...End can contain any number of operations but it will always result in a stack frame that is strictly larger than the stack prior to the Begin." I have some WIP generator code for `BeginEnd`, but it's not fully functional, so I have opted not to include it.
- [ ] Use GADTs to clean up code and make it safer (for example, `**TestSpecs` can become simpler).
- [ ] Create Gradescope wrapper around the property-based tests.

## Credit

I used some helpful parser code in `qcheck_tests/shrinkers.ml` provided by Professor Marco Gaboardi. This code makes the QCheck shrinkers easier to implement.

The vectors found in `vector_tests/vectors` were provided by the teaching staff: Professor Marco Gaboardi, Professor Abbas Attarwala, Qiancheng Fu, Cheng, and Weifan Chen.
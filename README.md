# BFBackend: A Compiler Backend for Brainfuck
BFBackend is a Rust crate which allows you to use Rust functions to generate Brainfuck code.

## Features
- Comparisons (equals, not equals, greater than, and less than)
- Text display
- Read strings of a specified length
- Setting single cells and arrays efficiently
- Efficiently matching on the value of a byte
- Multiplication, division, addition, subtraction, exponentiation
- General variable/memory manipulation utilities

## Concepts
- Variable: a container for a slice of memory and variable data
- ByteRef: a reference to a specific cell in memory as well as a reference to the variable which the cell is a part of
- MemoryRange: a pointer to a start cell along with the size of the data

## How to Use
- Create a `BfContext`:
    ```rs
    use bfbackend::BfContext;
    let ctx = BfContext::default();
    ```
- Run functions on it to generate Brainfuck code:
    ```rs
    let mut my_num = ctx.declare_and_set_byte(42);
    ctx.add_to_var(&mut my_num.get_byte_ref());
    ```
- Get your code:
    ```rs
    let code = ctx.build_code();
    println!("{code}");
    ```
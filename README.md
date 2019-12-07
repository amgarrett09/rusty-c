# rusty-c

This is a limited C compiler front-end, written in Rust.
The goal for this is mainly to learn how compiler front-ends work by writing
one myself. My approach is heavily inspired by 
[this series](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
by Nora Sandler.

Ultimately, I'd like to get this to a point where it can handle a
turing-complete subset of C. After that, I may end up getting into some
back-end stuff, like compiler optimizations, but I'm not sure.

## TODO
Besides expanding things the compiler can handle, I'd like to:

- [ ] Write automated tests
- [ ] Make better error messages
- [ ] Incorporate line numbers into the error messages
- [ ] Add ability to codegen LLVM IR

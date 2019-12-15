# CS4410
Compilers @ Northeastern University
Team: Anand Kumar (akumar) + David Reed (reedda)

See `README.md` under child folders for more granular information:

- `lexer/` -- Lexer information

- `parser/` -- Parser information (includes updated Lexer)

- `semant/` -- Initial semantic analysis (type-checking)

- `ir/` -- Complete semantic analysis (type-checking and frame analysis)

- `insn/` -- Instruction Selection

- `live/` -- Liveness Analysis

- `color/` -- Register allocation

## Integration

### Procedure
Following the textbook (chapter 12), we implemented Frame.string, a main routine, and completed
the implementations of procEntryExit2 and procEntryExit3. Then our focus was on testing (using
QtSpim as a SPIM software simulator) our factorial code. Our factorial assembler can be found in
`tests_insn/factorial.tig.s` (generated by the corresponding `tests_insn/factorial.tig` file).

### Points of Interest
- Our factorial assembler located in `tests_insn/factorial.tig.s`
- the extra instructions we pad around the body during prologue and epilogue in procEntryExit3. First, we
copy current FP to SP to get a new frame, then set new SP offset/allocate the frame, then store registers before the body. After the body, we load the registers back, deallocate the frame, move sp to fp then reset fp using link.
- Our main routine generates a new `.s` assembler listing for each file it compiles.

### Testing
We tested by generating the assembly for the test files we have (in various `test` directories) using `Main.compile "path/to/test.tig"`. Then, using QtSpim or command-line (`brew install spim`) we would test the compiled .s file.

### Superceding Previous Assignments
Our assignment 7 submission was late, and we expect a 10% penalty on the original grade. Additionally, we didn't compute liveness correctly. We fixed this by rewriting our logic to build the CFG in order to compute liveness.

# CS 4410 Instruction Selection
## Team: Anand Kumar (akumar) + David Reed (reedda)

---
How to Run the Instruction Selection step:

`sml -m sources.cm` **within** the `/insn` directory

Once in the SML REPL, use `Main.compile "filename"` on any Tiger file.

---

## References

We found these documents helpful in our implementation
* https://course.ccs.neu.edu/csu4410/spim_documentation.pdf
* Textbook

---

# Instruction Selection

By and large, the special cases for instruction selection that we've distinguished
have come from the book. 

For all of our instructions, we're using a DSL to indicate source and destination
registers as the targets for different commands: 'd0 and 's0 to indicate "the 
first register in the destination list" and "the first register in the destination
list", respectively.

---

# Changes from previous IR and Frame Analysis work

## Reserved registers

For this assignment, we initialized five reserved registers (temps, for now) 
to be referenced in the rest of the instruction selection. These five registered
are stored in the FRAME signature:

  - `FP` : The frame pointer
  - `RV` : The return value
  - `SP` : The top-of-stack pointer
  - `RA` : The return address
  - `ZERO` : The reserved zero register (used for no-ops, primarily)

## procEntryExit

For this assignment we added a stub for `procEntryExit2` and a basic implementation
for `procEntryExit3`. The important functionality of the latter is to add a prologue
and epilogue to each procedure.

As of now the resulting instruction lists are not yet being used as replacements
for the fragment list that we were creating beforehand, however to get around
this we're appending the procedures with existing IR tree segments to the fragment
list, throwing away the instruction sets otherwise.

## Adding the primary tree as a fragment

The main body of the IR tree that has been parsed wasn't previously being
attached to the fragment list being returned from `transProg`, so we have
remedied this by attaching it to the `main`-level frame and appending it to
the fragment list returned by `getResult`.

---

## Stylistic Choices

### Code for Impossible Errors

We recently realized that `ErrorMsg.impossible` exists, so we've replaced our bogus error
throwing with that.

## Testing

We tested our code against the test cases provided by Appel (`tests/`), alongside
our own tests for some special cases in isolation / not covered in the other test
folder (`tests_semant`).  We also ran the tests we had written for our previous
type-checker (`tests_type`), to validate that our functionality remains consistent.

Running individual tests:

```
fun test(0) = (Main.compile "../tests/queens.tig"; Main.compile "../tests/merge.tig") | test(x) = let val _ = Main.compile("../tests/test" ^ Int.toString x ^ ".tig") in test(x - 1) end;
```

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

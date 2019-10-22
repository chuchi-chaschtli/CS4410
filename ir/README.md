# CS 4410 Frame Analysis
## Team: Anand Kumar (akumar) + David Reed (reedda)

---

## References

We found these documents helpful in our implementation of the type-checker.
* https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
* http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf
* https://cs.nyu.edu/courses/fall13/CSCI-GA.2130-001/tiger-spec.pdf
* Textbook

---

## Implementation Decisions

Listed below are the interesting cases for our frame analysis.

### FindEscape

Our `FindEscape` module runs a simple tree walk against the AST to find variables
that escape the depth they were defined at. The base case for our walk is at the
`traverseVar` level, at which point we can do the check for whether or not we're
traversing a variable at a depth different from its definition. In this case,
we mark that var's `escape` as true.

One interesting case for Escape analysis is function calls, however we can
shortcut by mapping `trexp` against the arguments being passed into a function,
since if that variable is free for usage within a function being called, we
can assume that that variable escapes.

### Parameter frame allocation

We limit the number of function parameters that can be allocated to register locations
to 4. Any function parameters outside of the first 4 to a given function
are delegated to stack allocations instead of temp registers, regardless of
whether or not the previous 4 were register-allocated.

---

## Stylistic Choices

### Code for Impossible Errors

See previous `README.md` (`semant`). We have not added any superfluous error cases,
however we have not removed the ones that were present beforehand.

## Testing

We tested our code against the test cases provided by Appel (`tests/`), alongside
our own tests for some special cases in isolation / not covered in the other test
folder (`tests_semant`).


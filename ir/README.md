# CS 4410 Frame Analysis & Translation
## Team: Anand Kumar (akumar) + David Reed (reedda)

---
How to Run the Frame Analyzer / IR Translator:

`sml -m sources.cm` **within** the `/ir` directory

Once in the SML REPL, use `Analysis.check "filename"` on any Tiger file.

---

## References

We found these documents helpful in our implementation
* http://sml-family.org/Basis/list-pair.html
* http://sml-family.org/Basis/integer.html
* Textbook

---

# IR Translator

## Optimizations

### unCx (Ex (CONST n))
We can control jump to either t/f label depending on the 'truthiness' of n.

### Adding a no-op
To handle cases where the program doesn't typecheck (and some other minor edge cases),
we added a no-op expression which is just a register->register move (of the same temp),
which we can optimize away during register allocation at a later phase.

We include this in the optimization section here because this is an example of an
optimization that will be made at a later stage in the compiler.

## Implementation Decisions

Listed below are the interesting cases for our IR translation.

### If

`if TEST then THEN else ELSE`:

```
      CJMP TEST then else
then: THEN
      JMP done
else: ELSE
done:
```

### While

`while TEST do BODY`:

```
test: CJMP TEST body break
body: BODY
      JMP test
break:
```

### For
`for VAR <- LOW to HI do BODY`:

```
init: var <- lo
      CJMP (<= var hi) body break
body: BODY
      CJMP (< var hi) loop break
loop: var <- var + 1
      JMP body
break:
```

### Constructing SEQs
Ideally, a SEQ should be a `stm list`, instead of a `stm * stm`. However, we cannot
modify this because the canonicalizer is already going to be written for us. This is
unfortunate. To accommodate, we wrote a helper to convert a list of stms into a binary
tree `buildSeq`.

## Modifications to `semant.sml`

### Processing breaks
To handle break statements, we need a label to break to, when exiting a for loop or while loop.
We need to pass around the label, starting with an 'outermost label' for the GLOBAL level of the program
in transExp, and update that to a break label to pass in the recursive calls.

### Processing SeqExps
Our logic to handle SeqExps is correct for semantic analysis, but it doesn't actually return the translated
exp results needed by the IR, so we had to modify the case analysis for SeqExps to transform to IR properly.

---

# Frame Analyzer

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

### Datatypes & Definitions
The level datatype can be broken specifically into two cases:
* What we called `GLOBAL`, which represents the outermost level containing functionality like built-in functions
* And 'usable' `LEVEL`s, which represent levels in the program execution with a linked parent level.

Our FindEscape definition skeleton is analagous to the skeleton for the Semant file. This highlights that while we
are preparing to work with the IR, we are still in a state where we are mutating on the AST and producing a tree.

### Code for Impossible Errors

See previous `README.md` (`semant`). We have not added any superfluous error cases,
however we have not removed the ones that were present beforehand.

## Testing

We tested our code against the test cases provided by Appel (`tests/`), alongside
our own tests for some special cases in isolation / not covered in the other test
folder (`tests_semant`).  We also ran the tests we had written for our previous
type-checker (`tests_custom`), to validate that our functionality remains consistent.

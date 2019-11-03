# CS 4410 Parser
## Team: Anand Kumar (akumar) + David Reed (reedda)

---

## Update November 3rd

As of Nov 3rd, this folder's driver is not functional, thanks to reliance on IR
package files.

## References

We found these documents helpful in our implementation of the type-checker.
* https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
* http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf
* https://cs.nyu.edu/courses/fall13/CSCI-GA.2130-001/tiger-spec.pdf
* Textbook

---

## Implementation Decisions

Listed below are the interesting cases for our type checker (largely those
required for part 2 in the book implementation).

### Valid Breaks

To detect whether a Break is in the correct location in the AST, when we enter
a loop that is breakable (`WHILE` or `FOR`), we enter a dummy variable into the
environment to indicate that from this point on, `BREAK` is a valid statement.

The dummy variable used (`*breakable`) was chosen because variable names in Tiger
cannot begin with an asterisk, according to the grammar of the language. Thus we
know that we will not be colliding with possibly valid names.

### Mutually Recursive Types

For mutually recursive Type definitions, we create a list to maintain the Header
entries that will be inserted into the environment. We then move through the
type declarations provided, and insert dummy entries (`T.NAME(name, ref NONE)`)
into the environment for those declarations.

After we translate the bodies of the declarations with the header-inclusive type
environment, we then iterate over the list of dummy declarations that had been
previously stored and replace their `ref` values with the respective entry from
the filled type environment (see `transTypeDecls`).

#### Loop detection

For our loop detection over a type environment containing recursive types, we
maintain a list of the symbols that have been seen when traversing the actual
type definition for a given user-defined type. If we come across the same symbol
more than once in a chain of Name types, we know that we've come across a loop,
otherwise we can continue on in our list of type definitions.

Note that this algorithm runs in `O(n^2)` due to relying on `contains`, however
the method that Prof. Shivers described in class (based on the maximal length of
a non-looping recursive type declaration chain) _also_ runs in `O(n^2)`, not to
mention that in the context of our compiler we are dealing with relatively small
values for `n`, where `n` is the number of consecutive type declarations.

### Mutually Recursive Functions

Since function headers are required to provide their parameter and return types,
we can follow a process similar to mutually recursive types: add the function
headers to the environment before we parse the bodies, then parse the bodies with
that augmented environment.

Because mutually recursive functions carry more type information than type
declarations do, we do not have to do a `ref` backfill step.

### Nil Records

In order to more closely follow the book, we did not implement a Lattice for
our type system. This hamstrung us when attempting to allow for `nil` being a
valid value for `Record` types, as we ended up falling back on the hack of
allowing `nil` and `Record` _types_ be considered equal (the literal `nil` will
 have the type `nil`, which will be considered "the same" as the type `Record`).

## Stylistic Choices

### Code for Impossible Errors

 In a few locations (`transDec`, `transDecs`) we have case analysis that prints
 an error, yet is annotated with `NOTE should never occur`. The reason for this
 is that the cases that are provided are ruled out by our Grammar, however we
 prefer the safety of the SML compiler indicating that we have full case analysis.

### Base Case
Since we didn't implement a sort of lattice structure, we chose T.UNIT to represent
base case of programs that threw a type error. Note that it doesn't really matter
what the type is, since the interesting thing to the user is that there was a type error
that needs to be fixed.

## Testing

We tested our code against the test cases provided by Appel (`tests/`), alongside
our own tests for some special cases in isolation / not covered in the other test
folder (`tests_type`).

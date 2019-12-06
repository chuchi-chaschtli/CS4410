# CS 4410 Graph Coloring Register Allocation
## Team: Anand Kumar (akumar) + David Reed (reedda)

---
How to run the Register Allocation step:

`sml -m sources.cm` from top-level directory

Once in the SML REPL, use `Main.compile "filename"` on any Tiger file.

See testing section for details.

---

## References

We found these documents helpful in our implementation
* Textbook

---

## Register Allocation

The signatures and pseudocode for algorithms for this assignment pretty much
came entirely from the book source.

### Strategy

We started by working on functionality to color by looking at the textbook. Our
register allocator is "level 1" - it does not coalesce or spill.

We started by writing signatures to helpers we would need:
- set operations (union, diff, intersect) on tables
- stack operations (push, pop, peek)
- functions provided in book pseudocode (decrDegree, simplifyWorklist, etc)

Once we had the skeleton in place, we looked to implementing functionality by
carefully inspecting the pseudocode in the textbook and filling in the pieces.

### Challenges

Working on this assignment exposed a lot of bugs and missing features in various
other pieces of our code that we had to back-fill.

- our insn selection had some typos
- our Frame structures didn't expose information like number of registers available,
or the names of those registers to use as colors
- we struggled for some time on testing, both in terms of figuring out good test
cases and understanding the minute details of the graph coloring. Admittedly, we
were rusty on Tiger syntax and had to revisit the Tiger grammar spec in order to
write more test cases.

### Points of interest

We continued to leverage SML functionality like creating a BinaryMapFn to operate on Strings
(which is useful for maintaining colors, which are represented as Frame.register which is a string alias)

The factorial assembler listing from assignment 6 can be found at `tests_insn/factorial.tig.s`

### Known Bugs
The allocator does not adequately raise an error when spilling should occur.

---

## Testing

Due to time constraints in the last assignment, we had a lot of tech debt in terms
of testing the liveness analysis. This makes it harder to test the register allocator,
which requires the interference graph generated during liveness.

We wrote a few test files which can be found in `tests_color` that test some edge
cases not covered by the existing tiger programs in other directories (like spilling, etc)

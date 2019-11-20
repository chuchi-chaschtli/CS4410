# CS 4410 Liveness Analysis
## Team: Anand Kumar (akumar) + David Reed (reedda)

---
How to run the Liveness Analysis step:

`sml -m sources.cm` **within** the `/live` directory

We do not currently have a `main.sml` driver, however we do have a functional `show`.

---

## References

We found these documents helpful in our implementation
* Textbook

---

## Liveness Analysis

The signatures and pseudocode for algorithms for this assignment all came from
chapters 10 and 11 of the book. We stuck to the book wherever able.

### Strategy

Our approach for liveness is the set union algorithm described in the book, with
convergence checks at each iteration of liveness calculation whether or not the
`live in` or `live out` sets have changed.

### Move edges

Within our Liveness module, when we come across a definition resulting from a
register->register move, we do not create an interference edge on our graph.
We do not currently maintain any kind of move edges for coalescing, so future
work may need to incorporate that if we intend to coalesce reg->reg copies.

### Show

We largely copied the graph-printing logic from `printtree.sml`, as it remained
similar between the two.

---

## Response to prior feedback

In response to past feedback on assignments, we leveraged pattern-matching a bit
more in this assignment (see usage of `BinaryMapFn`, `ListSetFn`, and `ORD_KEY`).

---

## Testing

Due to time constraints we did not test this part of our compiler. Granted, this
is likely some tech debt we will pay down going forward. As a consequence we did
not write our own test files with "interesting" test cases for this assignment.

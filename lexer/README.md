## CS 4410 Lexer
### Team: Anand Kumar + David Reed

---

### Comments
Because comments nest in Tiger, we cannot use regular expressions to capture
comments, because regex cannot count and therefore cannot handle an arbitrarily
large number of nested comments.

As such, our implementation keeps a counter representing the depth of the comment
tree. When we first see a comment start delimiter, we do a state transition to
comment mode, and increment the depth counter. Every time we see a start delimiter,
we increment the counter. Similarly, when we see an end delimiter, we decrement
the counter. If the counter hits 0, we do a state transition back to the initial
state, because we know we are done processing comments and are back to processing
code.

When we are in a comment, we don't actually care what the program text is and can
just process trivially.

### String Literals

### Errors

### End-of-file (EOF)
There are two interesting cases that have to be handled when we see an EOF. If
we are in a comment or inside a string literal and see an EOF, we decided to use
the provided error reporting module to terminate the lexer. The lexer reports
the line number and line position of the comment or string that caused the error.
If we aren't in a comment or string literal, the EOF is allowed.

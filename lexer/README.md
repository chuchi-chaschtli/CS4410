## CS 4410 Lexer
### Team: Anand Kumar (akumar) + David Reed (reedda)

---

### References

We found these documents helpful in our implementation of the lexer.
* https://www.cs.princeton.edu/~appel/modern/ml/ml-lex/manual.html
* http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf
* http://www.asciitable.com/

---

### Formatting

Our code formatting is a bit inconsistent due to not establishing a unified code
style at the outset.

Besides this, in the Tiger's lexer definition `tiger.lex`, we separated our
regex state blocks by three newlines to make it easier visually.

When logging errors, they are output in the format:
`../relative/path/to/file:line.pos: error message`

---
### Testing

We tested our generated lexer directly against the sample test files provided
(see `tests` directory) using `driver.sml`, as well as wrote some of our own
test cases in the `sample_tests` directory for some of the more interesting
edge-cases relating to ASCII, string literals, and comments.

Likewise we discovered a feature in our Lexer wherein `123abc` gets lexed as
`INT(123)` and `ID(abc)`. This feature was left in, as we intend to delegate
handling of that edge-case to the Parser.

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

---

### String Literals

There are a lot of sub-cases in string literal lexing, which makes it more difficult
to handle correctly than comments.

* When we see a quote while in the initial state, we transition to the string state (or vice versa on a closing quote). When we
enter the string state, we reset the string buffer, which contains the temporary string as we lex the input until we close the string.

* When we see an escaped backslash while in the string state, we are in a particular escape sequence, depending on the proceeding character(s). We maintain an escape state, but this is more out of organization than necessity.
  * If the next character is `n`, `t`, or `\"`, we add that character to the current string buffer and transition back to the string state.
  * If the next character is `\\`, we are done escaping and can transition back to string state.
  * If we see a control character (aka, `\^c`), append that ASCII control char to the buffer and transition to string state.
  * If we see a whitespace character, transition to the whitespace state to process arbitrarily large amount of whitespace (including newlines). We will process whitespace until we hit a matching `\`, indicating that we are back in the string.
  * If we see a decimal ASCII code (`\ddd`), append that ASCII char to the buffer and transition to string state.

* If we see a printable character (ascii code [032, 126]), append that to the buffer directly. Unless...

---

### Errors

We handled errors in the following scenarios:

* EOF seen while in comment or string: error description with line number and position
* If we see a control character while not being escaped, error specifying illegal string with position
* If we see a non whitespace character while in a `\f___f\` string, error specifying illegal whitespace processing with position
* If we see a printable character while we are in an escape sequence, error specifying illegal escape sequence with position
* If we see an invalid character token while in the initial state, error appropriately and specify the position

---

### End-of-file (EOF)

There are two interesting cases that have to be handled when we see an EOF. If
we are in a comment or inside a string literal and see an EOF, we decided to use
the provided error reporting module to terminate the lexer. The lexer reports
the line number and line position of the comment or string that caused the error.
If we aren't in a comment or string literal, the EOF is allowed.

The built-in error reporting machinery acts on the lineNum and linePos ref vars. We increment the lineNum
and append to linePos in our rules, but the EOF processing happens in the built-in eof() function, and
does not modify those two ref vars first. As such, we increment the given pos (corresponding to yypos) first,
so that we can report the error on the line and position at which the error occurred.

## CS 4410 Lexer
### Team: Anand Kumar (akumar) + David Reed (reedda)

---

### References

We found these documents helpful in our implementation of the lexer.
* https://www.cs.princeton.edu/~appel/modern/ml/ml-lex/manual.html
* http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf
* http://www.asciitable.com/

---
### Testing

We tested directly against the sample test files (see `tests` directory) using
the provided `driver.sml`, as well as wrote some of our own test cases:
* `bad_comment.tig` (a tig file that shouldn't lex because of an unclosed comment)
* `bad_string.tig` (a tig file that shouldn't lex because of an unclosed string)

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
There are a lot of subcases in string literal lexing, which makes it more difficult
to handle correctly than comments.

* When we see a quote while in the initial state, we transition to the string state (or vice versa on a closing quote). When we
enter the string state, we reset the string buffer, which contains the temporary string as we lex the input until we close the string.
* When we see an escaped backslash while in the string state, we are in a particular escape sequence, depending on the proceeding character(s). We maintain an escape state, but this is more out of organization than necessity.
  * If the next character is `n`, `t`, or `\"`, we add that character to the current string buffer and transition back to the string state.
  * If the next character is `\\`, we are done escaping and can transition back to string state.
  * If we see a control character (aka, a non-printable ascii code), append that to the buffer and transition to string state.
  * If we see a whitespace character, transition to the whitespace state to process arbitrarily large amount of whitespace (including newlines).
* If we see a printable character (ascii code [32, 126]), append that to the buffer directly.

### Errors
We handled errors in the following scenarios:

* EOF seen while in comment or string: error description with line number and position
* If we see a control character while not being escaped, error specifying illegal string with position
* If we see a non whitespace character while in a f___f string, error specifying illegal whitespace processing with position
* If we see a printable character while we are in an escape sequence, error specifying illegal escape sequence with position
* If we see an invalid character token while in the initial state, error appropriately and specify the position

### End-of-file (EOF)
There are two interesting cases that have to be handled when we see an EOF. If
we are in a comment or inside a string literal and see an EOF, we decided to use
the provided error reporting module to terminate the lexer. The lexer reports
the line number and line position of the comment or string that caused the error.
If we aren't in a comment or string literal, the EOF is allowed.

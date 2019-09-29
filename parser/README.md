## CS 4410 Parser
### Team: Anand Kumar (akumar) + David Reed (reedda)

---

### References

We found these documents helpful in our implementation of the parser.
* https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
* http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf
* textbook

---

### Shift-Reduce Conflicts

* If-Then with a dangling else results in a shift-reduce conflict. Per in-class discussions, we shift, which means the ELSE binds to the most "recent" seen if statement.
* l-value expressions and array assignment expressions had a shift-reduce conflict in the case of `lvalue LBRACK expr RBRACK`, because our productions also define `lvalue -> ID` as a valid production. To disambiguate, we decided to add another production
rule `lvalue -> ID LBRACK expr LBRACK`.
* The binary operators we defined cause shift-reduce conflicts with the grammar productions for IF, WHILE, FOR, ASSIGN, ARRAY expressions. To fully evaluate the expression, we must shift.
* We resolved many shift-reduce conflicts by defining precedence hierarchy explicitly.
* We were left with two shift-reduce conflicts in FUNCTION and TYPE not being able to reduce, which signified that our grammar for declaration sequences was invalid as written, in that it wasn't supporting mutual recursion tightly. We rewrote the grammar for declarations:
```
decl_seq: type_decls not_type_decls   (A.TypeDec(rev(type_decls))::not_type_decls)
        | func_decls not_func_decls   (A.FunctionDec(rev(func_decls))::not_func_decls)
        | var_decls  not_var_decls    (var_decls @ not_var_decls)
```

With an example for our mutually recursive step:
```
not_type_decls: func_decls not_func_decls   (A.FunctionDec(rev(func_decls)) :: not_func_decls)
              | var_decls not_var_decls     (var_decls @ not_var_decls)
              |                             (nil)
```

### Other Points of Interest
* Part of designing an LR(1) Parser means our grammar should be written in a left-recursive way, so that our stack space is constant rather than linear. We implemented this approach. However, for sequences of elements (e.g sequence of declarations or expressions), this can be tricky. An example:

```
expr_list: expr_list COMMA expr    (???)
         | expr                    (expr::nil)
         |                         (nil)
```
How do we fill the semantic action for the case where we have to recurse? We could just do `expr::expr_list`, but then we have to reverse the tokens at the end using `rev()`. Alternatively, we could avoid using a `rev()` call by utilizing list concatenation like `expr_list @ (expr::nil)`. Both rev() and list concatenation are effectively linear runtime on the size of the list. However, we think the benefit of having a constant stack space outweighs the linear runtime cost on doing
list concatenation. We think it's more readable to use list concatenation over reversing the list at the end, because otherwise
it forces the reader to remember that the list is being created in reverse order.

* We also specified `%pure`. As discussed in class, this is just evaluating the semantic actions as we parse, instead of creating thunks to be deferred.

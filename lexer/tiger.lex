(*
  TODO: String literal parsing (different states? how the hell?)

  TODO: State handling in EOF function (trailing comment/string literal)

  TODO: Test/Validate existing lex capabilty, ensure lexer actually being
        generated and ran against STDIN

  TODO: Potentially convert state into %arg variables, depending on if valid as-is

  TODO: Validate we're covering all keywords, punctiation, and operators

  TODO: Data structure (map?) to make this parsing a little more abstracted
*)

(* user declarations *)

type pos = int
type lexresult = Tokens.token

val commentDepth = ref 0
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* If we hit EOF, we only care about current state, not code validation *)
fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% (* ML-Lex declarations *)

%s COMMENT ;

%% (* Rules *)

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

val STRING: (string) *  linenum * linenum -> token
(* TODO: make sure we handle ===> val EOF:  linenum * linenum -> token *)

<INITIAL> "/*" => (
                    YYBEGIN(COMMENT);
                    continue();
                  );
<COMMENT> "/*" => (
                    commentDepth := !commentDepth + 1;
                    continue();
                  );
<COMMENT> "*/" => (
                    (if commentDepth = 0
                     then YYBEGIN(INITIAL)
                     else commentDepth := !commentDepth - 1);
                    continue();
                  );
<COMMENT> .    => (continue());
(* TODO: Consider special error for */ when in INITIAL state? *)

(*
  Keywords:
    array, break, do, else, end, for,
    function, if, in, let, nil, of, then, to, type, var,
    while.
*)
<INITIAL> "type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL> "var" => (Tokens.VAR(yypos, yypos+3));
<INITIAL> "function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL> "break" => (Tokens.BREAK(yypos, yypos+5);
<INITIAL> "of" => (Tokens.OF(yypos, yypos+2));
<INITIAL> "end" => (Tokens.END(yypos, yypos+3));
<INITIAL> "in" => (Tokens.IN(yypos, yypos+2));
<INITIAL> "nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL> "let" => (Tokens.LET(yypos, yypos+3));
<INITIAL> "do" => (Tokens.DO(yypos, yypos+2));
<INITIAL> "to" => (Tokens.TO(yypos, yypos+2));
<INITIAL> "for" => (Tokens.FOR(yypos, yypos+3);
<INITIAL> "while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL> "else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL> "then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL> "if" => (Tokens.IF(yypos, yypos+2));
<INITIAL> "array" => (Tokens.ARRAY(yypos, yypos+5));

(*
  Punctuation and operators:
    (, ), [, ], {, }, :, :=,
    ., ,, ;, *, /, +, -, =, <>, >, <, >=, <=, &, |.
*)
<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> "|" => (Tokens.OR(yypos, yypos+1));
<INITIAL> "&" => (Tokens.AND(yypos, yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL> ">" => (Tokens.GT(yypos, yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL> "<" => (Tokens.LT(yypos, yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos+1));

<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos+1));

<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos+1));

<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));

(*
  An identifier starts with a letter, followed by zero or more letters, underscores, or digits.
  Keywords cannot be used as identifiers.
  Identifiers are case-sensitive.
*)
<INITIAL> [A-Za-z][A-Za-z0-9_]* => (Tokens.ID(yytext, yypos, yypos + String.size(yytext)));

(* An integer literal is a sequence of one or more digits from 0-9. *)
<INITIAL> [0-9]+ => (Tokens.INT(Int.fromString(yytext), yypos, yypos + String.size(yytext)));

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

(*
  TODO: String literal parsing (different states? how the hell?)

  TODO: State handling in EOF function (trailing comment/string literal)

  TODO: Test/Validate existing lex capability, ensure lexer actually being
        generated and ran against STDIN

  TODO: Potentially convert state into %arg variables, depending on if valid as-is

  TODO: Validate we're covering all keywords, punctuation, and operators

  TODO: Data structure (map?) to make this parsing a little more abstracted
*)

(* user declarations *)

type pos = int
type lexresult = Tokens.token

val commentDepth = ref 0

val isEscaping = ref false
val escapeBuffer = ref ""
val escapeStartPos = ref 0

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* If we hit end of file, we only care about current state, not code validation *)
fun eof() = (
  if (!commentDepth > 0) then
    err (hd(!linePos))
        ("EOF in comment detected at line number = " ^ Int.toString(!lineNum) ^
         " at line position " ^ Int.toString(hd(!linePos)))
  else if (isEscaping) then
    err (hd(!linePos))
        ("EOF in string detected at line number = " ^ Int.toString(!lineNum) ^
        "at line position " ^ Int.toString(hd(!linePos)))
  let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end)

%%

%s COMMENT ESCAPE;

%%

<INITIAL>\n	=> (lineNum := !lineNum+1;
                linePos := yypos :: !linePos;
                continue());
<COMMENT>\n	=> (lineNum := !lineNum+1;
                linePos := yypos :: !linePos;
                continue());

<INITIAL>[\ \t]+ => (continue());
<COMMENT>[\ \t]+ => (continue());

<INITIAL>"/*"  => (YYBEGIN(COMMENT);
                   commentDepth := 1;
                   continue());
<COMMENT> "/*" => (commentDepth := !commentDepth + 1;
                   continue());
<COMMENT> "*/" => (commentDepth := !commentDepth - 1;
                   (if (commentDepth = 0)
                    then YYBEGIN(INITIAL));
                   continue());
<COMMENT> .    => (continue());

"type"      => (Tokens.TYPE(yypos, yypos+4));
"var"       => (Tokens.VAR(yypos, yypos+3));
"function"  => (Tokens.FUNCTION(yypos, yypos+8));
"break"     => (Tokens.BREAK(yypos, yypos+5));
"of"        => (Tokens.OF(yypos, yypos+2));
"end"       => (Tokens.END(yypos, yypos+3));
"in"        => (Tokens.IN(yypos, yypos+2));
"nil"       => (Tokens.NIL(yypos, yypos+3));
"let"       => (Tokens.LET(yypos, yypos+3));
"do"        => (Tokens.DO(yypos, yypos+2));
"to"        => (Tokens.TO(yypos, yypos+2));
"for"       => (Tokens.FOR(yypos, yypos+3));
"while"     => (Tokens.WHILE(yypos, yypos+5));
"else"      => (Tokens.ELSE(yypos, yypos+4));
"then"      => (Tokens.THEN(yypos, yypos+4));
"if"        => (Tokens.IF(yypos, yypos+2));
"array"     => (Tokens.ARRAY(yypos, yypos+5));

":=" => (Tokens.ASSIGN(yypos, yypos+2));
"|"  => (Tokens.OR(yypos, yypos+1));
"&"  => (Tokens.AND(yypos, yypos+1));
">=" => (Tokens.GE(yypos, yypos+2));
">"  => (Tokens.GT(yypos, yypos+1));
"<=" => (Tokens.LE(yypos, yypos+2));
"<"  => (Tokens.LT(yypos, yypos+1));
"<>" => (Tokens.NEQ(yypos, yypos+2));
"="  => (Tokens.EQ(yypos, yypos+1));

"/" => (Tokens.DIVIDE(yypos, yypos+1));
"*" => (Tokens.TIMES(yypos, yypos+1));
"-" => (Tokens.MINUS(yypos, yypos+1));
"+" => (Tokens.PLUS(yypos, yypos+1));

"{" => (Tokens.LBRACE(yypos, yypos+1));
"}" => (Tokens.RBRACE(yypos, yypos+1));
"[" => (Tokens.LBRACK(yypos, yypos+1));
"]" => (Tokens.RBRACK(yypos, yypos+1));
"(" => (Tokens.LPAREN(yypos, yypos+1));
")" => (Tokens.RPAREN(yypos, yypos+1));

";" => (Tokens.SEMICOLON(yypos, yypos+1));
":" => (Tokens.COLON(yypos, yypos+1));
","	=> (Tokens.COMMA(yypos, yypos+1));

[A-Za-z][A-Za-z0-9_]* => (Tokens.ID(yytext, yypos, yypos + String.size(yytext)));
[0-9]+ => (Tokens.INT(Int.fromString(yytext), yypos, yypos + String.size(yytext)));

\" => (YYBEGIN(ESCAPE);
       isEscaping := true;
       escapeStartPos := yypos;
       escapeBuffer := "";
       continue());

<ESCAPE> \" => (YYBEGIN(INITIAL);
                isEscaping := false
                Tokens.String(!escapeBuffer, escapeStartPos, yypos);
                continue());

<INITIAL>. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
<ESCAPE>.  => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

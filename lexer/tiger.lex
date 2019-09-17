(* user declarations *)

type pos = int
type lexresult = Tokens.token

val commentDepth = ref 0

val isString = ref false
val strBuffer = ref ""
val strStartPos = ref 0

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* If we hit end of file, we only care about current state, not code validation *)
fun eof() = (
  let val pos = hd(!linePos) in
  if (!commentDepth > 0) then
    ErrorMsg.error pos
        ("EOF in comment detected at line number = " ^ Int.toString(!lineNum) ^
         " at line position " ^ Int.toString(pos))
  else if (!isString) then
    ErrorMsg.error pos
        ("EOF in string detected at line number = " ^ Int.toString(!lineNum) ^
        "at line position " ^ Int.toString(pos))
  else();
  Tokens.EOF(pos,pos) end)

fun appendBuffer(str) =
  strBuffer := !strBuffer ^ str;

%%

%s COMMENT STRING ESCAPE WHITESPACE;

%%

<INITIAL, COMMENT>\n	=> (lineNum := !lineNum + 1;
                          linePos := yypos :: !linePos;
                          continue());

<INITIAL, COMMENT>[\ \t\r]+ => (continue());

<INITIAL>"/*"  => (YYBEGIN(COMMENT);
                   commentDepth := 1;
                   continue());
<COMMENT> "/*" => (commentDepth := !commentDepth + 1;
                   continue());
<COMMENT> "*/" => (commentDepth := !commentDepth - 1;
                   (if (!commentDepth = 0)
                    then YYBEGIN(INITIAL)
                    else ());
                   continue());
<COMMENT> .    => (continue());

<INITIAL> "type"      => (Tokens.TYPE(yypos, yypos+4));
<INITIAL> "var"       => (Tokens.VAR(yypos, yypos+3));
<INITIAL> "function"  => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL> "break"     => (Tokens.BREAK(yypos, yypos+5));
<INITIAL> "of"        => (Tokens.OF(yypos, yypos+2));
<INITIAL> "end"       => (Tokens.END(yypos, yypos+3));
<INITIAL> "in"        => (Tokens.IN(yypos, yypos+2));
<INITIAL> "nil"       => (Tokens.NIL(yypos, yypos+3));
<INITIAL> "let"       => (Tokens.LET(yypos, yypos+3));
<INITIAL> "do"        => (Tokens.DO(yypos, yypos+2));
<INITIAL> "to"        => (Tokens.TO(yypos, yypos+2));
<INITIAL> "for"       => (Tokens.FOR(yypos, yypos+3));
<INITIAL> "while"     => (Tokens.WHILE(yypos, yypos+5));
<INITIAL> "else"      => (Tokens.ELSE(yypos, yypos+4));
<INITIAL> "then"      => (Tokens.THEN(yypos, yypos+4));
<INITIAL> "if"        => (Tokens.IF(yypos, yypos+2));
<INITIAL> "array"     => (Tokens.ARRAY(yypos, yypos+5));

<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> "|"  => (Tokens.OR(yypos, yypos+1));
<INITIAL> "&"  => (Tokens.AND(yypos, yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL> ">"  => (Tokens.GT(yypos, yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL> "<"  => (Tokens.LT(yypos, yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL> "="  => (Tokens.EQ(yypos, yypos+1));

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
<INITIAL> ","	=> (Tokens.COMMA(yypos, yypos+1));
<INITIAL> "."	=> (Tokens.DOT(yypos, yypos+1));

<INITIAL> [A-Za-z][A-Za-z0-9_]* => (Tokens.ID(yytext, yypos, yypos + String.size(yytext)));
<INITIAL> [0-9]+ => (Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + String.size(yytext)));

<INITIAL> \" => (YYBEGIN(STRING);
                 isString := true;
                strStartPos := yypos;
                strBuffer := "";
                continue());

<STRING> \" => (YYBEGIN(INITIAL);
                isString := false;
                Tokens.STRING(!strBuffer, !strStartPos, yypos));
<STRING> \\ => (YYBEGIN(ESCAPE);
                continue());
<STRING> [\032-\126] => (appendBuffer(yytext);
                         continue());
<STRING> . => (ErrorMsg.error yypos ("illegal string " ^ yytext); continue());


<ESCAPE> n  => (appendBuffer("\\n"); YYBEGIN(STRING); continue());
<ESCAPE> t  => (appendBuffer("\\t"); YYBEGIN(STRING); continue());
<ESCAPE> \" => (appendBuffer("\\\""); YYBEGIN(STRING); continue());
<ESCAPE> \\ => (appendBuffer("\\\\"); YYBEGIN(STRING); continue());
<ESCAPE> [\000-\031|\127] => (appendBuffer("\\" ^ yytext); YYBEGIN(STRING); continue());

<ESCAPE> [\ \t\f\r] => (YYBEGIN(WHITESPACE); continue());
<WHITESPACE> [\ \t\f\r]* => (continue());
<WHITESPACE> \\ => (YYBEGIN(STRING); continue());
<WHITESPACE> \n => (lineNum := !lineNum+1;
                    linePos := yypos :: !linePos;
                    continue());
<WHITESPACE> . => (ErrorMsg.error yypos ("illegal escape during whitespace " ^ yytext); continue());

<ESCAPE> . => (ErrorMsg.error yypos ("illegal escape \\" ^ yytext); continue());
<INITIAL>. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

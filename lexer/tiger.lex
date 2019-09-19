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

val lineCursor = ref 0
fun setCursorPast(yypos, yytext) = (lineCursor := (yypos + String.size(yytext)) - hd(!linePos))
fun initNewline(yypos) = (lineNum := !lineNum + 1;
                          linePos := yypos :: !linePos;
                          lineCursor := 1);

(* If we hit end of file, we only care about current state, not code validation *)
fun eof() = (
  let 
  val charPos = hd(!linePos) + !lineCursor
  in
  if (!commentDepth > 0) then
    ErrorMsg.error charPos
        (" EOF within unclosed comment")
  else if (!isString) then
    ErrorMsg.error charPos
        (" EOF in String literal")
  else();
  Tokens.EOF(charPos, charPos)
  end
)

fun appendBuffer(str) =
  strBuffer := !strBuffer ^ str;

%%

%s COMMENT STRING ESCAPE WHITESPACE;

eol = ("\013\010"|"\010"|"\013");
ws = [\ \t\f];

%%

<INITIAL, COMMENT> {eol}	=> (initNewline(yypos); continue());

<INITIAL, COMMENT> {ws}+ => (continue());

<INITIAL> "/*"  => (YYBEGIN(COMMENT);
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

<INITIAL> "type"      => (setCursorPast(yypos, yytext); Tokens.TYPE(yypos, yypos+4));
<INITIAL> "var"       => (setCursorPast(yypos, yytext); Tokens.VAR(yypos, yypos+3));
<INITIAL> "function"  => (setCursorPast(yypos, yytext); Tokens.FUNCTION(yypos, yypos+8));
<INITIAL> "break"     => (setCursorPast(yypos, yytext); Tokens.BREAK(yypos, yypos+5));
<INITIAL> "of"        => (setCursorPast(yypos, yytext); Tokens.OF(yypos, yypos+2));
<INITIAL> "end"       => (setCursorPast(yypos, yytext); Tokens.END(yypos, yypos+3));
<INITIAL> "in"        => (setCursorPast(yypos, yytext); Tokens.IN(yypos, yypos+2));
<INITIAL> "nil"       => (setCursorPast(yypos, yytext); Tokens.NIL(yypos, yypos+3));
<INITIAL> "let"       => (setCursorPast(yypos, yytext); Tokens.LET(yypos, yypos+3));
<INITIAL> "do"        => (setCursorPast(yypos, yytext); Tokens.DO(yypos, yypos+2));
<INITIAL> "to"        => (setCursorPast(yypos, yytext); Tokens.TO(yypos, yypos+2));
<INITIAL> "for"       => (setCursorPast(yypos, yytext); Tokens.FOR(yypos, yypos+3));
<INITIAL> "while"     => (setCursorPast(yypos, yytext); Tokens.WHILE(yypos, yypos+5));
<INITIAL> "else"      => (setCursorPast(yypos, yytext); Tokens.ELSE(yypos, yypos+4));
<INITIAL> "then"      => (setCursorPast(yypos, yytext); Tokens.THEN(yypos, yypos+4));
<INITIAL> "if"        => (setCursorPast(yypos, yytext); Tokens.IF(yypos, yypos+2));
<INITIAL> "array"     => (setCursorPast(yypos, yytext); Tokens.ARRAY(yypos, yypos+5));

<INITIAL> ":=" => (setCursorPast(yypos, yytext); Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> "|"  => (setCursorPast(yypos, yytext); Tokens.OR(yypos, yypos+1));
<INITIAL> "&"  => (setCursorPast(yypos, yytext); Tokens.AND(yypos, yypos+1));
<INITIAL> ">=" => (setCursorPast(yypos, yytext); Tokens.GE(yypos, yypos+2));
<INITIAL> ">"  => (setCursorPast(yypos, yytext); Tokens.GT(yypos, yypos+1));
<INITIAL> "<=" => (setCursorPast(yypos, yytext); Tokens.LE(yypos, yypos+2));
<INITIAL> "<"  => (setCursorPast(yypos, yytext); Tokens.LT(yypos, yypos+1));
<INITIAL> "<>" => (setCursorPast(yypos, yytext); Tokens.NEQ(yypos, yypos+2));
<INITIAL> "="  => (setCursorPast(yypos, yytext); Tokens.EQ(yypos, yypos+1));

<INITIAL> "/" => (setCursorPast(yypos, yytext); Tokens.DIVIDE(yypos, yypos+1));
<INITIAL> "*" => (setCursorPast(yypos, yytext); Tokens.TIMES(yypos, yypos+1));
<INITIAL> "-" => (setCursorPast(yypos, yytext); Tokens.MINUS(yypos, yypos+1));
<INITIAL> "+" => (setCursorPast(yypos, yytext); Tokens.PLUS(yypos, yypos+1));

<INITIAL> "{" => (setCursorPast(yypos, yytext); Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "}" => (setCursorPast(yypos, yytext); Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "[" => (setCursorPast(yypos, yytext); Tokens.LBRACK(yypos, yypos+1));
<INITIAL> "]" => (setCursorPast(yypos, yytext); Tokens.RBRACK(yypos, yypos+1));
<INITIAL> "(" => (setCursorPast(yypos, yytext); Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ")" => (setCursorPast(yypos, yytext); Tokens.RPAREN(yypos, yypos+1));

<INITIAL> ";" => (setCursorPast(yypos, yytext); Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> ":" => (setCursorPast(yypos, yytext); Tokens.COLON(yypos, yypos+1));
<INITIAL> ","	=> (setCursorPast(yypos, yytext); Tokens.COMMA(yypos, yypos+1));
<INITIAL> "."	=> (setCursorPast(yypos, yytext); Tokens.DOT(yypos, yypos+1));

<INITIAL> [A-Za-z][A-Za-z0-9_]* => (setCursorPast(yypos, yytext);
                                    Tokens.ID(yytext, yypos, yypos + String.size(yytext)));
<INITIAL> [0-9]+ => (setCursorPast(yypos, yytext);
                     Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + String.size(yytext)));

<INITIAL> \" => (YYBEGIN(STRING);
                 isString := true;
                 strStartPos := yypos;
                 strBuffer := "";
                 continue());

<STRING> \" => (YYBEGIN(INITIAL);
                isString := false;
                setCursorPast(yypos, yytext);
                Tokens.STRING(!strBuffer, !strStartPos, yypos));
<STRING> \\ => (YYBEGIN(ESCAPE);
                continue());
<STRING> [\032-\126] => (appendBuffer(yytext);
                         continue());
<STRING> {eol} => (initNewline(yypos);
                   ErrorMsg.error yypos (" illegal string: " ^ yytext); continue());
<STRING> . => (ErrorMsg.error yypos (" illegal string: " ^ yytext); continue());


<ESCAPE> n  => (appendBuffer("\\n"); YYBEGIN(STRING); continue());
<ESCAPE> t  => (appendBuffer("\\t"); YYBEGIN(STRING); continue());
<ESCAPE> \" => (appendBuffer("\\\""); YYBEGIN(STRING); continue());
<ESCAPE> \\ => (appendBuffer("\\\\"); YYBEGIN(STRING); continue());
<ESCAPE> [\000-\009|\011|\012|\014-\031|\127] => (appendBuffer("\\" ^ yytext); YYBEGIN(STRING); continue());
<ESCAPE> {ws}* => (YYBEGIN(WHITESPACE); continue());
<ESCAPE> {eol} => (initNewline(yypos); YYBEGIN(WHITESPACE); continue());

<WHITESPACE> {ws}* => (continue());
<WHITESPACE> {eol} => (initNewline(yypos); continue());
<WHITESPACE> \\ => (YYBEGIN(STRING); continue());
<WHITESPACE> . => (ErrorMsg.error yypos (" illegal escape during whitespace: " ^ yytext); continue());

<ESCAPE> . => (ErrorMsg.error yypos (" illegal escape: \\" ^ yytext); continue());
<INITIAL>. => (ErrorMsg.error yypos (" illegal character: " ^ yytext); continue());

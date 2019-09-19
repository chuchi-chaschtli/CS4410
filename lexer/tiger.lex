(* user declarations *)

type pos = int
type lexresult = Tokens.token

val commentDepth = ref 0

val isString = ref false
val strBuffer = ref ""
val strStartPos = ref 0

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val err = ErrorMsg.error

val lineCursor = ref 0
fun initNewline(yypos) = (lineNum := !lineNum + 1;
                          linePos := yypos :: !linePos;
                          lineCursor := 1);
fun setCursorPast(yypos, yytext) = (lineCursor := (yypos + String.size(yytext)) - hd(!linePos))
fun makeToken(tokenizer, yypos, yytext) = (tokenizer(yypos, yypos + String.size(yytext)));

fun setCursorPastThenMakeToken(tokenizer, yypos, yytext) = (
  setCursorPast(yypos, yytext);
  makeToken(tokenizer, yypos, yytext))

fun convertAsciiToString(str) =
    let val octalValue = String.substring(str, 1, 3)
        val charValue = chr(valOf(Int.fromString(octalValue)))
    in Char.toString(charValue) end

(* If we hit end of file, we only care about current state, not code validation *)
fun eof() = (
  let
  val charPos = hd(!linePos) + !lineCursor
  in
  if (!commentDepth > 0) then
    err charPos
      ("EOF within unclosed comment")
  else if (!isString) then
    err charPos
      ("EOF in String literal")
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
asciiCodes = (0[0-9]{2})|(1[0-1][0-9])|(12[0-7]);
legalStringAsciiCode = [\032-\033\035-\091\093-\126];

%%

<INITIAL, COMMENT, WHITESPACE> {ws}* => (continue());
<INITIAL, COMMENT, WHITESPACE> {eol} => (initNewline(yypos); continue());

<INITIAL> "/*"  => (YYBEGIN(COMMENT); commentDepth := 1; continue());



<COMMENT> "/*" => (commentDepth := !commentDepth + 1; continue());
<COMMENT> "*/" => (commentDepth := !commentDepth - 1;
                   (if (!commentDepth = 0)
                    then YYBEGIN(INITIAL)
                    else ());
                   continue());
<COMMENT> .    => (continue());



<INITIAL> "type"      => (setCursorPastThenMakeToken(Tokens.TYPE, yypos, yytext));
<INITIAL> "var"       => (setCursorPastThenMakeToken(Tokens.VAR, yypos, yytext));
<INITIAL> "function"  => (setCursorPastThenMakeToken(Tokens.FUNCTION, yypos, yytext));
<INITIAL> "break"     => (setCursorPastThenMakeToken(Tokens.BREAK, yypos, yytext));
<INITIAL> "of"        => (setCursorPastThenMakeToken(Tokens.OF, yypos, yytext));
<INITIAL> "end"       => (setCursorPastThenMakeToken(Tokens.END, yypos, yytext));
<INITIAL> "in"        => (setCursorPastThenMakeToken(Tokens.IN, yypos, yytext));
<INITIAL> "nil"       => (setCursorPastThenMakeToken(Tokens.NIL, yypos, yytext));
<INITIAL> "let"       => (setCursorPastThenMakeToken(Tokens.LET, yypos, yytext));
<INITIAL> "do"        => (setCursorPastThenMakeToken(Tokens.DO, yypos, yytext));
<INITIAL> "to"        => (setCursorPastThenMakeToken(Tokens.TO, yypos, yytext));
<INITIAL> "for"       => (setCursorPastThenMakeToken(Tokens.FOR, yypos, yytext));
<INITIAL> "while"     => (setCursorPastThenMakeToken(Tokens.WHILE, yypos, yytext));
<INITIAL> "else"      => (setCursorPastThenMakeToken(Tokens.ELSE, yypos, yytext));
<INITIAL> "then"      => (setCursorPastThenMakeToken(Tokens.THEN, yypos, yytext));
<INITIAL> "if"        => (setCursorPastThenMakeToken(Tokens.IF, yypos, yytext));
<INITIAL> "array"     => (setCursorPastThenMakeToken(Tokens.ARRAY, yypos, yytext));
<INITIAL> ":="        => (setCursorPastThenMakeToken(Tokens.ASSIGN, yypos, yytext));
<INITIAL> "|"         => (setCursorPastThenMakeToken(Tokens.OR, yypos, yytext));
<INITIAL> "&"         => (setCursorPastThenMakeToken(Tokens.AND, yypos, yytext));
<INITIAL> ">="        => (setCursorPastThenMakeToken(Tokens.GE, yypos, yytext));
<INITIAL> ">"         => (setCursorPastThenMakeToken(Tokens.GT, yypos, yytext));
<INITIAL> "<="        => (setCursorPastThenMakeToken(Tokens.LE, yypos, yytext));
<INITIAL> "<"         => (setCursorPastThenMakeToken(Tokens.LT, yypos, yytext));
<INITIAL> "<>"        => (setCursorPastThenMakeToken(Tokens.NEQ, yypos, yytext));
<INITIAL> "="         => (setCursorPastThenMakeToken(Tokens.EQ, yypos, yytext));
<INITIAL> "/"         => (setCursorPastThenMakeToken(Tokens.DIVIDE, yypos, yytext));
<INITIAL> "*"         => (setCursorPastThenMakeToken(Tokens.TIMES, yypos, yytext));
<INITIAL> "-"         => (setCursorPastThenMakeToken(Tokens.MINUS, yypos, yytext));
<INITIAL> "+"         => (setCursorPastThenMakeToken(Tokens.PLUS, yypos, yytext));
<INITIAL> "{"         => (setCursorPastThenMakeToken(Tokens.LBRACE, yypos, yytext));
<INITIAL> "}"         => (setCursorPastThenMakeToken(Tokens.RBRACE, yypos, yytext));
<INITIAL> "["         => (setCursorPastThenMakeToken(Tokens.LBRACK, yypos, yytext));
<INITIAL> "]"         => (setCursorPastThenMakeToken(Tokens.RBRACK, yypos, yytext));
<INITIAL> "("         => (setCursorPastThenMakeToken(Tokens.LPAREN, yypos, yytext));
<INITIAL> ")"         => (setCursorPastThenMakeToken(Tokens.RPAREN, yypos, yytext));
<INITIAL> ";"         => (setCursorPastThenMakeToken(Tokens.SEMICOLON, yypos, yytext));
<INITIAL> ":"         => (setCursorPastThenMakeToken(Tokens.COLON, yypos, yytext));
<INITIAL> ","	      => (setCursorPastThenMakeToken(Tokens.COMMA, yypos, yytext));
<INITIAL> "."	      => (setCursorPastThenMakeToken(Tokens.DOT, yypos, yytext));

<INITIAL> [A-Za-z][A-Za-z0-9_]* => (setCursorPast(yypos, yytext);
                                    Tokens.ID(yytext, yypos, yypos + String.size(yytext)));
<INITIAL> [0-9]+                => (setCursorPast(yypos, yytext);
                                    Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + String.size(yytext)));
<INITIAL> \" => (YYBEGIN(STRING);
                 isString := true;
                 strStartPos := yypos;
                 strBuffer := "";
                 continue());
<INITIAL> .  => (err yypos (" illegal character: " ^ yytext); continue());



<STRING> \" => (YYBEGIN(INITIAL);
                isString := false;
                setCursorPast(yypos, yytext);
                Tokens.STRING(!strBuffer, !strStartPos, yypos));
<STRING> {legalStringAsciiCode}* => (appendBuffer(yytext); continue());
<STRING> {eol} => (initNewline(yypos);
                   err yypos (" illegal string: " ^ yytext);
                   continue());
<STRING> \\ => (YYBEGIN(ESCAPE); continue());
<STRING> .  => (err yypos (" illegal string: " ^ yytext); continue());



<ESCAPE> n   => (appendBuffer("\\n"); YYBEGIN(STRING); continue());
<ESCAPE> t   => (appendBuffer("\\t"); YYBEGIN(STRING); continue());
<ESCAPE> \"  => (appendBuffer("\\\""); YYBEGIN(STRING); continue());
<ESCAPE> \\  => (appendBuffer("\\\\"); YYBEGIN(STRING); continue());
<ESCAPE> {asciiCodes} => (appendBuffer(convertAsciiToString("\\" ^ yytext));
                          YYBEGIN(STRING);
                          continue());
<ESCAPE> {ws}* => (YYBEGIN(WHITESPACE); continue());
<ESCAPE> {eol} => (initNewline(yypos); YYBEGIN(WHITESPACE); continue());
<ESCAPE> . => (err yypos (" illegal escape: \\" ^ yytext); continue());



<WHITESPACE> \\ => (YYBEGIN(STRING); continue());
<WHITESPACE> . => (err yypos (" illegal escape during whitespace: " ^ yytext);
                   continue());

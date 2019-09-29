(* user declarations *)

(* TODO fix missing control char lexing, i.e. "\^C" *)

type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val commentDepth = ref 0

val isString = ref false
val strBuffer = ref ""
val strStartPos = ref 0

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val err = ErrorMsg.error

val lineCursor = ref 1
fun setCursorPast(yypos, yytext) = (lineCursor := (yypos + String.size(yytext)) - hd(!linePos))
fun initNewline(yypos) = (lineNum := !lineNum + 1;
                          linePos := yypos :: !linePos;
                          lineCursor := 1);
fun makeToken(tokenizer, yypos, yytext) = (tokenizer(yypos-1, yypos + String.size(yytext)));

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

%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));

%s COMMENT STRING ESCAPE WHITESPACE;

eol = ("\013\010"|"\010"|"\013");
ws = [\ \t];

%%

<INITIAL, COMMENT, WHITESPACE> {ws}* => (continue());
<INITIAL, COMMENT, WHITESPACE> {eol} => (initNewline(yypos); continue());

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



<INITIAL> "type"      => (setCursorPast(yypos, yytext); makeToken(Tokens.TYPE, yypos, yytext));
<INITIAL> "var"       => (setCursorPast(yypos, yytext); makeToken(Tokens.VAR, yypos, yytext));
<INITIAL> "function"  => (setCursorPast(yypos, yytext); makeToken(Tokens.FUNCTION, yypos, yytext));
<INITIAL> "break"     => (setCursorPast(yypos, yytext); makeToken(Tokens.BREAK, yypos, yytext));
<INITIAL> "of"        => (setCursorPast(yypos, yytext); makeToken(Tokens.OF, yypos, yytext));
<INITIAL> "end"       => (setCursorPast(yypos, yytext); makeToken(Tokens.END, yypos, yytext));
<INITIAL> "in"        => (setCursorPast(yypos, yytext); makeToken(Tokens.IN, yypos, yytext));
<INITIAL> "nil"       => (setCursorPast(yypos, yytext); makeToken(Tokens.NIL, yypos, yytext));
<INITIAL> "let"       => (setCursorPast(yypos, yytext); makeToken(Tokens.LET, yypos, yytext));
<INITIAL> "do"        => (setCursorPast(yypos, yytext); makeToken(Tokens.DO, yypos, yytext));
<INITIAL> "to"        => (setCursorPast(yypos, yytext); makeToken(Tokens.TO, yypos, yytext));
<INITIAL> "for"       => (setCursorPast(yypos, yytext); makeToken(Tokens.FOR, yypos, yytext));
<INITIAL> "while"     => (setCursorPast(yypos, yytext); makeToken(Tokens.WHILE, yypos, yytext));
<INITIAL> "else"      => (setCursorPast(yypos, yytext); makeToken(Tokens.ELSE, yypos, yytext));
<INITIAL> "then"      => (setCursorPast(yypos, yytext); makeToken(Tokens.THEN, yypos, yytext));
<INITIAL> "if"        => (setCursorPast(yypos, yytext); makeToken(Tokens.IF, yypos, yytext));
<INITIAL> "array"     => (setCursorPast(yypos, yytext); makeToken(Tokens.ARRAY, yypos, yytext));
<INITIAL> ":="        => (setCursorPast(yypos, yytext); makeToken(Tokens.ASSIGN, yypos, yytext));
<INITIAL> "|"         => (setCursorPast(yypos, yytext); makeToken(Tokens.OR, yypos, yytext));
<INITIAL> "&"         => (setCursorPast(yypos, yytext); makeToken(Tokens.AND, yypos, yytext));
<INITIAL> ">="        => (setCursorPast(yypos, yytext); makeToken(Tokens.GE, yypos, yytext));
<INITIAL> ">"         => (setCursorPast(yypos, yytext); makeToken(Tokens.GT, yypos, yytext));
<INITIAL> "<="        => (setCursorPast(yypos, yytext); makeToken(Tokens.LE, yypos, yytext));
<INITIAL> "<"         => (setCursorPast(yypos, yytext); makeToken(Tokens.LT, yypos, yytext));
<INITIAL> "<>"        => (setCursorPast(yypos, yytext); makeToken(Tokens.NEQ, yypos, yytext));
<INITIAL> "="         => (setCursorPast(yypos, yytext); makeToken(Tokens.EQ, yypos, yytext));
<INITIAL> "/"         => (setCursorPast(yypos, yytext); makeToken(Tokens.DIVIDE, yypos, yytext));
<INITIAL> "*"         => (setCursorPast(yypos, yytext); makeToken(Tokens.TIMES, yypos, yytext));
<INITIAL> "-"         => (setCursorPast(yypos, yytext); makeToken(Tokens.MINUS, yypos, yytext));
<INITIAL> "+"         => (setCursorPast(yypos, yytext); makeToken(Tokens.PLUS, yypos, yytext));
<INITIAL> "{"         => (setCursorPast(yypos, yytext); makeToken(Tokens.LBRACE, yypos, yytext));
<INITIAL> "}"         => (setCursorPast(yypos, yytext); makeToken(Tokens.RBRACE, yypos, yytext));
<INITIAL> "["         => (setCursorPast(yypos, yytext); makeToken(Tokens.LBRACK, yypos, yytext));
<INITIAL> "]"         => (setCursorPast(yypos, yytext); makeToken(Tokens.RBRACK, yypos, yytext));
<INITIAL> "("         => (setCursorPast(yypos, yytext); makeToken(Tokens.LPAREN, yypos, yytext));
<INITIAL> ")"         => (setCursorPast(yypos, yytext); makeToken(Tokens.RPAREN, yypos, yytext));
<INITIAL> ";"         => (setCursorPast(yypos, yytext); makeToken(Tokens.SEMICOLON, yypos, yytext));
<INITIAL> ":"         => (setCursorPast(yypos, yytext); makeToken(Tokens.COLON, yypos, yytext));
<INITIAL> ","	        => (setCursorPast(yypos, yytext); makeToken(Tokens.COMMA, yypos, yytext));
<INITIAL> "."	        => (setCursorPast(yypos, yytext); makeToken(Tokens.DOT, yypos, yytext));
<INITIAL> [A-Za-z][A-Za-z0-9_]* => (setCursorPast(yypos, yytext);
                                    Tokens.ID(yytext, yypos, yypos + String.size(yytext)));
<INITIAL> [0-9]+                => (setCursorPast(yypos, yytext);
                                    Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + String.size(yytext)));
<INITIAL> \" => (YYBEGIN(STRING);
                 isString := true;
                 strStartPos := yypos;
                 strBuffer := "";
                 continue());
<INITIAL>. => (err yypos (" illegal character: " ^ yytext);
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
                   err yypos (" illegal string: " ^ yytext);
                   continue());
<STRING> . => (err yypos (" illegal string: " ^ yytext);
               continue());



<ESCAPE> n  => (appendBuffer("\n");
                YYBEGIN(STRING);
                continue());
<ESCAPE> t  => (appendBuffer("\t");
                YYBEGIN(STRING);
                continue());
<ESCAPE> \" => (appendBuffer("\"");
                YYBEGIN(STRING);
                continue());
<ESCAPE> \\ => (appendBuffer("\\");
                YYBEGIN(STRING);
                continue());
<ESCAPE> \^[A-Z@\[\]\\\^_\?] => (appendBuffer("\\" ^ yytext);
                                 YYBEGIN(STRING);
			                           continue());
<ESCAPE> [0-9]{3} => (let
                        val SOME decimalAscii = Int.fromString(yytext)
                      in
                        if ((decimalAscii >= 0) andalso (decimalAscii <= 127)) then
                           appendBuffer(Char.toString(chr(decimalAscii)))
                        else
                          err yypos (" illegal ASCII code: " ^ yytext)
                      end;
                      YYBEGIN(STRING);
                      continue());
<ESCAPE> {ws}* => (YYBEGIN(WHITESPACE);
                   continue());
<ESCAPE> {eol} => (initNewline(yypos);
                   YYBEGIN(WHITESPACE);
                   continue());
<ESCAPE> . => (err yypos (" illegal escape: \\" ^ yytext);
              continue());



<WHITESPACE> \\ => (YYBEGIN(STRING); continue());
<WHITESPACE> . => (err yypos (" illegal escape during whitespace: " ^ yytext);
                   continue());

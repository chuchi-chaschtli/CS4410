structure Mlex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
STRING | WHITESPACE | COMMENT | ESCAPE | INITIAL
    structure UserDeclarations = 
      struct

(* user declarations *)

(* TODO fix missing control char lexing, i.e. "\^C" *)

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
fun setCursorPast(yypos, yytext) = (lineCursor := (yypos + String.size(yytext)) - hd(!linePos))
fun initNewline(yypos) = (lineNum := !lineNum + 1;
                          linePos := yypos :: !linePos;
                          lineCursor := 1);
fun makeToken(tokenizer, yypos, yytext) = (tokenizer(yypos, yypos + String.size(yytext)));

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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (initNewline(yypos); continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN(COMMENT);
                   commentDepth := 1;
                   continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentDepth := !commentDepth + 1;
                   continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentDepth := !commentDepth - 1;
                   (if (!commentDepth = 0)
                    then YYBEGIN(INITIAL)
                    else ());
                   continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.TYPE, yypos, yytext))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.VAR, yypos, yytext))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.FUNCTION, yypos, yytext))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.BREAK, yypos, yytext))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.OF, yypos, yytext))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.END, yypos, yytext))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.IN, yypos, yytext))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.NIL, yypos, yytext))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.LET, yypos, yytext))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.DO, yypos, yytext))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.TO, yypos, yytext))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.FOR, yypos, yytext))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.WHILE, yypos, yytext))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.ELSE, yypos, yytext))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.THEN, yypos, yytext))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.IF, yypos, yytext))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.ARRAY, yypos, yytext))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.ASSIGN, yypos, yytext))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.OR, yypos, yytext))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.AND, yypos, yytext))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.GE, yypos, yytext))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.GT, yypos, yytext))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.LE, yypos, yytext))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.LT, yypos, yytext))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.NEQ, yypos, yytext))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.EQ, yypos, yytext))
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.DIVIDE, yypos, yytext))
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.TIMES, yypos, yytext))
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.MINUS, yypos, yytext))
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.PLUS, yypos, yytext))
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.LBRACE, yypos, yytext))
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.RBRACE, yypos, yytext))
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.LBRACK, yypos, yytext))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.RBRACK, yypos, yytext))
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.LPAREN, yypos, yytext))
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.RPAREN, yypos, yytext))
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.SEMICOLON, yypos, yytext))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.COLON, yypos, yytext))
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.COMMA, yypos, yytext))
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext); makeToken(Tokens.DOT, yypos, yytext))
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext);
                                    Tokens.ID(yytext, yypos, yypos + String.size(yytext)))
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (setCursorPast(yypos, yytext);
                                    Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + String.size(yytext)))
      end
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN(STRING);
                 isString := true;
                 strStartPos := yypos;
                 strBuffer := "";
                 continue()))
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err yypos (" illegal character: " ^ yytext);
              continue())
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN(INITIAL);
                isString := false;
                setCursorPast(yypos, yytext);
                Tokens.STRING(!strBuffer, !strStartPos, yypos))
      end
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN(ESCAPE);
                continue()))
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (appendBuffer(yytext);
                         continue())
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (initNewline(yypos);
                   err yypos (" illegal string: " ^ yytext);
                   continue())
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err yypos (" illegal string: " ^ yytext);
               continue())
      end
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (appendBuffer("\n");
                YYBEGIN(STRING);
                continue()))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (appendBuffer("\t");
                YYBEGIN(STRING);
                continue()))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (appendBuffer("\"");
                YYBEGIN(STRING);
                continue()))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (appendBuffer("\\");
                YYBEGIN(STRING);
                continue()))
fun yyAction59 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (appendBuffer("\\" ^ yytext);
                                 YYBEGIN(STRING);
			                           continue())
      end
fun yyAction60 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let
                        val SOME decimalAscii = Int.fromString(yytext)
                      in
                        if ((decimalAscii >= 0) andalso (decimalAscii <= 127)) then
                           appendBuffer(Char.toString(chr(decimalAscii)))
                        else
                          err yypos (" illegal ASCII code: " ^ yytext)
                      end;
                      YYBEGIN(STRING);
                      continue())
      end
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN(WHITESPACE);
                   continue()))
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;
      (initNewline(yypos);
                   YYBEGIN(WHITESPACE);
                   continue()))
fun yyAction63 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err yypos (" illegal escape: \\" ^ yytext);
              continue())
      end
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN(STRING); continue()))
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err yypos (" illegal escape during whitespace: " ^ yytext);
                   continue())
      end
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"`"
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction18(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ79(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ78(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ77(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ76(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ81(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ80(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ86(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ85(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = #"`"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction20(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ88(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ87(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction46(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                else if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"i"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"i"
                  then if inp = #"h"
                      then yyQ82(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"o"
                  then yyQ83(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"z"
              then if inp = #"y"
                  then yyQ84(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ89(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction13(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp = #"`"
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ91(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ90(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = #"`"
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ93(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ92(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction12(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction46(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ95(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ94(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"`"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ104(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ103(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ102(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ101(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ100(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ99(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ105(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #" "
                  then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #" "
                  then if inp = #"\t"
                      then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"f"
                  then yyQ96(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"!"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < #"!"
                  then if inp = #"\n"
                      then yyAction0(strm, yyNO_MATCH)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyAction0(strm, yyNO_MATCH)
                    else if inp = #" "
                      then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction0(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ97(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"f"
                  then yyQ96(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"f"
                  then if inp = #"`"
                      then yyAction0(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"v"
              then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ98(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ108(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction19(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ110(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ109(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction46(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ107(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"l"
                  then yyQ106(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = #"`"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ111(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction9(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"k"
              then yyQ115(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"k"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ114(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ113(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ112(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction22(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = #"`"
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"y"
              then yyQ119(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"y"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ118(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ117(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ116(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction46(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction46(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"`"
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ75(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ120(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ122(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ121(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ123(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ124(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"0"
              then yyAction47(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ124(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ124(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"0"
              then yyAction47(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ124(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ125(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #")"
                  then yyQ44(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #")"
                  then if inp = #" "
                      then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #" "
                      then if inp = #"\v"
                          then yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                            else if inp = #"\n"
                              then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                              else yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"\r"
                          then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"&"
                      then yyQ42(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"&"
                      then if inp = #"\""
                          then yyQ41(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"'"
                      then yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ43(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"0"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"-"
                      then yyQ48(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"-"
                      then if inp = #"+"
                          then yyQ46(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"*"
                          then yyQ45(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ47(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"."
                      then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"<"
                  then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #";"
                      then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #">"
                  then yyQ56(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"="
                  then yyQ55(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"l"
              then yyQ66(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"c"
                  then yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"c"
                  then if inp = #"]"
                      then yyQ59(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"]"
                      then if inp = #"["
                          then yyQ58(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"\\"
                          then yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"a"
                      then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"b"
                      then yyQ61(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"g"
                  then yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"g"
                  then if inp = #"e"
                      then yyQ63(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"d"
                      then yyQ62(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ64(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"i"
                  then yyQ65(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"v"
              then yyQ70(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"p"
                  then yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"p"
                  then if inp = #"n"
                      then yyQ67(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"m"
                      then yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ68(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"t"
                  then yyQ69(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"|"
              then yyQ73(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"|"
              then if inp = #"x"
                  then yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"x"
                  then yyQ71(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"{"
                  then yyQ72(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ57(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"}"
              then yyQ74(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"?"
              then yyQ34(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"?"
              then yyAction63(strm, yyNO_MATCH)
            else if inp <= #"_"
              then yyQ34(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ36(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ36(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ35(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"0"
              then yyAction63(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ35(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ26(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ37(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ37(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"\^N"
                  then yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < #"\^N"
                  then if inp = #"\n"
                      then yyQ26(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ25(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                          else yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                    else if inp = #"\r"
                      then yyQ27(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                      else yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp = #"\""
                  then yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < #"\""
                  then if inp = #" "
                      then yyQ25(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                      else yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp <= #"/"
                  then yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyQ29(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = #"f"
              then yyQ25(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"]"
                  then yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < #"]"
                  then if inp = #"\\"
                      then yyQ30(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                      else yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp = #"^"
                  then yyQ31(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = #"o"
              then yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ32(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = #"t"
              then yyQ33(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyQ24(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ22(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ23(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"!"
              then if inp = #"\v"
                  then yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ18(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"\n"
                      then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"\^N"
                  then yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\^N"
                  then if inp = #"\r"
                      then yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #" "
                  then yyQ18(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"/"
              then yyQ21(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"/"
              then if inp = #"*"
                  then yyQ20(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"f"
              then yyQ18(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"\n"
                      then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"\r"
                  then yyQ14(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"]"
              then yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ15(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"f"
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ6(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ8(strm', lastMatch)
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ5(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ6(strm', lastMatch)
                      else yyQ5(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ7(strm', lastMatch)
                  else yyQ5(strm', lastMatch)
            else if inp = #"\\"
              then yyQ10(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"\""
                  then yyQ9(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp <= #"~"
              then yyQ8(strm', lastMatch)
              else yyQ5(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | WHITESPACE => yyQ1(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ2(!(yystrm), yyNO_MATCH)
    | ESCAPE => yyQ3(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ4(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end

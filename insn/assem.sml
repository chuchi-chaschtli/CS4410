structure Assem = struct

  type reg = string
  type temp = Temp.temp
  type label = Temp.label

  datatype instr = OPER  of {assem: string,
                             dst: temp list,
                             src: temp list,
                             jump: label list option}
                 | LABEL of {assem: string, lab: Temp.label}
                 | MOVE  of {assem: string,
                             dst: temp,
                             src: temp}

  fun getTempsHelper(0, acc) = acc
    | getTempsHelper(n, acc) = Temp.newtemp()::acc

  fun getTemps(n) = getTempsHelper(n, nil)

  val FP = Temp.newtemp() (* frame pointer *)
  val RV = Temp.newtemp() (* return value *)
  val SP = Temp.newtemp() (* static pointer *)
  val RA = Temp.newtemp() (* return address *)
  val ZERO = Temp.newtemp() (* zero register *)

  val specialregs = [FP,RV,SP,RA,ZERO]
  val argregs = getTemps(4)
  val calleesaves = getTemps(8)
  val callersaves = getTemps(10)

  fun format saytemp =
    let
      fun speak(assem,dst,src,jump) =
        let val saylab = Symbol.name
            fun f(#"`":: #"s":: i::rest) =
                (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
              | f( #"`":: #"d":: i:: rest) =
                (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
              | f( #"`":: #"j":: i:: rest) =
                (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
              | f( #"`":: #"`":: rest) = #"`" :: f rest
              | f( #"`":: _ :: rest) = ErrorMsg.impossible "bad Assem format"
              | f(c :: rest) = (c :: f rest)
              | f nil = nil
        in
          implode(f(explode assem))
        end
    in fn OPER{assem,dst,src,jump=NONE} => speak(assem,dst,src,nil)
        | OPER{assem,dst,src,jump=SOME j} => speak(assem,dst,src,j)
        | LABEL{assem,...} => assem
        | MOVE{assem,dst,src} => speak(assem,[dst],[src],nil)
    end
end

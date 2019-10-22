structure Analysis : sig val check : string -> unit end =
struct
  fun check filename =
    let
      val absyn = Parse.parse filename
    in
      PrintAbsyn.print(TextIO.stdOut, absyn);
      Semant.transProg absyn
    end
end

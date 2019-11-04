structure Analysis : sig val check : string -> unit end =
struct
  fun check filename =
    let
      val absyn = Parse.parse filename
      val irTree = Semant.transProg absyn
    in
      PrintAbsyn.print(TextIO.stdOut, absyn);
      Printtree.printtree(TextIO.stdOut, Translate.unNx(irTree))
    end
end

structure Analysis : sig val check : string -> unit end =
struct
  fun check filename =
    let
      val absyn = Parse.parse filename
      val out = Semant.transProg absyn
    in
      out
    end
end

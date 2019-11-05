structure Analysis : sig val check : string -> unit end =
struct
  fun check filename =
    let
      val absyn = Parse.parse filename
      val fragments = Semant.transProg absyn
    in
      ()
    end
end

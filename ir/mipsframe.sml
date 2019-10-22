signature FRAME =
sig
  type access
  type frame
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name: frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
end

structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name: Temp.label, frameOffset: int ref, formals: access list}

  val wordLenBytes = 4

  fun newFrame {name, formals} =
    let
      val paramIndex = ref 1
      fun getParamOffset() =
        let val tmp = !paramIndex
        in 
          paramIndex := tmp+1;
          tmp * wordLenBytes
        end
      val formals' = map (fn escape => if escape
                                       then InFrame (getParamOffset())
                                       else InReg   (Temp.newtemp())) formals
    in
       (* TODO I'm not sure if `ref 0` is what we need here,
          however I'm not sure the alternative.
          Global var for frame count? *)
       {name=name, frameOffset=(ref 0), formals=formals'}
    end

  fun allocLocal (frame:frame) escape =
    let
      val {name=name, frameOffset=offset, formals=formals} = frame (* TODO: Do we need name/formals? *)
      val updatedOffset = !offset - wordLenBytes
    in
      offset := updatedOffset;
      if escape
      then InReg   (Temp.newtemp())
      else InFrame (updatedOffset)
    end

  fun name (frame:frame) = (#name frame)
  fun formals (frame:frame) = (#formals frame)
end

structure Frame : FRAME = MipsFrame

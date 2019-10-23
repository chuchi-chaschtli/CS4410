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
  val numDedicatedArgRegisters = 4

  fun newFrame {name, formals} =
    let
      val registersTaken = ref 0
      val paramIndex = ref 1
      fun getParamOffset() =
        let val tmp = !paramIndex
        in
          paramIndex := tmp+1;
          tmp * wordLenBytes
        end
      fun allocateFormal escape =
        (registersTaken := Int.min(!registersTaken + 1, numDedicatedArgRegisters + 1);
          if escape orelse !registersTaken > numDedicatedArgRegisters
          then InFrame (getParamOffset())
          else InReg   (Temp.newtemp()))
      val formals' = map allocateFormal formals
    in
       (* TODO I'm not sure if `ref 0` is what we need here,
          however I'm not sure the alternative.
          Global var for frame count? *)
       {name=name, frameOffset=(ref 0), formals=formals'}
    end

  fun allocLocal (frame:frame) escape =
    let
      val offset = (#frameOffset frame)
    in
      offset := !offset - wordLenBytes;
      if escape
      then InReg   (Temp.newtemp())
      else InFrame (!offset)
    end

  fun name (frame:frame) = (#name frame)
  fun formals (frame:frame) = (#formals frame)
end

structure Frame : FRAME = MipsFrame

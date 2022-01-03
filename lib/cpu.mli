module Make (G: Gui.G) : sig
  type cpu

  val boot : program:char array -> cpu

  val run : debug:bool -> frequency:int -> cpu -> unit
end

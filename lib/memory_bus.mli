module type Bus = sig
  type gpu

  type bus

  val gpu : bus -> gpu

  val init : program:char array -> bus

  val fetch_font : char -> bus -> char

  val fetch_ram : int -> bus -> char

  val write_ram : int -> char -> bus -> unit

  val write_vram : int -> bool -> bus -> unit

  val render : gpu -> unit
end

module Make (GUI: Gui.GUI) : Bus

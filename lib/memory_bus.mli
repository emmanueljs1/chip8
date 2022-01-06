module Make (GUI: Gui.GUI) : sig
  type gpu

  type bus

  val gpu : bus -> gpu

  val init : program:char array -> gui:GUI.gui-> bus

  val font_addr : int -> bus -> int

  val fetch_ram : int -> bus -> char

  val write_ram : int -> char -> bus -> unit

  val fetch_vram : int -> int -> bus -> bool

  val write_vram : int -> int -> bool -> bus -> unit
end

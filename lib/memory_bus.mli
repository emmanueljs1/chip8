module Make (GUI: Gui.GUI) : sig
  type gpu

  type bus

  val gpu : bus -> gpu

  val init : program:char array -> gui:GUI.gui-> bus

  val program_start : bus -> int

  val font_addr : int -> bus -> int

  val fetch_ram : int -> bus -> char

  val write_ram : int -> char -> bus -> unit

  val screen_dim : gpu -> int * int

  val fetch_vram : int -> int -> gpu -> bool

  val write_vram : int -> int -> bool -> gpu -> unit

  val clear_vram : gpu -> unit
end

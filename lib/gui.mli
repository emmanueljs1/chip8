module type GUI = sig
  type gui

  type screen

  val mk : unit -> gui

  val is_key_pressed : gui -> bool

  val read_key : gui -> char

  val mk_screen : width:int -> height:int -> gui -> screen

  val set_pixel : int -> int -> bool -> screen -> unit

  (* TODO: debug / external methods *)
end

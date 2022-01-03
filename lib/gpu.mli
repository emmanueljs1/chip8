module Make (GUI: Gui.GUI) : sig
  type gpu
  val init : unit -> gpu
  val write_vram : int -> gpu -> gpu
end

module type GPU = sig
  type gpu
  val init : unit -> gpu
  val write_vram : int -> bool -> gpu -> gpu
end

module Make (GUI: Gui.GUI) : GPU

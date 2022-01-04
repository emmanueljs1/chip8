module type GPU = sig
  type gpu
  val init : unit -> gpu
  val write_vram : int -> bool -> gpu -> gpu
end

module Make (GUI: Gui.GUI) = struct
  type gpu = { vram: bool array }

  let init () = { vram = Array.make 2048 false }

  let write_vram _ _ _ = failwith "unimplemented"
end

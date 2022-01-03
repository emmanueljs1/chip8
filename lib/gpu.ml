module Make (GUI: Gui.GUI) = struct
  type gpu = { vram: bool array }

  let init () = { vram = Array.make 2048 false }

  let write_vram _ _ = failwith "unimplemented"
end

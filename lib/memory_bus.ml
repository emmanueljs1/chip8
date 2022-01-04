module Make (GPU: Gpu.GPU) = struct
  type bus =
    { ram: char array
    ; gpu: GPU.gpu
    }

  let init ~program =
    let load_font (_: char array) : unit =
      failwith "unimplemented"
    in

    let ram = char_of_int 0 |> Array.make 4096 in
    load_font ram;
    Array.blit program 0 ram 0x0200 (Array.length program);
    { ram = ram; gpu = GPU.init () }

  let fetch addr bus = bus.ram.(addr)

  let write_mem _ _ _ = failwith "unimplemented"

  let write_vram _ _ _ = failwith "unimplemented"
end

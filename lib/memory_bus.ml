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


module Make (GUI: Gui.GUI) = struct
  type gpu = bool array

  type bus =
    { ram: char array
    ; gpu: gpu
    }

  let gpu bus = bus.gpu

  let init ~program =
    let load_font (_: char array) : unit =
      failwith "unimplemented"
    in

    let ram = char_of_int 0 |> Array.make 4096 in
    load_font ram;
    Array.blit program 0 ram 0x0200 (Array.length program);

    let vram = Array.make 2048 false in

    { ram = ram; gpu = vram }

  let fetch_font _ _ = failwith "unimplemented"

  let fetch_ram addr bus = bus.ram.(addr)

  let write_ram addr byte bus = bus.ram.(addr) <- byte

  let write_vram addr on bus = bus.gpu.(addr) <- on

  let render _ = failwith "unimplemented"
end

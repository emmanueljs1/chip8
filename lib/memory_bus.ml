module type Bus = sig
  type gpu

  type bus

  val gpu : bus -> gpu

  val init : program:char array -> bus

  val font_addr : int -> bus -> int

  val fetch_ram : int -> bus -> char

  val write_ram : int -> char -> bus -> unit

  val write_vram : int -> bool -> bus -> unit

  val render : gpu -> unit
end

let fonts =
  Array.of_list [
    0xF0; 0x90; 0x90; 0x90; 0xF0; (* 0 *)
    0x20; 0x60; 0x20; 0x20; 0x70; (* 1 *)
    0xF0; 0x10; 0xF0; 0x80; 0xF0; (* 2 *)
    0xF0; 0x10; 0xF0; 0x10; 0xF0; (* 3 *)
    0x90; 0x90; 0xF0; 0x10; 0x10; (* 4 *)
    0xF0; 0x80; 0xF0; 0x10; 0xF0; (* 5 *)
    0xF0; 0x80; 0xF0; 0x90; 0xF0; (* 6 *)
    0xF0; 0x10; 0x20; 0x40; 0x40; (* 7 *)
    0xF0; 0x90; 0xF0; 0x90; 0xF0; (* 8 *)
    0xF0; 0x90; 0xF0; 0x10; 0xF0; (* 9 *)
    0xF0; 0x90; 0xF0; 0x90; 0x90; (* A *)
    0xE0; 0x90; 0xE0; 0x90; 0xE0; (* B *)
    0xF0; 0x80; 0x80; 0x80; 0xF0; (* C *)
    0xE0; 0x90; 0x90; 0x90; 0xE0; (* D *)
    0xF0; 0x80; 0xF0; 0x80; 0xF0; (* E *)
    0xF0; 0x80; 0xF0; 0x80; 0x80; (* F *)
  ] |> Array.map char_of_int

let font_start_addr = 0x050
let font_end_addr = 0x090

module Make (GUI: Gui.GUI) = struct
  type gpu = bool array

  type bus =
    { ram: char array
    ; gpu: gpu
    }

  let gpu bus = bus.gpu

  let init ~program =
    let ram = char_of_int 0 |> Array.make 4096 in
    Array.blit fonts 0 ram font_start_addr font_end_addr;
    Array.blit program 0 ram 0x0200 (Array.length program);

    let vram = Array.make 2048 false in

    { ram = ram; gpu = vram }

  let font_addr hex_num bus =
    if hex_num >= 0x0 && hex_num <= 0xF then
      font_start_addr + (hex_num * 5)
    else
      char_of_int hex_num
      |> Printf.sprintf "Invalid font: %i (%c)" hex_num
      |> (fun s -> Invalid_argument s)
      |> raise

  let fetch_ram addr bus = bus.ram.(addr)

  let write_ram addr byte bus = bus.ram.(addr) <- byte

  let write_vram addr on bus = bus.gpu.(addr) <- on

  let render _ = failwith "unimplemented"
end

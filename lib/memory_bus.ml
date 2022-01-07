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

let screen_width = 64
let screen_height = 32

let screen_size = screen_width * screen_height

let program_start_addr = 0x0200

module Make (GUI: Gui.GUI) = struct
  type gpu =
    { vram: bool array
    ; display: GUI.screen
    }

  type bus =
    { ram: char array
    ; gpu: gpu
    }

  let gpu bus = bus.gpu

  let program_start _ = program_start_addr

  let init ~program ~gui =
    let ram = char_of_int 0 |> Array.make 4096 in
    Array.blit fonts 0 ram font_start_addr (Array.length fonts);
    let bytes_to_copy =
      min (4096 - program_start_addr) (Array.length program)
    in
    Array.blit program 0 ram program_start_addr bytes_to_copy;

    let vram = Array.make screen_size false in
    let display =
      GUI.mk_screen ~width:screen_width ~height:screen_height gui
    in
    let gpu = { vram = vram; display = display } in

    { ram = ram; gpu = gpu}

  let font_addr hex_num _ =
    if hex_num >= 0x0 && hex_num <= 0xF then
      font_start_addr + (hex_num * 5)
    else
      char_of_int hex_num
      |> Printf.sprintf "Invalid font: %i (%c)" hex_num
      |> (fun s -> Invalid_argument s)
      |> raise

  let fetch_ram addr bus = bus.ram.(addr)

  let write_ram addr byte bus = bus.ram.(addr) <- byte

  let screen_dim _ = screen_width, screen_height

  let fetch_vram x y gpu =
    let addr = (screen_width * y) + x in
    gpu.vram.(addr)

  let write_vram x y on gpu =
    let addr = (screen_width * y) + x in
    gpu.vram.(addr) <- on;
    GUI.set_pixel x y on gpu.display

  let clear_vram gpu =
    Array.fill gpu.vram 0 screen_size false;
    GUI.clear_screen gpu.display
end

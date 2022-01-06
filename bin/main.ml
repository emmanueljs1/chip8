open Chip8

module GraphicsGui = struct
  type screen = unit

  type gui = unit

  let mk _ =
    Graphics.open_graph "";
    ()

  let pixel_width = 10
  let pixel_height = 10

  let mk_screen ~width ~height _ =
    Graphics.resize_window (width * pixel_width) (height * pixel_height);
    Graphics.set_color Graphics.black;
    Graphics.fill_rect 0 0 (width * pixel_width) (height * pixel_height)

  let set_pixel x y on _ =
    let color = if on then Graphics.white else Graphics.black in
    Graphics.set_color color;
    let x' = x * pixel_width in
    let y' = y * pixel_height in
    Graphics.fill_rect x' y' pixel_width pixel_height

  let is_key_pressed = Graphics.key_pressed

  let read_key = Graphics.read_key
end

let () =
  let args = Sys.argv in
  let debug = Array.exists (fun s -> s = "--debug") args in
  let frequency =
    let fallback = 600 in
    match Array.find_opt (fun s -> String.starts_with ~prefix:"--frequency" s) args with
    | None -> fallback
    | Some(s) ->
        begin match String.split_on_char '=' s with
        | ["--frequency"; freq_str] ->
            begin try int_of_string freq_str with _ -> fallback end
        | _ -> fallback
        end
  in
  let program = char_of_int 0x00E0 |> Array.make 100 in
  let module Cpu = Cpu.Make(GraphicsGui) in
  let cpu = Cpu.boot ~program:program in
  Cpu.run ~debug:debug ~frequency:frequency cpu

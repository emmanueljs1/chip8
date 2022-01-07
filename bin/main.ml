open Chip8

module GraphicsGui = struct
  type screen =
    { pixel_width: int
    ; pixel_height: int
    ; screen_width: int
    ; screen_height: int
    }

  type gui = unit

  let mk _ =
    Graphics.open_graph "";
    ()

  let clear_screen screen =
    Graphics.set_color Graphics.black;
    Graphics.fill_rect 0 0 screen.screen_width screen.screen_height

  let mk_screen ~width ~height _ =
    let (pixel_width, pixel_height) = (10, 10) in
    let (screen_w, screen_h) =
      (width * pixel_width, height * pixel_height)
    in
    Graphics.resize_window screen_w screen_h;
    let screen =
      { pixel_width = pixel_width
      ; pixel_height = pixel_height
      ; screen_width = screen_w
      ; screen_height = screen_h
      }
    in
    clear_screen screen;
    screen

  let set_pixel x y on screen =
    let color = if on then Graphics.white else Graphics.black in
    Graphics.set_color color;
    let x' = x * screen.pixel_width in
    let y' = y * screen.pixel_height + screen.pixel_height in
    Graphics.fill_rect x' (screen.screen_height - y') screen.pixel_width screen.pixel_height

  let is_key_pressed = Graphics.key_pressed

  let read_key = Graphics.read_key
end

let args_opt (f: string -> 'a) (fallback: 'a) (opt: string) (args: string array) : 'a =
  let arg_opt = Printf.sprintf "--%s" opt in
  match Array.find_opt (String.starts_with ~prefix:arg_opt) args with
  | None -> fallback
  | Some(s) ->
      Printf.sprintf "arg_opt: %s, found: %s" arg_opt s |> print_endline;
      begin match String.split_on_char '=' s with
      | [arg_opt_str; opt_str] when arg_opt_str = arg_opt ->
          begin try f opt_str with _ -> fallback end
      | _ -> fallback
      end

let load_rom (filename: string) : char array =
  let program = Array.make (4096 - 0x200) (char_of_int 0) in
  let _ =
    let ic = open_in_bin filename in
    let rec loop n =
      if n >= 4096 - 0x200 then ()
      else
        try
          let byte = input_byte ic in
          program.(n) <- char_of_int byte;
          loop (n + 1)
        with End_of_file -> ()
    in
    loop 0
  in
  program

let () =
  let module Cpu = Cpu.Make(GraphicsGui) in

  let args = Sys.argv in
  let debug = Array.exists (fun s -> s = "--debug") args in

  if debug then Printexc.record_backtrace true;
  let frequency = args_opt int_of_string 600 "frequency" args in
  let rom_filename = args_opt (fun s -> s) "roms/test.ch8" "rom" args in

  let program = load_rom rom_filename in
  let cpu = Cpu.boot ~program:program in
  Cpu.run ~debug:debug ~frequency:frequency cpu

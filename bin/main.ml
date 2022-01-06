open Chip8

module StubGui = struct
  type screen = int

  type gui = int

  let mk () = failwith "unimplemented"

  let mk_screen ~width ~height _ = failwith "unimplemented"

  let set_pixel _ _ _ _ = failwith "unimplemented"

  let is_key_pressed _ _ = failwith "unimplemented"
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
  let module Cpu = Cpu.Make(StubGui) in
  let cpu = Cpu.boot ~program:program in
  Cpu.run ~debug:debug ~frequency:frequency cpu

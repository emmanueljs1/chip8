open Chip8

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
  let module Gui = struct end in
  let module Cpu = Cpu.Make(Gui) in
  let cpu = Cpu.boot ~program:program in
  Cpu.run ~debug:debug ~frequency:frequency cpu

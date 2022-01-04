type registers =
  { v0: char;
    v1: char;
    v2: char;
    v3: char;
    v4: char;
    v5: char;
    v6: char;
    v7: char;
    v8: char;
    v9: char;
    va: char;
    vb: char;
    vc: char;
    vd: char;
    ve: char;
    vf: char;
  }

let empty_registers : registers =
  { v0 = char_of_int 0
  ; v1 = char_of_int 0
  ; v2 = char_of_int 0
  ; v3 = char_of_int 0
  ; v4 = char_of_int 0
  ; v5 = char_of_int 0
  ; v6 = char_of_int 0
  ; v7 = char_of_int 0
  ; v8 = char_of_int 0
  ; v9 = char_of_int 0
  ; va = char_of_int 0
  ; vb = char_of_int 0
  ; vc = char_of_int 0
  ; vd = char_of_int 0
  ; ve = char_of_int 0
  ; vf = char_of_int 0
  }

let reg (addr: char) (registers: registers) : char =
  match int_of_char addr with
  | 0x0 -> registers.v0
  | 0x1 -> registers.v1
  | 0x2 -> registers.v2
  | 0x3 -> registers.v3
  | 0x4 -> registers.v4
  | 0x5 -> registers.v5
  | 0x6 -> registers.v6
  | 0x7 -> registers.v7
  | 0x8 -> registers.v8
  | 0x9 -> registers.v9
  | 0xA -> registers.va
  | 0xB -> registers.vb
  | 0xC -> registers.vc
  | 0xD -> registers.vd
  | 0xE -> registers.ve
  | 0xF -> registers.vf
  | _ ->
    Printf.sprintf "%c is not a valid register address" addr
    |> (fun s -> Invalid_argument s)
    |> raise

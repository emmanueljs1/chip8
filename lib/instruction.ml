open Util

let binary_op_of_hex (code: char): binary_op option =
  match code with
  | '1' -> Some Or
  | '2' -> Some And
  | '3' -> Some Xor
  | '4' -> Some Add
  | '5' -> Some (Subtract false)
  | '7' -> Some (Subtract true)
  | '6' -> Some (Shift true)
  | 'E' -> Some (Shift false)
  | _ -> None

type opcode =
  | Noop
  | ClearScreen
  | Set of set_source * set_dest
  | Add of add_source * add_dest
  | Display of int * int * int
  | EnterSubroutine of int
  | ReturnFromSubroutine
  | SkipIfRegHasValue of int * char * bool
  | SkipIfRegsAreEqual of int * int * bool
  | BinaryOp of int * binary_op * int
  | JumpWithOffset of int * bool
  | Random of int * char
  | SkipIfKey of int * bool

type instruction =
  { opcode: opcode
  ; duration_ms: float
  }

let int_of_hex (hex: char) : int = Hex.to_char '0' hex |> int_of_char

let int_of_hexes (h1: char) (h2: char) (h3: char) : int =
  let upper = Hex.to_char '0' h1 |> int_of_char |> (fun i -> i lsl 8) in
  let lower = Hex.to_char h2 h3 |> int_of_char in
  upper + lower

let decode_instruction (byte1: char) (byte2: char) : instruction =
  let (c1, c2), (c3, c4) = Hex.of_char byte1, Hex.of_char byte2 in
  let hex_encodings = List.map Char.uppercase_ascii [c1; c2; c3; c4] in
  match hex_encodings, String.init 4 (List.nth hex_encodings) with
  | _, "00E0" -> { opcode = ClearScreen; duration_ms = 109. }
  | ['1' as h; h1; h2; h3], _ | ['B' as h; h1; h2; h3], _ ->
      let with_offset = h = 'B' in
      let addr = int_of_hexes h1 h2 h3 in
      { opcode = JumpWithOffset (addr, with_offset) ; duration_ms = 105. }
  | ['6'; x; h1; h2], _ ->
      let reg = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = Set (RawByte value, Reg reg); duration_ms = 27. }
  | ['7'; x; h1; h2], _ ->
      let reg = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = Add (RawByte value, Reg reg); duration_ms = 45. }
  | ['A'; h1; h2; h3], _ ->
      let value = int_of_hexes h1 h2 h3 in
      { opcode = Set (Lit value, Index); duration_ms = 55. }
  | ['D'; x; y; h], _ ->
      let vx, vy = int_of_hex x, int_of_hex y in
      let n = int_of_hex h in
      { opcode = Display (vx, vy, n); duration_ms = 22734. }
  | ['2'; h1; h2; h3], _ ->
      let addr = int_of_hexes h1 h2 h3 in
      { opcode = EnterSubroutine addr; duration_ms = 105. }
  | _, "00EE" ->
      { opcode = ReturnFromSubroutine; duration_ms = 105. }
  | ['3' as h; x; h1; h2], _ | ['4' as h; x; h1; h2], _ ->
      let has_value = h = '3' in
      let reg = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = SkipIfRegHasValue (reg, value, has_value); duration_ms = 55. }
  | ['5' as h; x; y; '0'], _ | ['9' as h; x; y; '0'], _ ->
      let are_equal = h = '5' in
      let vx, vy = int_of_hex x, int_of_hex y in
      { opcode = SkipIfRegsAreEqual (vx, vy, are_equal); duration_ms = 73. }
  | ['8'; x; y; '0'], _ ->
      let vx, vy = int_of_hex x, int_of_hex y in
      { opcode = Set (Reg vy, Reg vx); duration_ms = 200. }
  | ['8'; x; y; h], _ when binary_op_of_hex h <> None ->
      let binary_op = binary_op_of_hex h |> Option.get in
      let vx, vy = int_of_hex x, int_of_hex y in
      { opcode = BinaryOp (vx, binary_op, vy); duration_ms = 200. }
  | ['C'; x; h1; h2], _ ->
      let vx = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = Random (vx, value); duration_ms = 164. }
  | ['E'; x; '9' as h; 'E'], _ | ['E'; x; 'A' as h; '1'], _ ->
      let vx = int_of_hex x in
      let if_key_pressed = h = '9' in
      { opcode = SkipIfKey (vx, if_key_pressed); duration_ms = 73. }
  | ['F'; x; '0'; '7'], _ | ['F'; x; '1'; '5'], _ | ['F'; x; '1'; '8'], _ ->
      let vx = int_of_hex x in
      let source : set_source = if c3 = '0' then Reg vx else DelayTimer in
      let dest =
        match c3, c4 with
        | '1', '5' -> DelayTimer
        | '1', '8' -> SoundTimer
        | _ -> Reg vx
      in
      { opcode = Set (source, dest); duration_ms = 45. }
  | ['F'; x; '1'; 'E'], _ ->
      let vx = int_of_hex x in
      { opcode = Set (Reg vx, Index); duration_ms = 86. }
  | _, unsupported ->
      Printf.sprintf "Warning: unsupported instruction %s" unsupported |> print_endline;
      { opcode = Noop; duration_ms = 0. }

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
  | Jump of int
  | SetRegToValue of int * char
  | AddValueToReg of int * char
  | SetIndexToValue of int
  | Display of int * int * int
  | EnterSubroutine of int
  | ReturnFromSubroutine
  | SkipIfRegHasValue of int * char
  | SkipIfRegDoesNotHaveValue of int * char
  | SkipIfRegsEqual of int * int
  | SkipIfRegsNotEqual of int * int
  | SetRegToReg of int * int
  | BinaryOp of int * binary_op * int
  | JumpWithOffset of int
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
  | '1' :: [h1; h2; h3], _ ->
      let addr = int_of_hexes h1 h2 h3 in
      { opcode = Jump addr ; duration_ms = 105. }
  | ['6'; x; h1; h2], _ ->
      let reg = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = SetRegToValue (reg, value); duration_ms = 27. }
  | ['7'; x; h1; h2], _ ->
      let reg = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = AddValueToReg (reg, value); duration_ms = 45. }
  | ['A'; h1; h2; h3], _ ->
      let value = int_of_hexes h1 h2 h3 in
      { opcode = SetIndexToValue value; duration_ms = 55. }
  | ['D'; x; y; h], _ ->
      let vx, vy = int_of_hex x, int_of_hex y in
      let n = int_of_hex h in
      { opcode = Display (vx, vy, n); duration_ms = 22734. }
  | ['2'; h1; h2; h3], _ ->
      let addr = int_of_hexes h1 h2 h3 in
      { opcode = EnterSubroutine addr; duration_ms = 105. }
  | _, "00ee" ->
      { opcode = ReturnFromSubroutine; duration_ms = 105. }
  | ['3'; x; h1; h2], _ ->
      let reg = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = SkipIfRegHasValue (reg, value); duration_ms = 55. }
  | ['4'; x; h1; h2], _ ->
      let reg = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = SkipIfRegDoesNotHaveValue (reg, value); duration_ms = 55. }
  | ['5'; x; y; '0'], _ ->
      let vx, vy = int_of_hex x, int_of_hex y in
      { opcode = SkipIfRegsEqual (vx, vy); duration_ms = 73. }
  | ['9'; x; y; '0'], _ ->
      let vx, vy = int_of_hex x, int_of_hex y in
      { opcode = SkipIfRegsNotEqual (vx, vy); duration_ms = 73. }
  | ['8'; x; y; '0'], _ ->
      let vx, vy = int_of_hex x, int_of_hex y in
      { opcode = SetRegToReg (vx, vy); duration_ms = 200. }
  | ['8'; x; y; h], _ when binary_op_of_hex h <> None ->
      let binary_op = binary_op_of_hex h |> Option.get in
      let vx, vy = int_of_hex x, int_of_hex y in
      { opcode = BinaryOp (vx, binary_op, vy); duration_ms = 200. }
  | ['B'; h1; h2; h3], _ ->
      let addr = int_of_hexes h1 h2 h3 in
      (* TODO: support alternate behavior *)
      { opcode = JumpWithOffset addr; duration_ms = 105. }
  | ['C'; x; h1; h2], _ ->
      let vx = int_of_hex x in
      let value = Hex.to_char h1 h2 in
      { opcode = Random (vx, value); duration_ms = 164. }
  | ['E'; x; '9' as h; 'E'], _ | ['E'; x; 'A' as h; '1'], _ ->
      let vx = int_of_hex x in
      let if_key_pressed = h = '9' in
      { opcode = SkipIfKey (vx, if_key_pressed); duration_ms = 73. }
  | _, unsupported ->
      Printf.sprintf "Warning: unsupported instruction %s" unsupported |> print_endline;
      { opcode = Noop; duration_ms = 0. }

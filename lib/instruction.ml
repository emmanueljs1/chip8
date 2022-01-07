  type opcode =
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

  type instruction =
    { opcode: opcode
    ; duration_ms: float
    }

  let int_of_hex (hex: char) : int = Hex.to_char '0' hex |> int_of_char

  let addr_of_hexes (h1: char) (h2: char) (h3: char) : int =
    let upper = Hex.to_char '0' h1 |> int_of_char |> (fun i -> i lsl 8) in
    let lower = Hex.to_char h2 h3 |> int_of_char in
    upper + lower

  let decode_instruction (byte1: char) (byte2: char) : instruction =
    let (c1, c2), (c3, c4) = Hex.of_char byte1, Hex.of_char byte2 in
    let hex_encodings = [c1; c2; c3; c4] in
    match hex_encodings, String.init 4 (List.nth hex_encodings) with
    | _, "00e0" -> { opcode = ClearScreen; duration_ms = 109. }
    | '1' :: [h1; h2; h3], _ ->
        let addr = addr_of_hexes h1 h2 h3 in
        { opcode = Jump addr ; duration_ms = 105. }
    | ['6'; x; h1; h2], _ ->
        let reg = int_of_hex x in
        let value = Hex.to_char h1 h2 in
        { opcode = SetRegToValue (reg, value); duration_ms = 27. }
    | ['7'; x; h1; h2], _ ->
        let reg = int_of_hex x in
        let value = Hex.to_char h1 h2 in
        { opcode = AddValueToReg (reg, value); duration_ms = 45. }
    | ['a'; h1; h2; h3], _ ->
        let upper = int_of_hex h1 |> (fun i -> i lsl 8) in
        let lower = Hex.to_char h2 h3 |> int_of_char in
        let value = upper + lower in
        { opcode = SetIndexToValue value; duration_ms = 55. }
    | ['d'; x; y; h], _ ->
        let vx = int_of_hex x in
        let vy = int_of_hex y in
        let n = int_of_hex h in
        { opcode = Display (vx, vy, n); duration_ms = 22734. }
    | ['2'; h1; h2; h3], _ ->
        let addr = addr_of_hexes h1 h2 h3 in
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
        let vx = int_of_hex x in
        let vy = int_of_hex y in
        { opcode = SkipIfRegsEqual (vx, vy); duration_ms = 73. }
    | ['9'; x; y; '0'], _ ->
        let vx = int_of_hex x in
        let vy = int_of_hex y in
        { opcode = SkipIfRegsNotEqual (vx, vy); duration_ms = 73. }
    | ['8'; x; y; '0'], _ ->
        let vx = int_of_hex x in
        let vy = int_of_hex y in
        { opcode = SetRegToReg (vx, vy); duration_ms = 200. }
    | _, unsupported ->
      Printf.sprintf "Unsupported instruction %s" unsupported
      |> (fun s -> Invalid_argument s)
      |> raise

  type opcode =
    | ClearScreen
    | Jump of int
    | SetRegToValue of int * char
    | AddValueToReg of int * char
    | SetIndexToValue of int
    | Display of int * int * int

  type instruction =
    { opcode: opcode
    ; duration_ms: float
    }

  let int_of_hex (hex: char) : int = Hex.to_char '0' hex |> int_of_char

  let decode_instruction (byte1: char) (byte2: char) : instruction =
    let (c1, c2), (c3, c4) = Hex.of_char byte1, Hex.of_char byte2 in
    let hex_encodings = [c1; c2; c3; c4] in
    match hex_encodings, String.init 4 (List.nth hex_encodings) with
    | _, "00e0" -> { opcode = ClearScreen; duration_ms = 109. }
    | '1' :: [h1; h2; h3], _ ->
        let upper = Hex.to_char '0' h1 |> int_of_char |> (fun i -> i lsl 8) in
        let lower = Hex.to_char h2 h3 |> int_of_char in
        let addr = upper + lower in
        { opcode = Jump addr ; duration_ms = 105. }
    | ['6'; x; h1; h2], _ ->
        let x = int_of_hex x in
        let value = Hex.to_char h1 h2 in
        { opcode = SetRegToValue (x, value); duration_ms = 27. }
    | ['7'; x; h1; h2], _ ->
        let x = int_of_hex x in
        let value = Hex.to_char h1 h2 in
        { opcode = AddValueToReg (x, value); duration_ms = 45. }
    | ['a'; h1; h2; h3], _ ->
        let upper = int_of_hex h1 |> (fun i -> i lsl 8) in
        let lower = Hex.to_char h2 h3 |> int_of_char in
        let value = upper + lower in
        { opcode = SetIndexToValue value; duration_ms = 55. }
    | ['d'; x; y; h], _ ->
        let x = int_of_hex x in
        let y = int_of_hex y in
        let n = int_of_hex h in
        { opcode = Display (x, y, n); duration_ms = 22734. }
    | _, unsupported ->
      Printf.sprintf "Unsupported instruction %s" unsupported
      |> (fun s -> Invalid_argument s)
      |> raise

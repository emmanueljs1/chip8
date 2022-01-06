  type opcode =
    | ClearScreen
    | Jump of int
    | SetRegToValue of int * char
    | AddValueToReg of int * char
    | SetIndexToValue of int

  type instruction =
    { opcode: opcode
    ; duration_ms: float
    }

  let decode_instruction (byte1: char) (byte2: char) : instruction =
    let (c1, c2), (c3, c4) = Hex.of_char byte1, Hex.of_char byte2 in
    let hex_encodings = [c1; c2; c3; c4] in
    match hex_encodings, String.init 4 (List.nth hex_encodings) with
    | _, "00E0" -> { opcode = ClearScreen; duration_ms = 109. }
    | '1' :: [h1; h2; h3], _ ->
        let upper = Hex.to_char '0' h1 |> int_of_char |> (fun i -> i lsl 8) in
        let lower = Hex.to_char h2 h3 |> int_of_char in
        let addr = upper + lower in
        { opcode = Jump addr ; duration_ms = 105. }
    | ['6'; x; h1; h2], _ ->
        let x = Hex.to_char '0' x |> int_of_char in
        let value = Hex.to_char h1 h2 in
        { opcode = SetRegToValue (x, value); duration_ms = 27. }
    | ['7'; x; h1; h2], _ ->
        let x = Hex.to_char '0' x |> int_of_char in
        let value = Hex.to_char h1 h2 in
        { opcode = AddValueToReg (x, value); duration_ms = 45. }
    | ['A'; h1; h2; h3], _ ->
        let upper = Hex.to_char '0' h1 |> int_of_char |> (fun i -> i lsl 8) in
        let lower = Hex.to_char h2 h3 |> int_of_char in
        let value = upper + lower in
        { opcode = SetIndexToValue value; duration_ms = 55. }
    | _, unsupported ->
      Printf.sprintf "Unsupported instruction %s" unsupported
      |> (fun s -> Invalid_argument s)
      |> raise

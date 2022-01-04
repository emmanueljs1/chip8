  type opcode =
    | ClearScreen

  type instruction =
    { opcode: opcode
    ; duration_ms: float
    }

  let decode_instruction (_: char) : instruction =
    failwith "unimplemented"

  type opcode =
    | ClearScreen

  type instruction =
    { opcode: opcode
    ; next_pc: int
    ; duration_ms: float
    }

  let decode_instruction (_: int) (_: char) : instruction =
    failwith "unimplemented"

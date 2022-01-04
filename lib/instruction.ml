  type opcode =
    | ClearScreen

  type next_pc_strategy =
    | Increment

  type instruction =
    { opcode: opcode
    ; duration_ms: float
    }

  let decode_instruction (_: char) : instruction =
    failwith "unimplemented"

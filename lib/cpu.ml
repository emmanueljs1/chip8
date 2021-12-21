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

type instruction =
  | ClearScreen

let decode_instruction (_: char) : instruction =
  failwith "uninplemented"

type cpu =
  { memory: char array
  ; stack: int list
  ; display: bool array
  ; sound_timer: char
  ; delay_timer: char
  ; index: int
  ; pc: int
  }

let execute_instruction : cpu -> instruction -> cpu =
  failwith "unimplemented"

let init_cpu () : cpu =
  { memory = char_of_int 0 |> Array.make 4096
  ; stack = []
  ; display = Array.make 2048 false
  ; sound_timer = char_of_int 0
  ; delay_timer = char_of_int 0
  ; index = 0
  ; pc = 0
  }

let step (cpu: cpu) : cpu =
  let encoded_instruction = Array.get (cpu.memory) cpu.pc in
  encoded_instruction |> decode_instruction |> execute_instruction cpu

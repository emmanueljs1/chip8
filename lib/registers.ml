module R = struct
  type registers = char array

  let empty _ = char_of_int 0 |> Array.make 16

  let set_register reg c registers =
    registers.(reg) <- c

  let register reg registers = registers.(reg)

  let print registers =
    Array.iteri (fun i v ->
      let _, reg = char_of_int i |> Hex.of_char in
      Printf.sprintf "V%c = %i" (Char.uppercase_ascii reg) (int_of_char v)
      |> print_endline
    ) registers
end

module R = struct
  type registers = char array

  let empty _ = char_of_int 0 |> Array.make 16

  let set_register reg c registers =
    registers.(reg) <- c

  let register reg registers = registers.(reg)
end

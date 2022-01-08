type binary_op =
  | Or
  | And
  | Xor
  | Add
  | Subtract of bool
  | Shift of bool

type set_source =
  | Reg of int
  | Lit of int
  | RawByte of char
  | DelayTimer

type set_dest =
  | Reg of int
  | DelayTimer
  | SoundTimer
  | Index

type add_source =
  | Reg of int
  | RawByte of char

type add_dest =
  | Reg of int
  | Index

let key_opt_of_char (c: char) : char option =
  let value_opt =
  match c with
    | '1' -> Some 0x1
    | '2' -> Some 0x2
    | '3' -> Some 0x3
    | '4' -> Some 0xC
    | 'Q' -> Some 0x4
    | 'W' -> Some 0x5
    | 'E' -> Some 0x6
    | 'R' -> Some 0xD
    | 'A' -> Some 0x7
    | 'S' -> Some 0x8
    | 'D' -> Some 0x9
    | 'F' -> Some 0xE
    | 'Z' -> Some 0xA
    | 'X' -> Some 0x0
    | 'C' -> Some 0xB
    | 'V' -> Some 0xF
    | _ -> None
  in
  Option.map char_of_int value_opt

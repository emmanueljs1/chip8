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

let print_set_source (ss: set_source) : string =
  match ss with
  | Reg i -> Printf.sprintf "Reg %d" i
  | Lit i -> Printf.sprintf "Lit %d" i
  | RawByte c -> Printf.sprintf "RawByte %c" c
  | DelayTimer -> "DelayTimer"

type set_dest =
  | Reg of int
  | DelayTimer
  | SoundTimer
  | Index

let print_set_dest (sd: set_dest) : string =
  match sd with
  | Reg i -> Printf.sprintf "Reg %d" i
  | DelayTimer -> "DelayTimer"
  | SoundTimer -> "SoundTimer"
  | Index -> "Index"

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
    | 'q' -> Some 0x4
    | 'w' -> Some 0x5
    | 'e' -> Some 0x6
    | 'r' -> Some 0xD
    | 'a' -> Some 0x7
    | 's' -> Some 0x8
    | 'd' -> Some 0x9
    | 'f' -> Some 0xE
    | 'z' -> Some 0xA
    | 'x' -> Some 0x0
    | 'c' -> Some 0xB
    | 'v' -> Some 0xF
    | _ -> None
  in
  Option.map char_of_int value_opt

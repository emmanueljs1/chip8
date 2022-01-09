module R : sig
  type registers

  val empty : unit -> registers

  val set_register : int -> char -> registers -> unit

  val register : int -> registers -> char

  val print : registers -> unit
end

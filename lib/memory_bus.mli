module Make (GPU: Gpu.GPU) : sig
  type bus

  val init : program:char array -> bus

  val fetch : int -> bus -> char

  val write_mem : int -> char -> bus -> bus

  val write_vram : int -> bool -> bus -> bus
end

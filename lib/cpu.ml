open Instruction
open Util
module Registers = Registers.R

type state =
  | Running
  | Paused
  | Off

module Make (GUI: Gui.GUI) = struct
  module Bus = Memory_bus.Make(GUI)

  type cpu =
    { bus: Bus.bus
    ; stack: int list
    ; sound_timer: char
    ; delay_timer: char
    ; index: int
    ; registers: Registers.registers
    ; pc: int
    ; state: state
    ; gui: GUI.gui
    }

  let boot ~program =
    let gui = GUI.mk () in
    let bus = Bus.init ~program:program ~gui:gui in

    { bus = bus
    ; stack = []
    ; sound_timer = char_of_int 0
    ; delay_timer = char_of_int 0
    ; index = 0
    ; registers = Registers.empty ()
    ; pc = Bus.program_start bus
    ; state = Running
    ; gui = gui
    }

  let execute_instruction (cpu: cpu) (ins: instruction) : cpu =
    let pc' = cpu.pc + 2 in
    match ins.opcode with
    | ClearScreen ->
        Bus.gpu cpu.bus |> Bus.clear_vram;
        { cpu with pc = pc' }
    | Set (source, dest) ->
        let source_value =
          match source with
          | Reg reg -> Registers.register reg cpu.registers |> int_of_char
          | Lit value -> value
          | RawByte c -> int_of_char c
          | DelayTimer -> cpu.delay_timer |> int_of_char
        in
        let cpu' =
          match source, dest with
          | Reg _, Reg reg | RawByte _, Reg reg | DelayTimer, Reg reg ->
              let value = char_of_int source_value in
              Registers.set_register reg value cpu.registers;
              cpu
          | Reg _, DelayTimer->
              { cpu with delay_timer = source_value mod 256 |> char_of_int }
          | Reg _, SoundTimer->
              { cpu with sound_timer = source_value mod 256 |> char_of_int }
          | Lit _, Index ->
              { cpu with index = source_value }
          | _ ->
              let source_pp = print_set_source source in
              let dest_pp = print_set_dest dest in
              Printf.sprintf "%s -> %s not a valid set ins" source_pp dest_pp
              |> failwith
        in
        { cpu' with pc = pc' }
    | Add (source, dest) ->
        let value_to_add =
          match source with
          | Reg reg -> Registers.register reg cpu.registers |> int_of_char
          | RawByte c -> int_of_char c
        in
        let current_value =
          match dest with
          | Reg reg -> Registers.register reg cpu.registers |> int_of_char
          | Index -> cpu.index
        in
        let cpu' =
          match source, dest with
          | RawByte _, Reg reg ->
              let new_value = (current_value + value_to_add) mod 256 in
              Registers.set_register reg (char_of_int new_value) cpu.registers;
              cpu
          | Reg _, Index ->
              (* TODO: support ambiguous behavior *)
              { cpu with index = current_value + value_to_add }
          | _ -> failwith "invalid source/dest combination for add instruction"
        in
        { cpu' with pc = pc' }
    | Display (r1, r2, n) ->
        let gpu = Bus.gpu cpu.bus in
        let (w, h) = Bus.screen_dim gpu in
        let vx = (Registers.register r1 cpu.registers |> int_of_char) mod w in
        let vy = (Registers.register r2 cpu.registers |> int_of_char) mod h in
        let sprite_addr = cpu.index in
        let rec loop row col x y sprite_byte flag =
          if row >= n || y >= h then
            flag
          else if col >= 8 || x >= w then
            let row' = row + 1 in
            let sprite_byte' = Bus.fetch_ram (sprite_addr + row') cpu.bus in
            loop row' 0 vx (y + 1) sprite_byte' flag
          else
            let bit = int_of_char sprite_byte |> (fun i ->
              (i lsr (7 - col)) land 1
            )
            in
            let flag' =
              if bit = 1 then
                if Bus.fetch_vram x y gpu then (
                  Bus.write_vram x y false gpu;
                  true
                ) else (
                  Bus.write_vram x y true gpu;
                  false
                )
              else
                false
            in
            loop row (col + 1) (x + 1) y sprite_byte flag'
        in
        let sprite_byte = Bus.fetch_ram sprite_addr cpu.bus in
        let flag =
          if loop 0 0 vx vy sprite_byte false then
            1
          else
            0
        in
        Registers.set_register 0xF (char_of_int flag) cpu.registers;
        { cpu with pc = pc' }
    | EnterSubroutine addr ->
        { cpu with pc = addr; stack = pc' :: cpu.stack }
    | ReturnFromSubroutine ->
        let addr, stack' =
          match cpu.stack with
          | addr :: tl -> addr, tl
          | _ -> raise Not_found
        in
        { cpu with pc = addr; stack = stack' }
    | SkipIfRegHasValue (reg, value, has_value) ->
        let reg_value = Registers.register reg cpu.registers in
        if (reg_value = value) = has_value then
          { cpu with pc = cpu.pc + 4 }
        else
          { cpu with pc = pc' }
    | SkipIfRegsAreEqual (x, y, are_equal) ->
        let vx = Registers.register x cpu.registers in
        let vy = Registers.register y cpu.registers in
        if (vx = vy) = are_equal then
          { cpu with pc = cpu.pc + 4 }
        else
          { cpu with pc = pc' }
    | BinaryOp (x, binary_op, y) ->
        let vx = Registers.register x cpu.registers |> int_of_char in
        let vy = Registers.register y cpu.registers |> int_of_char in
        let vx' =
          match binary_op with
          | Or -> vx lor vy
          | And -> vx land vy
          | Xor -> vx lxor vy
          | Add ->
              let added = vx + vy in
              let flag = if added > 255 then 1 else 0 in
              Registers.set_register 0xF (char_of_int flag) cpu.registers;
              added mod 256
          | Subtract reversed ->
              let op1 = if not reversed then vx else vy in
              let op2 = if not reversed then vy else vx in
              let ones_complement = op2 lxor 255 in
              let subtracted = op1 + ones_complement + 1 in
              let flag = if subtracted > 255 then 1 else 0 in
              Registers.set_register 0xF (char_of_int flag) cpu.registers;
              subtracted mod 256
          | Shift right ->
              (* TODO: support ambiguous behavior *)
              let shifted = if right then vx lsr 1 else vx lsl 1 in
              let flag = if right then vx land 1 else (vx lsr 7) in
              Registers.set_register 0xF (char_of_int flag) cpu.registers;
              shifted mod 256
        in
        Registers.set_register x (char_of_int vx') cpu.registers;
        { cpu with pc = pc' }
    | JumpWithOffset (addr, with_offset) ->
        let offset =
          if with_offset then
            (* TODO: support ambiguous behavior *)
            Registers.register 0x0 cpu.registers |> int_of_char
          else
            0
        in
        { cpu with pc = addr + offset }
    | Random (x, value) ->
        let rand = Random.int 256 in
        let res = (int_of_char value) land rand |> char_of_int in
        Registers.set_register x res cpu.registers;
        { cpu with pc = pc' }
    | SkipIfKey (x, if_key_pressed) ->
        let pc'' =
          if GUI.is_key_pressed cpu.gui then
            let vx = Registers.register x cpu.registers in
            match GUI.read_key cpu.gui |> key_opt_of_char with
            | Some key when (key = vx) = if_key_pressed -> pc' + 2
            | _ -> pc'
          else
            pc'
        in
        { cpu with pc = pc'' }
    | GetKey x ->
        (* TODO: consider only progressing if key released *)
        if GUI.is_key_pressed cpu.gui then
          match GUI.read_key cpu.gui |> key_opt_of_char with
          | Some key ->
              Registers.set_register x key cpu.registers;
              { cpu with pc = pc' }
          | None -> cpu
        else
          cpu
    | FontCharacter x ->
        let vx = Registers.register x cpu.registers in
        let font = (int_of_char vx) land 0xF in
        let addr = Bus.font_addr font cpu.bus in
        { cpu with index = addr; pc = pc' }
    | DecimalConversion x ->
        let vx = Registers.register x cpu.registers |> int_of_char in
        let dig1 = vx / 100 in
        let wihout_hundreds = vx - (dig1 * 100) |> max 0 in
        let dig2 = wihout_hundreds / 10 in
        let dig3 = wihout_hundreds - (dig2 * 10) |> max 0 in
        Bus.write_ram cpu.index (char_of_int dig1) cpu.bus;
        Bus.write_ram (cpu.index + 1) (char_of_int dig2) cpu.bus;
        Bus.write_ram (cpu.index + 2) (char_of_int dig3) cpu.bus;
        { cpu with pc = pc' }
    | Store x ->
        let rec loop i =
          if i > x then
            ()
          else (
            let vx = Registers.register i cpu.registers in
            Bus.write_ram (cpu.index + i) vx cpu.bus;
            loop (i + 1)
          )
        in
        loop 0;
        { cpu with pc = pc' }
    | Load x ->
        let rec loop i =
          if i > x then
            ()
          else
            let byte = Bus.fetch_ram (cpu.index + i) cpu.bus in
            Registers.set_register i byte cpu.registers;
            loop (i + 1)
        in
        loop 0;
        { cpu with pc = pc' }
    | Noop -> { cpu with pc = pc' }

  let fetch_ins_bytes (cpu: cpu) : char * char =
    Bus.fetch_ram cpu.pc cpu.bus,
    Bus.fetch_ram (cpu.pc + 1) cpu.bus

  let print (cpu: cpu) : unit  =
    let (byte1, byte2) = fetch_ins_bytes cpu in
    let _, ins = hex_of_bytes byte1 byte2 in
    Printf.sprintf "Instruction: %s" ins |> print_endline;
    Registers.print cpu.registers;
    Printf.sprintf "Index %i" cpu.index |> print_endline;
    let index_mem = Bus.fetch_ram cpu.index cpu.bus in
    int_of_char index_mem
    |> Printf.sprintf "Mem at index: %i"
    |> print_endline

  let pause_cpu (cpu: cpu) : cpu =
    print cpu;
    { cpu with state = Paused }

  let step (cpu: cpu) : cpu * float =
    let (byte1, byte2) = fetch_ins_bytes cpu in
    let instruction = decode_instruction byte1 byte2 in
    let cpu' = execute_instruction cpu instruction in
    cpu', instruction.duration_ms

  let run ?(breakpoints = []) ?(debug = false) ~frequency cpu =
    let cycle_time = 1. /. (float_of_int frequency) in
    let timer_update_time = 1. /. 60. in

    let cpu = ref cpu in
    let booted_at = Unix.gettimeofday () in
    let cpu_mutex = Mutex.create () in

    let timer_thread =
      let needs_timer_update cpu =
        int_of_char cpu.sound_timer > 0 || int_of_char cpu.delay_timer > 0
      in

      let decr_timers cpu =
        let sound_timer' =
          int_of_char cpu.sound_timer - 1 |> max 0 |> char_of_int
        in
        let delay_timer' =
          int_of_char cpu.delay_timer - 1 |> max 0 |> char_of_int
        in
        { cpu with sound_timer = sound_timer' ; delay_timer = delay_timer' }
      in

      Thread.create (fun init ->
        let rec timer_loop last_timer_tick =
          let now = Unix.gettimeofday () in
          let elapsed_since_last_tick = now -. last_timer_tick in

          match !cpu.state with
          | Running ->
             if elapsed_since_last_tick < timer_update_time then (
               timer_update_time -. elapsed_since_last_tick |> Thread.delay;
               timer_loop last_timer_tick
             ) else
              let timer_updates =
                elapsed_since_last_tick /. timer_update_time |> int_of_float
              in

              for _ = 1 to timer_updates do
                Mutex.lock cpu_mutex;
                if needs_timer_update !cpu then cpu := decr_timers !cpu;
                Mutex.unlock cpu_mutex;
              done;

              let elapsed_in_updates_calc =
                float_of_int timer_updates *. timer_update_time
              in
              timer_loop (last_timer_tick +. elapsed_in_updates_calc)
          | Paused ->
              if elapsed_since_last_tick > timer_update_time then
                let noop_updates =
                  elapsed_since_last_tick /. timer_update_time |> int_of_float
                in
                let noop_updates_elapsed =
                  float_of_int noop_updates *. timer_update_time
                in
                timer_loop (now +. noop_updates_elapsed);
          | Off -> Thread.exit ()
        in
        timer_loop init
      ) booted_at
    in

    let clock_thread =
      Thread.create (fun init ->
        let rec clock_loop last_clock_tick =
          let now = Unix.gettimeofday () in
          let elapsed_since_last_tick = now -. last_clock_tick in

          match !cpu.state with
          | Running ->
              if elapsed_since_last_tick < cycle_time then (
                let cycles_to_sleep = float_of_int frequency /. 50. |> max 10. in
                (cycle_time *. cycles_to_sleep) -. elapsed_since_last_tick
                |> Thread.delay;
                clock_loop last_clock_tick
              ) else
                let cycles_to_run =
                  elapsed_since_last_tick /. cycle_time |> int_of_float
                in

                let rec loop n time_elapsed_in_cycle =
                  let did_pause =
                    if debug then (
                      Mutex.lock cpu_mutex;
                      let (byte1, byte2) = fetch_ins_bytes !cpu in
                      let _, ins = hex_of_bytes byte1 byte2 in
                      let will_pause =
                        List.find_opt (instructions_match ins) breakpoints <> None
                      in
                      if will_pause then cpu := pause_cpu !cpu;
                      Mutex.unlock cpu_mutex;
                      will_pause
                    ) else
                      false
                  in

                  if did_pause then
                    0
                  else if n > 0 then (
                    Mutex.lock cpu_mutex;
                    let (cpu', duration_ms) = step !cpu in
                    cpu := cpu';
                    Mutex.unlock cpu_mutex;

                    let time_elapsed_in_cycle' =
                      time_elapsed_in_cycle +. (duration_ms /. 1_000_000.)
                    in

                    if time_elapsed_in_cycle' > cycle_time then (
                      let extra_cycles_time =
                        time_elapsed_in_cycle' -. cycle_time
                    in
                      let extra_cycles =
                        extra_cycles_time /. cycle_time |> ceil |> int_of_float
                      in
                      let time_elapsed_in_next_cycle =
                        extra_cycles_time -.
                          (float_of_int extra_cycles *. cycle_time)
                      in

                      if extra_cycles <= n - 1 then
                        loop (n - 1 - extra_cycles) time_elapsed_in_next_cycle
                      else
                        extra_cycles
                    ) else if time_elapsed_in_cycle' = cycle_time then
                      loop (n - 1) 0.
                    else
                      loop n time_elapsed_in_cycle'
                  ) else if time_elapsed_in_cycle > 0. then
                    1
                  else
                    0
                in

                let extra_cycles = loop cycles_to_run 0. in
                let elapsed_in_cycles =
                  float_of_int (cycles_to_run + extra_cycles) *. cycle_time
                in
                clock_loop (last_clock_tick +. elapsed_in_cycles)
          | Paused ->
              if elapsed_since_last_tick > cycle_time then
                let noop_cycles =
                  elapsed_since_last_tick /. cycle_time |> int_of_float
                in
                let noop_cycles_elapsed =
                  float_of_int noop_cycles *. cycle_time
                in
                clock_loop (now +. noop_cycles_elapsed)
              else
                clock_loop last_clock_tick
          | Off -> Thread.exit ()
        in
        clock_loop init
      ) booted_at
    in

    let _ =
      Thread.create (fun _ ->
        let rec loop () =
          match !cpu.state with
          | Running when int_of_char !cpu.sound_timer > 0 ->
              GUI.play_sound !cpu.gui;
              Thread.delay 1.;
          | Off -> Thread.exit ()
          | _ -> loop ()
          in
        loop ()
      ) ()
    in

    let _ =
      Thread.create (fun _ ->
        let rec loop () =
          match GUI.read_key !cpu.gui with
          | c when int_of_char c = 32 && debug ->
              Mutex.lock cpu_mutex;
              begin match !cpu.state with
              | Running -> cpu := pause_cpu !cpu
              | Paused -> cpu := { !cpu with state = Running }
              | _ -> ()
              end;
              Mutex.unlock cpu_mutex;
              loop ()
          | c when int_of_char c = 13 && debug ->
              Mutex.lock cpu_mutex;
              begin match !cpu.state with
              | Paused ->
                  let (cpu', _) = step !cpu in
                  cpu := cpu';
                  print !cpu
              | _ -> ()
              end;
              Mutex.unlock cpu_mutex;
              loop ()
          | c when int_of_char c = 27 ->
              Mutex.lock cpu_mutex;
              cpu := { !cpu with state = Off };
              Mutex.unlock cpu_mutex
          | _ -> loop ()
        in
        loop ()
      ) ()
    in

    Thread.join clock_thread;
    Thread.join timer_thread
end

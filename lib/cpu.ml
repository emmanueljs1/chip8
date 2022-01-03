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

let decode_instruction (_: char) : instruction * float =
  (* TODO: implement *)
  (ClearScreen,  1000.)

type cpu =
  { mem: char array
  ; stack: int list
  ; display: bool array
  ; sound_timer: char
  ; delay_timer: char
  ; index: int
  ; pc: int
  ; off: bool
  }

let needs_timer_update (cpu: cpu) : bool =
  int_of_char cpu.sound_timer > 0 || int_of_char cpu.delay_timer > 0

let decr_timers (cpu: cpu) : cpu =
  let sound_timer' = int_of_char cpu.sound_timer - 1 |> max 0 |> char_of_int in
  let delay_timer' = int_of_char cpu.delay_timer - 1 |> max 0 |> char_of_int in
  { cpu with sound_timer = sound_timer' ; delay_timer = delay_timer' }

let execute_instruction (cpu: cpu) (_: instruction) : cpu =
  (* TODO: implement *)
  { cpu with pc = cpu.pc + 1 }

let boot ~program:(m: char array): cpu =
  let load_font (_: char array) : unit =
    (* TODO: implement *)
    ()
  in

  let mem = char_of_int 0 |> Array.make 4096 in
  load_font mem;
  Array.blit m 0 mem 0x0200 (Array.length m);

  { mem = mem
  ; stack = []
  ; display = Array.make 2048 false
  ; sound_timer = char_of_int 0
  ; delay_timer = char_of_int 0
  ; index = 0
  ; pc = 0
  ; off = false
  }

let step (cpu: cpu) : cpu * float =
  let encoded_instruction = cpu.mem.(cpu.pc) in
  let instruction, duration = decode_instruction encoded_instruction in
  let cpu' = execute_instruction cpu instruction in
  cpu', duration

let run_with ~debug:(_: bool) ~freq:(hz: int) ~prog:(m: char array) : unit =
  let cycle_time = 1. /. (float_of_int hz) in
  let timer_update_time = 1. /. 60. in

  let cpu = boot ~program:m |> ref in
  let booted_at = Unix.gettimeofday () in
  let cpu_mutex = Mutex.create () in

  let timer_thread =
    Thread.create (fun init ->
      let rec timer_loop last_timer_tick =
        let now = Unix.gettimeofday () in

        (* TODO: exit thread if CPU is off *)

        let elapsed_since_last_tick = now -. last_timer_tick in

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
      in
      timer_loop init
    ) booted_at
  in

  let clock_thread =
    Thread.create (fun init ->
      let rec clock_loop last_clock_tick =
        let now = Unix.gettimeofday () in

        (* TODO: turn off cpu and exit thread based on user input *)

        let elapsed_since_last_tick = now -. last_clock_tick in

        if elapsed_since_last_tick < cycle_time then (
          let cycles_to_sleep = float_of_int hz /. 50. |> max 10. in
          (cycle_time *. cycles_to_sleep) -. elapsed_since_last_tick
          |> Thread.delay;
          clock_loop last_clock_tick
        ) else
          let cycles_to_run =
            elapsed_since_last_tick /. cycle_time |> int_of_float
          in

          let rec loop n time_elapsed_in_cycle =
            if n > 0 then (
              Mutex.lock cpu_mutex;
              let (cpu', duration_ms) = step !cpu in
              cpu := cpu';
              Mutex.unlock cpu_mutex;

              let time_elapsed_in_cycle' =
                time_elapsed_in_cycle +. (duration_ms /. 1_000_000.)
              in

              if time_elapsed_in_cycle' > cycle_time then (
                let extra_cycles_time = time_elapsed_in_cycle' -. cycle_time in
                let extra_cycles =
                  extra_cycles_time /. cycle_time |> ceil |> int_of_float
                in
                let time_elapsed_in_next_cycle =
                  extra_cycles_time -. (float_of_int extra_cycles *. cycle_time)
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
      in
      clock_loop init
    ) booted_at
  in

  Thread.join clock_thread;
  Thread.join timer_thread

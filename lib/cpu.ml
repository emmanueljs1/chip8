open Instruction
open Registers
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
    ; registers: registers
    ; pc: int
    ; state: state
    ; gui: GUI.gui
    }

  let boot ~program =
    let gui = GUI.mk () in

    { bus = Bus.init ~program:program ~gui:gui
    ; stack = []
    ; sound_timer = char_of_int 0
    ; delay_timer = char_of_int 0
    ; index = 0
    ; registers = empty_registers
    ; pc = 0
    ; state = Running
    ; gui = gui
    }

  let execute_instruction (cpu: cpu) (ins: instruction) : cpu =
    failwith "unimplemented"

  let step (cpu: cpu) : cpu * float =
    let encoded_instruction = Bus.fetch_ram cpu.pc cpu.bus in
    let instruction = decode_instruction encoded_instruction in
    let cpu' = execute_instruction cpu instruction in
    cpu', instruction.duration_ms

  let run ~debug ~frequency cpu =
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

    (* TODO: user input poll thread (debug + external) *)

    Thread.join clock_thread;
    Thread.join timer_thread
end

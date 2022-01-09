# OCaml CHIP-8 interpreter

This is a [CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) interpreter written in OCaml, writing a CHIP-8 interpreter is the "Hello world" of emulator development. There are some attempts for this interpreter to be a COSMAC VIP emulator -- it uses its instruction times in its clock loop, but only to a degree, and it has the modern behavior of newer CHIP-8 interpreters for instructions implemented.

Instructions pass general tests found online, though some CHIP-8 games are still not playbale.

## Usage

Selecting the rom to run can be done with `--rom` (e.g. `--rom=roms/my_rom.ch8`)

Keys 1-4, Q-R, A-F, and Z-V are mapped to each row of the original COSMAC VIP keypad (1 = 1, 2 = 2, 3 = 3, 4 = C, Q = 4, W = 5, E = 6, R = D, A = 7, S = 8, D = 9, F = E, Z = A, X = 0, C = B, V = F).

The program can be exited with `ESC`.

Running with `--debug` allows for pausing the program with `SPACE`. Instruction breakpoints are supported with `--breakpoints` (e.g. `--breakpoints=FX55`), stepping is done with `RET`, and continuing is done with `SPACE` (note that you must step out of the breakpoint first).

Cycle time is configurable with `--frequency`, the default is 600 Hz (time of 1 cycle = 1 s / 60).

## Building
Install [OPAM](https://opam.ocaml.org) and then build with [dune](https://dune.readthedocs.io/en/stable/).

## Screenshots
<img alt="Screen Shot 2022-01-09 at 4 22 04 PM" src="https://user-images.githubusercontent.com/19786004/148699370-8706ae99-59ef-4dbb-906c-9eda1700f140.png">
<img alt="Screen Shot 2022-01-09 at 4 22 56 PM" src="https://user-images.githubusercontent.com/19786004/148699397-e9337757-cf1c-433b-9ca9-aa6e2ee23055.png">

## Acknowledgements
* [This](https://tobiasvl.github.io/blog/write-a-chip-8-emulator) guide to building a CHIP-8 emulator was incredibly helpful and cross-referenced throughout the project
* [This](https://github.com/rylev/DMG-01) Gameboy rust emulator for inspiration (and the [talk](https://media.ccc.de/v/rustfest-rome-3-gameboy-emulator) for which prompted this project)

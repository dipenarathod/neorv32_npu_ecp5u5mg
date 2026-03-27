# FPGA Setup — NEORV32 Board Integration

This directory contains the files needed to synthesize the complete NEORV32 + NPU system on an FPGA. It includes the top-level VHDL wrapper, pin constraint files, and build scripts.

## Contents

| File | Purpose |
|------|---------|
| Top-level VHDL (e.g., `neorv32_ProcessorTop_MinimalBoot.vhd`) | Instantiates the NEORV32 processor, Wishbone bus, NPU peripheral, and (optionally) the camera interface. Maps I/O to FPGA pins. |
| Constraints file (`.lpf`) | Lattice constraints — maps logical signals to physical ECP5 FPGA pins (clock, UART TX/RX, LEDs, reset, camera I/O). |
| TCL build script | Creates and configures a complete Lattice Diamond project, adds all source files, and runs the full synthesis flow. |

## Building the Bitstream

### Using the TCL script (recommended)

```bash
cd "FPGA Setup"
pnmainc <tcl_script_name>.tcl
```

This creates a Diamond project, adds all NEORV32 core files and the NPU, runs synthesis → map → place & route → bitstream generation.

### Manually in Lattice Diamond

1. Create a new project targeting your ECP5 device (e.g., ECP5U5MG).
2. Add all NEORV32 `rtl/core/*.vhd` files to a library called `neorv32`.
3. Add the top-level VHDL from this directory as the top entity.
4. Add the NPU VHDL from [`RTL/`](../RTL/).
5. Import the `.lpf` constraints file.
6. Run: Synthesis → Map → Place & Route → Bitstream.

## Prerequisites

- [Lattice Diamond](https://www.latticesemi.com/latticediamond) (free license available)
- NEORV32 VHDL source — clone the [GNAT Academic Program fork](https://github.com/GNAT-Academic-Program/neorv32-setups) with `git clone --recurse-submodules`
- The NPU peripheral from [`RTL/`](../RTL/)

## Programming the FPGA

Use the Lattice Diamond Programmer to load the generated `.bit` file via JTAG.

After programming:
1. Connect a serial terminal at **19200 baud, 8N1**: `gtkterm --port /dev/ttyUSB0 --speed 19200`
2. Enable **Configuration → CR LF Auto** in GTKTerm.
3. Press the reset button — you should see the NEORV32 bootloader menu.

## Clock Configuration

The ECP5's PLL block can generate a higher system clock from the board's crystal oscillator (e.g., 12 MHz → 48 MHz). Make sure the `CLOCK_FREQUENCY` generic in the top-level VHDL matches your actual system clock — the NEORV32 uses this for UART baud rate calculation.

## Porting to a Different Board

To target a different ECP5 board (ULX3S, OrangeCrab, etc.) or a different FPGA family entirely:

1. Copy this directory as a starting point.
2. Update the constraints file with your board's pin assignments (`.lpf` for Lattice, `.xdc` for Xilinx, `.sdc`/`.qsf` for Intel).
3. Adjust `CLOCK_FREQUENCY` to match your oscillator or PLL output.
4. Adjust IMEM/DMEM size generics based on your FPGA's block RAM budget.
5. Rebuild.

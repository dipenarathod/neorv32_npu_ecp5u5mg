# ECP5 Files — Lattice ECP5-Specific Resources

This directory contains files specific to the **Lattice ECP5** FPGA platform — the primary, fully tested target for the Wishbone NPU.

## About the ECP5

The Lattice ECP5 family is a low-power, low-cost FPGA well-suited for embedded applications. Key characteristics relevant to this project:

- Up to 85K LUTs and abundant block RAM for tensor storage
- On-chip PLL for clock generation (e.g., 12 MHz oscillator → 48 MHz system clock)
- DSP blocks for arithmetic acceleration
- Supported by both Lattice Diamond (proprietary) and the open-source [Yosys](https://github.com/YosysHQ/yosys) + [nextpnr-ecp5](https://github.com/YosysHQ/nextpnr) toolchain

## Relationship to Other Directories

- **[`FPGA Setup/`](../FPGA%20Setup/)** contains the NEORV32 top-level wrapper, pin constraints, and TCL build scripts that target this FPGA.
- **[`RTL/`](../RTL/)** contains the NPU peripheral VHDL, which is platform-independent but tested primarily on the ECP5.

## Targeting a Different FPGA

The NPU peripheral in [`RTL/`](../RTL/) is pure behavioral VHDL with no ECP5-specific primitives. To target a different FPGA family (Xilinx Artix-7, Intel Cyclone, etc.), you only need to create new board integration files — the NPU VHDL itself does not change.

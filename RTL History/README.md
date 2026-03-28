# RTL History — Archived NPU Versions

This directory contains **older versions** of the NPU peripheral VHDL, preserved for reference. These snapshots show how the design evolved over the course of development.

All listed items build on the one listed below:
- wb_optimized_copy_R_to_A_conv2D_Dipen:
  - The activation function (states) + pooling function (states) are optimized to handle looping via state transitions.
  - Multi-input and output channel conv2d added.
  - Dense requantization + clamping was broken into two states to meet timing requirements.
  - Conv uses dense functions (and intermediate variables) where possible. Renamed variables to make more sense (but can be improved)
  - Added Copy R to A for faster copying of result ot input tensor (had to modify the BRAM read/write processes for A and R)
  - Revised NPU result write procedure
- wb_larger_tensors: 10KB for Inputs, 36KB for weights, 8KB for biases, 10KB for output
- wb_dense: Added Dense
- wb_softmax: Added SoftMax
- wb_activation: Added ReLU
- wb_sigmoid: Added Sigmoid
- wb_pooling: Added 2x2 MaxPooling and 2x2 Average Pooling
- wb_tensor_arithmetic: Demonstrates rough NPU design. Supports basic addition and subtraction between three tensors and stores the result in the result tensor
- wb_buttons_leds: Basic Wishbone communication (converted from Verilog: https://github.com/mattvenn/wishbone_buttons_leds/tree/caravel)

## Why Keep These?

- **Learning resource**: Comparing early versions to the current design in [`RTL/`](../RTL/) shows how the architecture was refined — from simpler per-operation modules to the unified single-FSM design.
- **Debugging reference**: If a regression is suspected, older versions provide a known baseline for comparison.
- **Design rationale**: The progression illustrates trade-offs between LUT usage, operation support, and code complexity.

## Current Version

The production NPU peripheral is in **[`RTL/`](../RTL/)** — always use that directory for integration and development. The files here are historical only.

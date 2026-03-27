# RTL History — Archived NPU Versions

This directory contains **older versions** of the NPU peripheral VHDL, preserved for reference. These snapshots show how the design evolved over the course of development.

## Why Keep These?

- **Learning resource**: Comparing early versions to the current design in [`RTL/`](../RTL/) shows how the architecture was refined — from simpler per-operation modules to the unified single-FSM design.
- **Debugging reference**: If a regression is suspected, older versions provide a known baseline for comparison.
- **Design rationale**: The progression illustrates trade-offs between LUT usage, operation support, and code complexity.

## Current Version

The production NPU peripheral is in **[`RTL/`](../RTL/)** — always use that directory for integration and development. The files here are historical only.

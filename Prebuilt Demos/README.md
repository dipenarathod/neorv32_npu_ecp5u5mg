# Prebuilt Demos — Ready-to-Flash Bitstreams & Binaries

This directory will contain prebuilt artifacts so users can try the NPU demos **without installing any build tools**. Just flash and go.

## Planned Contents

Each demo will include a **bitstream** (`.bit` file for the ECP5 FPGA) and a **firmware binary** (`.exe` file for the NEORV32 bootloader):

| Demo | Description | Status |
|------|-------------|--------|
| MNIST 28×28 | Handwritten digit classification | Coming soon |
| MNIST 14×14 | Digit classification (smaller model) | Coming soon |
| Breast Cancer | Binary classification on tabular data | Coming soon |
| Rock-Paper-Scissors | Grayscale image classification | Coming soon |
| Integration Test | Multi-model end-to-end verification | Coming soon |

## How to Use (once files are added)

### 1. Flash the bitstream

Program the `.bit` file to your ECP5 FPGA using Lattice Diamond Programmer (JTAG).

### 2. Upload the firmware

1. Connect a serial terminal: `gtkterm --port /dev/ttyUSB0 --speed 19200`
2. Enable **Configuration → CR LF Auto** in GTKTerm.
3. Reset the board — you'll see the NEORV32 bootloader menu.
4. Press `u` → **Ctrl+Shift+R** → select the `.exe` file → press `e` to execute.

### 3. Read the output

Inference results — predicted class and confidence scores — print to the serial console.

## Building from Source

To modify a demo or build it yourself, see:
- Firmware: [`Ada Files/`](../Ada%20Files/)
- FPGA bitstream: [`FPGA Setup/`](../FPGA%20Setup/)
- Model training: [`Python Files/`](../Python%20Files/)

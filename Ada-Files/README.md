# Ada Files — Firmware Libraries & Demo Applications

This directory contains all Ada software that runs **on** the NEORV32 RISC-V processor: the NPU driver libraries and the demo applications that use them.

## Libraries

### Wb_Npu_Helper (NPU Driver)

The core driver library that wraps the NPU's memory-mapped registers into clean Ada procedure calls. It handles tensor window I/O, index math, opcode dispatch, and start/done handshakes. All hardware access goes through the NEORV32 Ada HAL — the library never touches registers directly.

| Child Package | Responsibility |
|---------------|----------------|
| `Wb_Npu_Address_Map` | Modify this file to specify the addresses used for registers/tensors in the NPU |
| `Wb_Npu_Helper.Dense` | Dense-layer driver loops, weight/bias sequencing, requantization |
| `Wb_Npu_Helper.Activation` | ReLU, Sigmoid, and SoftMax application loops |
| `Wb_Npu_Helper.Pooling` | 2×2 max and average pooling loops |
| `Wb_Npu_Helper.Processing` | Higher-level inference pipeline helpers |
| `Wb_Npu_Helper.Utils` | Pack/unpack INT8 ↔ 32-bit word, fixed-point conversion |
| `Wb_Npu_Helper.Debug` | Register printing and debug output over UART |

## Demo Applications

Each demo is a standalone [Alire](https://alire.ada.dev/) project that demonstrates end-to-end inference on the NEORV32 + NPU.

| Demo | What It Does |
|------|--------------|
| **MNIST 14×14** | Classifies handwritten digits (0–9) from a resized MNIST Dataset. 28x28 images resized to 14x14 |
| **MNIST 28×28** | Classifies handwritten digits (0–9) from the MNIST Dataset at full resolution |
| **Breast Cancer** | Binary classification of a tumor as benign and malignant. Uses the Wisconsin Breast Cancer Dataset |
| **Integration Test** | **Uses the camera controller and Wishbone interconnect** Detect rock-paper-scissors hand gestures |

Each demo project contains:
- `src/*.adb` — main program that loads weights, runs inference, prints results
- `src/*_weights.ads` — pre-exported INT8 fixed-point weight constants
- `src/*_samples*.ads` — test input data (e.g., pixel arrays for specific digits)
- `src/runtime_support.*` — Exit handler for the NEORV32
- `alire.toml` / `*.gpr` — Alire project manifest and GNAT project file

## Build & Flash

```bash
cd path/to/DEMO_NAME

# 1. Build the Ada project
alr build

# 2. Convert ELF → raw binary
riscv64-elf-objcopy -O binary bin/test_cases_neorv32 bin/test_cases_neorv32.bin

# 3. Generate NEORV32 executable
image_gen -app_bin bin/test_cases_neorv32.bin bin/test_cases_neorv32.exe
```

### Upload to the NEORV32

1. Connect the FPGA board via a UART module.
2. Open GTKTerm: `gtkterm --port /dev/ttyUSB0 --speed 19200`
3. Enable **Configuration → CR LF Auto**.
4. Reset the board — you'll see the NEORV32 bootloader menu.
5. Press `u` → **Ctrl+Shift+R** → select the `.exe` file → press `e` to execute.

Results (predicted class, confidence scores) print to the serial console.

## Creating a New Demo

1. Copy an existing demo folder as a template.
2. Train your model in [`Python Files/`](../Python%20Files/) using Keras.
3. Export weights with the conversion script to get INT8 Ada constant arrays.
4. Replace the `*_weights.ads` and `*_samples*.ads` files.
5. Update the main `.adb` to sequence NPU operations matching your model architecture (layer order must match the trained model exactly).
6. Update `alire.toml` and the `.gpr` with the new project name.

## Troubleshooting

- **`alr build` fails**: Run `alr toolchain --select` to install the RISC-V cross-compiler.
- **`image_gen` not found**: Build it from the NEORV32 repo — `gcc image_gen.c -o image_gen && sudo cp image_gen /usr/local/bin/`.
- **Serial garbage**: Verify 19200 baud and CR LF Auto enabled in GTKTerm.
- **Upload hangs**: Send the `.exe` file (not `.bin` or ELF) using Ctrl+Shift+R (raw binary transfer).

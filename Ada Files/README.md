# Ada Files — Firmware Libraries & Demo Applications

This directory contains all Ada software that runs **on** the NEORV32 RISC-V processor: the NPU driver libraries and the demo applications that use them.

## Libraries

### Ada_ML (NPU Driver)

The core driver library that wraps the NPU's memory-mapped registers into clean Ada procedure calls. It handles tensor window I/O, index math, opcode dispatch, and start/done handshakes. All hardware access goes through the NEORV32 Ada HAL — the library never touches registers directly.

| Child Package | Responsibility |
|---------------|----------------|
| `Ada_ML.Dense` | Dense-layer driver loops, weight/bias sequencing, requantization |
| `Ada_ML.Activation` | ReLU, Sigmoid, and SoftMax application loops |
| `Ada_ML.Pooling` | 2×2 max and average pooling loops |
| `Ada_ML.Processing` | Higher-level inference pipeline helpers |
| `Ada_ML.Utils` | Pack/unpack INT8 ↔ 32-bit word, fixed-point conversion |
| `Ada_ML.Debug` | Register printing and debug output over UART |

**If you're porting to a different language or processor**, this library is the definitive reference for which registers to access, in what order, and how data should be formatted.

### Input_Output_Helper (I/O Utilities)

Shared utilities for formatted UART output and performance measurement.

| Child Package | Responsibility |
|---------------|----------------|
| `Input_Output_Helper.Utils` | Pack/unpack helpers, array conversion |
| `Input_Output_Helper.Debug` | Pretty-print tensors, vectors, and inference results |
| `Input_Output_Helper.Time_Measurements` | CPU cycle counting for benchmarking |

## Demo Applications

Each demo is a standalone [Alire](https://alire.ada.dev/) project that demonstrates end-to-end inference on the NEORV32 + NPU.

| Demo | Model | What It Does |
|------|-------|--------------|
| **MNIST 28×28** | 784 → Dense → 10 | Classifies handwritten digits (0–9) at full resolution |
| **Breast Cancer** | 30 → Dense → 2 | Binary classification on tabular medical data |
| **Integration Test** | Multiple models | Runs MNIST 14×14 + Rock-Paper-Scissors to verify all NPU operations |

Each demo project contains:
- `src/*.adb` — main program that loads weights, runs inference, prints results
- `src/*_weights.ads` — pre-exported INT8 fixed-point weight constants
- `src/*_samples*.ads` — test input data (e.g., pixel arrays for specific digits)
- `src/runtime_support.*` — bare-metal runtime support for the NEORV32
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

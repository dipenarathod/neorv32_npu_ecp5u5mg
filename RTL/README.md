# RTL — NPU Peripheral

This directory contains the current, production version of the Wishbone NPU — a hardware peripheral that accelerates neural network operations.

## Overview

The NPU is implemented as a **single Finite State Machine (FSM)** that handles all operations through opcode dispatch. This design reduces LUT usage by reusing the Wishbone bus logic across every operation, rather than having separate hardware modules for each one.

## NPU State Machine

```mermaid
stateDiagram-v2
    [*] --> Idle

    Idle --> Capture_Registers : start bit asserted
    Capture_Registers --> OPCode_Branch : opcode + context stored,\nbusy = 1

    OPCode_Branch --> Done : invalid opcode\nor NOP
    OPCode_Branch --> Activation_Read : ReLU / Sigmoid / SoftMax
    OPCode_Branch --> Pooling_Read : Max Pool / Avg Pool
    OPCode_Branch --> Dense_Read : Dense (GEMM)
    OPCode_Branch --> Conv_Read : Conv2D

    Activation_Read --> Activation_Calc : word loaded
    Activation_Calc --> Activation_Write : result ready
    Activation_Write --> Done : last element
    Activation_Write --> Activation_Read : more elements

    Pooling_Read --> Pooling_Calc : window loaded
    Pooling_Calc --> Pooling_Write : result ready
    Pooling_Write --> Done : last window
    Pooling_Write --> Pooling_Read : more windows

    Dense_Read --> Dense_Calc : inputs + weights loaded
    Dense_Calc --> Dense_Write : MAC + requantize done
    Dense_Write --> Done : last neuron
    Dense_Write --> Dense_Read : more neurons

    Conv_Read --> Conv_Calc : kernel window loaded
    Conv_Calc --> Conv_Write : MAC + bias + quantize done
    Conv_Write --> Done : last output pixel
    Conv_Write --> Conv_Read : more pixels

    Done --> Idle : busy = 0, done = 1
```

Each operation follows the same pattern: **Read** input data from tensor windows → **Calculate** the result → **Write** to tensor R. Simple operations (ReLU) complete their read in one cycle; complex operations (Conv2D, Dense) use composite read states that span multiple cycles to load all required data.

## Tensor Windows

Data is stored as **4 INT8 values packed into each 32-bit word**. The NPU uses on-chip BRAM tensor windows for all data:

```mermaid
flowchart LR
    subgraph CPU ["CPU writes"]
        direction TB
        TA["Tensor A\n(input data)"]
        TB["Tensor B\n(weights / bias)"]
        TC["Tensor C\n(bias / kernel)"]
    end

    subgraph NPU_FSM ["NPU processes"]
        FSM["FSM\nRead → Calc → Write"]
    end

    subgraph Result ["CPU reads"]
        TR["Tensor R\n(output)"]
    end

    TA --> FSM
    TB --> FSM
    TC --> FSM
    FSM --> TR
```

| Window | Used for Dense | Used for Conv2D | Used for Activation / Pooling |
|--------|---------------|-----------------|-------------------------------|
| **A** | Input vector | Input image | Input tensor |
| **B** | Weight matrix | Bias value | — |
| **C** | Bias vector | Kernel weights | — |
| **R** | Output vector | Output feature map | Output tensor |

## Key Registers

| Register | Purpose |
|----------|---------|
| CTRL | Start bit + opcode field |
| STATUS | Busy / done flags |
| DIM | Tensor dimensions |
| WORD_INDEX | Current word index for element-wise ops (ReLU, Sigmoid, SoftMax) |
| POOL_BASE_INDEX | Top-left index for pooling window |
| WEIGHT_BASE_INDEX, BIAS_INDEX, N_INPUTS | Dense layer configuration |
| SCALE, ZERO_POINT, QUANT_MULTIPLIER, QUANT_MULTIPLIER_RIGHT_SHIFT | Requantization (Dense) |
| SUM, SOFTMAX_MODE | SoftMax phase control |

## Internal VHDL Packages

The NPU top-level imports distinct packages to separate reusable math from bus/FSM logic:

| Package | Responsibility |
|---------|----------------|
| `tensor_operations_basic_arithmetic` | Tensor types/constants, packed INT8 add/sub utilities |
| `tensor_operations_dense` | MAC and requantization helpers |
| `tensor_operations_conv2d` | Conv2D operation logic |
| `tensor_operations_activation` | ReLU, Sigmoid, SoftMax helpers |
| `tensor_operations_pooling` | 2×2 index math, max/average functions |

## Wishbone Interface

Standard Wishbone B4 slave: `wb_clk_i`, `wb_rst_i`, `wb_cyc_i`, `wb_stb_i`, `wb_we_i`, `wb_adr_i`, `wb_dat_i`, `wb_dat_o`, `wb_ack_o`.

## Using in Your Own SoC

The NPU has **no dependencies** on the NEORV32 or any specific FPGA — it is pure behavioral VHDL.

1. Add the VHDL files from this directory to your synthesis project.
2. Connect the Wishbone slave port to your bus interconnect.
3. Assign a base address.
4. From your CPU: write data to tensor windows, configure registers, set opcode in CTRL, assert start, poll STATUS for done, read results from tensor R.

The Ada ML library in [`Ada Files/`](../Ada%20Files/) demonstrates the exact register access sequence for every operation.

## Older Versions

Archived versions from earlier in development are kept in [`RTL History/`](../RTL%20History/) for reference.

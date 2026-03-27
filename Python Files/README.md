# Python Files — Model Training & Weight Conversion

Python scripts, Jupyter notebooks, and utilities for training neural networks and converting their weights into a format the NPU can consume.

## Contents

This directory contains two categories of files:

### ML Models

Models are trained using **Keras** and can then be exported for deployment on the NPU.

| File | Dataset | Description |
|------|---------|-------------|
| MNIST test (14×14) | MNIST (downscaled) | Digit classification — smaller model for constrained FPGAs |
| MNIST test (28×28) | MNIST (full) | Digit classification at full 28×28 resolution |
| Breast cancer | Scikit-learn breast cancer | Binary classification (benign/malignant) on 30 tabular features |
| Rock-Paper-Scissors | Custom dataset | Grayscale image classification |

### Helper Scripts

| Script | Purpose |
|--------|---------|
| Weight conversion script | Converts trained model weights (floating-point) to **INT8 fixed-point** arrays formatted as Ada constant declarations. This is the critical bridge between training and deployment. |
| Data-to-image script | Converts raw tensor data (e.g., INT8 Q0.7 pixel arrays printed over UART) into viewable PNG images — essential for verifying camera captures before running inference. |

## Workflow

1. **Train** a model by running a script or notebook in this directory.
2. **Export** weights to INT8 fixed-point using the weight conversion script. This quantizes floating-point weights using scale and zero-point parameters, producing Ada `.ads` files with constant arrays.
3. **Embed** the exported arrays into a demo firmware project under [`Ada Files/`](../Ada%20Files/).
4. **Build and flash** the firmware onto the NEORV32.

## Prerequisites

```bash
pip install keras numpy matplotlib scikit-learn jupyter Pillow
```

## Adding a New Model

Keep the architecture within the operations the NPU supports:

- Dense (fully connected) layers
- Conv2D (2D convolution with N×N kernel)
- ReLU, Sigmoid, or SoftMax activation
- Max Pooling or Average Pooling (2×2)

After training, export weights and compare floating-point accuracy vs. quantized accuracy in the notebook before deploying to hardware. Some accuracy loss from INT8 quantization is expected.

## Visualizing Captured Images

When the OV5640 camera captures an image, the pixel tensor gets printed over UART as INT8 values. To view it:

1. Copy the printed tensor from the GTKTerm window.
2. Paste it into the `img_data` variable in the data-to-image script.
3. Run the script — an `output.png` will be generated from the Q0.7 tensor.

## Why Keras?

Keras was chosen for its straightforward API for building sequential CNN models and built-in dataset loaders (MNIST, etc.). The Ada ML framework del2.0 was considered but does not support CNN layers like Conv2D and pooling.

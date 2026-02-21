All peripherals are clients. All listed items build on the one listed below:
- wb_larger_tensors: 10KB for Inputs, 36KB for weights, 8KB for biases, 10KB for output
- wb_dense: Added Dense
- wb_softmax: Added SoftMax
- wb_activation: Added ReLU
- wb_sigmoid: Added Sigmoid
- wb_pooling: Added 2x2 MaxPooling and 2x2 Average Pooling
- wb_tensor_arithmetic: Demonstrates rough NPU design. Supports basic addition and subtraction between three tensors and stores the result in the result tensor
- wb_buttons_leds: Basic Wishbone communication (converted from Verilog: https://github.com/mattvenn/wishbone_buttons_leds/tree/caravel)

About register read/write and indexing:
- Registers, such as weight_base_reg, are most often indices into the tensor word array. They may run an internal loop in the FSM to extract bytes from a word, such as in pooling states. The least two significant bits can be used for the byte offset.
- Read/Writes into tensors from the Ada library use address arithmetic. Base address + index*4 helps traverse from word to the next word. get_tensor_offset() is used to convert the Ada-provided address into an index for the tensors (used in wishbone read/write).
- Dense calculates the word using the word register + mac counter (how many input-weight pairs are processed). The least two significant bits give the byte offset that allows calculating how many lanes we can process. (They are discarded when calculating the word index) 

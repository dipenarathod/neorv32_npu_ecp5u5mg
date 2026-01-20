import numpy as np
import tensorflow as tf
from pathlib import Path

model_path="14x14_mnist_model.keras"
output_path=Path("exported_weights_q07")

ada_ads_path = output_path / "tensors_mnist_14x14_words.ads"
ada_package_name = "tensors_mnist_14x14_words"

#Input and output scales are always int8 Q0.7
lhs_scale = 1.0/128.0
lhs_zp = 0

#Dense output must also be int8 Q0.7 (so following layers stay unchanged)
result_scale = 1.0/128.0
result_zp = 0

q_min_int8 = -128
q_max_int8 = 127


def get_scale_and_zero_point(r_min, r_max):
    r_min=min(float(r_min), 0.0)    #Ensure 0 is included if all numbers are > 0
    r_max=max(float(r_max), 0.0)    #Ensure 0 is included if all numbers are < 0

    #Avoid divide by zero when calculating zerop point using scale later
    if(r_max==r_min):
        scale=1.0
        zero_point=0
        return scale, zero_point

    #S=(r_max-r_min)/(q_max-q_min)
    scale=(r_max-r_min)/float(q_max_int8-q_min_int8)

    #Zero point = q_min-(r_min/Scale)
    zero_point=int(np.round(q_min_int8-(r_min/scale)))

    #Force zero point in valid int8 range
    zero_point = np.clip(zero_point, q_min_int8, q_max_int8)

    return scale, zero_point

#Clip to int8 range
def quantize_int8(x, scale, zero_point):
    #Quantized value=round(R/S + Z)
    q=np.round(x/scale + zero_point).astype(np.int32)
    q=np.clip(q, q_min_int8, q_max_int8).astype(np.int8)
    return q


#Convert real multiplier to quantized multipler and right shifts (required to convert quatized multiplier to real multiplier)
def quantize_multiplier_smaller_than_one(real_multiplier):
    s=0
    while(real_multiplier < 0.5):
        real_multiplier *= 2.0
        s += 1

    q=int(np.round(real_multiplier * (1 << 31)))#Convert real multiplier to int32. real number * 2^31

    #quantized multiplier can't be 1 (refer to paragraphs after eqn 7 in gemmlowp's guide). +1 is outside of the int8 Q0.7 range [-1,0.9982]
    if(q == (1 << 31)): #q should != 2^31
        q //= 2
        s -= 1

    quantized_multiplier=int(q)          
    right_shift=int(s)                   
    #The quantized multiplier is a number in range [-1,1) stored in a int32 number. Tehrefore, It is left shifted by 31 bits
    return quantized_multiplier, right_shift


def write_array(f, name, arr):
    f.write(name + "\n")
    flat=arr.flatten()
    f.write(f"Length of array={len(flat)}\n")
    for i, v in enumerate(flat):
        f.write(str(int(v)))
        if(i != len(flat)-1):
            f.write(", ")
        if((i + 1) % 20 == 0):  #I tried printing 20 numbers per line in text. 20 was an arbitrary choice. 20 numbers per line look good
            f.write("\n")
    f.write("\n\n")


#Convert int to strict 8-bits for left shifting it. The 8-bits will be stored in the 32-bit word
def int8_to_u8(v: int):
    return int(v) & 0xFF


#Pack 4 8-bit values in a 32-bit word
def pack4_u8_to_u32(b0, b1, b2, b3):
    return (b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | ((b3 & 0xFF) << 24)

#Pack an int8 array into a 32-bit words array
def pack_int8_array_to_words(arr_int8):
    flat = arr_int8.flatten()
    words = []
    for i in range(0, len(flat), 4):
        b0 = int8_to_u8(flat[i])
        b1 = 0
        b2 = 0
        b3 = 0
        if(i + 1 < len(flat)):
            b1 = int8_to_u8(flat[i + 1])
        if(i + 2 < len(flat)):
            b2 = int8_to_u8(flat[i + 2])
        if(i + 3 < len(flat)):
            b3 = int8_to_u8(flat[i + 3])
        words.append(pack4_u8_to_u32(b0, b1, b2, b3))
    return words

#Stric 32-bit representation
def int32_to_u32(v):
    return int(v) & 0xFFFFFFFF

#Helper to write integer in Ada
def write_ada_int(f, name, value: int):
    f.write(f"{name} : constant Integer := {int(value)};\n")

#Helper to write Ada word array.
def write_ada_word_array(f, name, words):
    f.write(f"{name} : constant Word_Array (Natural range 0 .. {len(words)-1}) := (\n")
    for i, w in enumerate(words):
        if(i!=len(words)-1):
            sep = ","
        else:
            sep = ""
        f.write(f"16#{w:08X}#{sep}\n")
    f.write(");\n\n")

def main():
    output_path.mkdir(parents=True, exist_ok=True)
    model=tf.keras.models.load_model(model_path)
    ada_file = open(ada_ads_path, "w")
    ada_file.write("with Ada_Ml_Library; use Ada_Ml_Library;\n\n")
    ada_file.write(f"package {ada_package_name} is\n\n")
    ada_entries = []
    for layer_index, layer in enumerate(model.layers):
        weights=layer.get_weights()
        if(not weights):
            #Skipping layers without weights
            continue

        base_name=f"layer_{layer_index}_{layer.name}"
        header_path=output_path/f"{base_name}.txt"

        with open(header_path, "w") as f:
            #Dense: weights and biases both
            if(len(weights)==2):
                w_float, b_float=weights

                w_scale, w_zp=get_scale_and_zero_point(np.min(w_float), np.max(w_float))

                #Quantize weights to int8
                w_q=quantize_int8(w_float, w_scale, w_zp)
                w_q = w_q.T #IMPORTANT bug Solved the problem
                #Quantize bias to int32 using bias_scale=lhs_scale * w_scale
                #bias is added to the accumulator, before requantization
                bias_scale=lhs_scale * w_scale
                b_q32=np.round(b_float/bias_scale).astype(np.int32)

                #Compute requantization multiplier for output:
                #real_multiplier=(lhs_scale * rhs_scale)/result_scale
                #lhs_scale=lhs_scale, rhs_scale=w_scale, result_scale=result_scale
                real_multiplier=(lhs_scale * w_scale)/result_scale

                quantized_mult, quantized_right_shift=quantize_multiplier_smaller_than_one(real_multiplier)

                #Write data to files
                f.write(f"{base_name}\n")
                f.write(f"W_scale={w_scale}\n")
                f.write(f"W_zp={w_zp}\n")
                f.write(f"bias_scale = lhs_scale * W_scale = {bias_scale}\n")
                f.write(f"real multiplier = (lhs_scale * W_scale)/result_scale = {real_multiplier}\n")
                f.write(f"quantized mult = {quantized_mult}\n")
                f.write(f"quantized right shift (right shift) = {quantized_right_shift}\n")
                f.write("\n")

                #Export arrays
                write_array(f, f"{base_name}_weights_int8", w_q.astype(np.int8))
                write_array(f, f"{base_name}_bias_int32", b_q32.astype(np.int32))

                w_words = pack_int8_array_to_words(w_q.astype(np.int8))
                b_words = [int32_to_u32(v) for v in b_q32.flatten()]
                
                prefix = base_name
                write_ada_int(ada_file, f"{prefix}_WZP", int(w_zp))
                write_ada_int(ada_file, f"{prefix}_Quantized_Multiplier", int(quantized_mult))
                write_ada_int(ada_file, f"{prefix}_Quantized_Right_Shift", int(quantized_right_shift))
                ada_file.write("\n")

                write_ada_word_array(ada_file, f"{prefix}_Weights_Words", w_words)
                write_ada_word_array(ada_file, f"{prefix}_Bias_Words", b_words)
            else:
                #Single-weights case for convolution. TODO. Wrote something incase
                for p_index, p_float in enumerate(weights):
                    p_scale, p_zp=get_scale_and_zero_point(np.min(p_float), np.max(p_float))
                    p_q=quantize_int8(p_float, p_scale, p_zp)
                    f.write(f"{base_name}_p{p_index}\n")
                    f.write(f"scale={p_scale}\n")
                    f.write(f"ZERO_POINT={p_zp}\n\n")
                    write_array(f, f"{base_name}_p{p_index}_int8", p_q.astype(np.int8))
    ada_file.write(f"end {ada_package_name};\n")
    ada_file.close()


if(__name__=="__main__"):
    main()

import keras
from keras import models
from keras import layers
from keras.datasets import mnist
from keras.utils import to_categorical
import numpy as np
import tensorflow as tf

def quantize_q07(x):
    x_clipped=np.clip(x, -1.0, 0.9921875) #Restrict weights in the numpy array to -1 to 127/128
    q=np.round(x_clipped * 128.0).astype(np.int16) #128=scaling factor
                                                     #int16 is used in the intermediate step 
                                                     #similar to our calculations in VHDL
    q=np.clip(q, -128, 127).astype(np.int8)       #Clip to int8
    return q

(train_images, train_labels), (test_images, test_labels)= mnist.load_data()

#Add number of channels
train_images=train_images.reshape(train_images.shape[0], 28, 28, 1)
test_images=test_images.reshape(test_images.shape[0], 28, 28, 1)

#Normalize images
train_images=train_images.astype('float32')/255.0
test_images=test_images.astype('float32')/255.0

train_14 = tf.image.resize(train_images, (14, 14), method="bilinear")
test_14  = tf.image.resize(test_images,  (14, 14), method="bilinear")

#Flatten
train_vec = tf.reshape(train_14, [-1, 14 * 14])  
test_vec  = tf.reshape(test_14,  [-1, 14 * 14])  

print("Train:", train_vec.shape, train_labels.shape)
print("Test :", test_vec.shape, test_labels.shape)

model = keras.Sequential([
    layers.Input(shape=(196,)),
    layers.Dense(32, activation="relu"),
    layers.Dense(10,activation="softmax")
])

model.compile(
    optimizer=keras.optimizers.Adam(1e-3),
    loss=keras.losses.SparseCategoricalCrossentropy(),
    metrics=["accuracy"]
)

model.fit(
    train_vec, train_labels,
    batch_size=256,
    epochs=20,
    validation_split=0.1,
    verbose=2
)

test_loss, test_acc = model.evaluate(test_vec, test_labels, verbose=0)
print("Test acc:", test_acc)

#print(train_vec[0])
print(quantize_q07(train_vec[0]))
print(train_labels[0])

model.save(filepath="14x14_mnist_model.keras")


from pathlib import Path
import random
ada_out_dir = Path("exported_samples_q07")
ada_out_dir.mkdir(parents=True, exist_ok=True)
ada_ads_path = ada_out_dir / "mnist_train_samples_14x14.ads"
ada_package_name = "mnist_train_samples_14x14"

def write_ada_int_array(f, name, flat):
    f.write(f"  {name} : constant Integer_Array (Natural range 0 .. {len(flat)-1}) := (\n")
    for i, w in enumerate(flat):
        sep = "," if i != len(flat) - 1 else ""
        f.write(f"  {int(w)}{sep}\n")
    f.write("  );\n")



#Convert tensors to numpy arrays for indexing
train_vec_np = train_vec.numpy()          #(N, 196)
train_labels_np = np.array(train_labels)  #N

sample_count = 10

random_numbers = [random.randint(0, len(train_vec_np)-1) for _ in range(sample_count)]

random_images=[]
random_labels=[]

for index in random_numbers:
    random_images.append(quantize_q07(train_vec_np[index])  )
    random_labels.append(train_labels_np[index])
  

with open(ada_ads_path, "w") as f:
    f.write("with Ada_Ml_Library; use Ada_Ml_Library;\n")
    f.write(f"package {ada_package_name} is\n")
    f.write(f"  Sample_Count : constant Natural := {sample_count};\n")
    f.write(f"  type Sample_Set is array (Natural range 0 .. Sample_Count - 1) of Integer_Array (Natural range 0 .. 195);\n")

    #Write Samples as a proper 2D Ada constant
    f.write("   Samples : constant Sample_Set := (\n")
    for s in range(sample_count):
        f.write(f"  {s} => (\n")
        flat = random_images[s].astype(np.int8).flatten()  # ensure Python int printing is sane
        for i, v in enumerate(flat):
            if(i!=len(flat)-1):
                sep = ","
            else:
                sep = ""
            f.write(f"  {int(v)}{sep}")
            if (i + 1) % 20 == 0:
                f.write("\n")
            else:
                f.write(" ")
        closer="\n)"
        if(s != sample_count - 1):
            closer = closer + ","
        closer = closer + "\n"
        f.write(closer)
    f.write(");\n")

    write_ada_int_array(f,"    Labels", random_labels)

    f.write(f"end {ada_package_name};\n")

print(f"Wrote Ada samples to: {ada_ads_path}")

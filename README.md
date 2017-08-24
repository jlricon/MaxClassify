# MaxClassify
This package is a wrapper around the maxent R package. It includes a few convenience functions to be able to work with it using text directly.

It has a few improvements over the base package:
* Models are S3 objects, to make it easier to work with them (add them to lists, for example)
* Multicore enabled. While the base training algorithm is unchanged, prediction is multithreaded, and so is a tuning function. These two changes preserve the underlying sparsity, allowing the model to use reasonable amounts of RAM even for big datasets.
* A function to get a table of the weights, to see which words are relevant for each class
* Uses quanteda to create training matrices, making it easier to get text ready to classify. Note that this package does not do cleaning. It expects clean input.

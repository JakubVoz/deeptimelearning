# deeptimelearning
Code of the paper "Deep learning from phylogenies for diversification analysis" including pretrained neural networks.

In this repository, you can find:
- simulators used for simulating training, validation and testing sets both for BD and BiSSE in the folder ./simulators.
- code for encoding phylogenies into Compact Diversity Vector (CDV) and Summary statistics (SS) for BD (SS and CDV) and BiSSE (CDV with tip information) models in the folder ./encoding
- code used for inference with maximum likelihood in the folder ./maxlikeli for both BD (with the package RPANDA) and BiSSE (with packages castor and diversitree)
- pretrained neural networks and scalers used in the paper, both for BD and BiSSE model, in the folder ./neural_networks/pretrained_NN
- code for inference with pretrained neural networks, for BD and Summary statistics (BD_ffnn_SS_mae.ipynb), for BD and CDV (BD_cnn_CDV_mae.ipynb) and for BiSSE and CDV (BiSSE_cnn_CDV_mae.ipynb) in the folder ./neural_networks/inference_using_pretrained_NN. These can be easily adapted for other pretrained NNs. 
- code for training the neural networks with an example for BD and summary statistics and for BiSSE and CDV in the folder ./neural_networks/training

## Preprint
Lambert S, Voznica J, Morlon H (2022)
__Deep learning from phylogenies for diversification analysis__. [bioRxiv](https://www.biorxiv.org/)

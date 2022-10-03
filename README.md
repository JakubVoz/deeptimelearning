# deeptimelearning

> Code of the paper "Deep learning from phylogenies for diversification analysis" including pre-trained neural networks.

### In this repository, you can find:

-------

- `simulators` used for simulating training, validation and testing sets both for BD and BiSSE in the folder [simulators](/simulators).
- `neural_networks` containing:
  - scripts for encoding phylogenies into Compact Diversity Vector (CDV) and Summary statistics (SS) for BD (SS and CDV) and BiSSE (CDV with tip information) models in the folder [encoding_phylo](/neural_networks/encoding_phylo). You call the scripts from terminal with: ```bash python3 encoding_script_name.py -t filename_forest.nwk > ./encoding_output.csv```
  - scripts for training the neural networks, both for CNN and FFNN using CDV or SS for BD and BiSSE models in the folder [training_NN](/neural_networks/training_NN).
  - the pre-trained neural networks and scalers used in the paper, both for BD and BiSSE model, in the folder [pretrained_NN](/neural_networks/pretrained_NN).
- `estimation` scripts for:
  - Maximum Likelihood Estimation (MLE) in the folder [MLE](/estimation/MLE) for both BD (with the package UDivEvo) and BiSSE (with packages castor and diversitree).
  - Neural Network (NN) estimation in the folder [NN](/estimation/NN) for both BD and BiSSE (using CNN and FFNN) on simulations and for BiSSE (using CNN) on the empirical phylogeny of primates. These can be easily adapted for other pre-trained NNs. Concerning the empirical phylogeny, you will find in the folder [empirical](/estimation/NN/empirical) a tutorial to use Jupiter Notebook and the pre-trained neural network as well as the codes for the sanity check performed in the paper on summary statistics.
- `data` containing all the data used and generated for the analysis on simulations and on the empirical phylogeny of primates in the folder [data](/data). Note that the simulated phylogenies are not provided here due to their memory use.
- `results_analysis` containing all the scripts for analysing the results of the estimation and making the figures and tables for the related paper in the folder [results_analysis](/results_analysis).

The simulators, scripts for encoding and MLE were part of a pipeline, thus you may need to adapt the input arguments.

## Preprint:

Lambert S, Voznica J, Morlon H (2022)
__Deep learning from phylogenies for diversification analysis__. [bioRxiv](https://www.biorxiv.org/)

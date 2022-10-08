# deeptimelearning

> Code of the paper "Deep learning from phylogenies for diversification analysis" including pre-trained neural networks.

### In this repository, you can find:

-------

- `simulators` used for simulating training, validation and testing sets both for BD (a python script) and BiSSE (an R script) in the folder [simulators](/simulators).
- `neural_networks` containing:
  - scripts for encoding phylogenies into Compact Diversity Vector (CDV) and Summary statistics (SS) for BD (SS and CDV) and BiSSE (CDV with tip information) models in the folder [encoding_phylo](/neural_networks/encoding_phylo). You call the scripts from terminal with (the input tree(s) must be in newick format): 
```bash
python3 encoding_script_name.py -t filename_forest.nwk > ./encoding_output.csv
```
  - scripts for training the neural networks (in ipynb format), both for CNN and FFNN using CDV or SS for BD and BiSSE models in the folder [training_NN](/neural_networks/training_NN).
  - the pre-trained neural networks (including models in json and weights in h5 format) and scalers used in the paper, both for BD and BiSSE model, in the folder [pretrained_NN](/neural_networks/pretrained_NN).
- `estimation` scripts for:
  - Maximum Likelihood Estimation (MLE) in the folder [MLE](/estimation/MLE) for both BD (with the package UDivEvo) and BiSSE (with packages castor and diversitree).
  - Neural Network (NN) estimation in the folder [NN](/estimation/NN) for both BD and BiSSE (using CNN and FFNN) on simulations and for BiSSE (using CNN) on the empirical phylogeny of primates. Concerning the empirical phylogeny, you will find in the folder [empirical](/estimation/NN/empirical) a tutorial to use Jupiter Notebook and the pre-trained neural network as well as the codes for the sanity checks performed in the paper.
- `data` containing all the data used and generated for the analysis on simulations and on the empirical phylogeny of primates in the folder [data](/data). Note that the simulated phylogenies are not provided here due the file sizes exceeding limits of github.
- `results_analysis` containing all the scripts for analysing the results of the estimation and making the figures and tables for the related paper in the folder [results_analysis](/results_analysis).

Scripts for simulations and encoding are callable functions.

## Preprint:

Lambert S, Voznica J, Morlon H (2022)
__Deep learning from phylogenies for diversification analysis__. [bioRxiv](https://www.biorxiv.org/content/10.1101/2022.09.27.509667v1)

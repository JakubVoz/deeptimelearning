## Constant BD

## Inferring net div and turn in MLE with custom function
## on phylogenies conditioned on 200 to 500 nb of tips
## with variable sampling fraction
## and informative starting value
## The custom function used here is the same
## as the one available at
## https://github.com/sophia-lambert/UDivEvo/blob/master/R/fitMLE_bdRho.R
## with the appropriate arguments
## but just with less options.

## make sure you have the package phytools and ape downloaded

args <- commandArgs(TRUE)
indice <- as.numeric(args[1])
set.seed(as.numeric(args[1])*10)
numb <- indice+1

######################################################################################
# Downloading data set
######################################################################################

load("../../../../estimation/MLE/simulations/BD/fitMLE_bdRho_custom.RData")
JForest <- phytools::read.newick(file='../X.nwk') # a forest of 500 trees
test_param <- read.csv(file = "../../../../sim_parameters.csv") # a table of simulated parameters and properties of the 500 phylogenies

tot_time_cal <- max(TreeSim::getx(JForest[[numb]]))

Ntips <- ape::Ntip(JForest[[numb]])

lamb_moments <- log(Ntips)/tot_time_cal

ysim <- test_param$sampling_frac[numb]

######################################################################################
# Inference
######################################################################################

fit_cBDB_known <- fit_bernoulli_yfixed(phylo = JForest[[numb]], tot_time = tot_time_cal,
                                            d= lamb_moments, epsi = 0, yj = ysim, cond = "crown",
                                            meth = "Nelder-Mead", YULE = FALSE)

fit_cBDB_known$d_par
fit_cBDB_known$epsi_par

b <- fit_cBDB_known$d_par/(1-fit_cBDB_known$epsi_par)
d <- fit_cBDB_known$d_par*fit_cBDB_known$epsi_pa/(1-fit_cBDB_known$epsi_par)

######################################################################################
# Saving
######################################################################################

res <- data.frame(turnover_rate = fit_cBDB_known$epsi_par, birth_rate = b, extinction_rate = d,
                  diversification_rate = fit_cBDB_known$d_par)

write.csv2(res, paste0("res", indice, ".csv"))


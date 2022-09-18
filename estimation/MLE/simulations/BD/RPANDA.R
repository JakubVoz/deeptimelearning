#!/usr/bin/env Rscript
## RPANDA on constant rate BD

# download package phytools, RPANDA, TreeSim, ape
library(phytools)
library(RPANDA)
library(TreeSim)
library(ape)

args <- commandArgs(TRUE)
indice <- as.numeric(args[1])
set.seed(as.numeric(args[1])*10)

# load trees
test_trees <- try(phytools::read.newick(file = args[2]))
sim_range <- read.table(args[3], header = FALSE)

outputfile1 <- args[4]
colnames(sim_range) = c("index", "turnover_rate","birth_rate","extinction_rate","sampling_frac","R_nought","tree_size")

tot_time_cal <- max(TreeSim::getx(test_trees))

Ntips <- ape::Ntip(test_trees)

lamb_moments <- log(Ntips)/tot_time_cal
ysim <- test_param$sampling_frac


f.lamb <-function(t,y){y[1]}
f.mu<-function(t,y){y[1]}
lamb_par<-c(lamb_moments)
mu_par<-c(0)
fit_cBDB_known <- RPANDA::fit_bd(phylo = test_trees, tot_time = tot_time_cal,f.lamb, f.mu, lamb_par, mu_par, f = ysim,
                                 meth = "Nelder-Mead", cst.lamb = TRUE, cst.mu = TRUE,
                                 cond = "crown")

# recover parameters of interest
d <- fit_cBDB_known$lamb_par-fit_cBDB_known$mu_par
epsi <- fit_cBDB_known$mu_par/fit_cBDB_known$lamb_par

# Savings
res <- data.frame(turnover_rate = epsi, birth_rate = fit_cBDB_known$lamb_par,
                  extinction_rate = fit_cBDB_known$mu_par,
                  diversification_rate = d)

write.csv(res, file=outputfile1, col.names = FALSE)

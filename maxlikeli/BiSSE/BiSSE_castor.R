#!/usr/bin/env Rscript
## BiSSE inference

# download package treeio, diversitree and castor
library(treeio)
library(castor)
library(diversitree)



args <- commandArgs(TRUE)
indice <- as.numeric(args[1])
set.seed(as.numeric(args[1])*10)

# load trees
test_trees <- try(treeio::read.nhx(file = args[2]))
sim_range <- read.table(args[3], header = FALSE)
outputfile1 <- args[4]

colnames(sim_range) = c('index', 'lambda1', 'lambda2', 'turnover', 'sampling_frac', 'tree_size','mu1', 'mu2', 'net_rate1' ,'net_rate2', 'q01', 'q10', 'lambda2_ratio', 'q01_ratio')
states <- matrix(data = test_trees@data$`[&&NHX-t_s`)
row.names(states) <- test_trees@phylo$tip.label
states[states == 1] <- 0
states[states == 2] <- 1

# Castor MLE
states <- matrix(data = test_trees@data$`[&&NHX-t_s`)
row.names(states) <- test_trees@phylo$tip.label

fit = castor::fit_musse(tree = test_trees@phylo,
                        Nstates            = 2,
                        tip_pstates        = states[,1],
                        sampling_fractions = sim_range$sampling_frac[1],
                        transition_rate_model = "ER")

# Savings
res <- data.frame(lambda1_castor = fit$parameters$birth_rates[1],
                  lambda2_castor = fit$parameters$birth_rates[2],
                  mu1_castor = fit$parameters$death_rates[1],
                  mu2_castor = fit$parameters$death_rates[2],
                  q01_castor = fit$parameters$transition_matrix[1,2],
                  q10_castor = fit$parameters$transition_matrix[2,1])

write.csv(res, file=outputfile1, col.names = FALSE)

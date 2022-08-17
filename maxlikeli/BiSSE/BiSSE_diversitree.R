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

# Diversitree MLE
LikeliBiSSE <- diversitree::make.bisse(tree = test_trees@phylo, states = states[,1], sampling.f = sim_range$sampling_frac[1])
# LikeliBiSSE <- diversitree::make.bisse(tree = test_trees@phylo, states = states[,1], sampling.f = 1)
p <- diversitree::starting.point.bisse(test_trees@phylo)
BiSSE_MLFit <- diversitree::find.mle(LikeliBiSSE, p)
# coef(BiSSE_MLFit)

# Diversitree MLE constrained
cstr_LikeliBiSSE <- diversitree::constrain(LikeliBiSSE,  q10 ~ q01)
cstr_BiSSE_MLFit <- diversitree::find.mle(cstr_LikeliBiSSE, p[1:5])


# Savings
res <- data.frame(lambda1_diversitree = coef(cstr_BiSSE_MLFit)[1],
                  lambda2_diversitree = coef(cstr_BiSSE_MLFit)[2],
                  mu1_diversitree = coef(cstr_BiSSE_MLFit)[3],
                  mu2_diversitree = coef(cstr_BiSSE_MLFit)[4],
                  q01_diversitree = coef(cstr_BiSSE_MLFit)[5],
                  q10_diversitree = coef(cstr_BiSSE_MLFit)[5])

write.csv(res, file=outputfile1, col.names = FALSE)

#!/usr/bin/env Rscript
#
# Simulations of the binary state dependent
## speciation and extinction
## conditionned on having 200-500 tips (prior to sampling)

## library required :
library("TreeSim")
library("castor")

args <- commandArgs(TRUE)

# parameter values table
params <- read.table(file=args[1], sep="\t", header=FALSE)
colnames(params) = c("index", 'lambda1', 'lambda2', 'turnover', 'sampling_frac', 'tree_size', 'mu1', 'mu2', 'net_rate1', 'net_rate2', 'q01', 'q10', 'lambda2_ratio', "q01_ratio")
indice <- as.integer(args[2])
seed_base <- as.integer(args[3])
step <- as.integer(args[4])
nb_retrials <- as.integer(args[5])
outtreefile <- args[6]
outstatsfile <- args[7]
outparamfile <- args[8]

# initialize count of tip states and forest
subpop_measures_out = c()
forest=c()

# simulation
for (i in 1:nrow(params)) {
  # counter retrials of simulation
  j <- 0

  # set seed
  set.seed(seed_base+indice*step+3+j)

  # prepare parameter matrices
  q=params[i,11]
  A = matrix(c(-q, q, q, -q), nrow=2, ncol=2)
  parameters = list(birth_rates         = c(as.numeric(params[i, 2]), as.numeric(params[i, 3])),
                    death_rates         = c(as.numeric(params[i, 7]), as.numeric(params[i, 8])),
                    transition_matrix_A = A)



  sim_bissentips <- list()
  sim_bissentips$success=FALSE
  while (sim_bissentips$success=="FALSE") {  # retry simulations
    set.seed(seed_base+indice*step+3+j)
    sim_bissentips = castor::simulate_dsse(Nstates       = 2,
                                             parameters    = parameters,
                                             max_tips      = as.integer(params[i,6]/params[i,5]), # prior to sampling
                                             sampling_fractions = params[i,5],
                                             no_full_extinction = FALSE,
                                             coalescent = TRUE)
    if(sim_bissentips$success=="FALSE"){
      j <- j+1
      #cat("phylo dead, no survivor \n")
    }

    if(j > nb_retrials) {
      # simulate just a small tree, that will be removed downstream in the pipeline
      while (sim_bissentips$success=="FALSE") {
        parameters_null = list(birth_rates = c(as.numeric(1), as.numeric(1)),
                          death_rates = c(as.numeric(0.2), as.numeric(0.2)),
                          transition_matrix_A = A)

        sim_bissentips = castor::simulate_dsse(Nstates       = 2,
                                               parameters    = parameters_null,
                                               max_tips      = 3, # prior to sampling
                                               sampling_fractions = 1,
                                               no_full_extinction = FALSE,
                                               coalescent = TRUE)
      }
    }
  }

  # systematically, the most abundant tip type should be t1:
  if (sum(sim_bissentips$tip_states==1) < sum(sim_bissentips$tip_states==2)) {
    to_convert = sim_bissentips$tip_states==2
    # switch the states:
    sim_bissentips$tip_states[to_convert] = 1
    sim_bissentips$tip_states[!to_convert] = 2
    # switch root state
    if (sim_bissentips$start_state==1) {
      sim_bissentips$start_state = 2
    } else {
      sim_bissentips$start_state = 1
    }
    # switch corresponding rates
    a <- params[i,2]
    params[i,2] <- params[i,3]
    params[i,3] <- a

    a <- params[i,7]
    params[i,7] <- params[i,8]
    params[i,8] <- a

    a <- params[i,9]
    params[i,9] <- params[i,10]
    params[i,10] <- a

  }

  tree_bissentips <- sim_bissentips$tree
  tree_bissentips$tip.label=paste0(tree_bissentips$tip.label,"[&&NHX:t_s=",sim_bissentips$tip_states,"]")
  tot_time <- max(TreeSim::getx(tree_bissentips))
  Ntot <- length(tree_bissentips$tip.label)

  # get stats
  stats = c(format(params[i,1], scientific=FALSE), sum(sim_bissentips$tip_states==1), sum(sim_bissentips$tip_states==2), Ntot, sim_bissentips$start_state, as.numeric(tot_time), j)
  if (i==1) {
    stats_tab = matrix(stats, nrow=1, ncol=7)
  }
  else {
    stats = matrix(stats, nrow=1, ncol=7)
    stats_tab = rbind(stats_tab, stats)
  }

  # write tree
  forest <- c(forest, c(tree_bissentips))
}
  # t_s = 1 is the most abundant one:



class(forest) = "multiPhylo"

# writing output files
write.table(params, file=outparamfile, sep='\t', col.names=F, row.names=F, quote=FALSE)
write.table(stats_tab, file=outstatsfile, sep='\t', col.names=F, row.names=F, quote=FALSE)
write.tree(forest, file=outtreefile, append=FALSE, digits=10)

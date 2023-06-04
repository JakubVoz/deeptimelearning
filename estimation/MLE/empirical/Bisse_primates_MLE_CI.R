## BiSSE likelihood inference on the empirical phylogeny

library(hisse)
library(parallel)
set.seed(1234)

# Loading data
phyUltra <- ape::read.tree(file = "/../deeptimelearning/data/empirical/primates_data/primatesUltra.tre")
traits_data <- read.csv("/../deeptimelearning/data/empirical/primates_data/data_traits.csv")

# Preparing data
phyUltra_sub <- ape::drop.tip(phy = phyUltra, tip = phyUltra$tip.label[-which(phyUltra$tip.label %in% traits_data$Species)])
traits_data <- traits_data[traits_data$Species %in% phyUltra_sub$tip.label,]
traits_data <- as.matrix(traits_data[-1])
species <- ape::Ntip(phyUltra_sub)
sampling.f <- species/381

# Preparing model
trans.rates.bisse <- TransMatMaker.old(hidden.states=FALSE)
trans.rates.bisse.red <- trans.rates.bisse
trans.rates.bisse.red[!is.na(trans.rates.bisse.red)] = 1
bisse.fit <- NA

# Likelihood inference
try(hisse.fit <- hisse(phy = phyUltra_sub, data = traits_data, f=rep(sampling.f, 2),
                       hidden.states=FALSE, turnover=c(1,2), eps=c(1,1),
                       trans.rate=trans.rates.bisse.red))
hisse.fit$solution[1]/(hisse.fit$solution[3]+1) # lambda 1
hisse.fit$solution[2]/(hisse.fit$solution[4]+1) # lambda 2
hisse.fit$solution[3] # turnover rate
hisse.fit$solution[5] # transition rate

# CI
supportHisse_fit <- hisse::SupportRegionHiSSE(hisse.fit)
quantile(supportHisse_fit$points.within.region[,2], 0.025)/(quantile(supportHisse_fit$points.within.region[,4], 0.025)+1) # CI low lambda 1
quantile(supportHisse_fit$points.within.region[,2], 0.975)/(quantile(supportHisse_fit$points.within.region[,4], 0.975)+1) # CI high lambda 1
quantile(supportHisse_fit$points.within.region[,3], 0.025)/(quantile(supportHisse_fit$points.within.region[,5], 0.025)+1) # CI low lambda 2
quantile(supportHisse_fit$points.within.region[,3], 0.975)/(quantile(supportHisse_fit$points.within.region[,5], 0.975)+1) # CI high lambda 2
quantile(supportHisse_fit$points.within.region[,4], 0.025) # CI low turnover rate
quantile(supportHisse_fit$points.within.region[,4], 0.975) # CI high turnover rate
quantile(supportHisse_fit$points.within.region[,6], 0.025) # CI low transition rate
quantile(supportHisse_fit$points.within.region[,6], 0.975) # CI high transition rate

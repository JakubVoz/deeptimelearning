## Statistical Analysis SupTable1 and Tree size threshold values

## BD

TARGETcst200_500_001_treesize_na.rm <- read.csv("/../deeptimelearning/data/simulations/inference_results/BD/TARGETcst200_500_001_treesize_na.rm.csv")
TARGETcst200_500_001_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/TARGETcst200_500_001_na.rm.rds")
listDFinference_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/listDFinference_na.rm.rds")
AbsErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/AbsErrorCal.rds")
ErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/ErrorCal.rds")
AbsRelErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/AbsRelErrorCal.rds")

TechniquesNames <- c("CNN-CDV", "FFNN-SS", "MLE", "FFNN-CDV")

# Supplementaty Table 1
#### Calculating the statistics for the paper

meanErrorBD <- lapply(seq_along(ErrorCal), function(i){apply(ErrorCal[[i]], 2, mean)})
names(meanErrorBD) <- TechniquesNames
meanErrorBD

medianErrorBD <- lapply(seq_along(ErrorCal), function(i){apply(ErrorCal[[i]], 2, median)})
names(medianErrorBD) <- TechniquesNames
medianErrorBD

meanAbsErrorBD <- lapply(seq_along(AbsErrorCal), function(i){apply(AbsErrorCal[[i]], 2, mean)})
names(meanAbsErrorBD) <- TechniquesNames
meanAbsErrorBD

medianAbsErrorBD <- lapply(seq_along(AbsErrorCal), function(i){apply(AbsErrorCal[[i]], 2, median)})
names(medianAbsErrorBD) <- TechniquesNames
medianAbsErrorBD

meanAbsRelErrorBD <- lapply(seq_along(AbsRelErrorCal), function(i){apply(AbsRelErrorCal[[i]], 2, mean)})
names(meanAbsRelErrorBD) <- TechniquesNames
meanAbsRelErrorBD

medianAbsRelErrorBD <- lapply(seq_along(AbsRelErrorCal), function(i){apply(AbsRelErrorCal[[i]], 2, median)})
names(medianAbsRelErrorBD) <- TechniquesNames
medianAbsRelErrorBD

PearCorCal <- lapply(seq_along(listDFinference_na.rm), function(i){as.data.frame(matrix(data = unlist(mapply(cor.test,as.list(listDFinference_na.rm[[i]]),as.list(TARGETcst200_500_001_na.rm))[seq(4, 36, 9)]), ncol = 4), col.names = colnames(listDFinference_na.rm[[i]]))})
names(PearCorCal) <- TechniquesNames


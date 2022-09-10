## Statistical Analysis SupTable2 and Tree size threshold values

## BiSSE

listDFinferenceWithCNNless_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/listDFinferenceWithCNNless_na.rmCNNless.rds")
TARGETBiSSE200_500_0_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/TARGETBiSSE200_500_0_na.rm.rds")
TARGETBiSSE200_500_0_treesize_na.rm <- read.csv2("/../deeptimelearning/data/simulations/inference_results/BiSSE/TARGETBiSSE200_500_0_treesize_na.rm.csv")
AbsErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/AbsErrorCalCNNless.rds")
ErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/ErrorCalCNNless.rds")
AbsRelErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/AbsRelErrorCalCNNless.rds")

TechniquesNames <- c("CNN-CDV", "MLE castor", "MLE diversitree", "FFNN-CDV", "CNN-CDV-less")

# Supplementaty Table 2
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

PearCorCal <- lapply(seq_along(listDFinferenceWithCNNless_na.rm), function(i){as.data.frame(matrix(data = unlist(mapply(cor.test,as.list(listDFinferenceWithCNNless_na.rm[[i]]),as.list(TARGETBiSSE200_500_0_na.rm))[seq(4, 45, 9)]), ncol = 5), col.names = colnames(listDFinferenceWithCNNless_na.rm[[i]]))})
names(PearCorCal) <- TechniquesNames
PearCorCal


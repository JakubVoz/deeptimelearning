## A priori check PCA Analysis link to figure 4

# install.packages("picante")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("ape")

## BiSSE

# Loading and calculating the state specific summary statistics for the empirical data of Gomez 2012

real_encoded_norm_factor <- read.csv("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/real_encoded_norm_factor.csv", header = FALSE)
real_encoded_sumstats <- read.csv("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/real_encoded_sumstats.csv", header = TRUE)
phyUltra <- ape::read.tree(file = "/../deeptimelearning/data/empirical/primates_data/primatesUltra.tre")
traits_data <- read.csv("/../deeptimelearning/data/empirical/primates_data/data_traits.csv")
traits_data <- traits_data[-1]

traits_data_phylo <- traits_data[match(phyUltra$tip.label, traits_data$Species),]

primates_sample <- matrix(0, ncol = length(traits_data$Species), nrow = 2)
primates_sample <- as.data.frame(primates_sample)
colnames(primates_sample) <- traits_data$Species
primates_sample[1,traits_data$states==0] <- 1
primates_sample[2,traits_data$states==1] <- 1

pd_primates <- picante::pd(primates_sample, phyUltra)

real_encoded_sumstats$pd_1 <- (pd_primates$PD/real_encoded_norm_factor$V2)[1]
real_encoded_sumstats$pd_2 <- (pd_primates$PD/real_encoded_norm_factor$V2)[2]

# write.csv2(real_encoded_sumstats[2:102], "/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/real_encoded_sumstats_withPD.csv")

# Loading and calculating the state specific summary statistics for the testing set

test_set_encoded_norm_factor <- read.csv("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/test_set_encoded_norm_factor.csv", header = FALSE)
test_set_encoded_sumstats <- read.csv("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/test_set_encoded_sumstats.csv")
test_set_encoded_sumstats <- test_set_encoded_sumstats[,-1]

test_set_encoded_sumstats$pd_1 <- NA
test_set_encoded_sumstats$pd_2 <- NA

# The simulated trees are not provided in the GitHub repo due to their memory use but the results of the script that follows is test_set_encoded_sumstats_withPD.csv
# for (i in 1:10000) {
#   testPerTrees_set_BiSSE <- treeio::read.nhx(file = paste0("/../deeptimelearning/data/empirical/sanity_check_pca/test_set/PerTrees/Tree",i,".nwk"))
#   matrix_sample <- matrix(0, ncol = length(testPerTrees_set_BiSSE@phylo$tip.label), nrow = 2)
#   matrix_sample <- as.data.frame(matrix_sample)
#   colnames(matrix_sample) <- testPerTrees_set_BiSSE@phylo$tip.label
#   matrix_sample[1,testPerTrees_set_BiSSE@data$`&&NHX-t_s`==1] <- 1
#   matrix_sample[2,testPerTrees_set_BiSSE@data$`&&NHX-t_s`==2] <- 1
#   pd_test <- picante::pd(matrix_sample, testPerTrees_set_BiSSE@phylo)
#   test_set_encoded_sumstats$pd_1[i] <- (pd_test$PD/test_set_encoded_norm_factor$V2[i])[1]
#   test_set_encoded_sumstats$pd_2[i] <- (pd_test$PD/test_set_encoded_norm_factor$V2[i])[2]
# }

# write.csv2(test_set_encoded_sumstats, "/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/test_set_encoded_sumstats_withPD.csv")

# PCA (variable already z transformed during PCA)

test_set_encoded_sumstats <- read.csv2("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/test_set_encoded_sumstats_withPD.csv")
real_encoded_sumstats  <- read.csv2("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/real_encoded_sumstats_withPD.csv")
test_set_encoded_sumstats <- test_set_encoded_sumstats[,-1]
real_encoded_sumstats <- real_encoded_sumstats[,-1]
test_real_encoded_sumstats <- rbind(test_set_encoded_sumstats, real_encoded_sumstats)

## PCA
pca_test_real <- FactoMineR::PCA(test_real_encoded_sumstats)
## Individuals coordinates
ind <- factoextra::get_pca_ind(pca_test_real)

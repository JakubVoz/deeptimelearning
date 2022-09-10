## Plotting paper figure 5

# install.packages("FactoMineR")
# install.packages("factoextra")

## BiSSE

# PCA (variable already z transformed during PCA)

test_set_encoded_sumstats <- read.csv2("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/test_set_encoded_sumstats_withPD.csv")
real_encoded_sumstats  <- read.csv2("/../deeptimelearning/data/empirical/sanity_check_pca/sum_stats/real_encoded_sumstats_withPD.csv")
test_set_encoded_sumstats <- test_set_encoded_sumstats[,-1]
real_encoded_sumstats <- real_encoded_sumstats[,-1]
test_real_encoded_sumstats <- rbind(test_set_encoded_sumstats, real_encoded_sumstats)

#
pca_test_real <- FactoMineR::PCA(test_real_encoded_sumstats)
## Individuals coordinates
ind <- factoextra::get_pca_ind(pca_test_real)

## Plotting figure 5
factoextra::fviz_pca_ind(pca_test_real, col.ind = c(rep("test set", 10000), "empirical"), label="none")
factoextra::fviz_pca_ind(pca_test_real, col.ind = c(rep("test set", 10000), "empirical"), label="none", axes = c(3,4))

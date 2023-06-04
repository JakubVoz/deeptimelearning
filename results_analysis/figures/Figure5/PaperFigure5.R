### Correlation of error

# BD

ErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/ErrorCal.rds")

colorsParam2 <- c("purple2", "darkorange2", "chartreuse3", "royalblue2")
birth <- as.data.frame(matrix(data = NA, ncol = length(ErrorCal), nrow = length(ErrorCal[[1]][,1])))
colnames(birth) <- names(ErrorCal)
death <- birth
netdiv <- birth
turn <- birth
for(i in 1:length(ErrorCal)){
  birth[,i] <- ErrorCal[[i]][,5]
  death[,i] <- ErrorCal[[i]][,6]
  netdiv[,i] <- ErrorCal[[i]][,7]
  turn[,i] <- ErrorCal[[i]][,8]
}

# Lambda
cor_mat_birth <- cor(birth, method = "pearson")
corrplot::corrplot(cor_mat_birth, addCoef.col = 'white',
                   type="lower", tl.srt = 45,
                   col=RColorBrewer::brewer.pal(n=8, name="BrBG"),
                   tl.col = colorsParam2, col.lim = c(0, 1),
                   title = expression(lambda),
                   cex.main = 2,
                   mar=c(1,0,1,0))
title(xlab = "Pearson correlation of estimates error", line = 2)

# Mu
cor_mat_death <- cor(death, method = "pearson")
corrplot::corrplot(cor_mat_death, addCoef.col = 'white',
                   type="lower", tl.srt = 45,
                   col=RColorBrewer::brewer.pal(n=8, name="BrBG"),
                   tl.col = colorsParam2, col.lim = c(0, 1),
                   title = expression(mu),
                   cex.main = 2,
                   mar=c(1,0,1,0))
title(xlab = "Pearson correlation of estimates error", line = 2)
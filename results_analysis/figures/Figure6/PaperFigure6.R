### Correlation of error

# BiSSE

ErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/ErrorCal.rds")

colorsParam2 <- c("purple2","chartreuse3", "olivedrab2", "royalblue2")
birth1 <- as.data.frame(matrix(data = NA, ncol = length(ErrorCal), nrow = length(ErrorCal[[1]][,1])))
colnames(birth1) <- names(ErrorCal)
birth2 <- birth1
death1 <- birth1
death2 <- birth1
q01 <- birth1
for(i in 1:length(ErrorCal)){
  birth1[,i] <- ErrorCal[[i]][,6]
  birth2[,i] <- ErrorCal[[i]][,7]
  death1[,i] <- ErrorCal[[i]][,8]
  death2[,i] <- ErrorCal[[i]][,9]
  q01[,i] <- ErrorCal[[i]][,10]
}

# Speciation rate state 1

cor_mat_birth1 <- cor(birth1, method = "pearson")
corrplot::corrplot(cor_mat_birth1, addCoef.col = 'black',
                   type="lower", tl.srt = 45,
                   col=RColorBrewer::brewer.pal(n=8, name="BrBG"),
                   tl.col = colorsParam2, col.lim = c(0, 1),
                   title = expression(lambda*1),
                   cex.main = 2,
                   mar=c(1,0,1.5,0))
title(xlab = "Pearson correlation of estimates error", line = 2)

# Speciation rate state 2

cor_mat_birth2 <- cor(birth2, method = "pearson")
corrplot::corrplot(cor_mat_birth2, addCoef.col = 'black',
                   type="lower", tl.srt = 45,
                   col=RColorBrewer::brewer.pal(n=8, name="BrBG"),
                   tl.col = colorsParam2, col.lim = c(0, 1),
                   title = expression(lambda*2),
                   cex.main = 2,
                   mar=c(1,0,1.5,0))
title(xlab = "Pearson correlation of estimates error", line = 2)

# Extinction rate state 1

cor_mat_death1 <- cor(death1, method = "pearson")
corrplot::corrplot(cor_mat_death1, addCoef.col = 'black',
                   type="lower", tl.srt = 45,
                   col=RColorBrewer::brewer.pal(n=8, name="BrBG"),
                   tl.col = colorsParam2, col.lim = c(0, 1),
                   title = expression(mu*1),
                   cex.main = 2,
                   mar=c(1,0,1.5,0))
title(xlab = "Pearson correlation of estimates error", line = 2)

# Extinction rate state 2

cor_mat_death2 <- cor(death2, method = "pearson")
corrplot::corrplot(cor_mat_death2, addCoef.col = 'black',
                   type="lower", tl.srt = 45,
                   col=RColorBrewer::brewer.pal(n=8, name="BrBG"),
                   tl.col = colorsParam2, col.lim = c(0, 1),
                   title = expression(mu*2),
                   cex.main = 2,
                   mar=c(1,0,1.5,0))
title(xlab = "Pearson correlation of estimates error", line = 2)

# Transition rate 12

cor_mat_q01 <- cor(q01, method = "pearson")
corrplot::corrplot(cor_mat_q01, addCoef.col = 'black',
                   type="lower", tl.srt = 45,
                   col=RColorBrewer::brewer.pal(n=8, name="BrBG"),
                   tl.col = colorsParam2, col.lim = c(0, 1),
                   title = expression("q12 = q21"),
                   cex.main = 2,
                   mar=c(1,0,1.5,0))
title(xlab = "Pearson correlation of estimates error", line = 2)


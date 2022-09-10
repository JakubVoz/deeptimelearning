## Plotting paper supplementary figure 2

# install.packages("dplyr")

## BiSSE

TARGETBiSSE200_500_0_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/TARGETBiSSE200_500_0_na.rm.rds")
listDFinference_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/listDFinference_na.rm.rds")

## Preparing plotting

RatesOfInterest <- c("lambda1", "lambda2", "mu1", "mu2", "q01")
nbEstimRates <- length(RatesOfInterest)

colorsParam1 <- c("purple1", "chartreuse1", "olivedrab1", "royalblue1")
colorsParam2 <- c("purple2","chartreuse3", "olivedrab2", "royalblue2")
colorsParam3 <- c("purple3" ,"chartreuse3", "olivedrab3", "royalblue3")
coloradjust <- sapply(colorsParam1, function(x){adjustcolor(x,alpha.f = 0.05)})

TechniquesNames <- c("CNN-CDV", "MLE castor", "MLE diversitree", "FFNN-CDV")

condInterest <- c(1:4)

## REGRESSION

# Regression plot 

lm0DF <- function(DFinference, DFtarget, PI = FALSE, length_predict = 500){
  EstimRates <- colnames(DFinference)
  TargRates <- colnames(DFtarget)
  if(!identical(EstimRates, TargRates)) stop("Not the same parameters ordered in the target and inference data frames")
  lm_list <- lapply(seq_len(ncol(DFinference)), function(i) lm(DFinference[, i] ~  0 +DFtarget[, i]))
  names(lm_list) <- paste0(colnames(DFinference), "lm0")
  df_sim <- NULL
  BFdf_predict <- NULL
  MinMax <- NULL
  if(PI==TRUE){
    min <- lapply(seq_len(ncol(DFinference)), function(i) min(DFinference[, i],DFtarget[, i]))
    max <- lapply(seq_len(ncol(DFinference)), function(i) max(DFinference[, i],DFtarget[, i]))
    BFdf_sim <- lapply(seq_len(ncol(DFinference)), function(i) seq(min[[i]],max[[i]], length.out = length_predict))
    df_sim <- as.data.frame(do.call(cbind, BFdf_sim))
    BFdf_predict <- lapply(seq_len(ncol(DFinference)),
                           function(i) predict(object = lm_list[[i]],
                                               newdata = as.data.frame(df_sim[,i]),
                                               interval = "predict", level = 0.95))
    colnames(df_sim) <- colnames(DFinference)
    names(BFdf_predict) <- colnames(DFinference)
    MinMax <- t(data.frame(min = unlist(min), max = unlist(max)))
    colnames(MinMax) <- colnames(DFinference)
  }
  return(list(lm = lm_list, PIsim = df_sim, PIpredict = BFdf_predict, MinMax = MinMax))
}

#

lm0Cal <- lapply(listDFinference_na.rm, function(x) lm0DF(x, TARGETBiSSE200_500_0_na.rm, PI = TRUE, length_predict = nrow(listDFinference_na.rm[[1]])))
names(lm0Cal) <- TechniquesNames

listMinMaxAll <- lapply(seq_along(lm0Cal), function(x) lm0Cal[[x]]$MinMax)
MinMaxAll <- as.data.frame(do.call(rbind, listMinMaxAll))
library("dplyr")
MaxEsim <- MinMaxAll %>%
  group_by_if(.,is.factor) %>%
  summarise_if(is.numeric, max)
MinEsim <- MinMaxAll %>%
  group_by_if(.,is.factor) %>%
  summarise_if(is.numeric, min)
MinMaxAll <- as.data.frame(rbind(MinEsim, MaxEsim))
row.names(MinMaxAll) <- row.names(lm0Cal$DLcst200_500SumStat01_MRE$MinMax)


loessDF <- function(DFinference, DFtarget){
  EstimRates <- colnames(DFinference)
  TargRates <- colnames(DFtarget)
  if(!identical(EstimRates, TargRates)) stop("Not the same parameters ordered in the target and inference data frames")
  loess_list <- lapply(seq_len(ncol(DFinference)), function(i) msir::loess.sd(DFinference[, i] ~ DFtarget[, i], nsigma = 1.96))
  names(loess_list) <- paste0(colnames(DFinference), "loess")
  return(loess_list)
}
loessCal <- lapply(listDFinference_na.rm, function(x) loessDF(x, TARGETBiSSE200_500_0_na.rm))
names(loessCal) <- TechniquesNames


# lAMBDA1

plot(listDFinference_na.rm[condInterest][[1]]$lambda1~TARGETBiSSE200_500_0_na.rm$lambda1, pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = expression(paste("Target ", lambda, "1")), bty='n',
     ylim = c(0,1), xlim = c(0,1), cex = 0.1, lwd = 0.5, cex.axis = 1.5, cex.lab = 2)

title(ylab = expression(paste("Predicted ", lambda, "1")), mgp = c(2.5, 1, 0), cex.lab = 2)

polygon(c(rev(loessCal[[condInterest[1]]]$lambda1loess$x), loessCal[[condInterest[1]]]$lambda1loess$x),
        c(rev(loessCal[[condInterest[1]]]$lambda1loess$upper), loessCal[[condInterest[1]]]$lambda1loess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$lambda1loess$x, loessCal[[condInterest[1]]]$lambda1loess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$lambda1loess$x, loessCal[[condInterest[1]]]$lambda1loess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$lambda1~TARGETBiSSE200_500_0_na.rm$lambda1, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][2])
polygon(c(rev(loessCal[[condInterest[2]]]$lambda1loess$x), loessCal[[condInterest[2]]]$lambda1loess$x),
        c(rev(loessCal[[condInterest[2]]]$lambda1loess$upper), loessCal[[condInterest[2]]]$lambda1loess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$lambda1loess$x, loessCal[[condInterest[2]]]$lambda1loess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$lambda1loess$x, loessCal[[condInterest[2]]]$lambda1loess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$lambda1~TARGETBiSSE200_500_0_na.rm$lambda1, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][3])
polygon(c(rev(loessCal[[condInterest[3]]]$lambda1loess$x), loessCal[[condInterest[3]]]$lambda1loess$x),
        c(rev(loessCal[[condInterest[3]]]$lambda1loess$upper), loessCal[[condInterest[3]]]$lambda1loess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$lambda1loess$x, loessCal[[condInterest[3]]]$lambda1loess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$lambda1loess$x, loessCal[[condInterest[3]]]$lambda1loess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$lambda1~TARGETBiSSE200_500_0_na.rm$lambda1, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][4])
polygon(c(rev(loessCal[[condInterest[4]]]$lambda1loess$x), loessCal[[condInterest[4]]]$lambda1loess$x),
        c(rev(loessCal[[condInterest[4]]]$lambda1loess$upper), loessCal[[condInterest[4]]]$lambda1loess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$lambda1loess$x, loessCal[[condInterest[4]]]$lambda1loess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$lambda1loess$x, loessCal[[condInterest[4]]]$lambda1loess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

lines(loessCal[[condInterest[2]]]$lambda1loess$x, loessCal[[condInterest[2]]]$lambda1loess$y, col = colorsParam3[condInterest][2], lwd= 2)
lines(loessCal[[condInterest[4]]]$lambda1loess$x, loessCal[[condInterest[4]]]$lambda1loess$y, col = colorsParam3[condInterest][4], lwd= 2)
lines(loessCal[[condInterest[1]]]$lambda1loess$x, loessCal[[condInterest[1]]]$lambda1loess$y, col = colorsParam3[condInterest][1], lwd= 2)
lines(loessCal[[condInterest[3]]]$lambda1loess$x, loessCal[[condInterest[3]]]$lambda1loess$y, col = colorsParam3[condInterest][3], lwd= 2)

abline(a = 0, b = 1, lty = 2, lwd= 1)

legend(-0.05, 1.01, legend=TechniquesNames[condInterest],
       col=colorsParam2[condInterest], lwd=2, cex=1.7, box.lty=0,
       bg="transparent",  y.intersp = 0.8)
mtext (paste0("n=",nrow(listDFinference_na.rm[[1]])), at = c(0), lwd=2, cex=1.7)

# LAMBDA2

plot(listDFinference_na.rm[condInterest][[1]]$lambda2~TARGETBiSSE200_500_0_na.rm$lambda2, pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = expression(paste("Target ", lambda, "2")), bty='n',
     ylim = c(0, 1), xlim = c(0, 1),
     cex = 0.1, lwd = 0.5, cex.axis = 1.5, cex.lab = 2)

title(ylab = expression(paste("Predicted ", lambda, "2")), mgp = c(2.5, 1, 0), cex.lab = 2)

polygon(c(rev(loessCal[[condInterest[1]]]$lambda2loess$x), loessCal[[condInterest[1]]]$lambda2loess$x),
        c(rev(loessCal[[condInterest[1]]]$lambda2loess$upper), loessCal[[condInterest[1]]]$lambda2loess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$lambda2loess$x, loessCal[[condInterest[1]]]$lambda2loess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$lambda2loess$x, loessCal[[condInterest[1]]]$lambda2loess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$lambda2~TARGETBiSSE200_500_0_na.rm$lambda2, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][2])
polygon(c(rev(loessCal[[condInterest[2]]]$lambda2loess$x), loessCal[[condInterest[2]]]$lambda2loess$x),
        c(rev(loessCal[[condInterest[2]]]$lambda2loess$upper), loessCal[[condInterest[2]]]$lambda2loess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$lambda2loess$x, loessCal[[condInterest[2]]]$lambda2loess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$lambda2loess$x, loessCal[[condInterest[2]]]$lambda2loess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$lambda2~TARGETBiSSE200_500_0_na.rm$lambda2, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][3])
polygon(c(rev(loessCal[[condInterest[3]]]$lambda2loess$x), loessCal[[condInterest[3]]]$lambda2loess$x),
        c(rev(loessCal[[condInterest[3]]]$lambda2loess$upper), loessCal[[condInterest[3]]]$lambda2loess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$lambda2loess$x, loessCal[[condInterest[3]]]$lambda2loess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$lambda2loess$x, loessCal[[condInterest[3]]]$lambda2loess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$lambda2~TARGETBiSSE200_500_0_na.rm$lambda2, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][4])
polygon(c(rev(loessCal[[condInterest[4]]]$lambda2loess$x), loessCal[[condInterest[4]]]$lambda2loess$x),
        c(rev(loessCal[[condInterest[4]]]$lambda2loess$upper), loessCal[[condInterest[4]]]$lambda2loess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$lambda2loess$x, loessCal[[condInterest[4]]]$lambda2loess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$lambda2loess$x, loessCal[[condInterest[4]]]$lambda2loess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

lines(loessCal[[condInterest[2]]]$lambda2loess$x, loessCal[[condInterest[2]]]$lambda2loess$y, col = colorsParam3[condInterest][2], lwd= 2)
lines(loessCal[[condInterest[4]]]$lambda2loess$x, loessCal[[condInterest[4]]]$lambda2loess$y, col = colorsParam3[condInterest][4], lwd= 2)
lines(loessCal[[condInterest[2]]]$lambda2loess$x, loessCal[[condInterest[1]]]$lambda2loess$y, col = colorsParam3[condInterest][1], lwd= 2)
lines(loessCal[[condInterest[3]]]$lambda2loess$x, loessCal[[condInterest[3]]]$lambda2loess$y, col = colorsParam3[condInterest][3], lwd= 2)

segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,
         lty = 2, lwd= 1)

# MU1

plot((listDFinference_na.rm[condInterest][[1]]$mu1)~(TARGETBiSSE200_500_0_na.rm$mu1), pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = "", bty='n',
     ylim = c(0, 1), xlim = c(0, 1),
     cex = 0.1, lwd = 0.5, cex.axis = 1.5)

title(xlab = expression(paste("Target ", mu, "1")), mgp = c(3, 1, 0), cex.lab = 2) 
title(ylab = expression(paste("Predicted ", mu, "1")), mgp = c(2.3, 1, 0), cex.lab = 2)

polygon(c(rev(loessCal[[condInterest[1]]]$mu1loess$x), loessCal[[condInterest[1]]]$mu1loess$x),
        c(rev(loessCal[[condInterest[1]]]$mu1loess$upper), loessCal[[condInterest[1]]]$mu1loess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$mu1loess$x, loessCal[[condInterest[1]]]$mu1loess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$mu1loess$x, loessCal[[condInterest[1]]]$mu1loess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$mu1~TARGETBiSSE200_500_0_na.rm$mu1, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][2])
polygon(c(rev(loessCal[[condInterest[2]]]$mu1loess$x), loessCal[[condInterest[2]]]$mu1loess$x),
        c(rev(loessCal[[condInterest[2]]]$mu1loess$upper), loessCal[[condInterest[2]]]$mu1loess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$mu1loess$x, loessCal[[condInterest[2]]]$mu1loess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$mu1loess$x, loessCal[[condInterest[2]]]$mu1loess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$mu1~TARGETBiSSE200_500_0_na.rm$mu1, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][3])
polygon(c(rev(loessCal[[condInterest[3]]]$mu1loess$x), loessCal[[condInterest[3]]]$mu1loess$x),
        c(rev(loessCal[[condInterest[3]]]$mu1loess$upper), loessCal[[condInterest[3]]]$mu1loess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$mu1loess$x, loessCal[[condInterest[3]]]$mu1loess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$mu1loess$x, loessCal[[condInterest[3]]]$mu1loess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$mu1~TARGETBiSSE200_500_0_na.rm$mu1, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][4])
polygon(c(rev(loessCal[[condInterest[4]]]$mu1loess$x), loessCal[[condInterest[4]]]$mu1loess$x),
        c(rev(loessCal[[condInterest[4]]]$mu1loess$upper), loessCal[[condInterest[4]]]$mu1loess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$mu1loess$x, loessCal[[condInterest[4]]]$mu1loess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$mu1loess$x, loessCal[[condInterest[4]]]$mu1loess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

lines(loessCal[[condInterest[2]]]$mu1loess$x, loessCal[[condInterest[2]]]$mu1loess$y, col = colorsParam3[condInterest][2], lwd= 2)
lines(loessCal[[condInterest[4]]]$mu1loess$x, loessCal[[condInterest[4]]]$mu1loess$y, col = colorsParam3[condInterest][4], lwd= 2)
lines(loessCal[[condInterest[1]]]$mu1loess$x, loessCal[[condInterest[1]]]$mu1loess$y, col = colorsParam3[condInterest][1], lwd= 2)
lines(loessCal[[condInterest[3]]]$mu1loess$x, loessCal[[condInterest[3]]]$mu1loess$y, col = colorsParam3[condInterest][3], lwd= 2)

segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,
         lty = 2, lwd= 1)

# MU2

plot((listDFinference_na.rm[condInterest][[1]]$mu2)~(TARGETBiSSE200_500_0_na.rm$mu2), pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = "", bty='n',
     ylim = c(0, 1), xlim = c(0, 1),
     cex = 0.1, lwd = 0.5, cex.axis = 1.5)

title(xlab = expression(paste("Target ", mu, "2")), mgp = c(3, 1, 0), cex.lab = 2) 
title(ylab = expression(paste("Predicted ", mu, "2")), mgp = c(2.3, 1, 0), cex.lab = 2)

polygon(c(rev(loessCal[[condInterest[1]]]$mu2loess$x), loessCal[[condInterest[1]]]$mu2loess$x),
        c(rev(loessCal[[condInterest[1]]]$mu2loess$upper), loessCal[[condInterest[1]]]$mu2loess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$mu2loess$x, loessCal[[condInterest[1]]]$mu2loess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$mu2loess$x, loessCal[[condInterest[1]]]$mu2loess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$mu2~TARGETBiSSE200_500_0_na.rm$mu2, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][2])
polygon(c(rev(loessCal[[condInterest[2]]]$mu2loess$x), loessCal[[condInterest[2]]]$mu2loess$x),
        c(rev(loessCal[[condInterest[2]]]$mu2loess$upper), loessCal[[condInterest[2]]]$mu2loess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$mu2loess$x, loessCal[[condInterest[2]]]$mu2loess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$mu2loess$x, loessCal[[condInterest[2]]]$mu2loess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$mu2~TARGETBiSSE200_500_0_na.rm$mu2, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][3])
polygon(c(rev(loessCal[[condInterest[3]]]$mu2loess$x), loessCal[[condInterest[3]]]$mu2loess$x),
        c(rev(loessCal[[condInterest[3]]]$mu2loess$upper), loessCal[[condInterest[3]]]$mu2loess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$mu2loess$x, loessCal[[condInterest[3]]]$mu2loess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$mu2loess$x, loessCal[[condInterest[3]]]$mu2loess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$mu2~TARGETBiSSE200_500_0_na.rm$mu2, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][4])
polygon(c(rev(loessCal[[condInterest[4]]]$mu2loess$x), loessCal[[condInterest[4]]]$mu2loess$x),
        c(rev(loessCal[[condInterest[4]]]$mu2loess$upper), loessCal[[condInterest[4]]]$mu2loess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$mu2loess$x, loessCal[[condInterest[4]]]$mu2loess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$mu2loess$x, loessCal[[condInterest[4]]]$mu2loess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

lines(loessCal[[condInterest[2]]]$mu2loess$x, loessCal[[condInterest[2]]]$mu2loess$y, col = colorsParam3[condInterest][2], lwd= 2)
lines(loessCal[[condInterest[4]]]$mu2loess$x, loessCal[[condInterest[4]]]$mu2loess$y, col = colorsParam3[condInterest][4], lwd= 2)
lines(loessCal[[condInterest[1]]]$mu2loess$x, loessCal[[condInterest[1]]]$mu2loess$y, col = colorsParam3[condInterest][1], lwd= 2)
lines(loessCal[[condInterest[3]]]$mu2loess$x, loessCal[[condInterest[3]]]$mu2loess$y, col = colorsParam3[condInterest][3], lwd= 2)

segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,
         lty = 2, lwd= 1)

# Q01

plot(listDFinference_na.rm[condInterest][[1]]$q01~TARGETBiSSE200_500_0_na.rm$q01, pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = "", bty='n',
     ylim = c(0, 0.1), xlim = c(0, 0.1),
     cex = 0.1, lwd = 0.5, cex.axis = 1.5)

title(xlab = expression("Predicted q12 = q21"), mgp = c(3, 1, 0), cex.lab = 2) 
title(ylab = expression("Target q12 = q21"), mgp = c(2.3, 1, 0), cex.lab = 2)

polygon(c(rev(loessCal[[condInterest[1]]]$q01loess$x), loessCal[[condInterest[1]]]$q01loess$x),
        c(rev(loessCal[[condInterest[1]]]$q01loess$upper), loessCal[[condInterest[1]]]$q01loess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$q01loess$x, loessCal[[condInterest[1]]]$q01loess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$q01loess$x, loessCal[[condInterest[1]]]$q01loess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$q01~TARGETBiSSE200_500_0_na.rm$q01, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][2])
polygon(c(rev(loessCal[[condInterest[2]]]$q01loess$x), loessCal[[condInterest[2]]]$q01loess$x),
        c(rev(loessCal[[condInterest[2]]]$q01loess$upper), loessCal[[condInterest[2]]]$q01loess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$q01loess$x, loessCal[[condInterest[2]]]$q01loess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$q01loess$x, loessCal[[condInterest[2]]]$q01loess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$q01~TARGETBiSSE200_500_0_na.rm$q01, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][3])
polygon(c(rev(loessCal[[condInterest[3]]]$q01loess$x), loessCal[[condInterest[3]]]$q01loess$x),
        c(rev(loessCal[[condInterest[3]]]$q01loess$upper), loessCal[[condInterest[3]]]$q01loess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$q01loess$x, loessCal[[condInterest[3]]]$q01loess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$q01loess$x, loessCal[[condInterest[3]]]$q01loess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$q01~TARGETBiSSE200_500_0_na.rm$q01, pch = 3, cex = 0.1, lwd = 0.5, col = colorsParam1[condInterest][4])
polygon(c(rev(loessCal[[condInterest[4]]]$q01loess$x), loessCal[[condInterest[4]]]$q01loess$x),
        c(rev(loessCal[[condInterest[4]]]$q01loess$upper), loessCal[[condInterest[4]]]$q01loess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$q01loess$x, loessCal[[condInterest[4]]]$q01loess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$q01loess$x, loessCal[[condInterest[4]]]$q01loess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

lines(loessCal[[condInterest[2]]]$q01loess$x, loessCal[[condInterest[2]]]$q01loess$y, col = colorsParam3[condInterest][2], lwd= 2)
lines(loessCal[[condInterest[4]]]$q01loess$x, loessCal[[condInterest[4]]]$q01loess$y, col = colorsParam3[condInterest][4], lwd= 2)
lines(loessCal[[condInterest[1]]]$q01loess$x, loessCal[[condInterest[1]]]$q01loess$y, col = colorsParam3[condInterest][1], lwd= 2)
lines(loessCal[[condInterest[3]]]$q01loess$x, loessCal[[condInterest[3]]]$q01loess$y, col = colorsParam3[condInterest][3], lwd= 2)

segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,
         lty = 2, lwd= 1)


## Plotting paper supplementary figure 1

# install.packages("dplyr")

## BD

TARGETcst200_500_001_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/TARGETcst200_500_001_na.rm.rds")
listDFinference_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/listDFinference_na.rm.rds")

## Preparing plotting

RatesOfInterest <- c("birth_rate", "extinction_rate", "diversification_rate", "turnover_rate")
nbEstimRates <- length(RatesOfInterest)

colorsParam1 <- c("purple1","darkorange1", "chartreuse1", "royalblue1")
colorsParam2 <- c("purple2", "darkorange2", "chartreuse3", "royalblue2")
colorsParam3 <- c("purple3", "darkorange3", "chartreuse3", "royalblue3")
coloradjust <- sapply(colorsParam1, function(x){adjustcolor(x,alpha.f = 0.05)})

TechniquesNames <- c("CNN-CDV", "FFNN-SS", "MLE", "FFNN-CDV")

condInterest <- c(1:4)

## REGRESSION

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


lm0Cal <- lapply(listDFinference_na.rm, function(x) lm0DF(x, TARGETcst200_500_001_na.rm, PI = TRUE, length_predict = nrow(listDFinference_na.rm[[1]])))
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
row.names(MinMaxAll) <- row.names(lm0Cal$DLcst200_500SumStat001_MRE$MinMax)


loessDF <- function(DFinference, DFtarget){
  EstimRates <- colnames(DFinference)
  TargRates <- colnames(DFtarget)
  if(!identical(EstimRates, TargRates)) stop("Not the same parameters ordered in the target and inference data frames")
  loess_list <- lapply(seq_len(ncol(DFinference)), function(i) msir::loess.sd(DFinference[, i] ~ DFtarget[, i], nsigma = 1.96))
  names(loess_list) <- paste0(colnames(DFinference), "loess")
  return(loess_list)
}
loessCal <- lapply(listDFinference_na.rm, function(x) loessDF(x, TARGETcst200_500_001_na.rm))
names(loessCal) <- TechniquesNames

# BIRTH

plot(listDFinference_na.rm[condInterest][[1]]$birth_rate~TARGETcst200_500_001_na.rm$birth_rate, pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = expression(paste("Target ", lambda)), bty='n',
     ylim = c(0, max(MinMaxAll$birth_rate[2], max(TARGETcst200_500_001_na.rm$birth_rate))), xlim = c(0,max(TARGETcst200_500_001_na.rm$birth_rate)),
     cex = 0.5, lwd = 0.5, cex.axis = 1.5, cex.lab = 2)

title(ylab = expression(paste("Predicted ", lambda)), mgp = c(2.5, 1, 0), cex.lab = 2) 

lines(loessCal[[condInterest[1]]]$birth_rateloess$x, loessCal[[condInterest[1]]]$birth_rateloess$y, col = colorsParam3[condInterest][1], lwd= 2)
polygon(c(rev(loessCal[[condInterest[1]]]$birth_rateloess$x), loessCal[[condInterest[1]]]$birth_rateloess$x),
        c(rev(loessCal[[condInterest[1]]]$birth_rateloess$upper), loessCal[[condInterest[1]]]$birth_rateloess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$birth_rateloess$x, loessCal[[condInterest[1]]]$birth_rateloess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$birth_rateloess$x, loessCal[[condInterest[1]]]$birth_rateloess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$birth_rate~TARGETcst200_500_001_na.rm$birth_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][2])
lines(loessCal[[condInterest[2]]]$birth_rateloess$x, loessCal[[condInterest[2]]]$birth_rateloess$y, col = colorsParam3[condInterest][2], lwd= 2)
polygon(c(rev(loessCal[[condInterest[2]]]$birth_rateloess$x), loessCal[[condInterest[2]]]$birth_rateloess$x),
        c(rev(loessCal[[condInterest[2]]]$birth_rateloess$upper), loessCal[[condInterest[2]]]$birth_rateloess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$birth_rateloess$x, loessCal[[condInterest[2]]]$birth_rateloess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$birth_rateloess$x, loessCal[[condInterest[2]]]$birth_rateloess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$birth_rate~TARGETcst200_500_001_na.rm$birth_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][3])
lines(loessCal[[condInterest[3]]]$birth_rateloess$x, loessCal[[condInterest[3]]]$birth_rateloess$y, col = colorsParam3[condInterest][3], lwd= 2)
polygon(c(rev(loessCal[[condInterest[3]]]$birth_rateloess$x), loessCal[[condInterest[3]]]$birth_rateloess$x),
        c(rev(loessCal[[condInterest[3]]]$birth_rateloess$upper), loessCal[[condInterest[3]]]$birth_rateloess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$birth_rateloess$x, loessCal[[condInterest[3]]]$birth_rateloess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$birth_rateloess$x, loessCal[[condInterest[3]]]$birth_rateloess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$birth_rate~TARGETcst200_500_001_na.rm$birth_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][4])
lines(loessCal[[condInterest[4]]]$birth_rateloess$x, loessCal[[condInterest[4]]]$birth_rateloess$y, col = colorsParam3[condInterest][4], lwd= 2)
polygon(c(rev(loessCal[[condInterest[4]]]$birth_rateloess$x), loessCal[[condInterest[4]]]$birth_rateloess$x),
        c(rev(loessCal[[condInterest[4]]]$birth_rateloess$upper), loessCal[[condInterest[4]]]$birth_rateloess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$birth_rateloess$x, loessCal[[condInterest[4]]]$birth_rateloess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$birth_rateloess$x, loessCal[[condInterest[4]]]$birth_rateloess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

segments(x0 = 0, y0 = 0, x1 = 0.51, y1 = 0.51,
         lty = 2, lwd= 1)

legend((MinMaxAll$birth_rate[1]-MinMaxAll$birth_rate[1]*2), MinMaxAll$birth_rate[2]+MinMaxAll$birth_rate[2]*(1/80), legend=TechniquesNames[condInterest],
       col=colorsParam2[condInterest], lwd=2, cex=1.7, box.lty=0,
       bg="transparent", y.intersp = 0.8)
mtext (paste0("n=",nrow(listDFinference_na.rm[[1]])), at = c(0), lwd=2, cex=1.7)

# EXTINCTION

plot(listDFinference_na.rm[condInterest][[1]]$extinction_rate~TARGETcst200_500_001_na.rm$extinction_rate, pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = expression(paste("Target ", mu)), bty='n',
     ylim = c(0, max(MinMaxAll$extinction_rate[2], max(TARGETcst200_500_001_na.rm$extinction_rate))), xlim = c(0, max(TARGETcst200_500_001_na.rm$extinction_rate)),
     cex = 0.5, lwd = 0.5, cex.axis = 1.5, cex.lab = 2)

title(ylab = expression(paste("Predicted ", mu)), mgp = c(2.3, 1, 0), cex.lab = 2) 

lines(loessCal[[condInterest[1]]]$extinction_rateloess$x, loessCal[[condInterest[1]]]$extinction_rateloess$y, col = colorsParam3[condInterest][1], lwd= 2)
polygon(c(rev(loessCal[[condInterest[1]]]$extinction_rateloess$x), loessCal[[condInterest[1]]]$extinction_rateloess$x),
        c(rev(loessCal[[condInterest[1]]]$extinction_rateloess$upper), loessCal[[condInterest[1]]]$extinction_rateloess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$extinction_rateloess$x, loessCal[[condInterest[1]]]$extinction_rateloess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$extinction_rateloess$x, loessCal[[condInterest[1]]]$extinction_rateloess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$extinction_rate~TARGETcst200_500_001_na.rm$extinction_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][2])
lines(loessCal[[condInterest[2]]]$extinction_rateloess$x, loessCal[[condInterest[2]]]$extinction_rateloess$y, col = colorsParam3[condInterest][2], lwd= 2)
polygon(c(rev(loessCal[[condInterest[2]]]$extinction_rateloess$x), loessCal[[condInterest[2]]]$extinction_rateloess$x),
        c(rev(loessCal[[condInterest[2]]]$extinction_rateloess$upper), loessCal[[condInterest[2]]]$extinction_rateloess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$extinction_rateloess$x, loessCal[[condInterest[2]]]$extinction_rateloess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$extinction_rateloess$x, loessCal[[condInterest[2]]]$extinction_rateloess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$extinction_rate~TARGETcst200_500_001_na.rm$extinction_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][3])
lines(loessCal[[condInterest[3]]]$extinction_rateloess$x, loessCal[[condInterest[3]]]$extinction_rateloess$y, col = colorsParam3[condInterest][3], lwd= 2)
polygon(c(rev(loessCal[[condInterest[3]]]$extinction_rateloess$x), loessCal[[condInterest[3]]]$extinction_rateloess$x),
        c(rev(loessCal[[condInterest[3]]]$extinction_rateloess$upper), loessCal[[condInterest[3]]]$extinction_rateloess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$extinction_rateloess$x, loessCal[[condInterest[3]]]$extinction_rateloess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$extinction_rateloess$x, loessCal[[condInterest[3]]]$extinction_rateloess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$extinction_rate~TARGETcst200_500_001_na.rm$extinction_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][4])
lines(loessCal[[condInterest[4]]]$extinction_rateloess$x, loessCal[[condInterest[4]]]$extinction_rateloess$y, col = colorsParam3[condInterest][4], lwd= 2)
polygon(c(rev(loessCal[[condInterest[4]]]$extinction_rateloess$x), loessCal[[condInterest[4]]]$extinction_rateloess$x),
        c(rev(loessCal[[condInterest[4]]]$extinction_rateloess$upper), loessCal[[condInterest[4]]]$extinction_rateloess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$extinction_rateloess$x, loessCal[[condInterest[4]]]$extinction_rateloess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$extinction_rateloess$x, loessCal[[condInterest[4]]]$extinction_rateloess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

segments(x0 = 0, y0 = 0, x1 = 0.51, y1 = 0.51,
         lty = 2, lwd= 1)

# NET DIVERSIFICATION

plot(listDFinference_na.rm[condInterest][[1]]$diversification_rate~TARGETcst200_500_001_na.rm$diversification_rate, pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = expression(paste("Target ", r)), bty='n',
     ylim = c(0, max(MinMaxAll$diversification_rate[2], max(TARGETcst200_500_001_na.rm$diversification_rate))), xlim = c(0, max(TARGETcst200_500_001_na.rm$diversification_rate)),
     cex = 0.5, lwd = 0.5, cex.axis = 1.5, cex.lab = 2)

title(ylab = expression(paste("Predicted ", r)), mgp = c(2.5, 1, 0), cex.lab = 2) 

lines(loessCal[[condInterest[1]]]$diversification_rateloess$x, loessCal[[condInterest[1]]]$diversification_rateloess$y, col = colorsParam3[condInterest][1], lwd= 2)
polygon(c(rev(loessCal[[condInterest[1]]]$diversification_rateloess$x), loessCal[[condInterest[1]]]$diversification_rateloess$x),
        c(rev(loessCal[[condInterest[1]]]$diversification_rateloess$upper), loessCal[[condInterest[1]]]$diversification_rateloess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$diversification_rateloess$x, loessCal[[condInterest[1]]]$diversification_rateloess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$diversification_rateloess$x, loessCal[[condInterest[1]]]$diversification_rateloess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$diversification_rate~TARGETcst200_500_001_na.rm$diversification_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][2])
lines(loessCal[[condInterest[2]]]$diversification_rateloess$x, loessCal[[condInterest[2]]]$diversification_rateloess$y, col = colorsParam3[condInterest][2], lwd= 2)
polygon(c(rev(loessCal[[condInterest[2]]]$diversification_rateloess$x), loessCal[[condInterest[2]]]$diversification_rateloess$x),
        c(rev(loessCal[[condInterest[2]]]$diversification_rateloess$upper), loessCal[[condInterest[2]]]$diversification_rateloess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$diversification_rateloess$x, loessCal[[condInterest[2]]]$diversification_rateloess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$diversification_rateloess$x, loessCal[[condInterest[2]]]$diversification_rateloess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$diversification_rate~TARGETcst200_500_001_na.rm$diversification_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][3])
lines(loessCal[[condInterest[3]]]$diversification_rateloess$x, loessCal[[condInterest[3]]]$diversification_rateloess$y, col = colorsParam3[condInterest][3], lwd= 2)
polygon(c(rev(loessCal[[condInterest[3]]]$diversification_rateloess$x), loessCal[[condInterest[3]]]$diversification_rateloess$x),
        c(rev(loessCal[[condInterest[3]]]$diversification_rateloess$upper), loessCal[[condInterest[3]]]$diversification_rateloess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$diversification_rateloess$x, loessCal[[condInterest[3]]]$diversification_rateloess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$diversification_rateloess$x, loessCal[[condInterest[3]]]$diversification_rateloess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$diversification_rate~TARGETcst200_500_001_na.rm$diversification_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][4])
lines(loessCal[[condInterest[4]]]$diversification_rateloess$x, loessCal[[condInterest[4]]]$diversification_rateloess$y, col = colorsParam3[condInterest][4], lwd= 2)
polygon(c(rev(loessCal[[condInterest[4]]]$diversification_rateloess$x), loessCal[[condInterest[4]]]$diversification_rateloess$x),
        c(rev(loessCal[[condInterest[4]]]$diversification_rateloess$upper), loessCal[[condInterest[4]]]$diversification_rateloess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$diversification_rateloess$x, loessCal[[condInterest[4]]]$diversification_rateloess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$diversification_rateloess$x, loessCal[[condInterest[4]]]$diversification_rateloess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,
         lty = 2, lwd= 1)

# TURNOVER

plot(listDFinference_na.rm[condInterest][[1]]$turnover_rate~TARGETcst200_500_001_na.rm$turnover_rate, pch = 3, col = colorsParam1[condInterest][1],
     ylab = "",
     xlab = expression(paste("Target ", epsilon)), bty='n',
     ylim = c(0, max(MinMaxAll$turnover_rate[2], max(TARGETcst200_500_001_na.rm$turnover_rate))), xlim = c(0, max(TARGETcst200_500_001_na.rm$turnover_rate)),
     cex = 0.5, lwd = 0.5, cex.axis = 1.5, cex.lab = 2)

title(ylab = expression(paste("Predicted ", epsilon)), mgp = c(2.5, 1, 0), cex.lab = 2) 

lines(loessCal[[condInterest[1]]]$turnover_rateloess$x, loessCal[[condInterest[1]]]$turnover_rateloess$y, col = colorsParam3[condInterest][1], lwd= 2)
polygon(c(rev(loessCal[[condInterest[1]]]$turnover_rateloess$x), loessCal[[condInterest[1]]]$turnover_rateloess$x),
        c(rev(loessCal[[condInterest[1]]]$turnover_rateloess$upper), loessCal[[condInterest[1]]]$turnover_rateloess$lower),
        col = coloradjust[condInterest[1]], border = FALSE)
lines(loessCal[[condInterest[1]]]$turnover_rateloess$x, loessCal[[condInterest[1]]]$turnover_rateloess$lower, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[1]]]$turnover_rateloess$x, loessCal[[condInterest[1]]]$turnover_rateloess$upper, col = colorsParam1[condInterest][1],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[2]]$turnover_rate~TARGETcst200_500_001_na.rm$turnover_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][2])
lines(loessCal[[condInterest[2]]]$turnover_rateloess$x, loessCal[[condInterest[2]]]$turnover_rateloess$y, col = colorsParam3[condInterest][2], lwd= 2)
polygon(c(rev(loessCal[[condInterest[2]]]$turnover_rateloess$x), loessCal[[condInterest[2]]]$turnover_rateloess$x),
        c(rev(loessCal[[condInterest[2]]]$turnover_rateloess$upper), loessCal[[condInterest[2]]]$turnover_rateloess$lower),
        col = coloradjust[condInterest[2]], border = FALSE)
lines(loessCal[[condInterest[2]]]$turnover_rateloess$x, loessCal[[condInterest[2]]]$turnover_rateloess$lower, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[2]]]$turnover_rateloess$x, loessCal[[condInterest[2]]]$turnover_rateloess$upper, col = colorsParam1[condInterest][2],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[3]]$turnover_rate~TARGETcst200_500_001_na.rm$turnover_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][3])
lines(loessCal[[condInterest[3]]]$turnover_rateloess$x, loessCal[[condInterest[3]]]$turnover_rateloess$y, col = colorsParam3[condInterest][3], lwd= 2)
polygon(c(rev(loessCal[[condInterest[3]]]$turnover_rateloess$x), loessCal[[condInterest[3]]]$turnover_rateloess$x),
        c(rev(loessCal[[condInterest[3]]]$turnover_rateloess$upper), loessCal[[condInterest[3]]]$turnover_rateloess$lower),
        col = coloradjust[condInterest[3]], border = FALSE)
lines(loessCal[[condInterest[3]]]$turnover_rateloess$x, loessCal[[condInterest[3]]]$turnover_rateloess$lower, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[3]]]$turnover_rateloess$x, loessCal[[condInterest[3]]]$turnover_rateloess$upper, col = colorsParam1[condInterest][3],
      lty = 3, lwd = 1)

points(listDFinference_na.rm[condInterest][[4]]$turnover_rate~TARGETcst200_500_001_na.rm$turnover_rate, pch = 3, cex = 0.5, lwd = 0.5, col = colorsParam1[condInterest][4])
lines(loessCal[[condInterest[4]]]$turnover_rateloess$x, loessCal[[condInterest[4]]]$turnover_rateloess$y, col = colorsParam3[condInterest][4], lwd= 2)
polygon(c(rev(loessCal[[condInterest[4]]]$turnover_rateloess$x), loessCal[[condInterest[4]]]$turnover_rateloess$x),
        c(rev(loessCal[[condInterest[4]]]$turnover_rateloess$upper), loessCal[[condInterest[4]]]$turnover_rateloess$lower),
        col = coloradjust[condInterest[4]], border = FALSE)
lines(loessCal[[condInterest[4]]]$turnover_rateloess$x, loessCal[[condInterest[4]]]$turnover_rateloess$lower, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)
lines(loessCal[[condInterest[4]]]$turnover_rateloess$x, loessCal[[condInterest[4]]]$turnover_rateloess$upper, col = colorsParam1[condInterest][4],
      lty = 3, lwd = 1)

segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,
         lty = 2, lwd= 1)


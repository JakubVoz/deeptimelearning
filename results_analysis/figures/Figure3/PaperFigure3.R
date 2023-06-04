## Plotting paper figure 3

# install.packages("beeswarm")

## BiSSE

AbsErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/AbsErrorCal.rds")
ErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/ErrorCal.rds")

## Preparing plotting

RatesOfInterest <- c("lambda1", "lambda2", "mu1", "mu2", "q01")
nbEstimRates <- length(RatesOfInterest)

colorsParam1 <- c("purple1", "chartreuse1", "olivedrab1", "royalblue1")
colorsParam2 <- c("purple2","chartreuse3", "olivedrab2", "royalblue2")
colorsParam3 <- c("purple3" ,"chartreuse3", "olivedrab3", "royalblue3")
coloradjust <- sapply(colorsParam1, function(x){adjustcolor(x,alpha.f = 0.05)})

TechniquesNames <- c("CNN-CDV", "MLE castor", "MLE diversitree", "FFNN-CDV")

#####

condInterest <- c(1:4)

#####

focusAbsE <- list(AbsErrorCal[[condInterest[1]]]$lambda1AbsError, AbsErrorCal[[condInterest[2]]]$lambda1AbsError,
                  AbsErrorCal[[condInterest[3]]]$lambda1AbsError, AbsErrorCal[[condInterest[4]]]$lambda1AbsError,
                  AbsErrorCal[[condInterest[1]]]$lambda2AbsError, AbsErrorCal[[condInterest[2]]]$lambda2AbsError,
                  AbsErrorCal[[condInterest[3]]]$lambda2AbsError, AbsErrorCal[[condInterest[4]]]$lambda2AbsError,
                  AbsErrorCal[[condInterest[1]]]$mu1AbsError, AbsErrorCal[[condInterest[2]]]$mu1AbsError,
                  AbsErrorCal[[condInterest[3]]]$mu1AbsError, AbsErrorCal[[condInterest[4]]]$mu1AbsError,
                  AbsErrorCal[[condInterest[1]]]$mu2AbsError, AbsErrorCal[[condInterest[2]]]$mu2AbsError,
                  AbsErrorCal[[condInterest[3]]]$mu2AbsError, AbsErrorCal[[condInterest[4]]]$mu2AbsError,
                  AbsErrorCal[[condInterest[1]]]$q01AbsError, AbsErrorCal[[condInterest[2]]]$q01AbsError,
                  AbsErrorCal[[condInterest[3]]]$q01AbsError, AbsErrorCal[[condInterest[4]]]$q01AbsError)
meanAbsE <- unlist(lapply(focusAbsE, function(x) mean(x, na.rm = TRUE)))

focusE <- list(ErrorCal[[condInterest[1]]]$lambda1Error, ErrorCal[[condInterest[2]]]$lambda1Error,
               ErrorCal[[condInterest[3]]]$lambda1Error, ErrorCal[[condInterest[4]]]$lambda1Error,
               ErrorCal[[condInterest[1]]]$lambda2Error, ErrorCal[[condInterest[2]]]$lambda2Error,
               ErrorCal[[condInterest[3]]]$lambda2Error, ErrorCal[[condInterest[4]]]$lambda2Error,
               ErrorCal[[condInterest[1]]]$mu1Error, ErrorCal[[condInterest[2]]]$mu1Error,
               ErrorCal[[condInterest[3]]]$mu1Error, ErrorCal[[condInterest[4]]]$mu1Error,
               ErrorCal[[condInterest[1]]]$mu2Error, ErrorCal[[condInterest[2]]]$mu2Error,
               ErrorCal[[condInterest[3]]]$mu2Error, ErrorCal[[condInterest[4]]]$mu2Error,
               ErrorCal[[condInterest[1]]]$q01Error, ErrorCal[[condInterest[2]]]$q01Error,
               ErrorCal[[condInterest[3]]]$q01Error, ErrorCal[[condInterest[4]]]$q01Error)
meanE <- unlist(lapply(focusE, function(x) mean(x, na.rm = TRUE)))

## BIAS
# Swarm plot pannels
# 1

# putting the values outside the bounds at the boundaries
which(ErrorCal[[condInterest[1]]]$lambda1Error>1)
which(ErrorCal[[condInterest[3]]]$lambda1Error>1)
which(ErrorCal[[condInterest[2]]]$lambda1Error>1)
ErrorCal2Outliers <- ErrorCal[[condInterest[2]]]$lambda1Error
ErrorCal2Outliers[which(ErrorCal[[condInterest[2]]]$lambda1Error>1)] <- 1
which(ErrorCal[[condInterest[4]]]$lambda1Error>1)
ErrorCal4Outliers <- ErrorCal[[condInterest[4]]]$lambda1Error
ErrorCal4Outliers[which(ErrorCal[[condInterest[4]]]$lambda1Error>1)] <- 1
which(ErrorCal[[condInterest[1]]]$lambda1Error<(-0.9))
which(ErrorCal[[condInterest[3]]]$lambda1Error<(-0.9))
which(ErrorCal[[condInterest[4]]]$lambda1Error<(-0.9))
which(ErrorCal[[condInterest[2]]]$lambda1Error<(-0.9))

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$lambda1Error, ErrorCal2Outliers,
                        ErrorCal[[condInterest[3]]]$lambda1Error, ErrorCal4Outliers),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.050,
                   col = colorsParam2[condInterest], labels = c(""),
                   ylim = c(-0.9, 1), cex.axis = 1.5)

mtext("Error", side = 2, line = 2.4, cex = 2.5)
grps <- round(meanAbsE[1:4], digits = 2)
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 2,
       col = colorsParam2[condInterest])
mtext ("MAE", at = 0.3, side = 1, line = 0.5, font=2, cex = 2)
grps <- c(expression(lambda*1), expression(lambda*2),
          expression(mu*1), expression(mu*2), expression("q12 = q21"))
mtext (grps[1], at = seq(from = (length(TechniquesNames[condInterest])+1)/2,
                         to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2.5)
points(meanE[1:4], col = "black", pch = 16)
abline(h = 0, lty = 2, lwd= 0.5)
grps2 <- c("CNN-CDV", "MLE \ncastor", "MLE \ndiversitree", "FFNN-CDV")
mtext (grps2,
       at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4],
       side = 1, line = -29, font=2, cex = 1.4,
       col = colorsParam2[condInterest])

# 2

# putting the values outside the bounds at the boundaries
which(ErrorCal[[condInterest[1]]]$lambda2Error>1)
which(ErrorCal[[condInterest[4]]]$lambda2Error>1)
which(ErrorCal[[condInterest[2]]]$lambda2Error>1)
ErrorCal2Outliers <- ErrorCal[[condInterest[2]]]$lambda2Error
ErrorCal2Outliers[which(ErrorCal[[condInterest[2]]]$lambda2Error>1)] <- 1
which(ErrorCal[[condInterest[3]]]$lambda2Error>1)
ErrorCal3Outliers <- ErrorCal[[condInterest[3]]]$lambda2Error
ErrorCal3Outliers[which(ErrorCal[[condInterest[3]]]$lambda2Error>1)] <- 1
which(ErrorCal[[condInterest[1]]]$lambda2Error<(-0.9))
which(ErrorCal[[condInterest[2]]]$lambda2Error<(-0.9))
which(ErrorCal[[condInterest[3]]]$lambda2Error<(-0.9))
which(ErrorCal[[condInterest[4]]]$lambda2Error<(-0.9))

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$lambda2Error, ErrorCal2Outliers,
                        ErrorCal3Outliers, ErrorCal[[condInterest[4]]]$lambda2Error),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.049,
                   col = colorsParam2[condInterest], labels = c(""),
                   ylim = c(-0.9, 1), cex.axis = 1.5)

grps <- round(meanAbsE[5:8], digits = 2)
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 2,
       col = colorsParam2[condInterest])
mtext ("MAE", at = 0.3, side = 1, line = 0.5, font=2, cex = 2)
grps <- c(expression(lambda*1), expression(lambda*2),
          expression(mu*1), expression(mu*2), expression("q12 = q21"))
mtext (grps[2], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2.5)
points(meanE[5:8], col = "black", pch = 16)
abline(h = 0, lty = 2, lwd= 0.5)
# grps2 <- c("CNN-CDV", "MLE castor", "MLE diversitree", "FFNN-CDV")
# legend(2.64, 1.14, legend=grps2,
#        col=colorsParam2[condInterest], lwd=2, cex=1.5, box.lty=0,
#        bg="transparent", y.intersp = 0.8)
mtext (paste0("n=",nrow(ErrorCal[[1]])), at = c(4), lwd=2, cex=1.5)

# 3

# putting the values outside the bounds at the boundaries
which(ErrorCal[[condInterest[1]]]$mu1Error>1)
which(ErrorCal[[condInterest[3]]]$mu1Error>1)
which(ErrorCal[[condInterest[2]]]$mu1Error>1)
ErrorCal2Outliers <- ErrorCal[[condInterest[2]]]$mu1Error
ErrorCal2Outliers[which(ErrorCal[[condInterest[2]]]$mu1Error>1)] <- 1
which(ErrorCal[[condInterest[4]]]$mu1Error>1)
ErrorCal4Outliers <- ErrorCal[[condInterest[4]]]$mu1Error
ErrorCal4Outliers[which(ErrorCal[[condInterest[4]]]$mu1Error>1)] <- 1
which(ErrorCal[[condInterest[1]]]$mu1Error<(-0.9))
which(ErrorCal[[condInterest[2]]]$mu1Error<(-0.9))
which(ErrorCal[[condInterest[3]]]$mu1Error<(-0.9))
which(ErrorCal[[condInterest[4]]]$mu1Error<(-0.9))

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$mu1Error, ErrorCal2Outliers,
                        ErrorCal[[condInterest[3]]]$mu1Error, ErrorCal4Outliers),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.058,
                   col = colorsParam2[condInterest], labels = c(""),
                   ylim = c(-0.9, 1), cex.axis = 1.5)

mtext("Error", side = 2, line = 2.4, cex = 2.5)
grps <- round(meanAbsE[9:12], digits = 2)
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 2,
       col = colorsParam2[condInterest])
mtext ("MAE", at = 0.3, side = 1, line = 0.5, font=2, cex = 2)
grps <- c(expression(lambda*1), expression(lambda*2),
          expression(mu*1), expression(mu*2), expression("q12 = q21"))
mtext (grps[3], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2.5)
abline(h = 0, lty = 2, lwd= 0.5)
points(meanE[9:12], col = "black", pch = 16)

# 4

# putting the values outside the bounds at the boundaries
which(ErrorCal[[condInterest[1]]]$mu2Error>1)
which(ErrorCal[[condInterest[4]]]$mu2Error>1)
which(ErrorCal[[condInterest[2]]]$mu2Error>1)
ErrorCal2Outliers <- ErrorCal[[condInterest[2]]]$mu2Error
ErrorCal2Outliers[which(ErrorCal[[condInterest[2]]]$mu2Error>1)] <- 1
which(ErrorCal[[condInterest[3]]]$mu2Error>1)
ErrorCal3Outliers <- ErrorCal[[condInterest[3]]]$mu2Error
ErrorCal3Outliers[which(ErrorCal[[condInterest[3]]]$mu2Error>1)] <- 1
which(ErrorCal[[condInterest[1]]]$mu2Error<(-0.9))
which(ErrorCal[[condInterest[2]]]$mu2Error<(-0.9))
which(ErrorCal[[condInterest[3]]]$mu2Error<(-0.9))
which(ErrorCal[[condInterest[4]]]$mu2Error<(-0.9))

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$mu2Error, ErrorCal2Outliers,
                        ErrorCal3Outliers, ErrorCal[[condInterest[4]]]$mu2Error),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.045,
                   col = colorsParam2[condInterest], labels = c(""),
                   ylim = c(-0.9, 1), cex.axis = 1.5)

grps <- round(meanAbsE[13:16], digits = 2)
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 2,
       col = colorsParam2[condInterest])
mtext ("MAE", at = 0.3, side = 1, line = 0.5, font=2, cex = 2)
grps <- c(expression(lambda*1), expression(lambda*2),
          expression(mu*1), expression(mu*2), expression("q12 = q21"))
mtext (grps[4], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2.5)
abline(h = 0, lty = 2, lwd= 0.5)
points(meanE[13:16], col = "black", pch = 16)

# 5

# putting the values outside the bounds at the boundaries
which(ErrorCal[[condInterest[1]]]$q01Error>0.06)
which(ErrorCal[[condInterest[2]]]$q01Error>0.06)
ErrorCal2Outliers <- ErrorCal[[condInterest[2]]]$q01Error
ErrorCal2Outliers[which(ErrorCal[[condInterest[2]]]$q01Error>0.06)] <- 0.06
which(ErrorCal[[condInterest[3]]]$q01Error>0.06)
ErrorCal3Outliers <- ErrorCal[[condInterest[3]]]$q01Error
ErrorCal3Outliers[which(ErrorCal[[condInterest[3]]]$q01Error>0.06)] <- 0.06
which(ErrorCal[[condInterest[4]]]$q01Error>0.06)
ErrorCal4Outliers <- ErrorCal[[condInterest[4]]]$q01Error
ErrorCal4Outliers[which(ErrorCal[[condInterest[4]]]$q01Error>0.06)] <- 0.06
which(ErrorCal[[condInterest[1]]]$q01Error<(-0.06))
which(ErrorCal[[condInterest[2]]]$q01Error<(-0.06))
which(ErrorCal[[condInterest[3]]]$q01Error<(-0.06))
which(ErrorCal[[condInterest[4]]]$q01Error<(-0.06))

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$q01Error, ErrorCal2Outliers,
                        ErrorCal3Outliers, ErrorCal4Outliers),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.056,
                   col = colorsParam2[condInterest], labels = c(""),
                   ylim = c(-0.06, 0.06), cex.axis = 1.5)

mtext("Error", side = 2, line = 2.4, cex = 2.5)
grps <- round(meanAbsE[17:20], digits = 4)
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 1.7,
       col = colorsParam2[condInterest])
mtext ("MAE", at = 0.2, side = 1, line = 0.5, font=2, cex = 1.7)
grps <- c(expression(lambda*1), expression(lambda*2),
          expression(mu*1), expression(mu*2), expression("q12 = q21"))
mtext (grps[5], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 1.8)
abline(h = 0, lty = 2, lwd= 0.5)
points(meanE[17:20], col = "black", pch = 16)




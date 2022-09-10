## Plotting paper figure 2

# install.packages("beeswarm")

## BD

AbsErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/AbsErrorCal.rds")
ErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/ErrorCal.rds")

## Preparing plotting

RatesOfInterest <- c("birth_rate", "extinction_rate", "diversification_rate", "turnover_rate")
nbEstimRates <- length(RatesOfInterest)

colorsParam1 <- c("purple1","darkorange1", "chartreuse1", "royalblue1")
colorsParam2 <- c("purple2", "darkorange2", "chartreuse3", "royalblue2")
colorsParam3 <- c("purple3", "darkorange3", "chartreuse3", "royalblue3")
coloradjust <- sapply(colorsParam1, function(x){adjustcolor(x,alpha.f = 0.05)})

TechniquesNames <- c("CNN-CDV", "FFNN-SS", "MLE", "FFNN-CDV")

condInterest <- c(1:4)

focusAbsE <- list(AbsErrorCal[[condInterest[1]]]$birth_rateAbsError, AbsErrorCal[[condInterest[2]]]$birth_rateAbsError,
                  AbsErrorCal[[condInterest[3]]]$birth_rateAbsError, AbsErrorCal[[condInterest[4]]]$birth_rateAbsError,
                  AbsErrorCal[[condInterest[1]]]$extinction_rateAbsError, AbsErrorCal[[condInterest[2]]]$extinction_rateAbsError,
                  AbsErrorCal[[condInterest[3]]]$extinction_rateAbsError, AbsErrorCal[[condInterest[4]]]$extinction_rateAbsError,
                  AbsErrorCal[[condInterest[1]]]$diversification_rateAbsError, AbsErrorCal[[condInterest[2]]]$diversification_rateAbsError,
                  AbsErrorCal[[condInterest[3]]]$diversification_rateAbsError, AbsErrorCal[[condInterest[4]]]$diversification_rateAbsError,
                  AbsErrorCal[[condInterest[1]]]$turnover_rateAbsError, AbsErrorCal[[condInterest[2]]]$turnover_rateAbsError,
                  AbsErrorCal[[condInterest[3]]]$turnover_rateAbsError, AbsErrorCal[[condInterest[4]]]$turnover_rateAbsError)
meanAbsE <- unlist(lapply(focusAbsE, function(x) mean(x, na.rm = TRUE)))

## BIAS
## Beeswarm four separate panels for the paper
## 1

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$birth_rateError, ErrorCal[[condInterest[2]]]$birth_rateError,
                        ErrorCal[[condInterest[3]]]$birth_rateError, ErrorCal[[condInterest[4]]]$birth_rateError),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.35,
                   col = colorsParam2[condInterest], labels = c(""))

mtext("Bias", side = 2, line = 2.4, cex = 2)
grps <- paste0(round(meanAbsE[1:4], digits = 2))
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 1.5,
       col = colorsParam2[condInterest])
mtext ("MAE", at = 0.5, side = 1, line = 0.5, font=2, cex = 1.5)
grps <- c(expression(lambda), expression(mu), expression(r), expression(epsilon))
mtext (grps[1], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2)
abline(h = 0, lty = 2, lwd= 0.5)

## 2

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$extinction_rateError, ErrorCal[[condInterest[2]]]$extinction_rateError,
                        ErrorCal[[condInterest[3]]]$extinction_rateError, ErrorCal[[condInterest[4]]]$extinction_rateError),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.35,
                   col = colorsParam2[condInterest], labels = c(""))

grps <- paste0(round(meanAbsE[5:8], digits = 2))
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 1.5,
       col = colorsParam2[condInterest])
grps <- c(expression(lambda), expression(mu), expression(r), expression(epsilon))
mtext (grps[2], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2)
abline(h = 0, lty = 2, lwd= 0.5)
legend(1, 0.38, legend=TechniquesNames[condInterest],
       col=colorsParam2[condInterest], lwd=2, cex=1, box.lty=0,
       bg="transparent")
mtext (paste0("n=",nrow(ErrorCal[[1]])), at = c(4), lwd=2, cex=1)

## 3

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$diversification_rateError, ErrorCal[[condInterest[2]]]$diversification_rateError,
                        ErrorCal[[condInterest[3]]]$diversification_rateError, ErrorCal[[condInterest[4]]]$diversification_rateError),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.35,
                   col = colorsParam2[condInterest], labels = c(""))

mtext("Bias", side = 2, line = 2.4, cex = 2)
grps <- paste0(round(meanAbsE[9:12], digits = 2))
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 1.5,
       col = colorsParam2[condInterest])
mtext ("MAE", at = 0.5, side = 1, line = 0.5, font=2, cex = 1.5)
grps <- c(expression(lambda), expression(mu), expression(r), expression(epsilon))
mtext (grps[3], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2)
abline(h = 0, lty = 2, lwd= 0.5)

## 4

beeswarm::beeswarm(list(ErrorCal[[condInterest[1]]]$turnover_rateError, ErrorCal[[condInterest[2]]]$turnover_rateError,
                        ErrorCal[[condInterest[3]]]$turnover_rateError, ErrorCal[[condInterest[4]]]$turnover_rateError),
                   method = c("swarm"), spacing = 1, corral = c("none"), cex = 0.35,
                   col = colorsParam2[condInterest], labels = c(""))

grps <- paste0(round(meanAbsE[13:16], digits = 2))
mtext (grps, at = (1:(nbEstimRates*length(TechniquesNames[condInterest])))[1:4], side = 1, line = 0.5, font=2, cex = 1.5,
       col = colorsParam2[condInterest])
grps <- c(expression(lambda), expression(mu), expression(r), expression(epsilon))
mtext (grps[4], at = seq(from = (length(TechniquesNames[condInterest])+1)/2, to = length(TechniquesNames[condInterest])*nbEstimRates,
                         by = length(TechniquesNames[condInterest])), side = 1, line = 2, font=2, cex = 2)
abline(h = 0, lty = 2, lwd = 0.5)


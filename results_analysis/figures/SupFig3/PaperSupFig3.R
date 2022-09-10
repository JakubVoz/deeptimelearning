## Plotting paper supplementary figure 3

# install.packages("quantreg")

## BD and BiSSE

## Relative absolute error in function of tree size

## BD

TARGETcst200_500_001_treesize_na.rm <- read.csv("/../deeptimelearning/data/simulations/inference_results/BD/TARGETcst200_500_001_treesize_na.rm.csv")
TARGETcst200_500_001_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/TARGETcst200_500_001_na.rm.rds")
listDFinference_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/listDFinference_na.rm.rds")
AbsRelErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/AbsRelErrorCal.rds")

TechniquesNames <- c("CNN-CDV", "FFNN-SS", "MLE", "FFNN-CDV")

#### Representing the correlation between tree size and parameter estimates

# Birth rate

rq_CNNbirth <- quantreg::rq(AbsRelErrorCal$`CNN-CDV`$birth_rateAbsRelError~TARGETcst200_500_001_treesize_na.rm$x)
new_birth_sim <- data.frame(birth_sim = seq((min(TARGETcst200_500_001_treesize_na.rm$x)-50),(max(TARGETcst200_500_001_treesize_na.rm$x)+50),
                                            length.out = 1000))
Q95_CNNbirth <- quantile(rq_CNNbirth$residuals, probs = c(0.025, 0.975))
lowQ95_CNNbirth <- rq_CNNbirth$coefficients[1]+rq_CNNbirth$coefficients[2]*new_birth_sim+Q95_CNNbirth[1]
upQ95_CNNbirth <- rq_CNNbirth$coefficients[1]+rq_CNNbirth$coefficients[2]*new_birth_sim+Q95_CNNbirth[2]

plot(AbsRelErrorCal$`CNN-CDV`$birth_rateAbsRelError~TARGETcst200_500_001_treesize_na.rm$x,
     pch = 3, col = "purple1", ylab = expression(paste(lambda, " relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0, 0.3),
     main = expression(lambda), cex.main=2)
abline(rq_CNNbirth, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_birth_sim$birth_sim), new_birth_sim$birth_sim), c(rev(upQ95_CNNbirth$birth_sim), lowQ95_CNNbirth$birth_sim), col = purple1adjust,
        border = FALSE)
lines(new_birth_sim$birth_sim, lowQ95_CNNbirth$birth_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_birth_sim$birth_sim, upQ95_CNNbirth$birth_sim, col = 'purple1', lty = 3, lwd = 1)

# Death rate

rq_CNNextinction <- quantreg::rq(AbsRelErrorCal$`CNN-CDV`$extinction_rateAbsRelError~TARGETcst200_500_001_treesize_na.rm$x)
new_extinction_sim <- data.frame(extinction_sim = seq((min(TARGETcst200_500_001_treesize_na.rm$x)-50),(max(TARGETcst200_500_001_treesize_na.rm$x)+50),
                                            length.out = 1000))
Q95_CNNextinction <- quantile(rq_CNNextinction$residuals, probs = c(0.025, 0.975))
lowQ95_CNNextinction <- rq_CNNextinction$coefficients[1]+rq_CNNextinction$coefficients[2]*new_extinction_sim+Q95_CNNextinction[1]
upQ95_CNNextinction <- rq_CNNextinction$coefficients[1]+rq_CNNextinction$coefficients[2]*new_extinction_sim+Q95_CNNextinction[2]

plot(AbsRelErrorCal$`CNN-CDV`$extinction_rateAbsRelError~TARGETcst200_500_001_treesize_na.rm$x,
     pch = 3, col = "purple1", ylab = expression(paste(mu, " relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,6),
     main = expression(mu), cex.main=2)
abline(rq_CNNextinction, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_extinction_sim$extinction_sim), new_extinction_sim$extinction_sim), c(rev(upQ95_CNNextinction$extinction_sim), lowQ95_CNNextinction$extinction_sim), col = purple1adjust,
        border = FALSE)
lines(new_extinction_sim$extinction_sim, lowQ95_CNNextinction$extinction_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_extinction_sim$extinction_sim, upQ95_CNNextinction$extinction_sim, col = 'purple1', lty = 3, lwd = 1)


## BiSSE

listDFinferenceWithCNNless_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/listDFinferenceWithCNNless_na.rmCNNless.rds")
TARGETBiSSE200_500_0_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/TARGETBiSSE200_500_0_na.rm.rds")
TARGETBiSSE200_500_0_treesize_na.rm <- read.csv2("/../deeptimelearning/data/simulations/inference_results/BiSSE/TARGETBiSSE200_500_0_treesize_na.rm.csv")
AbsRelErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/AbsRelErrorCalCNNless.rds")

TechniquesNames <- c("CNN-CDV", "MLE castor", "MLE diversitree", "FFNN-CDV", "CNN-CDV-less")

#### Representing the correlation between tree size and parameter estimates

# Speciation rate 1

rq_CNNlambda1 <- quantreg::rq(AbsRelErrorCal$`CNN-CDV`$lambda1AbsRelError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_lambda1_sim <- data.frame(lambda1_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                                length.out = 1000))
Q95_CNNlambda1 <- quantile(rq_CNNlambda1$residuals, probs = c(0.025, 0.975))
lowQ95_CNNlambda1 <- rq_CNNlambda1$coefficients[1]+rq_CNNlambda1$coefficients[2]*new_lambda1_sim+Q95_CNNlambda1[1]
upQ95_CNNlambda1 <- rq_CNNlambda1$coefficients[1]+rq_CNNlambda1$coefficients[2]*new_lambda1_sim+Q95_CNNlambda1[2]

plot(AbsRelErrorCal$`CNN-CDV`$lambda1AbsRelError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(lambda, "1 relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.4),
     main = expression(paste(lambda, "1")), cex.main=2)
abline(rq_CNNlambda1, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_lambda1_sim$lambda1_sim), new_lambda1_sim$lambda1_sim), c(rev(upQ95_CNNlambda1$lambda1_sim), lowQ95_CNNlambda1$lambda1_sim), col = purple1adjust,
        border = FALSE)
lines(new_lambda1_sim$lambda1_sim, lowQ95_CNNlambda1$lambda1_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_lambda1_sim$lambda1_sim, upQ95_CNNlambda1$lambda1_sim, col = 'purple1', lty = 3, lwd = 1)

# Speciation rate 2

rq_CNNlambda2 <- quantreg::rq(AbsRelErrorCal$`CNN-CDV`$lambda2AbsRelError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_lambda2_sim <- data.frame(lambda2_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                                length.out = 1000))
Q95_CNNlambda2 <- quantile(rq_CNNlambda2$residuals, probs = c(0.025, 0.975))
lowQ95_CNNlambda2 <- rq_CNNlambda2$coefficients[1]+rq_CNNlambda2$coefficients[2]*new_lambda2_sim+Q95_CNNlambda2[1]
upQ95_CNNlambda2 <- rq_CNNlambda2$coefficients[1]+rq_CNNlambda2$coefficients[2]*new_lambda2_sim+Q95_CNNlambda2[2]

plot(AbsRelErrorCal$`CNN-CDV`$lambda2AbsRelError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(lambda, "2 relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,1.2),
     main = expression(paste(lambda, "2")), cex.main=2)
abline(rq_CNNlambda2, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_lambda2_sim$lambda2_sim), new_lambda2_sim$lambda2_sim), c(rev(upQ95_CNNlambda2$lambda2_sim), lowQ95_CNNlambda2$lambda2_sim), col = purple1adjust,
        border = FALSE)
lines(new_lambda2_sim$lambda2_sim, lowQ95_CNNlambda2$lambda2_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_lambda2_sim$lambda2_sim, upQ95_CNNlambda2$lambda2_sim, col = 'purple1', lty = 3, lwd = 1)

# Extinction rate 1

rq_CNNmu1 <- quantreg::rq(AbsRelErrorCal$`CNN-CDV`$mu1AbsRelError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_mu1_sim <- data.frame(mu1_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                        length.out = 1000))
Q95_CNNmu1 <- quantile(rq_CNNmu1$residuals, probs = c(0.025, 0.975))
lowQ95_CNNmu1 <- rq_CNNmu1$coefficients[1]+rq_CNNmu1$coefficients[2]*new_mu1_sim+Q95_CNNmu1[1]
upQ95_CNNmu1 <- rq_CNNmu1$coefficients[1]+rq_CNNmu1$coefficients[2]*new_mu1_sim+Q95_CNNmu1[2]

plot(AbsRelErrorCal$`CNN-CDV`$mu1AbsRelError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(mu, "1 relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,8),
     main = expression(paste(mu, "1")), cex.main=2)
abline(rq_CNNmu1, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_mu1_sim$mu1_sim), new_mu1_sim$mu1_sim), c(rev(upQ95_CNNmu1$mu1_sim), lowQ95_CNNmu1$mu1_sim), col = purple1adjust,
        border = FALSE)
lines(new_mu1_sim$mu1_sim, lowQ95_CNNmu1$mu1_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_mu1_sim$mu1_sim, upQ95_CNNmu1$mu1_sim, col = 'purple1', lty = 3, lwd = 1)

# zoom

plot(AbsRelErrorCal$`CNN-CDV`$mu1AbsRelError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(mu, "1 relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,1),
     main = expression(paste(mu, "1")), cex.main=2)
abline(rq_CNNmu1, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_mu1_sim$mu1_sim), new_mu1_sim$mu1_sim), c(rev(upQ95_CNNmu1$mu1_sim), lowQ95_CNNmu1$mu1_sim), col = purple1adjust,
        border = FALSE)
lines(new_mu1_sim$mu1_sim, lowQ95_CNNmu1$mu1_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_mu1_sim$mu1_sim, upQ95_CNNmu1$mu1_sim, col = 'purple1', lty = 3, lwd = 1)

# Extinction rate 2

rq_CNNmu2 <- quantreg::rq(AbsRelErrorCal$`CNN-CDV`$mu2AbsRelError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_mu2_sim <- data.frame(mu2_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                        length.out = 1000))
Q95_CNNmu2 <- quantile(rq_CNNmu2$residuals, probs = c(0.025, 0.975))
lowQ95_CNNmu2 <- rq_CNNmu2$coefficients[1]+rq_CNNmu2$coefficients[2]*new_mu2_sim+Q95_CNNmu2[1]
upQ95_CNNmu2 <- rq_CNNmu2$coefficients[1]+rq_CNNmu2$coefficients[2]*new_mu2_sim+Q95_CNNmu2[2]

plot(AbsRelErrorCal$`CNN-CDV`$mu2AbsRelError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(mu, "2 relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,8),
     main = expression(paste(mu, "2")), cex.main=2)
abline(rq_CNNmu2, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_mu2_sim$mu2_sim), new_mu2_sim$mu2_sim), c(rev(upQ95_CNNmu2$mu2_sim), lowQ95_CNNmu2$mu2_sim), col = purple1adjust,
        border = FALSE)
lines(new_mu2_sim$mu2_sim, lowQ95_CNNmu2$mu2_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_mu2_sim$mu2_sim, upQ95_CNNmu2$mu2_sim, col = 'purple1', lty = 3, lwd = 1)

# zoom

plot(AbsRelErrorCal$`CNN-CDV`$mu2AbsRelError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(mu, "2 relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,1),
     main = expression(paste(mu, "2")), cex.main=2)
abline(rq_CNNmu2, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_mu2_sim$mu2_sim), new_mu2_sim$mu2_sim), c(rev(upQ95_CNNmu2$mu2_sim), lowQ95_CNNmu2$mu2_sim), col = purple1adjust,
        border = FALSE)
lines(new_mu2_sim$mu2_sim, lowQ95_CNNmu2$mu2_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_mu2_sim$mu2_sim, upQ95_CNNmu2$mu2_sim, col = 'purple1', lty = 3, lwd = 1)

# Transition rate

rq_CNNq01 <- quantreg::rq(AbsRelErrorCal$`CNN-CDV`$q01AbsRelError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_q01_sim <- data.frame(q01_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                        length.out = 1000))
Q95_CNNq01 <- quantile(rq_CNNq01$residuals, probs = c(0.025, 0.975))
lowQ95_CNNq01 <- rq_CNNq01$coefficients[1]+rq_CNNq01$coefficients[2]*new_q01_sim+Q95_CNNq01[1]
upQ95_CNNq01 <- rq_CNNq01$coefficients[1]+rq_CNNq01$coefficients[2]*new_q01_sim+Q95_CNNq01[2]

plot(AbsRelErrorCal$`CNN-CDV`$q01AbsRelError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(q12, " relative absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.8),
     main = expression(q12), cex.main=1.6)
abline(rq_CNNq01, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_q01_sim$q01_sim), new_q01_sim$q01_sim), c(rev(upQ95_CNNq01$q01_sim), lowQ95_CNNq01$q01_sim), col = purple1adjust,
        border = FALSE)
lines(new_q01_sim$q01_sim, lowQ95_CNNq01$q01_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_q01_sim$q01_sim, upQ95_CNNq01$q01_sim, col = 'purple1', lty = 3, lwd = 1)


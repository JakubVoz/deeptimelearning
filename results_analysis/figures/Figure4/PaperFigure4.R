## Plotting paper figure 4

## BD and BiSSE

## Absolute error in function of tree size

## BD

TARGETcst200_500_001_treesize_na.rm <- read.csv("/../deeptimelearning/data/simulations/inference_results/BD/TARGETcst200_500_001_treesize_na.rm.csv")
TARGETcst200_500_001_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/TARGETcst200_500_001_na.rm.rds")
listDFinference_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/listDFinference_na.rm.rds")
AbsErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BD/AbsErrorCal.rds")

TechniquesNames <- c("CNN-CDV", "FFNN-SS", "MLE", "FFNN-CDV")

#### Representing the correlation between tree size and parameter estimates

# Birth rate

lm_CNNbirth <- lm(AbsErrorCal$`CNN-CDV`$birth_rateAbsError~TARGETcst200_500_001_treesize_na.rm$x)
new_birth_sim <- data.frame(birth_sim = seq((min(TARGETcst200_500_001_treesize_na.rm$x)-50),(max(TARGETcst200_500_001_treesize_na.rm$x)+50),
                                            length.out = 1000))
Q95_CNNbirth <- quantile(lm_CNNbirth$residuals, probs = c(0.025, 0.975))
lowQ95_CNNbirth <- lm_CNNbirth$coefficients[1]+lm_CNNbirth$coefficients[2]*new_birth_sim+Q95_CNNbirth[1]
upQ95_CNNbirth <- lm_CNNbirth$coefficients[1]+lm_CNNbirth$coefficients[2]*new_birth_sim+Q95_CNNbirth[2]

plot(AbsErrorCal$`CNN-CDV`$birth_rateAbsError~TARGETcst200_500_001_treesize_na.rm$x,
     pch = 3, col = "purple1", ylab = expression(paste(lambda, " absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.13),
     main = expression(lambda), cex.main=2)
abline(lm_CNNbirth, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_birth_sim$birth_sim), new_birth_sim$birth_sim), c(rev(upQ95_CNNbirth$birth_sim), lowQ95_CNNbirth$birth_sim), col = purple1adjust,
        border = FALSE)
lines(new_birth_sim$birth_sim, lowQ95_CNNbirth$birth_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_birth_sim$birth_sim, upQ95_CNNbirth$birth_sim, col = 'purple1', lty = 3, lwd = 1)

# Death rate

lm_CNNextinction <- lm(AbsErrorCal$`CNN-CDV`$extinction_rateAbsError~TARGETcst200_500_001_treesize_na.rm$x)
new_extinction_sim <- data.frame(extinction_sim = seq((min(TARGETcst200_500_001_treesize_na.rm$x)-50),(max(TARGETcst200_500_001_treesize_na.rm$x)+50),
                                                      length.out = 1000))
Q95_CNNextinction <- quantile(lm_CNNextinction$residuals, probs = c(0.025, 0.975))
lowQ95_CNNextinction <- lm_CNNextinction$coefficients[1]+lm_CNNextinction$coefficients[2]*new_extinction_sim+Q95_CNNextinction[1]
upQ95_CNNextinction <- lm_CNNextinction$coefficients[1]+lm_CNNextinction$coefficients[2]*new_extinction_sim+Q95_CNNextinction[2]

plot(AbsErrorCal$`CNN-CDV`$extinction_rateAbsError~TARGETcst200_500_001_treesize_na.rm$x,
     pch = 3, col = "purple1", ylab = expression(paste(mu, " absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.13),
     main = expression(mu), cex.main=2)
abline(lm_CNNextinction, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_extinction_sim$extinction_sim), new_extinction_sim$extinction_sim), c(rev(upQ95_CNNextinction$extinction_sim), lowQ95_CNNextinction$extinction_sim), col = purple1adjust,
        border = FALSE)
lines(new_extinction_sim$extinction_sim, lowQ95_CNNextinction$extinction_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_extinction_sim$extinction_sim, upQ95_CNNextinction$extinction_sim, col = 'purple1', lty = 3, lwd = 1)


## BiSSE

listDFinferenceWithCNNless_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/listDFinferenceWithCNNless_na.rmCNNless.rds")
TARGETBiSSE200_500_0_na.rm <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/TARGETBiSSE200_500_0_na.rm.rds")
TARGETBiSSE200_500_0_treesize_na.rm <- read.csv2("/../deeptimelearning/data/simulations/inference_results/BiSSE/TARGETBiSSE200_500_0_treesize_na.rm.csv")
AbsErrorCal <- readRDS("/../deeptimelearning/data/simulations/inference_results/BiSSE/AbsErrorCalCNNless.rds")

TechniquesNames <- c("CNN-CDV", "MLE castor", "MLE diversitree", "FFNN-CDV", "CNN-CDV-less")

#### Representing the correlation between tree size and parameter estimates

# Speciation rate 1

lm_CNNlambda1 <- lm(AbsErrorCal$`CNN-CDV`$lambda1AbsError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_lambda1_sim <- data.frame(lambda1_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                                length.out = 1000))
Q95_CNNlambda1 <- quantile(lm_CNNlambda1$residuals, probs = c(0.025, 0.975))
lowQ95_CNNlambda1 <- lm_CNNlambda1$coefficients[1]+lm_CNNlambda1$coefficients[2]*new_lambda1_sim+Q95_CNNlambda1[1]
upQ95_CNNlambda1 <- lm_CNNlambda1$coefficients[1]+lm_CNNlambda1$coefficients[2]*new_lambda1_sim+Q95_CNNlambda1[2]

plot(AbsErrorCal$`CNN-CDV`$lambda1AbsError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(lambda, "1 absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.30),
     main = expression(paste(lambda, "1")), cex.main=2)
abline(lm_CNNlambda1, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_lambda1_sim$lambda1_sim), new_lambda1_sim$lambda1_sim), c(rev(upQ95_CNNlambda1$lambda1_sim), lowQ95_CNNlambda1$lambda1_sim), col = purple1adjust,
        border = FALSE)
lines(new_lambda1_sim$lambda1_sim, lowQ95_CNNlambda1$lambda1_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_lambda1_sim$lambda1_sim, upQ95_CNNlambda1$lambda1_sim, col = 'purple1', lty = 3, lwd = 1)

# Speciation rate 2

lm_CNNlambda2 <- lm(AbsErrorCal$`CNN-CDV`$lambda2AbsError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_lambda2_sim <- data.frame(lambda2_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                                length.out = 1000))
Q95_CNNlambda2 <- quantile(lm_CNNlambda2$residuals, probs = c(0.025, 0.975))
lowQ95_CNNlambda2 <- lm_CNNlambda2$coefficients[1]+lm_CNNlambda2$coefficients[2]*new_lambda2_sim+Q95_CNNlambda2[1]
upQ95_CNNlambda2 <- lm_CNNlambda2$coefficients[1]+lm_CNNlambda2$coefficients[2]*new_lambda2_sim+Q95_CNNlambda2[2]

plot(AbsErrorCal$`CNN-CDV`$lambda2AbsError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(lambda, "2 absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.30),
     main = expression(paste(lambda, "2")), cex.main=2)
abline(lm_CNNlambda2, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_lambda2_sim$lambda2_sim), new_lambda2_sim$lambda2_sim), c(rev(upQ95_CNNlambda2$lambda2_sim), lowQ95_CNNlambda2$lambda2_sim), col = purple1adjust,
        border = FALSE)
lines(new_lambda2_sim$lambda2_sim, lowQ95_CNNlambda2$lambda2_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_lambda2_sim$lambda2_sim, upQ95_CNNlambda2$lambda2_sim, col = 'purple1', lty = 3, lwd = 1)

# Extinction rate 1

lm_CNNmu1 <- lm(AbsErrorCal$`CNN-CDV`$mu1AbsError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_mu1_sim <- data.frame(mu1_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                        length.out = 1000))
Q95_CNNmu1 <- quantile(lm_CNNmu1$residuals, probs = c(0.025, 0.975))
lowQ95_CNNmu1 <- lm_CNNmu1$coefficients[1]+lm_CNNmu1$coefficients[2]*new_mu1_sim+Q95_CNNmu1[1]
upQ95_CNNmu1 <- lm_CNNmu1$coefficients[1]+lm_CNNmu1$coefficients[2]*new_mu1_sim+Q95_CNNmu1[2]

plot(AbsErrorCal$`CNN-CDV`$mu1AbsError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(mu, "1 absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.25),
     main = expression(paste(mu, "1")), cex.main=2)
abline(lm_CNNmu1, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_mu1_sim$mu1_sim), new_mu1_sim$mu1_sim), c(rev(upQ95_CNNmu1$mu1_sim), lowQ95_CNNmu1$mu1_sim), col = purple1adjust,
        border = FALSE)
lines(new_mu1_sim$mu1_sim, lowQ95_CNNmu1$mu1_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_mu1_sim$mu1_sim, upQ95_CNNmu1$mu1_sim, col = 'purple1', lty = 3, lwd = 1)

# Extinction rate 2

lm_CNNmu2 <- lm(AbsErrorCal$`CNN-CDV`$mu2AbsError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_mu2_sim <- data.frame(mu2_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                        length.out = 1000))
Q95_CNNmu2 <- quantile(lm_CNNmu2$residuals, probs = c(0.025, 0.975))
lowQ95_CNNmu2 <- lm_CNNmu2$coefficients[1]+lm_CNNmu2$coefficients[2]*new_mu2_sim+Q95_CNNmu2[1]
upQ95_CNNmu2 <- lm_CNNmu2$coefficients[1]+lm_CNNmu2$coefficients[2]*new_mu2_sim+Q95_CNNmu2[2]

plot(AbsErrorCal$`CNN-CDV`$mu2AbsError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(mu, "2 absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.25),
     main = expression(paste(mu, "2")), cex.main=2)
abline(lm_CNNmu2, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_mu2_sim$mu2_sim), new_mu2_sim$mu2_sim), c(rev(upQ95_CNNmu2$mu2_sim), lowQ95_CNNmu2$mu2_sim), col = purple1adjust,
        border = FALSE)
lines(new_mu2_sim$mu2_sim, lowQ95_CNNmu2$mu2_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_mu2_sim$mu2_sim, upQ95_CNNmu2$mu2_sim, col = 'purple1', lty = 3, lwd = 1)

# Transition rate

lm_CNNq01 <- lm(AbsErrorCal$`CNN-CDV`$q01AbsError~TARGETBiSSE200_500_0_treesize_na.rm$x)
new_q01_sim <- data.frame(q01_sim = seq((min(TARGETBiSSE200_500_0_treesize_na.rm$x)-50),(max(TARGETBiSSE200_500_0_treesize_na.rm$x)+50),
                                        length.out = 1000))
Q95_CNNq01 <- quantile(lm_CNNq01$residuals, probs = c(0.025, 0.975))
lowQ95_CNNq01 <- lm_CNNq01$coefficients[1]+lm_CNNq01$coefficients[2]*new_q01_sim+Q95_CNNq01[1]
upQ95_CNNq01 <- lm_CNNq01$coefficients[1]+lm_CNNq01$coefficients[2]*new_q01_sim+Q95_CNNq01[2]

plot(AbsErrorCal$`CNN-CDV`$q01AbsError[1:500]~TARGETBiSSE200_500_0_treesize_na.rm$x[1:500],
     pch = 3, col = "purple1", ylab = expression(paste(q12, " absolute error")),
     xlab = "Tree size", bty='n',
     cex = 0.5, lwd = 0.5, ylim = c(0,0.03),
     main = expression(q12), cex.main=1.6)
abline(lm_CNNq01, col = "purple3", lwd= 2)
purple1adjust <- adjustcolor( "purple1", alpha.f = 0.05)
polygon(c(rev(new_q01_sim$q01_sim), new_q01_sim$q01_sim), c(rev(upQ95_CNNq01$q01_sim), lowQ95_CNNq01$q01_sim), col = purple1adjust,
        border = FALSE)
lines(new_q01_sim$q01_sim, lowQ95_CNNq01$q01_sim, col = 'purple1', lty = 3, lwd = 1)
lines(new_q01_sim$q01_sim, upQ95_CNNq01$q01_sim, col = 'purple1', lty = 3, lwd = 1)


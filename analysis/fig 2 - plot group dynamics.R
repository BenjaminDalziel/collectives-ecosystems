# C-R Collectives: Plot group dynamics
#
# Sept 5, 2019

rm(list=ls())
graphics.off()


# Script parameters -------------------------------------------------------

runID <- 0.3183108               #K = 12000 
# <- 0.5518395             #K = 12000, short time steps, long-lived predators






# Adjust xlims so time series displayed have same num. of cycles ----------

#time domain for figure in main manuscript
burnin <- 2000
endcrop <- 4000


#time domain for figure in SI: short time steps, long-lived predators
#burnin <- 1000
#endcrop <- 2000   





# Take in data ------------------------------------------------------------

source("./functions/LoadSimulationData.R")

out <- LoadSimulationData(runID = runID, 
                          runPath = "./data/feedback/")
simdata <- out$simdata
simpara <- out$simpara
rm(out)

if(burnin) simdata <- simdata[-(1:burnin), ]
if(endcrop) simdata <- simdata[-((nrow(simdata)-endcrop):nrow(simdata)), ]


Timesteps <- simdata$Time

G <- simdata$NumberOfGroups
S <- simdata$NumberOfSingletonGroups
P <- simdata$PredPop
R <- simdata$PreyPop
e <- simdata$ExposureRate



# Calculate some additional quantities ------------------------------------

e0 <- pi * simpara$CaptureRadius^2 / simpara$ArenaLength^2

simdata$PredQuesting <- simdata$PredPop - simdata$PredHandling
simdata$ExposureRateQuesting <- simdata$PreyInCaptureRangeQuesting/simdata$PredQuesting/simdata$PreyPop
simdata$ExposureRateHandling <- simdata$PreyInCaptureRangeHandling/simdata$PredHandling/simdata$PreyPop
simdata$FractionQuesting <- simdata$PredQuesting/simdata$PredPop
simdata$FractionHandling <- 1 - simdata$FractionQuesting
ebar <- simdata$ExposureRateQuesting * simdata$FractionQuesting + simdata$ExposureRateHandling * simdata$FractionHandling
entropy <- simdata$ExposureRateQuesting/simdata$ExposureRateHandling
entropy[!is.finite(entropy)] <- NA





# Plot --------------------------------------------------------------------

#
par(mfcol = c(2,2))
par(mar = c(4,5,2,1))


z <- boxplot(e~G, plot=FALSE)
y <- z$stats
x <- as.numeric(z$names)
ylim <- range(c(y[2:4,]))
ylim[2] <- max(ylim[2], 1.1*e0)
plot(x, y[3,], ylim = ylim, xlab = "Number of consumer groups, G", ylab = "Encounter rate, e")
segments(x, y[2,], x, y[4,])
abline(e0,0)


#
z <- boxplot(R~G, plot=FALSE)
y <- z$stats
x <- as.numeric(z$names)
plot(y[3,], x, xlim = range(c(y[2:4,])), xlab = "Resources, R", ylab = "Number of consumer groups, G")
segments(y[2,], x, y[4,], x)
abline(e0,0)



#
plot(Timesteps, G, cex = 0.25, pch = 19, col = grey(0.4), xlab = "Time", ylab = "Number of consumer groups, G")
Gsmooth <- filter(G, filter = rep(1/100,100), circular = TRUE)
lines(Timesteps, Gsmooth, col = 2, lwd = 1.5)



#
plot(Timesteps, R, type = 'l', xlab = "Time", ylab = "Population size",yaxt = "n", ylim = c(0,8000))
axis(2, c(0, 4000, 8000))
lines(Timesteps, P, col = 2)
#legend("topright", lty = c(1,1), col = 1:2, legend = c("Resources", "Consumers"))





par(mfrow = c(2,2))
par(mar = c(4,5,2,1))



#
z <- boxplot(simdata$ExposureRateQuesting~G, plot=FALSE)
y <- z$stats
x <- as.numeric(z$names)
ylim <- range(c(y[2:4,]))
ylim[2] <- max(ylim[2], 1.1*e0)
plot(x, y[3,], ylim = ylim, xlab = "Number of consumer groups, G", 
     ylab = expression(paste("Encounter rate questing, ", e[q])))
segments(x, y[2,], x, y[4,])
abline(e0,0)

#
z <- boxplot(simdata$ExposureRateHandling~G, plot=FALSE)
y <- z$stats
x <- as.numeric(z$names)
ylim <- range(c(y[2:4,]))
ylim[2] <- max(ylim[2], 1.1*e0)
plot(x, y[3,], ylim = ylim, xlab = "Number of consumer groups, G", 
     ylab = expression(paste("Encounter rate handling, ", e[h])))
segments(x, y[2,], x, y[4,])
abline(e0,0)


#
z <- boxplot(entropy~G, plot=FALSE)
y <- z$stats
x <- as.numeric(z$names)
ylim = c(0.4,1.1)
plot(x, y[3,], ylim = ylim, xlab = "Number of consumer groups, G", yaxt= "n",
     ylab = expression(paste(e[q]/e[h])))
segments(x, y[2,], x, y[4,])
axis(2, c(0.5, 0.75, 1))

#
plot(simdata$PreyPop, log(simdata$FractionQuesting),
     pch = 19,
     col = grey(0.7),
     cex = 0.5,
     xlab = "Resource abundance, R",
     ylab = expression(paste("log ", phi)))


#
par(mfrow = c(1,3))

plot(log10(P/G), e, cex = 0.5, main = "a",
     xlab = "log10 Mean group size, log10 P/G",
     ylab = "Encounter rate, e", 
     pch = 19,
     col = grey(0.7))
x <- log10(P/G)
fit <- glm(e ~ x)
abline(e0,0)

xpred <- seq(min(x), max(x), len = 100)
pred <- predict(fit, newdata = data.frame(x = xpred), se.fit = TRUE)
lines(xpred, pred$fit, col = 2, lwd = 2)
lines(xpred, pred$fit + 5*pred$se.fit, lty = 3, col = 2, lwd = 2)
lines(xpred, pred$fit - 5*pred$se.fit, lty = 3, col = 2, lwd = 2)


#
boxplot(fit$residuals~G, cex = 0.5, main = "b",
     xlab = "Number of groups",
     ylab = "Excess encounter rate (not explained by group size)", 
     pch = 19,
     col = grey(0.7))


#
boxplot(log10(P/G) ~ G, cex = 0.5, main = "c",
        xlab = "Number of groups",
        ylab = "log10 Mean group size, log10 P/G", 
        pch = 19,
        col = grey(0.7))





#
par(mfrow = c(1,1))
plot(P, G, cex = R/max(R)+0.1, ylim = c(0,30),
     xlab = "Consumer population size, P",
     ylab = "Number of consumer groups, G")






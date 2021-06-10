# Plot Gause-like experiments from the agent based simulation

rm(list = ls())
graphics.off()


# Choose which data set to plot -------------------------------------------

#runID <- 0.7572809    #indep
runID <- 0.3487494    #clctv

# Invasibility experiment
#runID <- 0.7508081
#runID <- 0.7154632



# Source helper functions -------------------------------------------------

source("./functions/LoadSimulationData.R")




# Additional parameters ---------------------------------------------------

burnin <- 1
linecol <- c("red", "orange")

ylim <- c(0,330)
yax <- seq(0,300,100)

xlim = c(0,20000)

eylim <- c(0.002,0.012)
exlim <- c(0, 300)




# Some functions for this script ------------------------------------------

DoPlot <- function(simdata, linecol = c(1,2)){
  
  par(mar = c(4,4,2,1))
  plot(simdata$Time, simdata$PredPop - simdata$MutantPop, col = linecol[1], type = 'l',
       xlab = "Timestep", ylab = "Consumer population size", ylim = ylim, yaxt = 'n',xlim = xlim)
  lines(simdata$Time, simdata$MutantPop, col = linecol[2], lty = 3)
  axis(2, yax)
  
}


# Function to add GLM fit to existing scatter plot
AddFit <- function(x, y, col = 2, report = T){
  
  fit <- glm(y ~ x)
  xpred <- seq(min(x[is.finite(x)]), max(x[is.finite(x)]), length.out = 100)
  pred <- predict(fit, newdata = data.frame(x = xpred), se.fit = T)
  
  yhat <- pred$fit
  yhi <- pred$fit + 5 * pred$se.fit
  ylo <- pred$fit - 5 * pred$se.fit
  
  lines(xpred, yhat, col = col)
  lines(xpred, yhi, col = col, lty = 3)
  lines(xpred, ylo, col = col, lty = 3)
  
  if(report){
    s <- summary(fit)
    print(s)
    ediff <- s$coefficients[1,1] - e0
    print(paste("Intercept is ", abs(ediff/s$coefficients[1,2]), " s.e. from e0"))
  }
  
}






# Load data and make plots ------------------------------------------------

out <- LoadSimulationData(runID = runID, 
                          runPath = "./data/coexistence/")
simdata <- out$simdata
simpara <- out$simpara
rm(out)

DoPlot(simdata, linecol = linecol[1:2])


simdata <- simdata[-(1:burnin),]

R <- simdata$PreyPop
P <- simdata$PredPop - simdata$MutantPop
Q <- simdata$MutantPop
G <- simdata$NumberOfGroups - simdata$NumberOfMutantGroups
H <- simdata$NumberOfMutantGroups

X <- simdata$PreyInCaptureRange - simdata$PreyInCaptureRangeMutant
Y <- simdata$PreyInCaptureRangeMutant

e0 <- pi * simpara$CaptureRadius^2 / simpara$ArenaLength^2
eP <- X/(R*P)
eQ <- Y/(R*Q)
e <- (X+Y)/(R*(P+Q))




plot(Q, eQ, cex = 0.2, col = "grey", ylim = eylim, xlim = exlim, ylab = expression(e[Q]))
abline(e0,0)
AddFit(x = Q, y = eQ, col = 2, report = TRUE)
AddFit(x = Q[Q<150], y = eQ[Q<150], col = 4, report = TRUE)


plot(P, eP, cex = 0.2, ylim = eylim, xlim = exlim, col = "grey", ylab = expression(e[P]))
abline(e0,0)
AddFit(x = P, y = eP, col = 2, report = TRUE)
AddFit(x = P[P<200], y = eP[P<200], col = 4, report = TRUE)



plot(log(Q/P), log(eQ/eP), cex = 0.2, col = "grey",
     xlab = "Q/P",
     ylab = expression(e[Q]/e[P]),
     ylim = c(-1,1))
AddFit(x = log(Q/P), y = log(eQ/eP), col = 2)

# C-R Collectives: Plot group dynamics
#
# Sept 5, 2019

rm(list=ls())
graphics.off()

#no pursuit
runID <- 0.998932  #hi res, collective, no pursuit

burnin <- 1000
bw <- 100

workingdir <- "/Users/benjamda/Dropbox/Research/Active/CR-Collectives/Simulation output/general/"
scriptdir <- "/Users/benjamda/Dropbox/Research/Active/CR-Collectives/R Scripts/"

setwd(scriptdir)
source("SimulateRM.R")
source("LoadSimulationData.R")

out <- LoadSimulationData(runID = runID, runPath = workingdir)
simdata <- out$simdata
simpara <- out$simpara
rm(out)

simdata <- simdata[-(1:burnin),]

Timesteps <- simdata$Time

Graw <- simdata$NumberOfGroups
Praw <- simdata$PredPop
Rraw <- simdata$PreyPop
eraw <- simdata$ExposureRate

e0 <- pi * simpara$CaptureRadius^2 / simpara$ArenaLength^2

G <- filter(Graw, filter = rep(1/bw, bw))
P <- filter(Praw, filter = rep(1/bw, bw))
R <- filter(Rraw, filter = rep(1/bw, bw))
e <- filter(eraw, filter = rep(1/bw, bw))


# Function to calculate centered differences
cdiff <- function(x){
  
  c(NA, (diff(x[-1]) + diff(x[-length(x)]))/2, NA)
  
}


# Function to add GAM fit to existing scatter plot
AddFit <- function(x, y, col = 2, report = T){
  
  fit <- glm(y ~ x)
  xpred <- seq(min(x,na.rm=T), max(x,na.rm=T), length.out = 100)
  pred <- predict(fit, newdata = data.frame(x = xpred), se.fit = T)
  
  yhat <- pred$fit
  yhi <- pred$fit + 5 * pred$se.fit
  ylo <- pred$fit - 5 * pred$se.fit
  
  lines(xpred, yhat, col = col)
  lines(xpred, yhi, col = col, lty = 3)
  lines(xpred, ylo, col = col, lty = 3)
  
  if(report){
    print(summary(fit))
  }
  
}

# Superposed time series of number of groups and per-capita consumer recruitment
quartz(h=4, w=7)
par(pin = c(4,2))

plot(Timesteps, G, type = "l",
     xlab = "Time steps",
     ylab = "G")

par(new = T)
col <- 2
plot(Timesteps, cdiff(log(P))/simpara$TimeStep, axes = F, xlab = NA, ylab = NA, type = "l", col = 2, ylim = c(-0.2,0.2))
axis(side = 4, col.ticks = col, col.axis = col, col = col)
mtext(side = 4, line = 3, "dP/Pdt", col = col)


# Superposed time series of number of consumers with per-capita change in number of groups
quartz(h=4, w=7)
par(pin = c(4,2))

plot(Timesteps, P, type = "l",
     xlab = "Time steps",
     ylab = "P")

par(new = T)
col <- 2
plot(Timesteps, -cdiff(log(G))/simpara$TimeStep, axes = F, xlab = NA, ylab = NA, type = "l", col = 2, ylim = c(-0.15,0.15))
axis(side = 4, col.ticks = col, col.axis = col, col = col)
mtext(side = 4, line = 3, "dG/Gdt", col = col)



# Social --> ecological
quartz(h=4,w=4, title = "Social --> Ecological")
par(pin = c(2,2))

x <- G
y <- eraw

plot(x, y, cex = 0.2, pch = 19, col = "grey",
     xlab = "G",
     ylab = "e")

AddFit(x,y)
abline(e0, 0, lty = 2)

# Ecological --> social
dGGdt <- cdiff(log(G))/simpara$TimeStep
dPPdt <- cdiff(log(Praw))/simpara$TimeStep

x <- Praw 
y <- dGGdt

quartz(h=4,w=4, title = "Ecological --> Social")
par(pin = c(2,2))
plot(x, y, cex = 0.2, col = "grey",
     xlim = range(x[!is.na(y)]),
     xlab = "P",
     ylab = "dG/Gdt")
AddFit(x,y)




# Time series of number of consumer and number of groups
timerange <- c(2000, 3000)
quartz(h=4, w=7)
par(pin = c(4,2))

plot(Timesteps, G, type = "l",
     xlim = timerange,
     xlab = "Time steps",
     ylab = "G")

par(new = T)
col <- 2
plot(Timesteps, P, axes = F, xlab = NA, ylab = NA, type = "l", col = 2, xlim = timerange)
axis(side = 4, col.ticks = col, col.axis = col, col = col)
mtext(side = 4, line = 3, "P", col = col)


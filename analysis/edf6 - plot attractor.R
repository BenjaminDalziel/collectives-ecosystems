# Visualize flow in 3D, 
# with the third dimension as encounter rate

rm(list=ls())
graphics.off()

library(plot3D)
library(RColorBrewer)
library(viridis)

source("./functions/LoadSimulationData.R")

runID <- 0.1099243      #K = 8000, collective predator, no pursuit or avoidance, speeds = 5,5
runPath <- "./data/attractor/"


out <- LoadSimulationData(runID, runPath)
simdata <- out$simdata
simpara <- out$simpara


n <- 30
u <- seq(-1,1,len=n)
f <- 3/4 * (1-u^2)
f <- f/sum(f)

simdata$ExposureRateSmooth <- filter(simdata$ExposureRate, filter = f)



x <- simdata$PreyPop
y <- simdata$PredPop
z <- simdata$ExposureRateSmooth
w <- simdata$Crowding/simdata$PredPop
w[1] <- 0
w[2] <- 1


burnin <- 100
x <- x[-(1:burnin)]
y <- y[-(1:burnin)]
z <- z[-(1:burnin)]
w <- w[-(1:burnin)]


lwd <- 1

xlim <- c(-325, 7500)
ylim <- c(-100, 2000)
zlim <- c(0.01, 0.032)



# Simulation data
scatter3D(x, y, z, 
          phi = 25, 
          theta = -45, 
          type = "l",
          bty = "n",
          colkey = FALSE,
          colvar = log(x*z),
          col = cividis(11))
          
par(xpd=T)
z0 <- rep(0.004, length(z))
points3D(x, y, z0, 
         type='l',
         cex = 0.1,
         pch = 19,
         lwd = lwd, colvar = y-x,
         col = grey(0.9),
         colkey = FALSE,
         add = TRUE)          



# Analytical approximation
# Simulation data

e0 <- pi * simpara$InteractionRadius^2 / simpara$ArenaLength^2 

scatter3D(x, y, x/(x+250), 
          phi = 25, 
          theta = -45, 
          type = "l",
          bty = "n",
          colkey = FALSE,
          colvar = log(x*z),
          col = cividis(11))

par(xpd=T)
z0 <- rep(0.01, length(z))
points3D(x, y, z0, 
         type='l',
         cex = 0.1,
         pch = 19,
         lwd = lwd, colvar = y-x,
         col = grey(0.9),
         colkey = FALSE,
         add = TRUE)      


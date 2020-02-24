# C-R Collectives: Sensitivity analysis - stability
#
# Dec 17, 2019

rm(list = ls())
#graphics.off()

#setwd("E:/Dropbox/Research/Active/CR-Collectives/")   #PC
setwd("~/Dropbox/Research/Active/CR-Collectives/")

timerange <- c(0, 10000)
poprange <- c(0, 2000)

timeaxis <- seq(0, max(timerange), by = 2000)
popaxis <- seq(0, max(poprange), by = 1000)

runID <- c(0.1330027,       #independent consumer, collective resource
           0.522023,        #collective consumer, collective resource
           0.7798233,       #independent consumer, pursuit and avoidance
           0.6888894,       #collective consumer, pursuit and avoidance   
           0.8340011,       #independent consumer, alignment only, high turn rate (null for Vicsek-like model)
           0.2924905,       #collective consumer, alignment only, high turn rate (Vicsek-like model)
           0.1606093,       #independent consumer, low noise (high turn rate to allow more influence of noise)
           0.9631181,       #collective consumer, low noise (high turn rate to allow more influence of noise)
           0.7796913,       #independent consumer, high noise (high turn rate "" "")
           0.6788843)       #collective consumer, high noise (high turn rate "" "")    
           

workingdir <- "./Simulation output/sensitivity/"
scriptdir <- "./R Scripts/"

source(paste0(scriptdir, "SimulateRM.R"))
source(paste0(scriptdir,"LoadSimulationData.R"))


par(mfrow = c(5,2))
par(mar = c(4,5,2,1))
par(xpd = T)

for(i in 1:length(runID)){
  
  out <- LoadSimulationData(runID = runID[i], runPath = workingdir)
  simdata <- out$simdata
  simpara <- out$simpara
  rm(out)
  
  plot(simdata$Time, simdata$PredPop,
       xlim = timerange,
       ylim = poprange,
       axes = FALSE,
       col = ifelse(i%%2 == 0, 2, 1),
       xlab = "Time", ylab = "Consumer\npopulation",
       type = "l")
  
  axis(1, timeaxis)
  axis(2, popaxis)
  
  text(min(timeaxis), max(popaxis), letters[i], font = 2)
  
}


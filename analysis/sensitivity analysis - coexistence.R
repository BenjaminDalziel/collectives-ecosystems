# C-R Collectives: Sensitivity analysis - coexistence
#
# Dec 17, 2019

rm(list = ls())
#graphics.off()

#setwd("E:/Dropbox/Research/Active/CR-Collectives/")   #PC
setwd("~/Dropbox/Research/Active/CR-Collectives/")

timerange <- c(0, 5000)
poprange <- c(0, 500)

timeaxis <- seq(0, max(timerange), by = 1000)
popaxis <- seq(0, max(poprange), by = 250)

runID <- c(0.4964034,       #independent consumer, collective resource
           0.07171703,      #collective consumer, collective resource
           0.5846629,       #independent consumer, pursuit and avoidance
           0.6267713,       #collective consumer, pursuit and avoidance   
           0.7710378,       #independent consumer, alignment only, high turn rate (null for Vicsek-like model)
           0.07171703,      #collective consumer, alignment only, high turn rate (Vicsek-like model)
           0.7537025,       #independent consumer, low noise (high turn rate to allow more influence of noise)
           0.7193274,       #collective consumer, low noise (high turn rate to allow more influence of noise)
           0.3630697,       #independent consumer, high noise (high turn rate "" "")
           0.4881863)       #collective consumer, high noise (high turn rate "" "")    



workingdir <- "./Simulation output/sensitivity/"
scriptdir <- "./R Scripts/"

source(paste0(scriptdir, "SimulateRM.R"))
source(paste0(scriptdir,"LoadSimulationData.R"))


#quartz(h=7, w=6)
par(mfrow = c(5,2))
par(mar = c(4,5,0.5,1))
par(xpd = T)

for(i in 1:length(runID)){
  
  out <- LoadSimulationData(runID = runID[i], runPath = workingdir)
  simdata <- out$simdata
  simpara <- out$simpara
  rm(out)
  
  plot(simdata$Time, simdata$PredPop - simdata$MutantPop,
       xlim = timerange,
       ylim = poprange,
       axes = FALSE,
       col = ifelse(i%%2 == 0, "red", "black"),
       xlab = "Time", ylab = "Consumer\npopulation",
       type = "l")
  
  lines(simdata$Time, simdata$MutantPop, col = ifelse(i%%2 == 0, "orange", "grey"), lty = 3)
  
  axis(1, timeaxis)
  axis(2, popaxis)
  
  if(i%%2 == 0){
    legendcols <- c("red", "orange")
  }else{
    legendcols <- c("black", "grey")
  }
  if(i<=2){
    legend("topright", lty = c(1,3), col = legendcols, cex = 0.8,bty = "n",
           legend = c("Superior competitor, P",
                      "Inferior competitor, Q"))
  }
  
  
  text(min(timeaxis), max(popaxis), letters[i], font = 2)
  
}


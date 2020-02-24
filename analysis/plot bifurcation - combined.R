# C-R Collectives: Plot bifurcation diagram for collective and non collective simulations
# 
# Nov 29, 2018

rm(list=ls())
graphics.off()



# Script parameters -------------------------------------------------------

batchnumber_independent <- 16
batchnumber_collective <- 17
nobs <- 2001
burnin <- 500
dt <- 5
Nmax <- 5000
Pmax <- 2000
Tmax <- 10000
workingdir_base <- "/Users/benjamda/Dropbox/Research/Active/CR-Collectives/Simulation output/"
scriptdir <- "/Users/benjamda/Dropbox/Research/Active/CR-Collectives/R Scripts/"

rockblue <- rgb(0,0,0,maxColorValue = 255)
rockpink <- "red" #rgb(231,28,140,maxColorValue = 255)


# Source other scripts ----------------------------------------------------

setwd(scriptdir)
source("SimulateRM.R")




# Load parameter data for independent case --------------------------------

workingdir <- paste(workingdir_base, 'batch', batchnumber_independent, sep = '')
setwd(workingdir)
parafiles <- dir(pattern = "p")
nsim <- length(parafiles)

para <- read.csv(parafiles[1])
for(isim in 2:nsim){
  para <- rbind(para, read.csv(parafiles[isim]))
}

para <- para[order(para$CarryingCapacity),]
para_indep <- para
rm(parafiles)







# Get analytical trajectories ---------------------------------------------


nana <- 100
K_ana <- seq(1000,10000, len = nana)

maxP_ana_indep <- rep(NA,nana)
minP_ana_indep <- rep(NA,nana)

maxP_ana_clctv <- rep(NA,nana)
minP_ana_clctv <- rep(NA,nana)

for(i in 1:nana){

  p <- para[1,]
  p$CarryingCapacity <- K_ana[i]
  
  times <- seq(0, 10^6, len=1000)
  
  out <- SimulateRM(p, times, Nmax = Nmax, Pmax = Pmax, y = 0)
  P <-  out$Trajectories[-(1:burnin),3]
  maxP_ana_indep[i] <- max(tail(P, 1000))
  minP_ana_indep[i] <- min(tail(P, 1000))
  
  #out <- SimulateRM(p, times, Nmax = Nmax, Pmax = Pmax, y = 250)
  #P <-  out$Trajectories[-(1:burnin),3]
  #maxP_ana_clctv[i] <- max(P, 1000)
  #minP_ana_clctv[i] <- min(P, 1000)
  
  print(i/nana)
  
}



# Get simulation data independent ------------------------------------------
maxP_sim_indep <- rep(NA,nsim)
minP_sim_indep <- rep(NA,nsim)

Pmat_indep <- matrix(NA, nobs, nsim)
emat_indep <- matrix(NA, nobs, nsim)

pnt_indep <- list()

for(isim in 1:nsim){
  
  sim <- read.csv(paste("d",para$RunID[isim],".csv",sep=''))
  
  Pmat_indep[1:length(sim$PredPop),isim] <- sim$PredPop
  emat_indep[1:length(sim$PredPop),isim] <- sim$PreyInCaptureRange/sim$PreyPop/sim$PredPop
  
  P <- sim$PredPop[-(1:burnin)]
  if(!any(is.na(P)) & length(P)){
    maxP_sim_indep[isim] <- max(P)
    minP_sim_indep[isim] <- min(P)
    
    #pnt_indep[[isim]] <- which(diff(sign(diff(P, na.pad = FALSE)), na.pad = FALSE) < 0)
    
  }
  
}

Pmat_indep[is.na(Pmat_indep)] <- 0



# Load parameter data for collective case --------------------------------

workingdir <- paste(workingdir_base, 'batch', batchnumber_collective, sep = '')
setwd(workingdir)
parafiles <- dir(pattern = "p")
nsim <- length(parafiles)

para <- read.csv(parafiles[1])
for(isim in 2:nsim){
  para <- rbind(para, read.csv(parafiles[isim]))
}

para <- para[order(para$CarryingCapacity),]
para_clctv <- para
rm(parafiles)





# Get simulation data collective ------------------------------------------


maxP_sim_clctv <- rep(NA,nsim)
minP_sim_clctv <- rep(NA,nsim)

Pmat_clctv <- matrix(NA, nobs, nsim)
emat_clctv <- matrix(NA, nobs, nsim)

for(isim in 1:nsim){
  
  sim <- read.csv(paste("d",para$RunID[isim],".csv",sep=''))
  
  Pmat_clctv[1:length(sim$PredPop),isim] <- sim$PredPop
  emat_clctv[1:length(sim$PredPop),isim] <- sim$PreyInCaptureRange/sim$PreyPop/sim$PredPop
  
  P <- sim$PredPop[-(1:burnin)]
  if(!any(is.na(P)) & length(P)){
    maxP_sim_clctv[isim] <- max(P)
    minP_sim_clctv[isim] <- min(P)
  }
  
}

Pmat_clctv[is.na(Pmat_clctv)] <- 0



# Bifurcation plot --------------------------------------------------------

# Indepependent alone
quartz(h=4,w=4)
plot(0, type='n', yaxt = 'n', xaxt = 'n',
     xlim=c(2000,8000), ylim=log(c(10,3000)), 
     xlab="Resource carrying capacity", ylab="Consumer population size", cex.lab = 1.2)

axlab <- c(10,100,1000)
axloc <- log(axlab)
axis(2, at = axloc, labels = axlab, cex.axis=1.2)

axis(1, at = c(3000,5000,7000), cex.axis=1.2)

points(para_indep$CarryingCapacity, log(maxP_sim_indep),bg=rockblue,col='white',pch=22,cex=1.25)
points(para_indep$CarryingCapacity, log(minP_sim_indep),bg=rockblue,col='white',pch=22,cex=1.25)

lines(K_ana, log(maxP_ana_indep))
lines(K_ana, log(minP_ana_indep))

segments(out$KHopf, 0, out$KHopf, 10^6, col = 'grey')

#legend('bottomright', legend = "Independent", text.col = rockblue, bty = 'n')


# Collective alone
quartz(h=4,w=4)
plot(0, type='n', 
     xlim=c(2000,8000), ylim=log(c(10,3000)), yaxt = 'n', xaxt = 'n',
     xlab="Resource carrying capacity", ylab="Consumer population size",cex.lab=1.25)

axlab <- c(10,100,1000)
axloc <- log(axlab)
axis(2, at = axloc, labels = axlab,cex.axis=1.25)

axis(1, at = c(3000,5000,7000),cex.axis=1.25)

points(para_clctv$CarryingCapacity, log(maxP_sim_clctv),bg=rockpink,col='white',pch=21,cex=1.25)
points(para_clctv$CarryingCapacity, log(minP_sim_clctv),bg=rockpink,col='white',pch=21,cex=1.25)

lines(K_ana, log(maxP_ana_indep), lwd = 1)
lines(K_ana, log(minP_ana_indep), lwd = 1)

segments(out$KHopf, 0, out$KHopf, 10^6, col = 'grey')

#lines(K_ana, log(maxP_ana_clctv), lwd = 1, col = 2)
#lines(K_ana, log(minP_ana_clctv), lwd = 1, col = 2)


#legend('bottomright', legend = "Collective", text.col = rockpink, bty = 'n')


# Combined
quartz(h=4,w=4)
plot(0, type='n', 
     xlim=c(2000,8000), ylim=log(c(10,3000)), yaxt = 'n', xaxt = 'n',
     xlab="Resource carrying capacity", ylab="Consumer population size")

axlab <- c(10,100,1000)
axloc <- log(axlab)
axis(2, at = axloc, labels = axlab)

axis(1, at = c(3000,5000,7000))

points(para_indep$CarryingCapacity, log(maxP_sim_indep),pch=15,cex=0.6)
points(para_indep$CarryingCapacity, log(minP_sim_indep),pch=15,cex=0.6)

points(para_clctv$CarryingCapacity, log(maxP_sim_clctv),col=rockpink,pch=21)
points(para_clctv$CarryingCapacity, log(minP_sim_clctv),col=rockpink,pch=21)

lines(K_ana, log(maxP_ana_indep), lwd = 1)
lines(K_ana, log(minP_ana_indep), lwd = 1)

segments(out$KHopf, 0, out$KHopf, 10^6, lty=2)

#lines(K_ana, log(maxP_ana_clctv), lwd = 1, col = 2)
#lines(K_ana, log(minP_ana_clctv), lwd = 1, col = 2)



# Image of time series ----------------------------------------------------

plotimages <- FALSE
if(plotimages){

library(RColorBrewer)
heat <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

r <- range(c(range(Pmat_indep), range(Pmat_clctv)))

quartz(h=6,w=6)
z <- Pmat_indep    
z[1] <- r[1]     #standardizing range of data so image uses common colorscheme
z[2] <- r[2]
image(x=1:nobs, 
      y=para_indep$CarryingCapacity, 
      z = z, 
      xlab = "Time",
      ylab = "Resource carrying capacity",
      useRaster = T,
      col = heat(100))


quartz(h=6,w=6)
z <- Pmat_clctv    
z[1] <- r[1]     #standardizing range of data so image uses common colorscheme
z[2] <- r[2]
image(x=1:nobs, 
      y=para_clctv$CarryingCapacity, 
      z = z, 
      xlab = "Time",
      ylab = "Resource carrying capacity",
      useRaster = T,
      col = heat(100))

}

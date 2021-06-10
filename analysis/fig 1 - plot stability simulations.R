# C-R Collectives: Plot bifurcation diagram for 
# collective and non collective simulations

rm(list=ls())
graphics.off()



# Script parameters -------------------------------------------------------

nobs <- 2001
burnin <- 500
dt <- 5
Nmax <- 5000
Pmax <- 2000
Tmax <- 10000

col1 <- rgb(0,0,0)
col2 <- rgb(1,0,0)


# Source other scripts ----------------------------------------------------

source("./functions/SimulateRM.R")




# Load parameter data for independent case --------------------------------

parafiles <- dir(path = "./data/stability_independent/", 
                 pattern = "p",
                 full.names = TRUE)
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
  
  print(i/nana)
  
}



# Get simulation data independent ------------------------------------------
maxP_sim_indep <- rep(NA,nsim)
minP_sim_indep <- rep(NA,nsim)

Pmat_indep <- matrix(NA, nobs, nsim)
emat_indep <- matrix(NA, nobs, nsim)

pnt_indep <- list()

for(isim in 1:nsim){
  
  filename <- paste0("d",para$RunID[isim],".csv")
  filename <- paste0("./data/stability_independent/", filename)
  sim <- read.csv(filename)
  
  Pmat_indep[1:length(sim$PredPop),isim] <- sim$PredPop
  emat_indep[1:length(sim$PredPop),isim] <- sim$PreyInCaptureRange/sim$PreyPop/sim$PredPop
  
  P <- sim$PredPop[-(1:burnin)]
  if(!any(is.na(P)) & length(P)){
    maxP_sim_indep[isim] <- max(P)
    minP_sim_indep[isim] <- min(P)
  }
  
}

Pmat_indep[is.na(Pmat_indep)] <- 0



# Load parameter data for collective case --------------------------------

parafiles <- dir(path = "./data/stability_collective/",
                 pattern = "p",
                 full.names = TRUE)
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
  
  filename <- paste0("d",para$RunID[isim],".csv")
  filename <- paste0("./data/stability_collective/", filename)
  sim <- read.csv(filename)
  
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
plot(0, type='n', yaxt = 'n', xaxt = 'n',
     xlim=c(2000,8000), ylim=log(c(10,3000)), 
     xlab="Resource carrying capacity", ylab="Consumer population size", cex.lab = 1.2)

axlab <- c(10,100,1000)
axloc <- log(axlab)
axis(2, at = axloc, labels = axlab, cex.axis=1.2)

axis(1, at = c(3000,5000,7000), cex.axis=1.2)

points(para_indep$CarryingCapacity, log(maxP_sim_indep),bg=col1,col='white',pch=22,cex=1.25)
points(para_indep$CarryingCapacity, log(minP_sim_indep),bg=col1,col='white',pch=22,cex=1.25)

lines(K_ana, log(maxP_ana_indep))
lines(K_ana, log(minP_ana_indep))

segments(out$KHopf, 0, out$KHopf, 10^6, col = 'grey')



# Collective alone
plot(0, type='n', 
     xlim=c(2000,8000), ylim=log(c(10,3000)), yaxt = 'n', xaxt = 'n',
     xlab="Resource carrying capacity", ylab="Consumer population size",cex.lab=1.25)

axlab <- c(10,100,1000)
axloc <- log(axlab)
axis(2, at = axloc, labels = axlab,cex.axis=1.25)

axis(1, at = c(3000,5000,7000),cex.axis=1.25)

points(para_clctv$CarryingCapacity, log(maxP_sim_clctv),bg=col2,col='white',pch=21,cex=1.25)
points(para_clctv$CarryingCapacity, log(minP_sim_clctv),bg=col2,col='white',pch=21,cex=1.25)

lines(K_ana, log(maxP_ana_indep), lwd = 1)
lines(K_ana, log(minP_ana_indep), lwd = 1)

segments(out$KHopf, 0, out$KHopf, 10^6, col = 'grey')


# Combined
plot(0, type='n', 
     xlim=c(2000,8000), ylim=log(c(10,3000)), yaxt = 'n', xaxt = 'n',
     xlab="Resource carrying capacity", ylab="Consumer population size")

axlab <- c(10,100,1000)
axloc <- log(axlab)
axis(2, at = axloc, labels = axlab)

axis(1, at = c(3000,5000,7000))

points(para_indep$CarryingCapacity, log(maxP_sim_indep),pch=15,cex=0.6)
points(para_indep$CarryingCapacity, log(minP_sim_indep),pch=15,cex=0.6)

points(para_clctv$CarryingCapacity, log(maxP_sim_clctv),col=col2,pch=21)
points(para_clctv$CarryingCapacity, log(minP_sim_clctv),col=col2,pch=21)

lines(K_ana, log(maxP_ana_indep), lwd = 1)
lines(K_ana, log(minP_ana_indep), lwd = 1)

segments(out$KHopf, 0, out$KHopf, 10^6, lty=2)


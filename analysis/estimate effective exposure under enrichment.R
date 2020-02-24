# C-R Collectives: Estimate attack rate from simulation data 
# 
# Jan 4, 2019

rm(list=ls())
graphics.off()


# Source some functions needed below ----------------------------------------
setwd("/Users/benjamda/Dropbox/Research/Active/CR-Collectives/R Scripts/")
source("LoadSimulationData.R")
source("LoadBatch.R")
source("SimulateRM.R")
source("GetEps.R")



# Get epsilon correction factor for each batch ----------------------------

out <- GetEps(16)
eps_indep <- out$eps
KHopf_indep <- out$KHopf
dur_indep <- out$dur
K_indep <- out$K
KHopf0_indep <- out$KHopf0

out <- GetEps(17)
eps_clctv <- out$eps
KHopf_clctv <- out$KHopf
K_clctv <- out$K
dur_clctv <- out$dur
KHopf0_clctv <- out$KHopf0
rm(out)






# Plots -------------------------------------------------------------------

cex0 <- 1.2
cex_clctv <- cex0 * dur_clctv/max(dur_clctv)
cex_indep <- cex0 * dur_indep/max(dur_indep)

rockblue <- rgb(106,130,151,maxColorValue = 255)
rockpink <- rgb(141,50,80,maxColorValue = 255)

quartz(h=4,w=4)
plot(K_indep, KHopf_indep,
     #col = 'white',
     #bg = rockblue,
     col = 'black',
     pch = 22,
     #cex = cex_indep,
     xlim = c(2000,8000),
     ylim = c(2000,8000),
     xlab = "Carrying capacity, K",
     ylab = "Critical value, K*")

points(K_clctv, KHopf_clctv, 
       #cex = cex_clctv, 
       #col = 'white', 
       #bg = rockpink, 
       col = 'red',
       pch = 19)


abline(0,1)
abline(KHopf0_indep[1],0)

legend('topleft',col = c('red','black'), pch = c(19,22), legend = c('Collective','Independent'), bty='n')

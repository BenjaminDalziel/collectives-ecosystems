# Estimate attack rate from simulation data 
# and plot K* vs K


rm(list=ls())
graphics.off()


# Source some functions needed below ----------------------------------------
source("./functions/LoadSimulationData.R")
source("./functions/LoadBatch.R")
source("./functions/SimulateRM.R")
source("./functions/GetEps.R")



# Get epsilon correction factor for each batch ----------------------------

out <- GetEps(batchpath = "./data/stability_independent/")
eps_indep <- out$eps
KHopf_indep <- out$KHopf
dur_indep <- out$dur
K_indep <- out$K
KHopf0_indep <- out$KHopf0

out <- GetEps(batchpath = "./data/stability_collective/")
eps_clctv <- out$eps
KHopf_clctv <- out$KHopf
K_clctv <- out$K
dur_clctv <- out$dur
KHopf0_clctv <- out$KHopf0
rm(out)






# Plots -------------------------------------------------------------------

plot(K_indep, KHopf_indep,
     col = 'black',
     pch = 22,
     xlim = c(2000,8000),
     ylim = c(2000,8000),
     xlab = "Carrying capacity, K",
     ylab = "Critical value, K*")

points(K_clctv, KHopf_clctv, 
       col = 'red',
       pch = 19)


abline(0,1)
abline(KHopf0_indep[1],0)
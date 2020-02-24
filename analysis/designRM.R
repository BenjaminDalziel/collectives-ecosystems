# C-R Collectives: Tool for parameterizing a Rosenzweig-MacArthur model to get the desired shape and scale for trajectories
#
# Oct 18, 2018


rm(list=ls())
graphics.off()


# Script parameters
scriptdir <- "/Users/benjamda/Dropbox/Research/Active/CR-Collectives/R Scripts/"



# Model parameters --------------------------------------------------------

# Scaling factors:
#
# Tau, kappa and sigma control the scale of the horizontal and vertical axes and the arena length, 
# without changing the shape of the trajectories. Changing kappa changes arena length, which can then be readjusted with sigma.
#
# Epsilon varies the carrying capacity while all else remains constant, to look at effect of enrichment.

tau <- 1          #Time scaling
kappa <- 4000     #Population size scaling 
sigma <- 1/4      #Spatial scaling. Adjust this to control arena length. Capture rate is modified to compensate.
epsilon <- 1      #Enrichment factor. Carrying capacity = epsilon * kappa




# Base constants:
#
# These control the shape of the trajectories

b0 <- 0.3         #Conversion probability
r0 <- 0.03        #Birth rate constant
c0 <- 0.00006     #Capture rate constant             #0.000015
h0 <- 5           #Handling time constant
m0 <- 0.02        #Death rate constant
s0 <- 5           #Capture radius                   #10
L0 <- 0.1         #Base arena length constant



# Setup, do and plot simulations ------------------------------------------

para <- list(ConversionProbability = b0,
             BirthRate = r0 / tau, 
             CarryingCapacity = epsilon * kappa,
             CaptureRate = c0 * sigma^2 * kappa / tau,
             HandlingTime = h0 * tau,
             DeathRate = m0 / tau,
             CaptureRadius = s0,
             ArenaLength = L0 * sigma * kappa,
             InitialPreyPopulationSize = 0.25 * kappa,
             InitialPredPopulationSize = 0.025 * kappa)

tmax <- 2000 * tau
dt <- 5 * tau
Nmax <- kappa
Pmax <- 0.2 * kappa

# Load scripts
setwd(scriptdir)
source("simulateRM.R")




# Get results from analytical model
times <- seq(0,tmax,dt)
out <- SimulateRM(para, times = times, Nmax = Nmax, Pmax = Pmax)




# Plots
quartz(h=3,w=8)
par(mfrow = c(1,2))



# Plot phase plane 
plot(0, type = 'n', 
     xlim = range(out$ResourceAbundanceSequence), 
     ylim = range(out$ConsumerAbundanceSequence), 
     xlab = "Resource", 
     ylab = "Consumer")
lines(out$ConsumerNullcline,out$ConsumerAbundanceSequence,col='grey')
lines(out$ResourceAbundanceSequence,out$ResourceNullcline,col='grey')

lines(out$Trajectories[,2],out$Trajectories[,3],col = 'darkblue')



# Plot time series
plot(out$Trajectories[,1]/(60), out$Trajectories[,2], type='l', col='blue', 
     xlab = "Time (minutes)", ylab="Population size",
     ylim = c(0,para$CarryingCapacity)
     )
lines(out$Trajectories[,1]/(60), out$Trajectories[,3], col = 'darkred', lty=3)


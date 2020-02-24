# C-R Collectives: Simulate exploitation competition between two consumers using a Rosenzweig-MacArthur model for each
#
# Mar 14, 2019


#rm(list=ls())
#graphics.off()


para <- list(ConversionProbability = 0.3,
             BirthRate = 0.03,
             CarryingCapacity = 2000,
             CaptureRate = 0.015,
             MutantAdvantage = 0.9,
             HandlingTime = 5,
             DeathRate = 0.02,
             CaptureRadius = 5,
             ArenaLength = 100,
             InitialPreyPopulationSize = 2000,
             InitialPredPopulationSize = 20,
             InitialMutantPopulationSize = 10)



SimulateRM_competition <- function(para, times, Nmax = 4000, Pmax = 1000){
 
  library(deSolve)
  
  # Map simulation parameters and intial conditions to ODE parameters
  b <- para$ConversionProbability 
  r <- para$BirthRate  
  K <- para$CarryingCapacity
  c <- para$CaptureRate 
  z <- para$MutantAdvantage
  h <- para$HandlingTime
  m <- para$DeathRate 
  
  x <- pi * para$CaptureRadius^2/para$ArenaLength^2
  
  R0 <- para$InitialPreyPopulationSize
  P0 <- para$InitialPredPopulationSize - para$InitialMutantPopulationSize
  Q0 <- para$InitialMutantPopulationSize
  
  
  # ODE model
  RMfun <- function(t,X,parms){
    
    R <- X[1]
    P <- X[2]
    Q <- X[3]
    
    a <- c * x
    A <- z*a
    
    f <- a*R / (1 + h*a*R)    
    g <- A*R / (1 + h*A*R)    
    
    dR <- r*R*(1-R/K) - f*P - g*Q
    dP <- b*f*P - m*P
    dQ <- b*g*Q - m*Q
    
    res <- c(dR,dP,dQ)
    list(res)
    
  }
  
  
  # Run model
  Xstart <- c(R0,P0,Q0)
  Xout <- lsoda(Xstart,times,RMfun)
  
  
  
  # Package data and return
  out <- list(Xout)
  names(out) <- c("Trajectories")
  return(out)
   
  
}


out <- SimulateRM_competition(para, times = 1:5000)


col1 <- 'darkblue'
col2 <- 'seagreen'


quartz(h=3, w=5)
par(mar = c(4,4,2,1))
plot(out$Trajectories[,1],out$Trajectories[,2],  
     type = 'n', 
     xlab = "Time", 
     ylab = "Consumer population size",
     ylim = c(0,300))
lines(out$Trajectories[,1], out$Trajectories[,3], col = col1)
lines(out$Trajectories[,1], out$Trajectories[,4], col = col2, lty = 3)

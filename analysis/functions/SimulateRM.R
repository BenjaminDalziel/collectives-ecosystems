# Simulate Rosenzweig-MacArthur model 

SimulateRM <- function(para, times, Nmax = 4000, Pmax = 1000, y = 0){
  
  library(deSolve)
  
  
  
  # Map simulation parameters and intial conditions to ODE parameters
  b <- para$ConversionProbability 
  r <- para$BirthRate  
  K <- para$CarryingCapacity
  c <- para$CaptureRate 
  h <- para$HandlingTime
  m <- para$DeathRate 
  
  x <- pi * para$CaptureRadius^2/para$ArenaLength^2

  N0 <- para$InitialPreyPopulationSize
  P0 <- para$InitialPredPopulationSize
  
  
  
  
  
  # ODE model
  RMfun <- function(t,X,parms){
    
    N <- X[1]
    P <- X[2]
    
    a <- c * x * N / (y + N)
    f <- a*N / (1 + h*a*N)    

    dN <- r*N*(1-N/K) - f*P
    dP <- b*f*P - m*P
    
    res <- c(dN,dP)
    list(res)
    
  }
  
  
  
  
  # Run model
  Xstart <- c(N0,P0)
  Xout <- lsoda(Xstart,times,RMfun)
  
  

  
  # Nullclines
  Pbar <- function(N){
    a <- c * x * N / (y + N)
    return( r*N*(1 - N/K) * ( (1 + h*a*N)/(a*N) ) )
  }
  
  dP <- function(N){
    a <- c * x * N / (y + N)
    f <- a*N / (1 + h*a*N) 
    return(b*f - m)
  }
  
  Nbar <- function(P){
    return(rep(uniroot(dP,c(1,Nmax))$root, length(P)))
  }
  
  
  
  
  # Sequences for use in plotting 
  Nseq <- seq(0,Nmax, len = 100)
  Pseq <- seq(0,Pmax, len = 100)
  
  
  
  
  # Location of Hopf bifurcation 
  a <- c * x
  KHopf = (1 / a) * (1 / h + (2 * m) /(b - h * m))
  

  
  # Package data and return
  out <- list(Xout, Pbar(Nseq), Nbar(Pseq), Nseq, Pseq, KHopf)
  names(out) <- c("Trajectories", 
                  "ResourceNullcline", 
                  "ConsumerNullcline",
                  "ResourceAbundanceSequence", 
                  "ConsumerAbundanceSequence",
                  "KHopf")
  return(out)
  
}
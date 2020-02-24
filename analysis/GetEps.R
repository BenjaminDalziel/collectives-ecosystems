GetEps <- function(batchID){
  
  library(utils)
  batchPath <- '/Users/benjamda/Dropbox/Research/Active/CR-Collectives/Simulation output/'
  
  # Loss function for fitting eps
  obj <- function(eps, doplot=FALSE){
    
    p <- simpara
    p$CaptureRate <- eps * p$CaptureRate
    out <- SimulateRM(para = p, times =  simdata$Time)
    xhat <- log(out$Trajectories[,2])
    yhat <- log(out$Trajectories[,3])
    
    mxhat <- mean(xhat, na.rm=T)
    myhat <- mean(yhat, na.rm=T)
    sxhat <- sd(xhat, na.rm=T)
    syhat <- sd(yhat, na.rm=T)
    
    loss <- sqrt( (mx - mxhat)^2 + (my - myhat)^2 + (sx - sxhat)^2 + (sy - syhat)^2)
    
    if(doplot){
      
      plot(x,y,xlim = range(c(x,xhat)), ylim = range(c(y,yhat)))
      points(xhat, yhat, col = 2)
      
    }
    
    return(loss)
    
  }
  
  
  # Fit eps by attempting to match the first and second moments of phase density
  out <- LoadBatch(batchID = batchID, batchPath = batchPath)
  batchpara <- out$batchpara
  batchdata <- out$batchdata
  
  nsim <- nrow(batchpara)
  eps <- rep(NA,nsim)
  KHopf <- rep(NA,nsim)
  KHopf0 <- rep(NA,nsim)
  K <- rep(NA,nsim)
  dur <- rep(NA,nsim)
  
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  
  for(isim in 1:nsim){
    
    simpara <- batchpara[isim,]
    simdata <- batchdata[[isim]]
    
    dur[isim] <- nrow(simdata)
    
    x <- log(simdata$PreyPop)
    y <- log(simdata$PredPop)
    
    x[!is.finite(x)] <- 0
    y[!is.finite(y)] <- 0
    
    mx <- mean(x, na.rm=T)
    my <- mean(y, na.rm=T)
    sx <- sd(x, na.rm=T)
    sy <- sd(y, na.rm=T)
    
    out <- optimize(obj, interval = c(0.25,1.5))
    eps[isim] <- out$minimum
    
    p <- simpara
    p$CaptureRate <- eps[isim] * p$CaptureRate
    out <- SimulateRM(para = p, times =  simdata$Time)
    KHopf[isim] <- out$KHopf
    K[isim] <- simpara$CarryingCapacity
    
    p <- simpara
    out <- SimulateRM(para = p, times =  simdata$Time)
    KHopf0[isim] <- out$KHopf
    
    setTxtProgressBar(pb, isim)
    
  }
  
  return(list(eps = eps, KHopf = KHopf, KHopf0 = KHopf0, K = K, dur = dur))
  
}
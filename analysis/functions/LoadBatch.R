LoadBatch <- function(batchpath) {
  # Loads a batch of simulation data within the path
  #
  # Args:
  #   batchPath: The path to the folder associated with that batch
  #
  # Returns:
  #   A list containing  
  #     batchpara: A data frame of the parameters for each simulation in the batch
  #     batchdata: A list of dataframes containing the respective data for each simulation
  
  parafiles <- dir(path = batchpath,
                   full.names = TRUE,
                   pattern = "p")
  nsim <- length(parafiles)
  
  batchpara <- read.csv(parafiles[1])
  if(nsim>1){
    for(isim in 2:nsim){
      batchpara <- rbind(batchpara, read.csv(parafiles[isim]))
    }
  }

  batchdata <- list(NULL)
  for(isim in 1:nsim){
    
    filename <- paste0('d', batchpara$RunID[isim],'.csv')
    filename <- paste0(batchpath, filename)
    simdata <- read.csv(filename)
    simdata$ExposureRate <- simdata$PreyInCaptureRange/simdata$PredPop/simdata$PreyPop
    
    batchdata[[isim]] <- simdata
    
  }

  return(list(batchpara = batchpara, batchdata = batchdata))

}
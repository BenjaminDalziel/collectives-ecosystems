LoadSimulationData <- function(runID, runPath) {
  # Loads a simulation data with the specified runID and path
  #
  # Args:
  #   runID: A number between 0 and 1 which is the ID of the simulation run
  #   path: The path to the files associated with that simulation run
  #
  # Returns:
  #   A list containing two dataframes. 
  #   One contains time series of the state variables for that simulation run, 
  #   the other contains the parameters.
  
  datafilename <- paste0('d',runID,'.csv')
  datafilename <- paste0(runPath, datafilename)
  simdata <- read.csv(datafilename)
  
  #simdata$CollectivePred <- as.logical(simdata$CollectivePred)
  #simdata$CollectivePrey <- as.logical(simdata$CollectivePrey)
   
  simdata$ExposureRate <- simdata$PreyInCaptureRange/simdata$PredPop/simdata$PreyPop
    
  parafilename <- paste0('p',runID,'.csv')
  parafilename <- paste0(runPath, parafilename)
  simpara <- read.csv(parafilename)
  
  out <- list(simdata = simdata, simpara = simpara)

}
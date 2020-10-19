
#####################################################
#  modelforABC function:
# This function runs a stochastic SIR model and outputs 
# peak prevalence and/or prevalence at times 50 & 75 as
# targets to be used by the methods as target statistics for 
# the ABC simulations

#####################################################
library(SimInf)
#####################################################


modelforABC = function(parameters, # a vector containing parameters beta and gamma
                       times=1:75, # units of time to simulate the epidemic
                       targetTimes=c(50,75), #Time points to output prevalence
                       peakPrevalence = T){ # Logical: determines scenario 
  
  u0 = data.frame(S = c(990), # initial number of susceptible individuals
                  I = c(10), # initial number of infected individuals
                  R = c(0)) # initial number of infected individuals
  
  model <- SIR(u0, # dataframe containing initial compartmental values
               times,  
               beta = parameters[1], # beta value
               gamma = parameters[2]) # gamma value
  
  result <- run(model, 
                threads = 1) # run stochastic SIR model  
  

    prev <- prevalence(result, I~.) # compute prevalence

  targets <- prev[targetTimes,2] # save prevalence at time points 50 & 75
  
  if (peakPrevalence){return(c(targets,max(prev[,2])))} # if peakPrevalence is true, return prevalence at time points 50 & 75 and peakPrevalence
  else(return(targets)) # else ruturn prevalence at time points 50 & 75
}


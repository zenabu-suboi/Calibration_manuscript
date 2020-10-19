
#######################################################
library(gmp) #contains chooseZ() function to computes the binomial coefficient
library(dplyr) # contains sample_n function, samples rows using weights as probs
#######################################################
source("SIR_model.R") # source file containing model to be used
#######################################################

# BcSIR code for scenario 2

### Sampling stage
bayesianML3 <- function(randDraw,# number of random draws to be made from prior distributions () 
                       popsize = 1000){ # population size
  
  BML3.loglik3 <- c()# empty vector to store joint log-likelihoods
  
  betaPrior <- runif(randDraw, min = 0, max = 1)# prior distribution of beta
  gammaPrior <- runif(randDraw, min = 0, max = 0.5)# prior distribution of gamma
  
  p3 <- matrix(replicate(3, 0), randDraw, 3)# matrix of zeros with randDraw rows and 3 columns to store model outputs
  
  
  for(i in 1:randDraw){
    
    p3[i,] <- modelforABC(c(betaPrior[i], gammaPrior[i])) # run model and save output
    
    x3 <- c(0.622, 0.371, 0.677) * popsize # observed output / targets
    
    loglik3 <- c()# empty vector to store log-likelihoods
 
    ### Importance Stage:
    
    for(j in 1:length(x3)){
     
      loglik3[j] <- log(chooseZ(popsize, x3[j])) + 
        (x3[j])*log(p3[i,j]) +
        (popsize-(x3[j]))*
        log(1-p3[i,j]) # computes log-likelihood of each target statistic
    }
    
    
    BML3.loglik3[i] <- sum(loglik3) # computes joint log-likelihood of target statistics 
    
  }
  
  #Assign parameter combinations to their joint log-likelihoods
  BMLE.result3 <- data.frame(betaPrior,gammaPrior, BML3.loglik3)
  
  return(BMLE.result3)
  
}


#Run desired number of simulations, compute log-likelihoods and store results

set.seed(121) # set seed for reproducibility

  randDraw = 75000 # number of simulations
  popsize = 1000 # population size
  
  BMLE3 <- bayesianML3(randDraw, popsize)# run model to obtain log-likelihoods 
  
########################################################
  # calculate weights
  
  likelihood <- exp( BMLE3$BML3.loglik3) # compute likelihood
  
  weight3 <- likelihood/sum(likelihood) # compute weights
  
  BMLE3.weight3 <- data.frame( BMLE3$betaPrior,
                               BMLE3$gammaPrior,
                               likelihood,
                               weight3) # dataframe containing parameter combinations, likelihoods and weights 
  
  nameCols3 <- c("betaPrior", "gammaPrior", "likelihood", "weight3")
  
  colnames(BMLE3.weight3) <- nameCols3 # assign column names to dataframe
  
  ######################################################
  ### Resample Stage:
  
  #Resample to obtain desired posterior using computed weights
  
  resampleSize <- 5000 # posterior size
  
  BMLE.post.3 <- sample_n(BMLE3.weight3,
                          size = resampleSize,
                          replace = T, 
                          weight = BMLE3.weight3$weight3)# sample rows with high weight with replacement 
  

#########################################################
library(EasyABC) # contains ABC algorithms in R
#########################################################
source("SIR_model.R") # source file containing model to be used
################################################################################

# Sequential ABC Lenormand

set.seed(121) # set seed for reproducibility


ABC_seq3 <- ABC_sequential(method = "Lenormand",# sequential algorithm to implement
                         model = modelforABC, # SIR_model
                         prior = list(c("unif",0,1),# define prior distribution for beta
                                      c("unif",0,0.5)),# define prior distribution for gamma
                         nb_simul = 10000,# initial number of simulations
                         summary_stat_target = c(0.622, 0.371, 0.677), # a vector of target statistics
                         p_acc_min = 0.4, #stopping criterion of the algorithm
                         progress_bar = T,# show simulation progress
                         verbose = T) #save intermediate results to current directory



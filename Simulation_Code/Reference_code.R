
#################################################################################
#setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("SIR_model.R")

#################################################################################
##########################################################################
## runnunig rej ABC in parallel 
library(doParallel)
library(foreach)

##########################################################################
# Calculate the number of cores
no_cores <- detectCores()  # detects number of cores on computer

cl <- makeCluster(no_cores) # makes cluster with number of cores detected

registerDoParallel(cl)  # registers the cluster

ABC_rejref3 <- foreach(simulations = 1:8,# splits 5.000.000 model runs into 8 to be run by each cores
                      .combine = c,
                      .packages = c("SimInf", "EasyABC")) %dopar%
  
  ABC_rejection(model = modelforABC, 
                prior = list(c("unif",0.1,0.4),
                             c("unif",0.01,0.03)), 
                summary_stat_target = c(0.622, 0.371, 0.677),
                nb_simul = 625000, # model runs for each core
                tol = 1,
                progress_bar = T,
                verbose = T) 

stopImplicitCluster() # stops cluster and reverts back to using only one core/ exit cluster

################################################################################
################################################################################
# check compute time

computime3 =  ABC_rejref3$computime # computes time taken by each core to run simulations 


##################################################################################
#gathering desired output and save 

gather_params3 <- ABC_rejref3[seq(1, length(ABC_rejref3), 7)] # Gathers all parameter combinations

all_params3 <- (rbind(gather_params3[[1]], 
                               gather_params3[[2]],
                               gather_params3[[3]],
                               gather_params3[[4]],
                               gather_params3[[5]],
                               gather_params3[[6]],
                               gather_params3[[7]],
                               gather_params3[[8]])) # binds parameters as one matrix


#gather_params[[1]]

saveRDS(all_params3, "ref3_params.rds") # save as R data set

###################################################################################
###################################################################################
# gather stats 

gather_stats3 <- ABC_rejref3[seq(2, length(ABC_rejref3), 7)] # Gathers all summary statistics 

all_stats3 <- (rbind(gather_stats3[[1]], 
                              gather_stats3[[2]],
                              gather_stats3[[3]],
                              gather_stats3[[4]],
                              gather_stats3[[5]],
                              gather_stats3[[6]],
                              gather_stats3[[7]],
                              gather_stats3[[8]])) # binds summary statistics as one matrix


#gather_params[[1]]

saveRDS(all_stats3, "ref3_stats.rds")# save stats as R data set

########################################################
# retain desired posterior

ref3 <- abc(target = c(0.622, 0.371, 0.677),
            param = all_params3,
            sumstat = all_stats3,
            tol = 0.001,
            method = "rejection") 


saveRDS(object = ref3$unadj.values , file = 'refposterior3.rds')




require(IMIS)
require(SimDesign)
require(MASS)
require(mvtnorm)
require(SimInf)
require(faraway)
library(ggplot2)
library(cowplot)
library(microbenchmark)

sample.prior <- function(n){ # samples prior as a matrix 
  r1 <- logit(runif(n,0,1))
  r2 <- logit(runif(n,0,0.5))
  r3 <- as.matrix(cbind(r1,r2))
  colnames(r3) <- c("beta","gamma")
  return(r3)
}	

prior <- function(theta){ # gives the density of each row of prior combination
  r1 <- dunif(ilogit(theta[1,1]),0,1)
  r2 <- dunif(ilogit(theta[1,2]),0,0.5)
  r3 <- (as.numeric(r1*r2))
  return(r3)
}

likelihood <- function (theta) {
  likvect <- numeric()
  for(i in 1:nrow(theta)){
    liksav <- 0
    n=1
    u0 <- data.frame(S = rep(990, n),
                     I = rep(10, n), 
                     R = rep(0, n))
    
    tspan <- seq(from = 1, to = 75, by = 1)
    
    thetloc <- ilogit(theta)
    
    model <- SIR(u0 = u0, 
                 tspan = tspan,
                 beta = thetloc[i,1], 
                 gamma = thetloc[i,2])
    #model runtime
   # time <- microbenchmark(result <- run(model = model), # runs the SIR model and outputs results
    #                       times = 1) # measures time in nanoseconds (/10^9)
    
    #writeLines( as.character(time$time),
     #           record_time_imis3, sep = "\n") 
    
    result <- run(model = model)
                  
    dfresult <- trajectory(model = result, node = 1)
    
    model1 <- dfresult[dfresult$time==50,"I"]/1000 # prev at 50
    model2 <- dfresult[dfresult$time==75,"I"]/1000 # prev at 75
    model3 <- max(dfresult$I)/1000 # peak prev
    
    liksav <- numeric()
    ntarget <- 1000
    ktarget1 <- 622
    liksav1 <- dbinom(x= ktarget1, 
                     size= ntarget,
                     prob= model1, 
                     log=T) # likelihood at time 50
    ktarget2 <- 371
    liksav2 <- liksav1 + dbinom(x= ktarget2,
                              size= ntarget, 
                              prob= model2,
                              log=T) # joint likelihood 1
    ktarget3 <- 677
    liksav <-  liksav2 + dbinom(x= ktarget3,
                                        size = ntarget, 
                                        prob = model3,
                                        log=T) # joint likelihood
    liksav <- exp(liksav)
    
    likvect <- append(likvect, 
                      liksav, 
                      after= length(likvect)) # store in a vecto
  }
  return(likvect)
}

# record model runtime
#record_time_imis3 <- file("mytime_imis_3targets.txt")
#open(record_time_imis3, "w")

set.seed(232)

microbenchmark(
  result3b = IMIS(3000, 5000, 16, 0), times = 1)# yields 
# 75000 model runs

#close(record_time_imis3) ## close file connection
#unlink(record_time_imis3)


  #imis stats
  imis3b_stats <- as.data.frame((result3b$stat))
  saveRDS(imis3b_stats, file = "imis3b_stats.rds" )
 # readRDS("imis3b_stats.rds")

# plot IMIS posterior
imis_post3b <- as.data.frame(ilogit(result3b$resample))
saveRDS(imis_post3b, file = "imis3b_posterior.rds" )

imis3b_plot <- ggplot(imis_post3b,
                    aes(x=imis_post3b$beta, 
                        y=imis_post3b$gamma)) +
  
  geom_point( size = 0.5, alpha = 0.1) +
  
  ggtitle("S2_IMIS") +
  
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))

plot_grid(
imis2b_plot,
imis3b_plot)

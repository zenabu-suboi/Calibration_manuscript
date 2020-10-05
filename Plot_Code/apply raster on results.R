
library(raster)

#Scenario 1 (two targets)

### raster results for rejection 2 targets

ras.rej2 <- rasterize(mydat_2targets_rej$beta,
                    mydat_2targets_rej$gamma)
# plot(ras.rej2, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="Rejection ABC")
ras.rej2.mat <- as.matrix(ras.rej2)
sum(ras.rej2.mat)

### raster results for Sequential 2 targets

ras.seq2 <- rasterize(mydat_2targets_seq$beta,
                    mydat_2targets_seq$gamma)
# plot(ras.seq2, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="Sequential ABC")
ras.seq2.mat <- as.matrix(ras.seq2)
sum(ras.seq2.mat)


### raster results for abcsmc 2 targets

ras.abcsmc2 <- rasterize(mydat_2targets_abcsmc$beta,
                      mydat_2targets_abcsmc$gamma)
# plot(ras.abcsmc2, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="abcsmcuential ABC")
ras.abcsmc2.mat <- as.matrix(ras.abcsmc2)
sum(ras.abcsmc2.mat)
### raster results for bmle 2 targets

ras.BC_SIR2 <- rasterize(mydat_2targets_BC_SIR$beta,
                    mydat_2targets_BC_SIR$gamma)
# plot(ras.bmle2, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="BMLE")
ras.BC_SIR2.mat <- as.matrix(ras.BC_SIR2)
sum(ras.BC_SIR2.mat)


### raster results for imis 2 targets

ras.imis2 <- rasterize(mydat_2targets_imis$beta,
                      mydat_2targets_imis$gamma)
# plot(ras.imis2, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="imis ABC")
ras.imis2.mat <- as.matrix(ras.imis2)
sum(ras.imis2.mat)

##############################################################
# scenario 2 , 3 targets

#par(mfrow=c(2,2))

### raster results for rejection 3 targets

ras.rej3 <- rasterize(mydat_3targets_rej$beta,
                    mydat_3targets_rej$gamma)
# plot(ras.rej3, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="Rejection ABC")
ras.rej3.mat <- as.matrix(ras.rej3)
sum(ras.rej3.mat)

### raster results for Sequential 3 targets

ras.seq3 <- rasterize(mydat_3targets_seq$beta,
                    mydat_3targets_seq$gamma)
# plot(ras.seq3, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="Sequential ABC")
ras.seq3.mat <- as.matrix(ras.seq3)
sum(ras.seq3.mat)


### raster results for abcsmc 3 targets

ras.abcsmc3 <- rasterize(mydat_3targets_abcsmc$beta,
                      mydat_3targets_abcsmc$gamma)
# plot(ras.abcsmc3, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="abcsmc")
ras.abcsmc3.mat <- as.matrix(ras.abcsmc3)
sum(ras.abcsmc3.mat)
### raster results for bmle 3 targets

ras.BC_SIR3 <- rasterize(mydat_3targets_BC_SIR$beta,
                     mydat_3targets_BC_SIR$gamma)
# plot(ras.BC_SIR3, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="BMLE")
ras.BC_SIR3.mat <- as.matrix(ras.BC_SIR3)
sum(ras.BC_SIR3.mat)

### raster results for imis 3 targets
ras.imis3 <- rasterize(mydat_3targets_imis$beta,
                      mydat_3targets_imis$gamma)
# plot(ras.imis3, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="imis")
ras.imis3.mat <- as.matrix(ras.imis3)
sum(ras.imis3.mat)


##############################################################################
######################################################################

# ref sample 
#par(mfrow=c(1,2))

ras.ref2 <- rasterize(mydat_2targets_ref$beta,
                     mydat_2targets_ref$gamma)
# plot(ras.ref2, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="Reference posterior")
ras.ref2.mat <- as.matrix(ras.ref2)
sum(ras.ref2.mat)


ras.ref3 <- rasterize(mydat_3targets_ref$beta,
                    mydat_3targets_ref$gamma)
# plot(ras.ref3, 
#      col=grey(100:1/100), 
#      useRaster=F,
#      main="Reference posterior")
ras.ref3.mat <- as.matrix(ras.ref3)
sum(ras.ref3.mat)



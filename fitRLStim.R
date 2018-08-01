rm(list=ls())
#install.packages('R2jags')
library('R2jags')
setwd('Insert folder you want to save files to here') 
bugsdir <- 'Insert folder containing model files here' 

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------ Hierarchical Estimation ------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# --------------------------------------------------------- #
# --------------------------------------------------------- #
# ---------------- Hierarchical RL Model ------------------ #
# --------------------------------------------------------- #
# --------------------------------------------------------- #

# ----------------------------------------------- #
# ----------------------------------------------- #
# ---------- BUGS model specification ----------- #
# ----------------------------------------------- #
# ----------------------------------------------- #

nStim <- k  # Number of picture-pseudoword associations
nRep <- n  # Number of repetitions per association
nPart <- N # Number of participants

data <- list("nRep","nStim","nPart","C","R") # Data input for JAGS

# List of sampling starting values to give to JAGS
myinits <- list(
  list(eta = rep(.1,N), betaAcc = rep(.1,N)),  # Starting values chain 1
  list(eta = rep(.5,N), betaAcc = rep(.5,N)),  # Starting values chain 2
  list(eta = rep(.9,N), betaAcc = rep(.9,N))  # Starting values chain 3
)

parameters <- c("eta","beta","etaGroupMean","etaGroupPrecision","betaGroupMean","betaGroupPrecision")  # Parameters saved to check sampling results

time <- proc.time()  # Keep track of the sampling time

# Save all representative samples ((n.iter*(n.chains - n.burnin))/n.thin) from the posterior distribution
samples <- jags(data, inits=myinits, parameters,
                model.file ="modelHierarchicalRLStim.txt",
                n.chains=3, n.iter=10000, n.burnin=10000/2, n.thin=10,
                DIC=T, working.directory=bugsdir)

(proc.time() - time)[3]/60  # Check how long the sampling took

# ----------------------------------------------- #
# ----------------------------------------------- #
# ------------ Results: BUGS output ------------- #
# ----------------------------------------------- #
# ----------------------------------------------- #

# Save samples in separate objects
etaEst <- samples$BUGSoutput$sims.list$eta
betaEst <- samples$BUGSoutput$sims.list$beta
etaGroupMeanEst <- samples$BUGSoutput$sims.list$etaGroupMean
etaGroupPrecisionEst <- samples$BUGSoutput$sims.list$etaGroupPrecision
betaGroupMeanEst <- samples$BUGSoutput$sims.list$betaGroupMean
betaGroupPrecisionEst <- samples$BUGSoutput$sims.list$betaGroupPrecision

# Check convergence of sampling chains
#range(samples$BUGSoutput$summary[,'Rhat'])
#length(which(samples$BUGSoutput$summary[,'Rhat'] >= 1.1)) # Check how many sampling chains did not converge
#which(samples$BUGSoutput$summary[,'Rhat'] >= 1.1)  # Check which chains did not converge

# Compute the estimated parameter values over all samples
etaEstim <- apply(etaEst,2,mean)
betaEstim <- apply(betaEst,2,mean)
etaGroupMeanEstim <- mean(etaGroupMeanEst)
etaGroupPrecisionEstim <- mean(etaGroupPrecisionEst)
betaGroupMeanEstim <- mean(betaGroupMeanEst)
betaGroupPrecisionEstim <- mean(betaGroupPrecisionEst)

DICHier <- samples$BUGSoutput$DIC

# Only use this 'save output' when you want to save the hierchical estimation results in separate file
# Save output
#save.image(file=paste0('SimulationOutput',nIter,'HierN',nPart,'n',nRep,'k',nStim,'.RData'))
#rm(samples,data,myinits,parameters,time,nStim,nRep,nPart,etaEst,betaEst,etaGroupMeanEst,etaGroupPrecisionEst,betaGroupMeanEst,betaGroupPrecisionEst,etaEstim,betaEstim,etaGroupMeanEstim,etaGroupPrecisionEstim,betaGroupMeanEstim,betaGroupPrecisionEstim)


# --------------------------------------------------------- #
# --------------------------------------------------------- #
# --------------- Hierarchical PSRL Model ----------------- #
# --------------------------------------------------------- #
# --------------------------------------------------------- #

# ----------------------------------------------- #
# ----------------------------------------------- #
# ---------- BUGS model specification ----------- #
# ----------------------------------------------- #
# ----------------------------------------------- #

nStim <- k  # Number of picture-pseudoword associations
nRep <- n  # Number of repetitions per association
nPart <- N # Number of participants

data2 <- list("nRep","nStim","nPart","C","R") # Data input for JAGS

# List of sampling starting values to give to JAGS
myinits2 <- list(
  list(eta = rep(.1,N), betaAcc = rep(.1,N), pi = .1), # Starting values chain 1
  list(eta = rep(.5,N), betaAcc = rep(.5,N), pi = .5), # Starting values chain 2
  list(eta = rep(.9,N), betaAcc = rep(.9,N), pi = .9)   # Starting values chain 3
)

parameters2 <- c("strategy","eta","beta","pi","etaGroupMean","etaGroupPrecision","betaGroupMean","betaGroupPrecision")  # Parameters saved to check sampling results

time <- proc.time()  # Keep track of the sampling time

# Save all representative samples ((n.iter*(n.chains - n.burnin))/n.thin) from the posterior distribution
samples2 <- jags(data2, inits=myinits2, parameters2,
                 model.file ="modelHierarchicalPSRLStim.txt",
                 n.chains=3, n.iter=10000, n.burnin=10000/2, n.thin=10,
                 DIC=T, working.directory=bugsdir)

(proc.time() - time)[3]/60  # Check how long the sampling took

# --------------------------------------------------------- #
# --------------------------------------------------------- #
# ----------------- Results: BUGS output ------------------ #
# --------------------------------------------------------- #
# --------------------------------------------------------- #

# Save samples in separate objects
etaEst2 <- samples2$BUGSoutput$sims.list$eta
betaEst2 <- samples2$BUGSoutput$sims.list$beta
piEst2 <- samples2$BUGSoutput$sims.list$pi
etaGroupMeanEst2 <- samples2$BUGSoutput$sims.list$etaGroupMean
etaGroupPrecisionEst2 <- samples2$BUGSoutput$sims.list$etaGroupPrecision
betaGroupMeanEst2 <- samples2$BUGSoutput$sims.list$betaGroupMean
betaGroupPrecisionEst2 <- samples2$BUGSoutput$sims.list$betaGroupPrecision
strategyEst2 <- samples2$BUGSoutput$sims.list$strategy

# Check convergence of sampling chains
#range(samples2$BUGSoutput$summary[,'Rhat'])
#length(which(samples2$BUGSoutput$summary[,'Rhat'] >= 1.1)) # Check how many sampling chains did not converge
#which(samples2$BUGSoutput$summary[,'Rhat'] >= 1.1)  # Check which sampling chains did not converge

# Compute the estimated LR and Decisiveness over all samples
etaEstim2 <- apply(etaEst2,2,mean)
betaEstim2 <- apply(betaEst2,2,mean)
piEstim2 <- apply(piEst2,2,mean)
etaGroupMeanEstim2 <- mean(etaGroupMeanEst2)
etaGroupPrecisionEstim2 <- mean(etaGroupPrecisionEst2)
betaGroupMeanEstim2 <- mean(betaGroupMeanEst2)
betaGroupPrecisionEstim2 <- mean(betaGroupPrecisionEst2)
strategyEstim2 <- sapply(list(strategyFirstStim=apply(strategyEst2[,,1],2,mean),
                      strategySecondStim=apply(strategyEst2[,,2],2,mean),
                      strategyThirdStim=apply(strategyEst2[,,3],2,mean),
                      strategyFourthStim=apply(strategyEst2[,,4],2,mean)),round)

DICHierPSRL <- samples2$BUGSoutput$DIC
#data.frame(HierRL=DICHier,HierPSRL=DICHierPSRL,row.names='DIC')

# Only use this 'save output' when you want to save the hierarchical PSRL estimation results in separate file 
# Save output
#save.image(file=paste0('SimulationOutput',nIter,'HierPSRLN',nPart,'n',nRep,'k',nStim,'.RData'))
#rm(samples2,data2,myinits2,parameters2,time,nStim,nRep,nPart,etaEst2,betaEst2,piEst2,etaGroupMeanEst2,etaGroupPrecisionEst2,betaGroupMeanEst2,betaGroupPrecisionEst2,piGroupMeanEst2,piGroupPrecisionEst2,strategyEst2,etaEstim2,betaEstim2,piEstim2,etaGroupMeanEstim2,etaGroupPrecisionEstim2,betaGroupMeanEstim2,betaGroupPrecisionEstim2,piGroupMeanEstim2,piGroupPrecisionEstim2,strategyEstim2)

# Save output
save.image(file=paste0('SimulationOutput',nIter,'N',nPart,'n',nRep,'k',nStim,'.RData'))
}

rm(list=ls())
#install.packages('R2jags')
library('R2jags')
setwd('Insert folder you want to save files to here') 
bugsdir <- 'Insert folder containing model files here' 

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------- Non-hierarchical Estimation ---------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# --------------------------------------------------------- #
# --------------------------------------------------------- #
# ------------------- Simple RL Model --------------------- #
# --------------------------------------------------------- #
# --------------------------------------------------------- #

# ----------------------------------------------- #
# ----------------------------------------------- #
# ---------- BUGS model specification ----------- #
# ----------------------------------------------- #
# ----------------------------------------------- #

nStim <- 1  # Number of picture-pseudoword associations
nRep <- n*k  # Number of repetitions per association
nPart <- N # Number of participants

data <- list("nRep","nPart","C","R") # Data input for JAGS

# List of sampling starting values to give to JAGS
myinits <- list(
  list(eta = rep(.1,N), beta = rep(1,N)),  # Starting values chain 1
  list(eta = rep(.5,N), beta = rep(5,N)),  # Starting values chain 2
  list(eta = rep(.9,N), beta = rep(9,N))  # Starting values chain 3
)

parameters <- c("eta","beta")  # Parameters saved to check sampling results

time <- proc.time()  # Keep track of the sampling time

# Save all representative samples ((n.iter*(n.chains - n.burnin))/n.thin) from the posterior distribution
samples <- jags(data, inits=myinits, parameters,
                model.file ="modelSimpleRL.txt",
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

# Check convergence of sampling chains
#range(samples$BUGSoutput$summary[,'Rhat'])
#length(which(samples$BUGSoutput$summary[,'Rhat'] >= 1.1)) # Check how many sampling chains did not converge
#which(samples$BUGSoutput$summary[,'Rhat'] >= 1.1)  # Check which chains did not converge

# Compute the estimated parameter values over all samples
etaEstim <- apply(etaEst,2,mean)
betaEstim <- apply(betaEst,2,mean)

DICSimple <- samples$BUGSoutput$DIC

# Only use this 'save output' when you want to save the simple estimation results in separate file
# Save output
#save.image(file=paste0('SimulationOutput',nIter,'SimpleN',nPart,'n',nRep,'k',nStim,'.RData'))
#rm(samples,data,myinits,parameters,time,nStim,nRep,nPart,etaEst,betaEst,etaEstim,betaEstim)


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

nStim <- 1  # Number of picture-pseudoword associations
nRep <- n*k  # Number of repetitions per association
nPart <- N # Number of participants

data2 <- list("nRep","nPart","C","R") # Data input for JAGS

# List of sampling starting values to give to JAGS
myinits2 <- list(
  list(eta = rep(.1,N), betaAcc = rep(.1,N)),  # Starting values chain 1
  list(eta = rep(.5,N), betaAcc = rep(.5,N)),  # Starting values chain 2
  list(eta = rep(.9,N), betaAcc = rep(.9,N))  # Starting values chain 3
)

parameters2 <- c("eta","beta","etaGroupMean","etaGroupPrecision","betaGroupMean","betaGroupPrecision")  # Parameters saved to check sampling results

time <- proc.time()  # Keep track of the sampling time

# Save all representative samples ((n.iter*(n.chains - n.burnin))/n.thin) from the posterior distribution
samples2 <- jags(data2, inits=myinits2, parameters2,
                model.file ="modelHierarchicalRL.txt",
                n.chains=3, n.iter=10000, n.burnin=10000/2, n.thin=10,
                DIC=T, working.directory=bugsdir)

(proc.time() - time)[3]/60  # Check how long the sampling took

# ----------------------------------------------- #
# ----------------------------------------------- #
# ------------ Results: BUGS output ------------- #
# ----------------------------------------------- #
# ----------------------------------------------- #

# Save samples in separate objects
etaEst2 <- samples2$BUGSoutput$sims.list$eta
betaEst2 <- samples2$BUGSoutput$sims.list$beta
etaGroupMeanEst2 <- samples2$BUGSoutput$sims.list$etaGroupMean
etaGroupPrecisionEst2 <- samples2$BUGSoutput$sims.list$etaGroupPrecision
betaGroupMeanEst2 <- samples2$BUGSoutput$sims.list$betaGroupMean
betaGroupPrecisionEst2 <- samples2$BUGSoutput$sims.list$betaGroupPrecision

# Check convergence of sampling chains
#range(samples2$BUGSoutput$summary[,'Rhat'])
#length(which(samples2$BUGSoutput$summary[,'Rhat'] >= 1.1)) # Check how many sampling chains did not converge
#which(samples2$BUGSoutput$summary[,'Rhat'] >= 1.1)  # Check which chains did not converge

# Compute the estimated LR and Decisiveness over all samples
etaEstim2 <- apply(etaEst2,2,mean)
betaEstim2 <- apply(betaEst2,2,mean)
etaGroupMeanEstim2 <- mean(etaGroupMeanEst2)
etaGroupPrecisionEstim2 <- mean(etaGroupPrecisionEst2)
betaGroupMeanEstim2 <- mean(betaGroupMeanEst2)
betaGroupPrecisionEstim2 <- mean(betaGroupPrecisionEst2)

DICHier <- samples2$BUGSoutput$DIC

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

nStim <- 1  # Number of picture-pseudoword associations
nRep <- n*k  # Number of repetitions per association
nPart <- N # Number of participants

data3 <- list("nRep","nPart","C","R") # Data input for JAGS

# List of sampling starting values to give to JAGS
myinits3 <- list(
  list(eta = rep(.1,N), betaAcc = rep(.1,N), pi = .1), # Starting values chain 1
  list(eta = rep(.5,N), betaAcc = rep(.5,N), pi = .5), # Starting values chain 2
  list(eta = rep(.9,N), betaAcc = rep(.9,N), pi = .9)   # Starting values chain 3
)

parameters3 <- c("strategy","eta","beta","pi","etaGroupMean","etaGroupPrecision","betaGroupMean","betaGroupPrecision")  # Parameters saved to check sampling results

time <- proc.time()  # Keep track of the sampling time

# Save all representative samples ((n.iter*(n.chains - n.burnin))/n.thin) from the posterior distribution
samples3 <- jags(data3, inits=myinits3, parameters3,
                 model.file ="modelHierarchicalPSRL.txt",
                 n.chains=3, n.iter=10000, n.burnin=10000/2, n.thin=10,
                 DIC=T, working.directory=bugsdir)

(proc.time() - time)[3]/60  # Check how long the sampling took

# --------------------------------------------------------- #
# --------------------------------------------------------- #
# ----------------- Results: BUGS output ------------------ #
# --------------------------------------------------------- #
# --------------------------------------------------------- #

# Save samples in separate objects
etaEst3 <- samples3$BUGSoutput$sims.list$eta
betaEst3 <- samples3$BUGSoutput$sims.list$beta
piEst3 <- samples3$BUGSoutput$sims.list$pi
etaGroupMeanEst3 <- samples3$BUGSoutput$sims.list$etaGroupMean
etaGroupPrecisionEst3 <- samples3$BUGSoutput$sims.list$etaGroupPrecision
betaGroupMeanEst3 <- samples3$BUGSoutput$sims.list$betaGroupMean
betaGroupPrecisionEst3 <- samples3$BUGSoutput$sims.list$betaGroupPrecision
strategyEst3 <- samples3$BUGSoutput$sims.list$strategy

# Check convergence of sampling chains
#range(samples3$BUGSoutput$summary[,'Rhat'])
#length(which(samples3$BUGSoutput$summary[,'Rhat'] >= 1.1)) # Check how many sampling chains did not converge
#which(samples3$BUGSoutput$summary[,'Rhat'] >= 1.1)  # Check which sampling chains did not converge

# Compute the estimated parameter values over all samples
etaEstim3 <- apply(etaEst3,2,mean)
betaEstim3 <- apply(betaEst3,2,mean)
piEstim3 <- apply(piEst3,2,mean)
etaGroupMeanEstim3 <- mean(etaGroupMeanEst3)
etaGroupPrecisionEstim3 <- mean(etaGroupPrecisionEst3)
betaGroupMeanEstim3 <- mean(betaGroupMeanEst3)
betaGroupPrecisionEstim3 <- mean(betaGroupPrecisionEst3)
strategyEstim3 <- round(apply(strategyEst3,2,mean))
#length(which(Z!=strategyEstim3))

DICHierPSRL <- samples3$BUGSoutput$DIC
#data.frame(SimpleRL=DICSimple,HierRL=DICHier,HierPSRL=DICHierPSRL,row.names='DIC')

# Only use this 'save output' when you want to save the hierarchical PSRL estimation results in separate file 
# Save output
#save.image(file=paste0('SimulationOutput',nIter,'HierPSRLN',nPart,'n',nRep,'k',nStim,'.RData'))
#rm(samples3,data3,myinits3,parameters3,time,nStim,nRep,nPart,etaEst3,betaEst3,piEst3,etaGroupMeanEst3,etaGroupPrecisionEst3,betaGroupMeanEst3,betaGroupPrecisionEst3,piGroupMeanEst3,piGroupPrecisionEst3,strategyEst3,etaEstim3,betaEstim3,piEstim3,etaGroupMeanEstim3,etaGroupPrecisionEstim3,betaGroupMeanEstim3,betaGroupPrecisionEstim3,piGroupMeanEstim3,piGroupPrecisionEstim3,strategyEstim3)

# Save output
save.image(file=paste0('SimulationOutput',nIter,'N',nPart,'n',nRep,'k',nStim,'.RData'))
}

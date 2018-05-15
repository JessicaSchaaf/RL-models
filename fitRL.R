rm(list=ls())
#install.packages('R2jags')
library('R2jags')
setwd('Insert folder you want to save files to here') 
bugsdir <- 'Insert folder containing model files here' 

# --------------------------------------------------------- #
# --------------------------------------------------------- #
# --------------- Simulation of Data Sets ----------------- #
# --------------------------------------------------------- #
# --------------------------------------------------------- #

for(nIter in 1:100){

# ------------------------------------ #
# ----- Data set characteristics ----- #
# ------------------------------------ #

k <- 4  # Number of picture-pseudoword associations
n <- 24*3  # Number of repetitions per association [repetitions per block * number of blocks]
N <- 30  # Number of participants

# ------------- Initial choice probability -------------- #
pC <- .5  # Prior probability to choose response option 2

# ----- Reward probabilities of both choice options ----- #
pLow <- .2 # Lowest reward probability
pHigh <- .8 # Highest reward probability
# To check whether code resembles learning change pLow to 0 and pHigh to 1

# ----- Sample person parameters ----- #
etaGroupMeanTrue <- mean(c(0.47,0.40,0.43,0.43))  # Group-level mean: Gershman (2015) Table 2 Model 1 
etaGroupSETrue <- sd(c(0.47,0.40,0.43,0.43))  # Group-level se: Gershman (2015) Table 2 Model 1
etaGroupSDTrue <- etaGroupSETrue*sqrt(166)  # Group-level sd: Gershman (2015) N = 166
etaGroupPrecisionTrue <- 1/(etaGroupSDTrue^2)
etaTrue <- rbeta(N,etaGroupMeanTrue*etaGroupPrecisionTrue,etaGroupPrecisionTrue*(1 - etaGroupMeanTrue))  # Individual-level LR
#range(etaTrue)  # Check range of LR

betaGroupMeanTrue <- mean(c(3.24,2.82,4.32,3.04))/10  # Group-level mean: Gershman(2015) Table 2 Model 1
betaGroupPrecisionTrue <- etaGroupPrecisionTrue
betaTrueUntrans <- rbeta(N,betaGroupMeanTrue*betaGroupPrecisionTrue,betaGroupPrecisionTrue*(1 - betaGroupMeanTrue))  # Individual-level Decisiveness on [0,1]
betaTrue <- 10*betaTrueUntrans  # Individual-level Decisiveness on [0,10]
#range(betaTrue)  # Check range of Decisiveness

piGroupMeanTrue <- (0.75+1)/2
piGroupPrecisionTrue <- 8
piTrue <- rbeta(N,piGroupMeanTrue*piGroupPrecisionTrue,piGroupPrecisionTrue*(1-piGroupMeanTrue))  # Individual-level Strategy Probability
#range(piTrue)  # Check range of Strategy Probability

# Check individual-level distribution
#paste0('Beta(',round((etaGroupMeanTrue*etaGroupPrecisionTrue),2),',',round(((1-etaGroupMeanTrue)*etaGroupPrecisionTrue),2),')')
#paste0('Beta(',round((betaGroupMeanTrue*betaGroupPrecisionTrue),2),',',round(((1-betaGroupMeanTrue)*betaGroupPrecisionTrue),2),')')
#paste0('Beta(',round((piGroupMeanTrue*piGroupPrecisionTrue),2),',',round(((1-piGroupMeanTrue)*piGroupPrecisionTrue),2),')')

# ---------------------------- #
# ----- Create data sets ----- #
# ---------------------------- #

# Create strategy matrix with strategy probability pi
Z <- numeric(N)
for(j in 1:N){
  Z[j] <- rbinom(1,1,1)  # Use this if each participant learns
  #Z[j] <- rbinom(1,1,0)  # Use this if each participant guesses
  #Z[j] <- rbinom(1,1,prob=piTrue[j])  # Use this if each participant either learns or guesses
}

# Create matrix with reward probability per trial
pROption1 <- sample(c(pLow,pHigh),1,.5)
pROption2 <- ifelse(pROption1 == pLow, pROption2 <- pHigh, pROption2 <- pLow)

# Create matrix of 'correct' responses (i.e. response with maximum probability of a reward)
correct <- ifelse(pROption1 > pROption2, 0, 1)

# Create empty matrices to fill
C <- matrix(NA,N,k*n)  # Response matrix
R <- matrix(NA,N,k*n)  # Matrix of obtained rewards
V1 <- matrix(NA,N,k*n)  # Matrix of reward estimates of response option 1  
V2 <- matrix(NA,N,k*n)  # Matrix of reward estimates of response option 2
delta <- matrix(NA,N,k*n)  # Matrix of prediction errors
pChoice <- matrix(NA,N,k*n)  # Matrix of choice probabilities

# Set starting values for all parameters
V1[,1] <- V2[,1] <- .5
pChoice[,1] <- .5

for(j in 1:N){  
  C[j,1] <- rbinom(1,1,pC)  # Sample first response per stimulus
  ifelse(C[j,1] == 0, R[j,1] <- rbinom(1,1,pROption1), R[j,1] <- rbinom(1,1,pROption2))  # Sample first reward
  ifelse(C[j,1] == 0, delta[j,1] <- R[j,1] - V1[j,1], delta[j,1] <- R[j,1] - V2[j,1])  # Compute first prediction error
}

# Create responses and rewards
for(i in 2:(n*k)){
  for(j in 1:N){
    if(Z[j] == 0){  # If people guess ...
      C[j,i] <- rbinom(1,1,pC)
      ifelse(C[j,i] == 0, R[j,i] <- rbinom(1,1,pROption1), R[j,i] <- rbinom(1,1,pROption2))
      pChoice[j,i] <- pC
    } else if(Z[j] == 1){  # If people learn ...
        ifelse(C[j,(i-1)] == 0, V1[j,i] <- V1[j,(i-1)] + etaTrue[j]*delta[j,(i-1)], V1[j,i] <- V1[j,(i-1)]) 
        ifelse(C[j,(i-1)] == 0, V2[j,i] <- V2[j,(i-1)], V2[j,i] <- V2[j,(i-1)] + etaTrue[j]*delta[j,(i-1)])
        pChoice[j,i] <- 1/(1 + exp(-1*betaTrue[j]*(V2[j,i] - V1[j,i])))
        C[j,i] <- rbinom(1,1,pChoice[j,i])
        ifelse(C[j,i] == 0, R[j,i] <- rbinom(1,1,pROption1), R[j,i] <- rbinom(1,1,pROption2)) 
        ifelse(C[j,i] == 0, delta[j,i] <- R[j,i] - V1[j,i], delta[j,i] <- R[j,i] - V2[j,i])
      }
  }
}

#sum(is.na(V1))  # If this gives 0 each participant learned; if this gives n*k*N - N each participant guessed

# Plot the cumulative sum of rewards against the items for first n items
#plot(cumsum(R[1,]),type='l',xlim=c(0,n),ylim=c(0,n),las=1,bty='n',xlab='Item',ylab='Cumulative Sum')
#for(j in 2:N){
#  lines(cumsum(R[j,]),col=(j+1))  # Lines should be steeper at the right side of the x-axis
#}

# Plot probability to choose highest reward option (resembles learning curve should move to 0 if pROption1 > pROption2 and the other way around)
#plot(pChoice[1,],type='l',xlim=c(0,n),ylim=c(0,1),las=1,bty='n',xlab='Item',ylab='P Correct')
#for(j in 2:N){
#  lines(pChoice[j,],col=(j+1))
#}

# Create matrix with performance - number of 'correct' decisions.
perf <- matrix(NA,N,k*n)
for(i in 1:(n*k)){  # See how well participants perform by checking whether they chose the choice option with the highest reward probability
  for(j in 1:N){
    ifelse(C[j,i] == correct, perf[j,i] <- 1, perf[j,i] <- 0)
  }
}

#cor(rowSums(perf[,-(1:n/2)]),rowSums(R[,-(1:n/2)]))  # Check whether participants choose the option with the highest reward more often when they obtain a reward
#round(cor(betaTrue,etaTrue),2)  # Check correlation between LR and Decisiveness

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

# Inspect relationship true and estimated value
#mean(etaTrue-etaEstim)  # Compute mean difference between true and estimated LR
#cor(etaTrue,etaEstim)  # Compute correlation between true and estimated LR
#mean(betaTrue-betaEstim)  # Compute mean difference between true and estimated Decisiveness
#cor(betaTrue,betaEstim)  # Compute correlation between true and estimates Decisiveness

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

# Inspect relationship true and estimated value
#mean(etaTrue-etaEstim)  # Compute mean difference between true and estimated LR
#cor(etaTrue,etaEstim)  # Compute correlation between true and estimated LR
#mean(betaTrue-betaEstim)  # Compute mean difference between true and estimated Decisiveness
#cor(betaTrue,betaEstim)  # Compute correlation between true and estimates Decisiveness
#etaGroupMeanTrue-etaGroupMeanEstim  # Compute difference between true and estimated group-mean LR
#etaGroupPrecisionTrue-etaGroupPrecisionEstim  # Compute difference between true and estimated group-precision LR
#betaGroupMeanTrue-betaGroupMeanEstim  # Compute difference between true and estimated group-mean Decisiveness
#betaGroupPrecisionTrue-betaGroupPrecisionEstim  # Compute difference between true and estimated group-precision Decisiveness

DICHier <- samples2$BUGSoutput$DIC

# Only use this 'save output' when you want to save the hierchical estimation results in separate file
# Save output
#save.image(file=paste0('SimulationOutput',nIter,'HierN',nPart,'n',nRep,'k',nStim,'.RData'))
#rm(samples,data,myinits,parameters,time,nStim,nRep,nPart,etaEst,betaEst,etaGroupMeanEst,etaGroupPrecisionEst,betaGroupMeanEst,betaGroupPrecisionEst,etaEstim,betaEstim,etaGroupMeanEstim,etaGroupPrecisionEstim,betaGroupMeanEstim,betaGroupPrecisionEstim)


# --------------------------------------------------------- #
# --------------------------------------------------------- #
# ------------------ Extended RL Model -------------------- #
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
  list(eta = rep(.1,N), betaAcc = rep(.1,N), pi = rep(.1,N)), # Starting values chain 1
  list(eta = rep(.5,N), betaAcc = rep(.5,N), pi = rep(.5,N)), # Starting values chain 2
  list(eta = rep(.9,N), betaAcc = rep(.9,N), pi = rep(.9,N))   # Starting values chain 3
)

parameters3 <- c("strategy","eta","beta","pi","etaGroupMean","etaGroupPrecision","betaGroupMean","betaGroupPrecision","piGroupMean","piGroupPrecision")  # Parameters saved to check sampling results

time <- proc.time()  # Keep track of the sampling time

# Save all representative samples ((n.iter*(n.chains - n.burnin))/n.thin) from the posterior distribution
samples3 <- jags(data3, inits=myinits3, parameters3,
                 model.file ="modelExtendedRL.txt",
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
piGroupMeanEst3 <- samples3$BUGSoutput$sims.list$piGroupMean
piGroupPrecisionEst3 <- samples3$BUGSoutput$sims.list$piGroupPrecision
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
piGroupMeanEstim3 <- mean(piGroupMeanEst3)
piGroupPrecisionEstim3 <- mean(piGroupPrecisionEst3)
strategyEstim3 <- round(apply(strategyEst3,2,mean))

# Inspect relationship true and estimated value
#mean(etaTrue-etaEstim3)  # Compute mean difference between true and estimated LR
#cor(etaTrue,etaEstim3)  # Compute correlation between true and estimated LR
#mean(betaTrue-betaEstim3)  # Compute mean difference between true and estimated Decisiveness
#cor(betaTrue,betaEstim3)  # Compute correlation between true and estimates Decisiveness
#mean(piTrue-piEstim3)  # Compute mean difference between true and estimated Strategy Probability
#cor(piTrue,piEstim3)  # Compute corrrelation between true and estimated Strategy Probability
#etaGroupMeanTrue-etaGroupMeanEstim3  # Compute difference between true and estimated group-mean LR
#etaGroupPrecisionTrue-etaGroupPrecisionEstim3  # Compute difference between true and estimated group-precision LR
#betaGroupMeanTrue-betaGroupMeanEstim3  # Compute difference between true and estimated group-mean Decisiveness
#betaGroupPrecisionTrue-betaGroupPrecisionEstim3  # Compute difference between true and estimated group-precision Decisiveness
#piGroupMeanTrue-piGroupMeanEstim3  # Compute difference between true and estimated group-mean Strategy Probability
#piGroupPrecisionTrue-piGroupPrecisionEstim3  # Compute difference between true and estimated group-precision Strategy Probability
#length(which(Z!=strategyEstim3))

DICExt <- samples3$BUGSoutput$DIC
#data.frame(SimpleRL=DICSimple,HierRL=DICHier,ExtendedRL=DICExt,row.names='DIC')

# Only use this 'save output' when you want to save the extended estimation results in separate file 
# Save output
#save.image(file=paste0('SimulationOutput',nIter,'ExtendedN',nPart,'n',nRep,'k',nStim,'.RData'))
#rm(samples3,data3,myinits3,parameters3,time,nStim,nRep,nPart,etaEst3,betaEst3,piEst3,etaGroupMeanEst3,etaGroupPrecisionEst3,betaGroupMeanEst3,betaGroupPrecisionEst3,piGroupMeanEst3,piGroupPrecisionEst3,strategyEst3,etaEstim3,betaEstim3,piEstim3,etaGroupMeanEstim3,etaGroupPrecisionEstim3,betaGroupMeanEstim3,betaGroupPrecisionEstim3,piGroupMeanEstim3,piGroupPrecisionEstim3,strategyEstim3)

# Save output
save.image(file=paste0('SimulationOutput',nIter,'N',nPart,'n',nRep,'k',nStim,'.RData'))
}

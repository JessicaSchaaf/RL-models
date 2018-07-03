rm(list=ls())
setwd('C:/Users/jvsch/Dropbox/FMRIStudiesDevelopmentalPsych/BehavioralData')
files <- read.table('overview_files.txt')

N <- nrow(files)
n <- 24
k <- 4

C <- matrix(NA,N,n*k)
R <- matrix(NA,N,n*k)

for(iter in 1:10){
  #name=files[iter,1]  # For positive valence conditions
  name=files[iter,2]  # For negative valence conditions
  setwd(paste0('C:/Users/jvsch/Dropbox/FMRIStudiesDevelopmentalPsych/BehavioralData/',100+iter))
  #responses <- read.table(file=paste0(name,'.txt'),header=TRUE,fill=TRUE)  # For positive valence conditions
  responses <- read.table(file=paste0(name,'.txt'),header=TRUE,fill=TRUE,skip=2)  # For negative valence conditions
  responses <- responses[responses$block==1,]
  responses <- responses[order(responses$word1),]
  responses[which(responses$fbduration=='latescreen'),c('accurate','fbtype')] <- NA
  C[iter,] <- responses$accurate
  R[iter,] <- as.character(responses$fbtype)
  #R[which(R == 'pos')] <- 1  # For positive valence conditions
  R[which(R == 'neg')] <- 0   # For negative valence conditions
  #R[which(R == 'no')] <- 0  # For positive valence conditions
  R[which(R == 'no')] <- 1  # For negative valence conditions
  R <- matrix(as.numeric(R),N,n*k)
}

for(iter in 11:19){
  #name=files[iter,1]  # For positive valence conditions
  name=files[iter,2]  # For negative valence conditions
  setwd(paste0('C:/Users/jvsch/Dropbox/FMRIStudiesDevelopmentalPsych/BehavioralData/',100+iter+1))
  #responses <- read.table(file=paste0(name,'.txt'),header=TRUE,fill=TRUE)  # For positive valence conditions
  responses <- read.table(file=paste0(name,'.txt'),header=TRUE,fill=TRUE,skip=2)  # For negative valence conditions
  responses <- responses[responses$block==1,]
  responses <- responses[order(responses$word1),]
  responses[which(responses$fbduration=='latescreen'),c('accurate','fbtype')] <- NA
  C[iter,] <- responses$accurate
  R[iter,] <- as.character(responses$fbtype)
  #R[which(R == 'pos')] <- 1  # For positive valence conditions
  R[which(R == 'neg')] <- 0   # For negative valence conditions
  #R[which(R == 'no')] <- 0  # For positive valence conditions
  R[which(R == 'no')] <- 1  # For negative valence conditions
  R <- matrix(as.numeric(R),N,n*k)
}

for(iter in 1:19){
  #name=files[iter+19,1]  # For positive valence conditions
  name=files[iter+19,2]  # For negative valence conditions
  setwd(paste0('C:/Users/jvsch/Dropbox/FMRIStudiesDevelopmentalPsych/BehavioralData/',200+iter))
  #responses <- read.table(file=paste0(name,'.txt'),header=TRUE,fill=TRUE)  # For positive valence conditions
  responses <- read.table(file=paste0(name,'.txt'),header=TRUE,fill=TRUE,skip=2)  # For negative valence conditions
  responses <- responses[responses$block==1,]
  responses <- responses[order(responses$word1),]
  responses[which(responses$fbduration=='latescreen'),c('accurate','fbtype')] <- NA
  C[iter+19,] <- responses$accurate
  R[iter+19,] <- as.character(responses$fbtype)
  #R[which(R == 'pos')] <- 1  # For positive valence conditions
  R[which(R == 'neg')] <- 0   # For negative valence conditions
  #R[which(R == 'no')] <- 0  # For positive valence conditions
  R[which(R == 'no')] <- 1  # For negative valence conditions
  R <- matrix(as.numeric(R),N,n*k)
}

# ----------------------------------------------- #
# ----------------------------------------------- #
# ----------- Handling missing data ------------- #
# ----------------------------------------------- #
# ----------------------------------------------- #

#miss <- which(is.na(C), arr.ind=TRUE)
#for(i in 1:nrow(miss)){
#  print(paste0('NA.array[',i,'] <- C[',miss[i,1],',',miss[i,2],']'))
#}

# Move missing data to end.
moveNA <- function(x){
  c(x[!is.na(x)],x[is.na(x)])
 }

C[,1:n]<-t(apply(C[,1:n],1,moveNA))
C[,(n+1):(2*n)]<-t(apply(C[,(n+1):(2*n)],1,moveNA))
C[,(2*n+1):(3*n)]<-t(apply(C[,(2*n+1):(3*n)],1,moveNA))
C[,(3*n+1):(4*n)]<-t(apply(C[,(3*n+1):(4*n)],1,moveNA))

R[,1:n]<-t(apply(R[,1:n],1,moveNA))
R[,(n+1):(2*n)]<-t(apply(R[,(n+1):(2*n)],1,moveNA))
R[,(2*n+1):(3*n)]<-t(apply(R[,(2*n+1):(3*n)],1,moveNA))
R[,(3*n+1):(4*n)]<-t(apply(R[,(3*n+1):(4*n)],1,moveNA))

C[which(is.na(C))] <- 99
R[which(is.na(R))] <- 99

nRepNA <- matrix(NA,N,k)
for(pers in 1:N){
  nRepNA[pers,1] <- length(which(C[pers,1:n] != 99))
  nRepNA[pers,2] <- length(which(C[pers,(n+1):(2*n)] != 99))
  nRepNA[pers,3] <- length(which(C[pers,(2*n+1):(3*n)] != 99))
  nRepNA[pers,4] <- length(which(C[pers,(3*n+1):(k*n)] != 99))
}
 
# ---------------------------------------------- #
# ---------------------------------------------- #
# -------------- Estimation with --------------- #
# ------------- Extended RL Model -------------- #
# ---------------------------------------------- #
# ---------------------------------------------- #

library('R2jags')

bugsdir <- 'C:/Users/jvsch/Documents/Master/Onderzoek Reinforcement Learning/RealData Anne-Wil'

nStim <- k  # Number of picture-pseudoword associations
nRep <- n  # Number of repetitions per association
nPart <- N # Number of participants
nRepNA <- nRepNA

data <- list("nStim","nRep","nRepNA","nPart","C","R") # Data input for JAGS

# List of sampling starting values to give to JAGS
myinits <- list(
  list(eta = rep(.1,N), betaAcc = rep(.1,N), pi = .1), # Starting values chain 1
  list(eta = rep(.5,N), betaAcc = rep(.5,N), pi = .5), # Starting values chain 2
  list(eta = rep(.9,N), betaAcc = rep(.9,N), pi = .9)   # Starting values chain 3
)

parameters <- c("strategy","eta","beta","pi","etaGroupMean","etaGroupPrecision","betaGroupMean","betaGroupPrecision")  # Parameters saved to check sampling results

time <- proc.time()  # Keep track of the sampling time

# Save all representative samples ((n.iter*n.chains - n.burnin)/n.thin) from the posterior distribution
samples <- jags(data, inits=myinits, parameters,
                model.file ="modelExtendedRLStimNA.txt",
                n.chains=3, n.iter=1000, n.burnin=100, n.thin=1,
                DIC=T, working.directory=bugsdir)

(proc.time() - time)[3]/60  # Check how long the sampling took

# --------------------------------------------------------- #
# --------------------------------------------------------- #
# ----------------- Results: BUGS output ------------------ #
# --------------------------------------------------------- #
# --------------------------------------------------------- #

# Save samples in separate objects
etaEst <- samples$BUGSoutput$sims.list$eta
betaEst <- samples$BUGSoutput$sims.list$beta
piEst <- samples$BUGSoutput$sims.list$pi
etaGroupMeanEst <- samples$BUGSoutput$sims.list$etaGroupMean
etaGroupPrecisionEst <- samples$BUGSoutput$sims.list$etaGroupPrecision
betaGroupMeanEst <- samples$BUGSoutput$sims.list$betaGroupMean
betaGroupPrecisionEst <- samples$BUGSoutput$sims.list$betaGroupPrecision
strategyEst <- samples$BUGSoutput$sims.list$strategy

# Check convergence of sampling chains
#range(samples$BUGSoutput$summary[,'Rhat'])
#length(which(samples$BUGSoutput$summary[,'Rhat'] >= 1.1)) # Check how many sampling chains did not converge
#which(samples$BUGSoutput$summary[,'Rhat'] >= 1.1)  # Check which sampling chains did not converge

# Compute the estimated parameter values over all samples
etaEstim <- apply(etaEst,2,mean)
betaEstim <- apply(betaEst,2,mean)
piEstim <- mean(piEst)
etaGroupMeanEstim <- mean(etaGroupMeanEst)
etaGroupPrecisionEstim <- mean(etaGroupPrecisionEst)
betaGroupMeanEstim <- mean(betaGroupMeanEst)*10
betaGroupPrecisionEstim <- mean(betaGroupPrecisionEst)
strategyEstim <- sapply(list(strategyFirstStim=apply(strategyEst[,,1],2,mean),
                             strategySecondStim=apply(strategyEst[,,2],2,mean),
                             strategyThirdStim=apply(strategyEst[,,3],2,mean),
                             strategyFourthStim=apply(strategyEst[,,4],2,mean)),round)

DICExt <- samples$BUGSoutput$DIC

setwd('C:/Users/jvsch/Documents/Master/Onderzoek Reinforcement Learning/RealData Anne-Wil')
#save.image(file='ResultsRLDataPosBlock1.RData')
#save.image(file='ResultsRLDataPosBlock2.RData')
save.image(file='ResultsRLDataNegBlock1.RData')
#save.image(file='ResultsRLDataNegBlock2.RData')


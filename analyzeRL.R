# ------------------------------------------------------ #
# ------------------------------------------------------ #
# -------------- Analysis Simulations ------------------ #
# ------------------------------------------------------ #
# ------------------------------------------------------ #

rm(list=ls())
library('R2jags')

# Define the folder where the .RData files with simulation results are stored
setwd('Insert folder containing data files here')

# Specify the characteristics of the data set
nStim <- 4  # Number of picture-pseudoword associations
nRep <- 144  # Number of repetitions per association
nPart <- 30  # Number of participants
nIter <- 100  # Number of replications ran

# -------------------------------------- #
# ---------- Extended RL Model --------- #
# ----------- with Stimuli ---------- #
# -------------------------------------- #

# --------------------------- #
# --------------------------- #
# ---- Convergence check ---- #
# --------------------------- #
# --------------------------- #

# First check the convergence of the sampling chains
convergence <- matrix(NA,nIter,4)

for(it in 1:nIter){  # Loop over all replications 
  #load(paste0('SimulationOutput',it,'LearningN',nPart,'n',nRep,'k',nStim,'.RData'))  # Load each data file from Condition I = Learning
  #load(paste0('SimulationOutput',it,'GuessingN',nPart,'n',nRep,'k',nStim,'.RData'))  # Load each data file from Condition II = Guessing
  load(paste0('SimulationOutput',it,'Mixed1N',nPart,'n',nRep,'k',nStim,'.RData'))  # Load each data file from Condition III = Mixed I
  #load(paste0('SimulationOutput',it,'Mixed2N',nPart,'n',nRep,'k',nStim,'.RData'))  # Load each data file from Condition IV = Mixed II
  convergence[it,1] <- it
# R-hat (Gelman & Rubin, 1992) values above 1.1 indicate that the Markov chain failed to converge.
  convergence[it,2] <- length(which(samplesHier$BUGSoutput$summary[,'Rhat'] >= 1.1))  # Inspect convergence hierarchical RL model
  convergence[it,3] <- length(which(samplesHierPSRL$BUGSoutput$summary[1:(N*2+6),'Rhat'] >= 1.1))  # Inspect convergence hierarchical PSRL model
  convergence[it,4] <- length(which(samplesHierPSRL$BUGSoutput$summary[-(1:(N*2+6)),'Rhat'] >= 1.1))
}
colnames(convergence) <- c('iter','notConvergedHier','notConvergedExt','notConvergedExtStrategy')
convergence  # This matrix shows how many chains per replication failed to converge.

write.csv(convergence,file=paste0('convergenceMixed1N',N,'n',n,'k',k,'.csv'))

# --------------------------- #
# --------------------------- #
# ----- Load simulation ----- #
# --------- Results --------- #
# --------------------------- #
# --------------------------- #

# First the individual-level results ...
ind <- list(meanEtaTrue='',sdEtaTrue='',meanEtaEstimHier='',sdEtaEstimHier='',meanEtaEstimExt='',sdEtaEstimExt='',meanBetaTrue='',sdBetaTrue='',meanBetaEstimHier='',sdBetaEstimHier='',meanBetaEstimExt='',sdBetaEstimExt='',meanPiTrue='',meanPiEstim='')

for(it in 1:nIter){
  k <- 4  # Number of picture-pseudoword associations
  N <- 30  # Number of repetitions per association
  n <- 144  # Number of participants
  
  #load(paste0('SimulationOutput',it,'LearningN',N,'n',n,'k',k,'.RData'))
  #load(paste0('SimulationOutput',it,'GuessingN',N,'n',n,'k',k,'.RData'))
  load(paste0('SimulationOutput',it,'Mixed1N',N,'n',n,'k',k,'.RData'))
  #load(paste0('SimulationOutput',it,'Mixed2N',N,'n',n,'k',k,'.RData'))
  ind[[1]][[it]] <- format(round(mean(etaTrue),3),nsmall=3)
  ind[[2]][[it]] <- format(round(sd(etaTrue),3),nsmall=3)
  ind[[3]][[it]] <- format(round(mean(etaEstimHier),3),nsmall=3)
  ind[[4]][[it]] <- format(round(sd(etaEstimHier),3),nsmall=3)
  ind[[5]][[it]] <- format(round(mean(etaEstimExt),3),nsmall=3)
  ind[[6]][[it]] <- format(round(sd(etaEstimExt),3),nsmall=3)
  ind[[7]][[it]] <- format(round(mean(betaTrue),3),nsmall=3)
  ind[[8]][[it]] <- format(round(sd(betaTrue),3),nsmall=3)
  ind[[9]][[it]] <- format(round(mean(betaEstimHier),3),nsmall=3)
  ind[[10]][[it]] <- format(round(sd(betaEstimHier),3),nsmall=3)
  ind[[11]][[it]] <- format(round(mean(betaEstimExt),3),nsmall=3)
  ind[[12]][[it]] <- format(round(sd(betaEstimExt),3),nsmall=3)
  #ind[[13]][[it]] <- 1  # Use this in Learning Condition
  #ind[[13]][[it]] <- 0  # Use this in Guessing Condition
  ind[[13]][[it]] <- format(round(piTrue,3),nsmall=3)  # Use this in Mixed Conditions
  ind[[14]][[it]] <- format(round(piEstimExt,3),nsmall=3)
}

ind <- as.data.frame(ind)

# ... Then the group-level results
group <- list(etaTrueGroup='',precEtaTrueGroup='',meanEtaEstimGroupHier='',precEtaEstimGroupHier='',meanEtaEstimGroupExt='',precEtaEstimGroupExt='',betaTrueGroup='',precBetaTrueGroup='',meanBetaEstimGroupHier='',precBetaEstimGroupHier='',meanBetaEstimGroupExt='',precBetaEstimGroupExt='')

for(it in 1:Iterations){
  k <- 4  # Number of picture-pseudoword associations
  N <- 30  # Number of repetitions per association
  n <- 144  # Number of participants
  #load(paste0('SimulationOutput',it,'LearningN',N,'n',n,'k',k,'.RData'))
  #load(paste0('SimulationOutput',it,'GuessingN',N,'n',n,'k',k,'.RData'))
  load(paste0('SimulationOutput',it,'Mixed1N',N,'n',n,'k',k,'.RData'))
  #load(paste0('SimulationOutput',it,'Mixed2N',N,'n',n,'k',k,'.RData'))
  
  group[[1]] <- etaGroupMeanTrue
  group[[2]] <- etaGroupPrecisionTrue
  group[[3]][[it]] <- format(round(mean(etaGroupMeanEstHier),3),nsmall=3)
  group[[4]][[it]] <- format(round(mean(etaGroupPrecisionEstHier),3),nsmall=3)
  group[[5]][[it]] <- format(round(mean(etaGroupMeanEstExt),3),nsmall=3)
  group[[6]][[it]] <- format(round(mean(etaGroupPrecisionEstExt),3),nsmall=3)
  group[[7]] <- betaGroupMeanTrue*10
  group[[8]] <- betaGroupPrecisionTrue
  group[[9]][[it]] <- format(round(mean(betaGroupMeanEstHier*10),3),nsmall=3)
  group[[10]][[it]] <- format(round(mean(betaGroupPrecisionEstHier),3),nsmall=3)
  group[[11]][[it]] <- format(round(mean(betaGroupMeanEstExt*10),3),nsmall=3)
  group[[12]][[it]] <- format(round(mean(betaGroupPrecisionEstExt),3),nsmall=3)
}

group <- as.data.frame(group)

res <- data.frame(etaTrue=ind$meanEtaTrue,
                  etaEstHier=ind$meanEtaEstimHier,
                  etaEstExt=ind$meanEtaEstimExt,
                  betaTrue=ind$meanBetaTrue,
                  betaEstHier=ind$meanBetaEstimHier,
                  betaEstExt=ind$meanBetaEstimExt,
                  etaGroupMeanTrue=group$etaTrueGroup,
                  etaGroupMeanEstHier=group$meanEtaEstimGroupHier,
                  etaGroupMeanEstExt=group$meanEtaEstimGroupExt,
                  etaGroupPrecisionTrue=group$precEtaTrueGroup,
                  etaGroupPrecisionEstHier=group$precEtaEstimGroupHier,
                  etaGroupPrecisionEstExt=group$precEtaEstimGroupExt,
                  betaGroupMeanTrue=group$betaTrueGroup,
                  betaGroupMeanEstHier=group$meanBetaEstimGroupHier,
                  betaGroupMeanEstExt=group$meanBetaEstimGroupExt,
                  betaGroupPrecisionTrue=group$precBetaTrueGroup,
                  betaGroupPrecisionEstHier=group$precBetaEstimGroupHier,
                  betaGroupPrecisionEstExt=group$precBetaEstimGroupExt,
                  piTrue=ind$meanPiTrue,
                  piEstExt=ind$meanPiEstim)

#write.csv(res,file=paste0('parameterEstimatesLearningN',N,'n',n,'k',k,'.csv'))
#write.csv(res,file=paste0('parameterEstimatesGuessingN',N,'n',n,'k',k,'.csv'))
write.csv(res,file=paste0('parameterEstimatesMixed1N',N,'n',n,'k',k,'.csv'))
#write.csv(res,file=paste0('parameterEstimatesMixed2N',N,'n',n,'k',k,'.csv'))

# Save the results
setwd('Insert folder you want to save estimates to here')


# Compute percentage of 95% CIs that contain true parameter value
perc <- list(etaHier='',etaExt='',betaHier='',betaExt='',piExt='',strategies='',DIC='')

for(it in 1:nIter){
  counterEtaHier <- 0
  counterEtaExt <- 0
  counterBetaHier <- 0
  counterBetaExt <- 0
  
  #load(paste0('SimulationOutput',it,'LearningN30n144k4.RData'))
  #load(paste0('SimulationOutput',it,'GuessingN30n144k4.RData'))
  load(paste0('SimulationOutput',it,'Mixed1N30n144k4.RData'))
  #load(paste0('SimulationOutput',it,'Mixed2N30n144k4.RData'))
  
  for(pp in 1:N){
    if(etaTrue[pp] >= quantile(etaEstHier[,pp],c(.025,.975))[1] & etaTrue[pp] <= quantile(etaEstHier[,pp],c(.025,.975))[2]){
      counterEtaHier <- counterEtaHier + 1
    }
    if(etaTrue[pp] >= quantile(etaEstExt[,pp],c(.025,.975))[1] & etaTrue[pp] <= quantile(etaEstExt[,pp],c(.025,.975))[2]){
      counterEtaExt <- counterEtaExt + 1
    }
    if(betaTrue[pp] >= quantile(betaEstHier[,pp],c(.025,.975))[1] & betaTrue[pp] <= quantile(betaEstHier[,pp],c(.025,.975))[2]){
      counterBetaHier <- counterBetaHier + 1
    }
    if(betaTrue[pp] >= quantile(betaEstExt[,pp],c(.025,.975))[1] & betaTrue[pp] <= quantile(betaEstExt[,pp],c(.025,.975))[2]){
      counterBetaExt <- counterBetaExt + 1
    }
    
    counterPiExt <- ifelse(piTrue >= quantile(piEstExt,c(.025,.975))[1] & piTrue <= quantile(piEstExt,c(.025,.975))[2], 1, 0)
    counterDIC <- ifelse(DICExt < DICHier, 1, 0)
  }
  
  # Compute proportion of correctly classified strategies per participant
  prop <- matrix(NA,N,k)
  
  for(stim in 1:k){
    for(pp in 1:N){
      prop[pp,stim] <- length(which(strategyEstExt[,pp,stim] == Z[pp,stim]))/length(strategyEstExt[,pp,stim]) 
    }
  }
  
  perc[[1]][[it]] <- counterEtaHier
  perc[[2]][[it]] <- counterEtaExt
  perc[[3]][[it]] <- counterBetaHier
  perc[[4]][[it]] <- counterBetaExt
  perc[[5]][[it]] <- counterPiExt
  perc[[6]][[it]] <- round(mean(prop)*100,2)
  perc[[7]][[it]] <- counterDIC
}

res2 <- as.data.frame(perc)

#write.csv(res2,file=paste0('percentagesLearningN',N,'n',n,'k',k,'.csv'))
#write.csv(res2,file=paste0('percentagesGuessingN',N,'n',n,'k',k,'.csv'))
write.csv(res2,file=paste0('percentagesMixed1N',N,'n',n,'k',k,'.csv'))
#write.csv(res2,file=paste0('percentagesMixed2N',N,'n',n,'k',k,'.csv'))


# Compute average width of the individual 95% credible intervals
widthCI <- list(meanWidthEtaHier='',meanWidthEtaExt='',meanWidthBetaHier='',meanWidthBetaExt='',meanWidthPiExt='')

for(nit in 1:100){
  #load(paste0('SimulationOutput',nit,'LearningN30n144k4.RData'))
  #load(paste0('SimulationOutput',nit,'GuessingN30n144k4.RData'))
  load(paste0('SimulationOutput',nit,'Mixed1N30n144k4.RData'))
  #load(paste0('SimulationOutput',nit,'Mixed2N30n144k4.RData'))
  widthCIEtaHier <- numeric()
  widthCIEtaExt <- numeric()
  widthCIBetaHier <- numeric()
  widthCIBetaExt <- numeric()
  
  for(i in 1:ncol(etaEstHier)){
    widthCIEtaHier[i] <- quantile(etaEstHier[,i],c(.025,.975))[2]-quantile(etaEstHier[,i],c(.025,.975))[1]
  }
  for(i in 1:ncol(etaEstExt)){
    widthCIEtaExt[i] <- quantile(etaEstExt[,i],c(.025,.975))[2]-quantile(etaEstExt[,i],c(.025,.975))[1]
  }
  for(i in 1:ncol(betaEstHier)){
    widthCIBetaHier[i] <- quantile(betaEstHier[,i],c(.025,.975))[2]-quantile(betaEstHier[,i],c(.025,.975))[1]
  }
  for(i in 1:ncol(betaEstExt)){
    widthCIBetaExt[i] <- quantile(betaEstExt[,i],c(.025,.975))[2]-quantile(betaEstExt[,i],c(.025,.975))[1]
  }
  
  widthCIPiExt <- quantile(piEstExt,c(.025,.975))[2]-quantile(piEstExt,c(.025,.975))[1]
  
  widthCI[[1]][[nit]] <- mean(widthCIEtaHier)
  widthCI[[2]][[nit]] <- mean(widthCIEtaExt)
  widthCI[[3]][[nit]] <- mean(widthCIBetaHier)
  widthCI[[4]][[nit]] <- mean(widthCIBetaExt)
  widthCI[[5]][[nit]] <- mean(widthCIPiExt)
  
}

widthCI <- as.data.frame(widthCI)

#write.csv(widthCI,file=paste0('widthCILearningN',N,'n',n,'k',k,'.csv'))
#write.csv(widthCI,file=paste0('widthCIGuessingN',N,'n',n,'k',k,'.csv'))
write.csv(widthCI,file=paste0('widthCIMixed1N',N,'n',n,'k',k,'.csv'))
#write.csv(widthCI,file=paste0('widthCIMixed2N',N,'n',n,'k',k,'.csv'))

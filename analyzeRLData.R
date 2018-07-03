setwd('C:/Users/jvsch/Documents/Master/Onderzoek Reinforcement Learning/RealData Anne-Wil')

# ----------------------------------------------------- #
# ------ Positive valence condition; first block ------ #
# ----------------------------------------------------- #

# Save individual estimates for LR and Decisiveness
# Save group estimates of mean and precision LR and Decisiveness, Strategy Probability and DIC
load('C:/Users/jvsch/Documents/Master/Onderzoek Reinforcement Learning/RealData Anne-Wil/ResultsRLDataPosBlock1.RData')
datInd <- cbind(LRHierPos1=round(etaEstimHier,2),LRExtPos1=round(etaEstimExt,2),DecHierPos1=round(betaEstimHier,2),DecExtPos1=round(betaEstimExt,2))
datGroup <- cbind(LRHierMean=round(etaGroupMeanEstimHier,2),LRHierPrec=round(etaGroupPrecisionEstimHier,2),
                  LRExtMean=round(etaGroupMeanEstimExt,2),LRExtPrec=round(etaGroupPrecisionEstimExt,2),
                  DecHierMean=round(betaGroupMeanEstimHier*10,2),DecHierPrec=round(betaGroupPrecisionEstimHier,2),
                  DecExtMean=round(betaGroupMeanEstimExt,2),DecExtPrec=round(betaGroupPrecisionEstimExt,2),
                  piEstExt=round(piEstimExt,2),DICHier=round(DICHier,2),DICExt=round(DICExt,2))

# Save estimated strategies
write.table(data.frame(id=c(101:110,112:120,201:219),strategyEstimExt),file='strategiesRLDataPosBlock1.xls',quote=FALSE,sep="\t",row.names=FALSE)

# ----------------------------------------------------- #
# ----- Positive valence condition; second block ------ #
# ----------------------------------------------------- #

# Save individual estimates for LR and Decisiveness
# Save group estimates of mean and precision LR and Decisiveness, Strategy Probability and DIC
load('C:/Users/jvsch/Documents/Master/Onderzoek Reinforcement Learning/RealData Anne-Wil/ResultsRLDataPosBlock2.RData')
datInd <- cbind(datInd,LRHierPos2=round(etaEstimHier,2),LRExtPos2=round(etaEstimExt,2),DecHierPos2=round(betaEstimHier,2),DecExtPos2=round(betaEstimExt,2))
datGroup <- rbind(datGroup,cbind(round(etaGroupMeanEstimHier,2),round(etaGroupPrecisionEstimHier,2),
                                 round(etaGroupMeanEstimExt,2),round(etaGroupPrecisionEstimExt,2),
                                 round(betaGroupMeanEstimHier*10,2),round(betaGroupPrecisionEstimHier,2),
                                 round(betaGroupMeanEstimExt,2),round(betaGroupPrecisionEstimExt,2),
                                 round(piEstimExt,2),DICHier=round(DICHier,2),DICExt=round(DICExt,2)))

# Save estimated strategies
write.table(data.frame(id=c(101:110,112:120,201:219),strategyEstimExt),file='strategiesRLDataPosBlock2.xls',quote=FALSE,sep="\t",row.names=FALSE)

# ----------------------------------------------------- #
# ------ Negative valence condition; first block ------ #
# ----------------------------------------------------- #

# Save individual estimates for LR and Decisiveness
# Save group estimates of mean and precision LR and Decisiveness, Strategy Probability and DIC
load('C:/Users/jvsch/Documents/Master/Onderzoek Reinforcement Learning/RealData Anne-Wil/ResultsRLDataNegBlock1.RData')
datInd <- cbind(datInd,LRHierNeg1=round(etaEstimHier,2),LRExtNeg1=round(etaEstimExt,2),DecHierNeg1=round(betaEstimHier,2),DecExtNeg1=round(betaEstimExt,2))
datGroup <- rbind(datGroup,cbind(round(etaGroupMeanEstimHier,2),round(etaGroupPrecisionEstimHier,2),
                                 round(etaGroupMeanEstimExt,2),round(etaGroupPrecisionEstimExt,2),
                                 round(betaGroupMeanEstimHier*10,2),round(betaGroupPrecisionEstimHier,2),
                                 round(betaGroupMeanEstimExt,2),round(betaGroupPrecisionEstimExt,2),
                                 round(piEstimExt,2),DICHier=round(DICHier,2),DICExt=round(DICExt,2)))

# Save estimated strategies
write.table(data.frame(id=c(101:110,112:120,201:219),strategyEstimExt),file='strategiesRLDataNegBlock1.xls',quote=FALSE,sep="\t",row.names=FALSE)

# ----------------------------------------------------- #
# ----- Negative valence condition; second block ------ #
# ----------------------------------------------------- #

# Save individual estimates for LR and Decisiveness
# Save group estimates of mean and precision LR and Decisiveness, Strategy Probability and DIC
load('C:/Users/jvsch/Documents/Master/Onderzoek Reinforcement Learning/RealData Anne-Wil/ResultsRLDataNegBlock2.RData')
datInd <- cbind(datInd,LRHierNeg2=round(etaEstimHier,2),LRExtNeg2=round(etaEstimExt,2),DecHierNeg2=round(betaEstimHier,2),DecExtNeg2=round(betaEstimExt,2))
datGroup <- rbind(datGroup,cbind(round(etaGroupMeanEstimHier,2),round(etaGroupPrecisionEstimHier,2),
                                 round(etaGroupMeanEstimExt,2),round(etaGroupPrecisionEstimExt,2),
                                 round(betaGroupMeanEstimHier*10,2),round(betaGroupPrecisionEstimHier,2),
                                 round(betaGroupMeanEstimExt,2),round(betaGroupPrecisionEstimExt,2),
                                 round(piEstimExt,2),DICHier=round(DICHier,2),DICExt=round(DICExt,2)))

# Save estimated strategies
write.table(data.frame(id=c(101:110,112:120,201:219),strategyEstimExt),file='strategiesRLDataNegBlock2.xls',quote=FALSE,sep="\t",row.names=FALSE)

datInd <- cbind(id=c(101:110,112:120,201:219),datInd[,c(1,2,5,6,9,10,13,14,3,4,7,8,11,12,15,16)])
rownames(datGroup) <- c('Pos1','Pos2','Neg1','Neg2')

write.table(datInd,file='estimatedIndParametersRLData.xls',quote=FALSE,sep="\t",row.names=FALSE)
write.table(datGroup,file='estimatedGroupParametersRLData.xls',quote=FALSE,sep="\t",row.names=TRUE,col.names=NA)

# Plot group-level distribution
# Get average means and precisions across conditions
resGroup <- apply(datGroup,2,mean)

# Plot estimated group-level distribution
plot(density(rbeta(100000,resGroup[1]*resGroup[2],(1-resGroup[1])*resGroup[2])),las=1,xlab='LR',ylab='Posterior Density',main=paste0('LR ~ Beta(',round((resGroup[1]*resGroup[2]),3),', ',round(((1-resGroup[1])*resGroup[2]),3),')'),bty='n',xlim=c(0,1),ylim=c(0,8))
# Empirical prior suggested by Gershman (2016)
lines(density(rbeta(100000,.012,.021)),col='red')

plot(density(rbeta(100000,(resGroup[3]/10)*resGroup[4],(1-(resGroup[3]/10))*resGroup[4])),las=1,xlab='Decisiveness',ylab='Posterior Density',main=paste0('Decisiveness ~ Beta(',round(((resGroup[3]/10)*resGroup[4]),3),', ',round(((1-(resGroup[3]/10))*resGroup[4]),3),')'),bty='n',xlim=c(0,1))
# Empirical prior suggested by Gershman (2016)
lines(density(rgamma(100000,4.83,.73)/10),col='red')
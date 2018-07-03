setwd('Insert folder containing results here')

# Save individual estimates for LR and Decisiveness
# Save group estimates of mean and precision LR and Decisiveness, Strategy Probability and DIC
load('Insert file containing results here.RData')
datInd <- cbind(LRHierPos1=round(etaEstimHier,2),LRHierPSRLPos1=round(etaEstimHierPSRL,2),DecHierPos1=round(betaEstimHier,2),DecHierPSRLPos1=round(betaEstimHierPSRL,2))
datGroup <- cbind(LRHierMean=round(etaGroupMeanEstimHier,2),LRHierPrec=round(etaGroupPrecisionEstimHier,2),
                  LRHierPSRLMean=round(etaGroupMeanEstimHierPSRL,2),LRHierPSRLPrec=round(etaGroupPrecisionEstimHierPSRL,2),
                  DecHierMean=round(betaGroupMeanEstimHier*10,2),DecHierPrec=round(betaGroupPrecisionEstimHier,2),
                  DecHierPSRLMean=round(betaGroupMeanEstimHierPSRL,2),DecHierPSRLPrec=round(betaGroupPrecisionEstimHierPSRL,2),
                  piEstHierPSRL=round(piEstimHierPSRL,2),DICHier=round(DICHier,2),DICHierPSRL=round(DICHierPSRL,2))

# Save estimated strategies
write.table(data.frame(id=c('Insert participant numbers here'),strategyEstimHierPSRL),file='Insert file name here.xls',quote=FALSE,sep="\t",row.names=FALSE)

datInd <- cbind(id=c('Insert participant numbers here'),datInd[,c(1,2,5,6,9,10,13,14,3,4,7,8,11,12,15,16)])
rownames(datGroup) <- c('Insert names of conditions here')

write.table(datInd,file='Insert file name here.xls',quote=FALSE,sep="\t",row.names=FALSE)
write.table(datGroup,file='Insert file name.xls',quote=FALSE,sep="\t",row.names=TRUE,col.names=NA)

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

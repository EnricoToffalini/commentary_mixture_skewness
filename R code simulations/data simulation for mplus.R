
##############################

rm(list=ls())
library(lavaan)
library(MASS)
library(semTools)

set.seed(1)

##############################

## BASED ON OBSERVED (NON-NORMAL) PARAMETERS

# simulation parameters
N = 428
corMatrix = lav_matrix_lower2full(c(
  1,
  0.516, 1,
  0.704, 0.597, 1
))
skewness = c(0.98,-0.23,-0.68)
kurtosis = c(0.69,-0.50,0.67)

niter = 10
for(i in 1:niter){
  pop = data.frame(round(mvrnonnorm(N,mu=rep(0,nrow(corMatrix)),Sigma=corMatrix,skewness=skewness,kurtosis=kurtosis),3))
  colnames(pop) = c("w1mft", "w1mps", "w1no")
  
  write.table(pop,file=paste0("data_obs_",i,".csv"),sep=",",row.names=F,col.names=F)
}

##############################

## WITH PERFECT NORMALITY

# simulation parameters
N = 428
corMatrix = lav_matrix_lower2full(c(
1,
0.516, 1,
0.704, 0.597, 1
))
skewness = 0
kurtosis = 0

niter = 10
for(i in 1:niter){
  pop = data.frame(round(mvrnonnorm(N,mu=rep(0,nrow(corMatrix)),Sigma=corMatrix,skewness=skewness,kurtosis=kurtosis),3))
  colnames(pop) = c("w1mft", "w1mps", "w1no")
  
  write.table(pop,file=paste0("data_gauss_",i,".csv"),sep=",",row.names=F,col.names=F)
}

##############################


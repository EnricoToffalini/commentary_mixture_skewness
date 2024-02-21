
##############################

rm(list=ls())
library(lavaan)
library(MASS)
library(semTools)
library(mclust)

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

niter = 100
detectedNclusters_obs = rep(NA,niter)
for(i in 1:niter){
  pop = data.frame(round(mvrnonnorm(N,mu=rep(0,nrow(corMatrix)),Sigma=corMatrix,skewness=skewness,kurtosis=kurtosis),3))
  colnames(pop) = c("w1mft", "w1mps", "w1no")
  fit = Mclust(pop,G=1:3)
  detectedNclusters_obs[i] = fit$G
}
table(detectedNclusters_obs)

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

niter = 100
detectedNclusters_gauss = rep(NA,niter)
for(i in 1:niter){
  pop = data.frame(round(mvrnonnorm(N,mu=rep(0,nrow(corMatrix)),Sigma=corMatrix,skewness=skewness,kurtosis=kurtosis),3))
  colnames(pop) = c("w1mft", "w1mps", "w1no")
  fit = Mclust(pop,G=1:3)
  detectedNclusters_gauss[i] = fit$G
}
table(detectedNclusters_gauss)

##############################

save.image("gmm simulations.RData")

##############################


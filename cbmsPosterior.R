library(rstan)
library(StanHeaders)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)
library(ggmcmc)
library(coda)

#Load data
cbmsY<-c(25.26666,17.8,17.2666,26.96666,30.433333,17.866666,19.9)
#This is data from an expert that I'm trying to use as an informed prior
cbmsFreqs<-c(1,1.5,1.5,1.5,2.75,2.75,2.75,3.75,4.75,4.75,4.75,4.75,6,6,6,
             6.5,7.75,7.75,7.75,9)
cbmsFreqs<-12*cbmsFreqs

#CBMS
#Find MLE mu and sigma for shape parameter
cbmsMLE<-fitdist(cbmsFreqs,"weibull",method="mle")
denscomp(list(cbmsMLE),legendtext="Weibull",xlab="CBMS Weibull")
mean(rnorm(n=100000,mean=cbmsMLE_shape,sd=cbmsMLE_scale))
cbmsMLE_shape<-as.numeric(cbmsMLE$estimate[1])
cbmsMLE_shapeSE<-as.numeric(cbmsMLE$sd[1])
cbmsMLE_scale<-as.numeric(cbmsMLE$estimate[2])
cbmsMLE_scaleSE<-as.numeric(cbmsMLE$sd[2])
#convert to lognormal mean and se
cbmsLocation<-log(cbmsMLE_shape^2/sqrt(cbmsMLE_shapeSE^2+cbmsMLE_shape^2))
cbmsLocationSE<-sqrt(log(1+cbmsMLE_shapeSE^2/cbmsMLE_shape^2))
cbmsLocationTau<-(1/cbmsLocationSE)^2

#Get Full lognormal distributions for alpha and sigma parameters in weibull
cbmsScale<-log(cbmsMLE_scale^2/sqrt(cbmsMLE_scaleSE^2+cbmsMLE_scale^2))
cbmsScaleSE<-sqrt(log(1+cbmsMLE_scaleSE^2/cbmsMLE_scale^2))
cbmsScaleTau<-(1/cbmsScaleSE)^2


#What the histograms should look like
#Alpha, shape parameter
hist(rlnorm(n=1000,meanlog=cbmsLocation,sdlog=cbmsLocationSE))
#Sigma, scale parameter
hist(rlnorm(n=1000,meanlog=cbmsScale,sdlog=cbmsScaleSE))
#what I get if I generate random samples from my prior using MLE parameterization
hist(rweibull(n=1000,cbmsMLE_shape,cbmsMLE_scale))

#### Stan Model ####
n=7
stan_data <- list(n=n,cbmsY=cbmsY,cbmsMLE_shape=cbmsMLE_shape,cbmsMLE_shapeSE=cbmsMLE_shapeSE,
                  cbmsMLE_scale=cbmsMLE_scale,cbmsMLE_scaleSE=cbmsMLE_scaleSE)


fit <- stan(file="stantest.stan", data = stan_data, warmup = 5000, iter = 10000, chains = 3, cores = 1, thin = 1)
fit
hist(rweibull(n=1000,shape=2.43,scale=21.49))
plot(fit)

#Convergence Diagnostics
s1 <- mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))

S1 <- ggs(s1)
S1<-get_family(S1,family=c("alpha","sigma"))


#sigma and alpha traceplots
  ggs_traceplot(S1,family="sigma")+ggtitle("CBMS Sigma Traceplot")+ylab("Parameter Value")+
    theme_dark()
  ggs_traceplot(S1,family="alpha")+ggtitle("CBMS Alpha Traceplot")+ylab("Parameter Value")+
    theme_dark()

summary(fit)
              
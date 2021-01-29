library(rstan)
library(StanHeaders)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)

#Load data
jbpdsY<-c(16.3, 24.3333333,10.5666666,10.9333333,2.8333333,2.8,16.3,8.0666666,26.4,5.16666,16.2666666,
          12.1666666,24.3333333,24.333333)
#This is data from an expert that I'm trying to use as an informed prior
jbpdsFreqs<-c(3.75,5,5,6,6.5,6.75,6.75,7.75,7.75,7.75,8.5,8.5,8.5,9,9.25,10,10,
              10,10.25,10.75)
jbpdsFreqs<-jbpdsFreqs*12

#jbpds
#Find MLE mu and sigma for shape parameter
jbpdsMLE<-fitdist(jbpdsFreqs,"weibull",method="mle")
denscomp(list(jbpdsMLE),legendtext="Weibull",xlab="JBPDS Weibull")
jbpdsMLE_shape<-as.numeric(jbpdsMLE$estimate[1])
jbpdsMLE_shapeSE<-as.numeric(jbpdsMLE$sd[1])
jbpdsMLE_scale<-as.numeric(jbpdsMLE$estimate[2])
jbpdsMLE_scaleSE<-as.numeric(jbpdsMLE$sd[2])
#convert to lognormal mean and se
jbpdsLocation<-log(jbpdsMLE_shape^2/sqrt(jbpdsMLE_shapeSE^2+jbpdsMLE_shape^2))
jbpdsLocationSE<-sqrt(log(1+jbpdsMLE_shapeSE^2/jbpdsMLE_shape^2))
jbpdsLocationTau<-(1/jbpdsLocationSE)^2

#Get Full lognormal distributions for alpha and sigma parameters in weibull
jbpdsScale<-log(jbpdsMLE_scale^2/sqrt(jbpdsMLE_scaleSE^2+jbpdsMLE_scale^2))
jbpdsScaleSE<-sqrt(log(1+jbpdsMLE_scaleSE^2/jbpdsMLE_scale^2))
jbpdsScaleTau<-(1/jbpdsScaleSE)^2


#What the histograms should look like
#Alpha, shape parameter
hist(rlnorm(n=1000,meanlog=jbpdsLocation,sdlog=jbpdsLocationSE))
#Sigma, scale parameter
hist(rlnorm(n=1000,meanlog=jbpdsScale,sdlog=jbpdsScaleSE))
#what I get if I generate random samples from my prior using MLE parameterization
hist(rweibull(n=1000,jbpdsMLE_shape,jbpdsMLE_scale))

#### Stan Model ####
n=length(jbpdsY)
stan_data <- list(n=n,jbpdsY=jbpdsY,jbpdsMLE_shape=jbpdsMLE_shape,jbpdsMLE_shapeSE=jbpdsMLE_shapeSE,
                  jbpdsMLE_scale=jbpdsMLE_scale,jbpdsMLE_scaleSE=jbpdsMLE_scaleSE)


fit2 <- stan(file="stantest2.stan", data = stan_data, warmup = 5000, iter = 10000, chains = 3, cores = 1, thin = 1)
fit2
plot(fit2)


hist(rweibull(n=1000,shape=1.76,scale=16.44))

#Convergence Diagnostics
s2 <- mcmc.list(lapply(1:ncol(fit2), function(x) mcmc(as.array(fit2)[,x,])))
S2 <- ggs(s2)

#sigma and alpha traceplots
ggs_traceplot(S2,family="sigma")+ggtitle("JBPDS Sigma Traceplot")+ylab("Parameter Value")+
  theme_dark()
ggs_traceplot(S2,family="alpha")+ggtitle("JBPDS Alpha Traceplot")+ylab("Parameter Value")+
  theme_dark()

summary(fit2)

library(rstan)
library(StanHeaders)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)

#Load data
rwsY<-c(1.0000000, 7.1333333, 15.6000000, 21.3333333, 25.8333333, 14.6333333, 4.3666667)
#This is data from an expert that I'm trying to use as an informed prior
rwsFreqs<-c(1.75,1.75, 2.75,2.75,3.75,3.75,3.75,4.5,4.5,5.0,6.75,6.75,6.75,
            6.75,7.5,7.5,8,9.25,9.25,10.75)
rwsFreqs<-rwsFreqs*12

#rws
#Find MLE mu and sigma for shape parameter
rwsMLE<-fitdist(rwsFreqs,"weibull",method="mle")
denscomp(list(rwsMLE),legendtext="Weibull",xlab="rws Weibull")
rwsMLE_shape<-as.numeric(rwsMLE$estimate[1])
rwsMLE_shapeSE<-as.numeric(rwsMLE$sd[1])
rwsMLE_scale<-as.numeric(rwsMLE$estimate[2])
rwsMLE_scaleSE<-as.numeric(rwsMLE$sd[2])
#convert to lognormal mean and se
rwsLocation<-log(rwsMLE_shape^2/sqrt(rwsMLE_shapeSE^2+rwsMLE_shape^2))
rwsLocationSE<-sqrt(log(1+rwsMLE_shapeSE^2/rwsMLE_shape^2))
rwsLocationTau<-(1/rwsLocationSE)^2

#Get Full lognormal distributions for alpha and sigma parameters in weibull
rwsScale<-log(rwsMLE_scale^2/sqrt(rwsMLE_scaleSE^2+rwsMLE_scale^2))
rwsScaleSE<-sqrt(log(1+rwsMLE_scaleSE^2/rwsMLE_scale^2))
rwsScaleTau<-(1/rwsScaleSE)^2


#What the histograms should look like
#Alpha, shape parameter
hist(rlnorm(n=1000,meanlog=rwsLocation,sdlog=rwsLocationSE))
#Sigma, scale parameter
hist(rlnorm(n=1000,meanlog=rwsScale,sdlog=rwsScaleSE))
#what I get if I generate random samples from my prior using MLE parameterization
hist(rweibull(n=1000,rwsMLE_shape,rwsMLE_scale))

#### Stan Model ####
n=length(rwsY)
stan_data <- list(n=n,rwsY=rwsY,rwsMLE_shape=rwsMLE_shape,rwsMLE_shapeSE=rwsMLE_shapeSE,
                  rwsMLE_scale=rwsMLE_scale,rwsMLE_scaleSE=rwsMLE_scaleSE)


fit3 <- stan(file="stantest3.stan",data = stan_data, warmup = 100000, iter = 200000, chains = 3, cores = 2, thin = 1)
fit3

hist(rweibull(n=1000,shape=.91,scale=13.21))
plot(fit3)

#Convergence Diagnostics
s3 <- mcmc.list(lapply(1:ncol(fit3), function(x) mcmc(as.array(fit)[,x,])))

S3 <- ggs(s3)

#sigma and alpha traceplots
ggs_traceplot(S3,family="sigma",original_burnin=TRUE)+ggtitle("RWS Sigma Traceplot")+ylab("Parameter Value")+
  theme_dark()
ggs_traceplot(S3,family="alpha",original_burnin=TRUE)+ggtitle("RWS Alpha Traceplot")+ylab("Parameter Value")+
  theme_dark()

summary(fit3)



library(rstan)
library(StanHeaders)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)

#Load data
cbmsY<-c(25.26666,17.8,7.83333,10.43333,30.433333,17.866666,19.9)
#This is data from an expert that I'm trying to use as an informed prior
cbmsFreqs<-c(1,1.5,1.5,1.5,2.75,2.75,2.75,3.75,4.75,4.75,4.75,4.75,6,6,6,
             6.5,7.75,7.75,7.75,9)
cbmsFreqs<-12*cbmsFreqs

#CBMS
#Find MLE mu and sigma for shape parameter
cbmsMLE<-fitdist(cbmsFreqs,"weibull",method="mle")
denscomp(list(cbmsMLE),legendtext="Weibull",xlab="CBMS Weibull")
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
                  cbmsMLE_scale=cbmsMLE_scale,cbmeMLE_scaleSE=cbmsMLE_scaleSE)


write("
    data{
      int n;
      real cbmsMLE_shape;
      real cbmsMLE_shapeSE;
      real cbmsMLE_scale;
      real cbmsMLE_scaleSE;
      vector[n] cbmsY;
    }
    
    //Parameters
    parameters{
      //priors from MLE
        real alpha;
        real sigma;
    }
    
    transformed parameters{
          //adjust location to lognormal
          real cbmsLocation =log(cbmsMLE_shape^2/sqrt(cbmsMLE_shapeSE^2+cbmsMLE_shape^2));
          real cbmsLocationSE = sqrt(log(1+cbmsMLE_shapeSE^2/cbmsMLE_shape^2));
          real cbmsLocationTau = (1/cbmsLocationSE)^2;
          
          //adjust scale to lognormal
          real cbmsScale = log(cbmsMLE_scale^2/sqrt(cbmsMLE_scaleSE^2+cbmsMLE_scale^2));
          real cbmsScaleSE = sqrt(log(1+cbmsMLE_scaleSE^2/cbmsMLE_scale^2));
          real cbmsScaleTau = (1/cbmsScaleSE)^2;
    }
    
  //Model
    model{
    //Priors
    alpha ~ lognormal(cbmsLocation,cbmsLocationTau);
    sigma ~ lognormal(cbmsScale,cbmsScaleTau);
    
    //Likelihood 
    cbmsY ~ weibull(alpha,sigma);
    }
  generated quantities{
    }",
  "mod.stan")

stanc("mod.stan")
stan_model="mod.stan"

fit <- stan(file=stan_model, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)



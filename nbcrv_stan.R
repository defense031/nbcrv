library(rstan)
library(tidyverse)
library(ggplot2)
library(coda)

#Load data
stanData<-read.csv("cost_RECLEANED.csv")

#Calculate mean and variance of expert priors via MLE
    #RWS
    rwsMLE<-fitdist(rwsFreqs,"weibull",method="mle")
    denscomp(list(rwsMLE),legendtext=plot.legend,xlab="RWS Weibull")
    rwsMLE_mu<-rwsMLE$estimate[1]
    rwsMLE_sigma<-rwsMLE$sd[1]
    #Convert to lognormal mean and sigma
    rwsLocation<-log(rwsMLE_mu^2/sqrt(rwsMLE_sigma^2+rwsMLE_mu^2))
    rwsShape<-sqrt(log(1+rwsMLE_sigma^2/rwsMLE_mu^2))
    hist(rlnorm(n=10000,meanlog=rwsLocation,sdlog=rwsShape))

    #CBMS
    #Find MLE mu and sigma for shape parameter
    cbmsMLE<-fitdist(cbmsFreqs,"weibull",method="mle")
    denscomp(list(cbmsMLE),legendtext=plot.legend,xlab="CBMS Weibull")
    cbmsMLE_mu<-as.numeric(cbmsMLE$estimate[1])
    cbmsMLE_sigma<-as.numeric(cbmsMLE$estimate[2])
    #convert to lognormal mean and sigma
    cbmsLocation<-log(cbmsMLE_mu^2/sqrt(cbmsMLE_sigma^2+cbmsMLE_mu^2))
    cbmsShape<-sqrt(log(1+cbmsMLE_sigma^2/cbmsMLE_mu^2))
    
    sqrt(cbmsMLE_mu^2*(exp(cbmsShape^2)-1))
    
mean(rweibull(n=10000,shape=cbmsMLE_mu,scale=cbmsMLE_sigma))

    #Graph of CBMS prior
    hist(rlnorm(n=10000,
           meanlog=cbmsLocation,
           sdlog=cbmsShape))
    

    #JBPDS
    #Find MLE mu and sigma for shape parameter
    jbpdsMLE<-fitdist(jbpdsFreqs,"weibull",method="mle")
    denscomp(list(jbpdsMLE),legendtext=plot.legend,xlab="JBPDS Weibull")
    jbpdsMLE_mu<-as.numeric(jbpdsMLE$estimate[1])
    jbpdsMLE_sigma<-as.numeric(jbpdsMLE$sd[1])
    #convert to lognormal mean and sigma
    jbpdsLocation<-log(jbpdsMLE_mu^2/sqrt(jbpdsMLE_sigma^2+jbpdsMLE_mu^2))
    jbpdsShape<-sqrt(log(1+jbpdsMLE_sigma^2/jbpdsMLE_mu^2))
      #Graph of jbpds prior
      hist(rlnorm(n=10000,meanlog=jbpdsLocation,sdlog=jbpdsShape))
      



fe <- fitdist(credit$turnInLag, "exp")
##Ended up just going with an exponential.  All of the ones over 100 days occured in September
plot.legend <- "Weibull using MLE"
denscomp(list(fe), legendtext ="Fit",xlab="Count of Turn In Lag (Days)",
         main="Observed Turn in Lag")
summary(glm(turnInLag~Received.Credit,data=credit,family=poisson))


#Seems as though most turn in credits show up in under 40 days
#91% of turn ins are completed in under 40 days...I'm combortable saying it's a month
length(which(credit$turnInLag<40))/length(credit$turnInLag)



#Specify data
    #vehicles, v
    v=seq(from=1,to=4,by=1)

    #creates uninformative prior for gamma parameter (Preventive maintenance effect)
    k=.001
    ro=.001
    
    #Create allotment to pass into funding vector once per quarter, 
    #funding at m=0 will start with 60000, so allotment[1]=0
    allotment=c(0,0,0,60000,0,0,60000,0,0,60000,0,0)


mod.stan=
  "
### Data Block
    data{
    data=stanData
    k=k
    ro=ro
    allotment=allotment
      
    }
    //Parameters
    parameters{
    
    int N;
    real funding;
    real cost;
    int<lower=1,upper=12> m;
    
    //vehicles 1-4
      real V1_Fail;
      real V2_Fail;
      real V3_Fail;
      real V4_Fail;
    
    //priors from MLE
      real rwsShape;
      real rwsLocation;
      real cbmsShape;
      real cbmsLocation
      real jbpdsShape;
      real jbpdsLocation;
      
    //Preventinve Maintenance (this would be nice to add in to the model but I'm not sure how
    //without making a second weibull draw?)
      //binary, whether PM occurred or not based on funds availability
      //real PM;
      //effect of PM
      //real gamma;
      
    //Boolean failure variables for component of each vehicle
      int cbms1_Fail>(int m, int cbms1)
      int cbms2_Fail>(int m, int cbms2)
      int cbms3_Fail>(int m, int cbms3)
      int cbms4_Fail>(int m, int cbms4)
      int rws1_Fail>(int m, int rws1)
      int rws2_Fail>(int m, int rws2)
      int rws3_Fail>(int m, int rws3)
      int rws4_Fail>(int m, int rws4)
      int jbpds1_Fail>(int m, int jbpds1)
      int jbpds2_Fail>(int m, int jbpds2)
      int jbpds3_Fail>(int m, int jbpds3)
      int jbpds4_Fail>(int m, int jbpds4)
    }
    
  //Model
    model{
  
  for(i in 1:N){ 
   for(m in 1:12){ 
   
    //Prior for PM
    gamma[m]~rgamma(k,ro)
    
    //Likelihood 
    cbms1[m]~rweib(cbmsShape[m], cbmsLocation[m] + I(PM[m])*gamma[m])
    cbms2[m]~rweib(cbmsShape[m], cbmsLocation[m] + I(PM[m])*gamma[m])
    cbms3[m]~rweib(cbmsShape[m], cbmsLocation[m] + I(PM[m])*gamma[m])
    cbms4[m]~rweib(cbmsShape[m], cbmsLocation[m] + I(PM[m])*gamma[m])
    
    rws1[m]~rweib(rwsShape[m], rwsLocation[m] + I(PM[m])*gamma[m])
    rws2[m]~rweib(rwsShape[m], rwsLocation[m] + I(PM[m])*gamma[m])
    rws3[m]~rweib(rwsShape[m], rwsLocation[m] + I(PM[m])*gamma[m])
    rws4[m]~rweib(rwsShape[m], rwsLocation[m] + I(PM[m])*gamma[m])
    
    jbpds1[m]~rweib(jbpdsShape[m], jbpdsLocation[m] + I(PM[m])*gamma[m])
    jbpds2[m]~rweib(jbpdsShape[m], jbpdsLocation[m] + I(PM[m])*gamma[m])
    jbpds3[m]~rweib(jbpdsShape[m], jbpdsLocation[m] + I(PM[m])*gamma[m])
    jbpds4[m]~rweib(jbpdsShape[m], jbpdsLocation[m] + I(PM[m])*gamma[m])
    
    }
    
    
  //Transformed Parameters
    transformed parameters{
      
    //Vehicle failure status based on failure of any single component
      V1_Fail[m]= max(c(cbms1_Fail,rws1_Fail,jbpds1_Fail))
      V2_Fail[m]= max(c(cbms2_Fail,rws2_Fail,jbpds2_Fail))
      V3_Fail[m]= max(c(cbms3_Fail,rws3_Fail,jbpds3_Fail))
      V4_Fail[m]= max(c(cbms4_Fail,rws4_Fail,jbpds4_Fail))
      
      //Find Total Cost to Fix plus routine cost of 13,000 per month
      cost[m]=367000*sum(cbms1_Fail[m]+cbms2_Fail[m]+cbms3_Fail[m]+cbms4_Fail[m])+
              121000*sum(rws1_Fail[m]+rws2_Fail[m]+rws3_Fail[m]+rws4_Fail[m])+
              171000*sum(jbpds1_Fail[m]+jbpds2_Fail[m]+jbpds3_Fail[m]+jbpds4_Fail[m])+
              13000
              
      //Find credits based on credit amount if failure occurred in month prior
      credit[m]=170000*sum(cbms1_Fail[m-1]+cbms2_Fail[m-1]+cbms3_Fail[m-1]+cbms4_Fail[m-1])+
                35000*sum(rws1_Fail[m-1]+rws2_Fail[m-1]+rws3_Fail[m-1]+rws4_Fail[m-1])+
                35000*sum(jbpds1_Fail[m-1]+jbpds2_Fail[m-1]+jbpds3_Fail[m-1]+jbpds4_Fail[m-1])+
      
      //Funding at end of month is equal to the starting funding less costs + credits
      f[0]=60000
      funding[m]=funding[m-1]-cost[m]+credit[m]+allotment[m]+allotment
      
      
  //Test if pieces are broken and fix them each month
      if (V1_Fail[m]>0){
        if funding[m]>cost[m]{
      }
      
         if (V2_Fail[m]>0){
        if funding[m]>cost[m]{
          funding=funding[m]-cost[m]
        }
      }
      
      //Readiness as a % of vehicles FMC
      R[m]= (4-sum(c(V1,V2,V3,V4)))/4
      
    }
    }
    
    
    "
    
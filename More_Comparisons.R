library(fitdistrplus)
library(tidyverse)

####CBMS
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



####JBPDS
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



###RWS
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
  
  
  
cbms_alphaPriorMean<-2.09
cbms_alphaPriorSE<-.38
cbms_sigmaPriorMean<-63.22
cbms_sigmaPriorSE<-7.11
#Converting to log mean and sd for each plot
#Alpha CBMS Prior
cbmsLocation_alphaPriorMean<-log(cbms_alphaPriorMean^2/sqrt(cbms_alphaPriorSE^2+cbms_alphaPriorMean^2))
cbmsLocation_alphaPriorSE<-sqrt(log(1+cbms_alphaPriorSE^2/cbms_alphaPriorMean^2))
#Sigma CBMS Prior
cbmsScale_sigmaPriorMean<-log(cbms_sigmaPriorMean^2/sqrt(cbms_sigmaPriorSE^2+cbms_sigmaPriorMean^2))
cbmsScale_sigmaPriorSE<-sqrt(log(1+cbms_sigmaPriorSE^2/cbms_sigmaPriorMean^2))

###RWS Priors
rws_alphaPriorMean<-2.41
rws_alphaPriorSE<-.43
rws_sigmaPriorMean<-77.01
rws_sigmaPriorSE<-7.51
#Alpha RWS Prior
rwsLocation_alphaPriorMean<-log(rws_alphaPriorMean^2/sqrt(rws_alphaPriorSE^2+rws_alphaPriorMean^2))
rwsLocation_alphaPriorSE<-sqrt(log(1+cbms_alphaPriorSE^2/cbms_alphaPriorMean^2))
#Sigma RWS Prior
rwsScale_sigmaPriorMean<-log(rws_sigmaPriorMean^2/sqrt(rws_sigmaPriorSE^2+rws_sigmaPriorMean^2))
rwsScale_sigmaPriorSE<-sqrt(log(1+rws_sigmaPriorSE^2/rws_sigmaPriorMean^2))


###JBPDS Priors
jbpds_alphaPriorMean<-4.96
jbpds_alphaPriorSE<-.91
jbpds_sigmaPriorMean<-103.4
jbpds_sigmaPriorSE<-4.89

#JBPDS Alpha Prior
jbpdsLocation_alphaPriorMean<-log(jbpds_alphaPriorMean^2/sqrt(jbpds_alphaPriorSE^2+jbpds_alphaPriorMean^2))
jbpdsLocation_alphaPriorSE<-sqrt(log(1+jbpds_alphaPriorSE^2/jbpds_alphaPriorMean^2))
#Sigma JBPDS Prior
jbpdsScale_sigmaPriorMean<-log(jbpds_sigmaPriorMean^2/sqrt(jbpds_sigmaPriorSE^2+jbpds_sigmaPriorMean^2))
jbpdsScale_sigmaPriorSE<-sqrt(log(1+jbpds_sigmaPriorSE^2/jbpds_sigmaPriorMean^2))

#####Posterior#####

###CBMS Posterior
cbms_alphaPostMean<-4.3
cbms_alphaPostSE<-.04
cbms_sigmaPostMean<-24.45
cbms_sigmaPostSE<-.39

#Location posterior alpha
cbmsLocation_alphaPostMean<-log(cbms_alphaPostMean^2/sqrt(cbms_alphaPostSE^2+cbms_alphaPostMean^2))
cbmsLocation_alphaPostSE<-sqrt(log(1+cbms_alphaPostSE^2/cbms_alphaPostMean^2))
#Sigma CBMS Posterior
cbmsScale_sigmaPostMean<-log(cbms_sigmaPriorMean^2/sqrt(cbms_sigmaPriorSE^2+cbms_sigmaPostMean^2))
cbmsScale_sigmaPostSE<-sqrt(log(1+cbms_sigmaPriorSE^2/cbms_sigmaPriorMean^2))

###RWS
rws_alphaPostMean<-1.22
rws_alphaPostSE<-.001
rws_sigmaPostMean<-15.59
rws_sigmaPostSE<-.05

#Alpha RWS Post
rwsLocation_alphaPostMean<-log(rws_alphaPostMean^2/sqrt(rws_alphaPostSE^2+rws_alphaPostMean^2))
rwsLocation_alphaPostSE<-sqrt(log(1+cbms_alphaPostSE^2/cbms_alphaPostMean^2))
#Sigma RWS Post
rwsScale_sigmaPostMean<-log(rws_sigmaPostMean^2/sqrt(rws_sigmaPostSE^2+rws_sigmaPostMean^2))
rwsScale_sigmaPostSE<-sqrt(log(1+rws_sigmaPostSE^2/rws_sigmaPostMean^2))

###JBPDS
jbpds_alphaPostMean<-1.76
jbpds_alphaPostSE<-.02
jbpds_sigmaPostMean<-16.44
jbpds_sigmaPostSE<-.23
#JBPDS Alpha Post
jbpdsLocation_alphaPostMean<-log(jbpds_alphaPostMean^2/sqrt(jbpds_alphaPostSE^2+jbpds_alphaPostMean^2))
jbpdsLocation_alphaPostSE<-sqrt(log(1+jbpds_alphaPostSE^2/jbpds_alphaPostMean^2))
#Sigma JBPDS Post
jbpdsScale_sigmaPostMean<-log(jbpds_sigmaPostMean^2/sqrt(jbpds_sigmaPostSE^2+jbpds_sigmaPostMean^2))
jbpdsScale_sigmaPostSE<-sqrt(log(1+jbpds_sigmaPostSE^2/jbpds_sigmaPostMean^2))


###CBMS Alpha
N=5
len=.001
NN<-length(seq(from=0,to=N,by=len))
cbmsComp_alpha<-as.data.frame(matrix(data=NA,nrow=2*length(seq(from=0,to=N,by=len)),ncol=3))
colnames(cbmsComp_alpha)<-c("nums","dens","Prior")

cbmsComp_alpha[1:NN,1]<-seq(from=0,to=N,by=len)
cbmsComp_alpha[(NN+1):(NN*2),1]<-seq(from=0,to=N,by=len)
cbmsComp_alpha[1:NN,3]<-FALSE
cbmsComp_alpha[(NN+1):(NN*2),3]<-TRUE
cbmsComp_alpha$Prior<-as.factor(cbmsComp_alpha$Prior)
#Prior
cbmsComp_alpha[1:NN,2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=cbmsLocation_alphaPriorMean,sdlog=cbmsLocation_alphaPriorSE)
#Posterior
cbmsComp_alpha[(NN+1):(NN*2),2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=cbmsLocation_alphaPostMean,sdlog=cbmsLocation_alphaPostSE)
#Plot
yy<-ggplot(data=cbmsComp_alpha,aes(x=nums,y=dens))
yy+geom_line(aes(col=Prior),size=1.5)+ggtitle("CBMS Alpha Prior vs Posterior Distribution (LogNormal)")+
  xlab("Weight")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+
  theme_dark()

###CBMS Sigma
N=250
len=.1
NN<-length(seq(from=0,to=N,by=len))
cbmsComp_sigma<-as.data.frame(matrix(data=NA,nrow=2*length(seq(from=0,to=N,by=len)),ncol=3))
colnames(cbmsComp_sigma)<-c("nums","dens","Prior")

cbmsComp_sigma[1:NN,1]<-seq(from=0,to=N,by=len)
cbmsComp_sigma[(NN+1):(NN*2),1]<-seq(from=0,to=N,by=len)
cbmsComp_sigma[1:NN,3]<-FALSE
cbmsComp_sigma[(NN+1):(NN*2),3]<-TRUE
cbmsComp_sigma$Prior<-as.factor(cbmsComp_sigma$Prior)
#Prior
cbmsComp_sigma[1:NN,2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=cbmsScale_sigmaPriorMean,sdlog=cbmsScale_sigmaPriorSE)
#Posterior
cbmsComp_sigma[(NN+1):(NN*2),2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=cbmsScale_sigmaPostMean,sdlog=cbmsScale_sigmaPostSE)
#Plot
yy<-ggplot(data=cbmsComp_sigma,aes(x=nums,y=dens))
yy+geom_line(aes(col=Prior),size=1.5)+ggtitle("CBMS Sigma Prior vs Posterior Distribution (LogNormal)")+
  xlab("Weight")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+
  theme_dark()




#### JBPDS Plots
###JBPDS Alpha
N=7
len=.01
NN<-length(seq(from=0,to=N,by=len))
jbpdsComp_alpha<-as.data.frame(matrix(data=NA,nrow=2*length(seq(from=0,to=N,by=len)),ncol=3))
colnames(jbpdsComp_alpha)<-c("nums","dens","Prior")

jbpdsComp_alpha[1:NN,1]<-seq(from=0,to=N,by=len)
jbpdsComp_alpha[(NN+1):(NN*2),1]<-seq(from=0,to=N,by=len)
jbpdsComp_alpha[1:NN,3]<-FALSE
jbpdsComp_alpha[(NN+1):(NN*2),3]<-TRUE
jbpdsComp_alpha$Prior<-as.factor(jbpdsComp_alpha$Prior)
#Prior
jbpdsComp_alpha[1:NN,2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=jbpdsLocation_alphaPriorMean,sdlog=jbpdsLocation_alphaPriorSE)
#Posterior
jbpdsComp_alpha[(NN+1):(NN*2),2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=jbpdsLocation_alphaPostMean,sdlog=jbpdsLocation_alphaPostSE)
#Plot
yy<-ggplot(data=jbpdsComp_alpha,aes(x=nums,y=dens))
yy+geom_line(aes(col=Prior),size=1.5)+ggtitle("JBPDS Alpha Prior vs Posterior Distribution (LogNormal)")+
  xlab("Weight")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+
  theme_dark()

###JBPDS Sigma
N=125
len=.1
NN<-length(seq(from=0,to=N,by=len))
jbpdsComp_sigma<-as.data.frame(matrix(data=NA,nrow=2*length(seq(from=0,to=N,by=len)),ncol=3))
colnames(jbpdsComp_sigma)<-c("nums","dens","Prior")

jbpdsComp_sigma[1:NN,1]<-seq(from=0,to=N,by=len)
jbpdsComp_sigma[(NN+1):(NN*2),1]<-seq(from=0,to=N,by=len)
jbpdsComp_sigma[1:NN,3]<-FALSE
jbpdsComp_sigma[(NN+1):(NN*2),3]<-TRUE
jbpdsComp_sigma$Prior<-as.factor(jbpdsComp_sigma$Prior)
#Prior
jbpdsComp_sigma[1:NN,2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=jbpdsScale_sigmaPriorMean,sdlog=jbpdsScale_sigmaPriorSE)
#Posterior
jbpdsComp_sigma[(NN+1):(NN*2),2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=jbpdsScale_sigmaPostMean,sdlog=jbpdsScale_sigmaPostSE)
#Plot
yy<-ggplot(data=jbpdsComp_sigma,aes(x=nums,y=dens))
yy+geom_line(aes(col=Prior),size=1)+ggtitle("JBPDS Sigma Prior vs Posterior Distribution (LogNormal)")+
  xlab("Weight")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+
  theme_dark()



### RWS Plots
###RWS Alpha
N=3.5
len=.001
NN<-length(seq(from=0,to=N,by=len))
rwsComp_alpha<-as.data.frame(matrix(data=NA,nrow=2*length(seq(from=0,to=N,by=len)),ncol=3))
colnames(rwsComp_alpha)<-c("nums","dens","Prior")

rwsComp_alpha[1:NN,1]<-seq(from=0,to=N,by=len)
rwsComp_alpha[(NN+1):(NN*2),1]<-seq(from=0,to=N,by=len)
rwsComp_alpha[1:NN,3]<-FALSE
rwsComp_alpha[(NN+1):(NN*2),3]<-TRUE
rwsComp_alpha$Prior<-as.factor(rwsComp_alpha$Prior)
#Prior
rwsComp_alpha[1:NN,2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=rwsLocation_alphaPriorMean,sdlog=rwsLocation_alphaPriorSE)
#Posterior
rwsComp_alpha[(NN+1):(NN*2),2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=rwsLocation_alphaPostMean,sdlog=rwsLocation_alphaPostSE)
#Plot
yy<-ggplot(data=rwsComp_alpha,aes(x=nums,y=dens))
yy+geom_line(aes(col=Prior),size=1)+ggtitle("RWS Alpha Prior vs Posterior Distribution (LogNormal)")+
  xlab("Weight")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+
  theme_dark()

###CBMS Sigma
N=90
len=.1
NN<-length(seq(from=0,to=N,by=len))
rwsComp_sigma<-as.data.frame(matrix(data=NA,nrow=2*length(seq(from=0,to=N,by=len)),ncol=3))
colnames(rwsComp_sigma)<-c("nums","dens","Prior")

rwsComp_sigma[1:NN,1]<-seq(from=0,to=N,by=len)
rwsComp_sigma[(NN+1):(NN*2),1]<-seq(from=0,to=N,by=len)
rwsComp_sigma[1:NN,3]<-FALSE
rwsComp_sigma[(NN+1):(NN*2),3]<-TRUE
rwsComp_sigma$Prior<-as.factor(rwsComp_sigma$Prior)
#Prior
rwsComp_sigma[1:NN,2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=rwsScale_sigmaPriorMean,sdlog=rwsScale_sigmaPriorSE)
#Posterior
rwsComp_sigma[(NN+1):(NN*2),2]<-dlnorm(x=seq(from=0,to=N,by=len),meanlog=rwsScale_sigmaPostMean,sdlog=rwsScale_sigmaPostSE)
#Plot
yy<-ggplot(data=rwsComp_sigma,aes(x=nums,y=dens))
yy+geom_line(aes(col=Prior),size=1)+ggtitle("RWS Sigma Prior vs Posterior Distribution (LogNormal)")+
  xlab("Weight")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+
  theme_dark()




###Prior and Posterior component weibull plots
#CBMS
N1=100
len1=.1
NN1<-length(seq(from=0,to=N1,by=len1))
cbmsWeibComp<-as.data.frame(matrix(data=NA,nrow=2*NN1,ncol=3))
colnames(cbmsWeibComp)<-c("nums","dens","Prior")
cbmsWeibComp[1:NN1,1]<-seq(from=0,to=N1,by=len1)
cbmsWeibComp[(NN1+1):(NN1*2),1]<-seq(from=0,to=N1,by=len1)
cbmsWeibComp[1:NN1,3]<-FALSE
cbmsWeibComp[(NN1+1):(NN1*2),3]<-TRUE
cbmsWeibComp$Prior<-as.factor(cbmsWeibComp$Prior)
#Prior
cbmsWeibComp[1:NN1,2]<-dweibull(x=seq(from=0,to=N1,by=len1),shape=2.09,scale=63.22)
#Posterior
cbmsWeibComp[(NN1+1):(NN1*2),2]<-dweibull(x=seq(from=0,to=N1,by=len1),shape=4.3,scale=24.45)
#Plot
cbmsWeibPlot<-ggplot(data=cbmsWeibComp,aes(x=nums,y=dens))
cbmsWeibPlot+geom_line(aes(col=Prior),size=1)+ggtitle("CBMS Prior vs Posterior Given Mean Alpha/Sigma Values")+
  xlab("Months")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+theme_dark()

#JBPDS
N2=150
len2=.1
NN2<-length(seq(from=0,to=N2,by=len1))
jbpdsWeibComp<-as.data.frame(matrix(data=NA,nrow=2*NN2,ncol=3))
colnames(jbpdsWeibComp)<-c("nums","dens","Prior")
jbpdsWeibComp[1:NN2,1]<-seq(from=0,to=N2,by=len2)
jbpdsWeibComp[(NN2+1):(NN2*2),1]<-seq(from=0,to=N2,by=len2)
jbpdsWeibComp[1:NN2,3]<-FALSE
jbpdsWeibComp[(NN2+1):(NN2*2),3]<-TRUE
jbpdsWeibComp$Prior<-as.factor(jbpdsWeibComp$Prior)
#Prior
jbpdsWeibComp[1:NN2,2]<-dweibull(x=seq(from=0,to=N2,by=len2),shape=4.96,scale=103.4)
#Posterior
jbpdsWeibComp[(NN2+1):(NN2*2),2]<-dweibull(x=seq(from=0,to=N2,by=len2),shape=1.76,scale=16.44)
#Plot
jbpdsWeibPlot<-ggplot(data=jbpdsWeibComp,aes(x=nums,y=dens))
jbpdsWeibPlot+geom_line(aes(col=Prior),size=1)+ggtitle("JBPDS Prior vs Posterior Given Mean Alpha/Sigma Values")+
  xlab("Months")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+theme_dark()


#RWS
N3=150
len3=.1
NN3<-length(seq(from=0,to=N3,by=len3))
rwsWeibComp<-as.data.frame(matrix(data=NA,nrow=2*NN3,ncol=3))
colnames(rwsWeibComp)<-c("nums","dens","Prior")
rwsWeibComp[1:NN3,1]<-seq(from=0,to=N3,by=len3)
rwsWeibComp[(NN3+1):(NN3*2),1]<-seq(from=0,to=N3,by=len3)
rwsWeibComp[1:NN3,3]<-FALSE
rwsWeibComp[(NN3+1):(NN3*2),3]<-TRUE
rwsWeibComp$Prior<-as.factor(rwsWeibComp$Prior)
#Prior
rwsWeibComp[1:NN3,2]<-dweibull(x=seq(from=0,to=N3,by=len3),shape=2.41,scale=77.01)
#Posterior
rwsWeibComp[(NN3+1):(NN3*2),2]<-dweibull(x=seq(from=0,to=N3,by=len3),shape=1.22,scale=15.59)
#Plot
rwsWeibPlot<-ggplot(data=rwsWeibComp,aes(x=nums,y=dens))
rwsWeibPlot+geom_line(aes(col=Prior),size=1)+ggtitle("RWS Prior vs Posterior Given Mean Alpha/Sigma Values")+
  xlab("Months")+ylab("Density")+scale_colour_discrete(name  ="Distribution",
                                                       labels=c("Prior", "Posterior"))+theme_dark()



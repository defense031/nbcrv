#This script uses data from the bayesian models in STAN to simulate readiness
#of a fleet of 4 NBCRV vehicles over the course of a Fiscal Year

library(scales)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)


###### Readiness Simulation ######
#Estimates from posterior stan analysis
#Weibull estimates
cbmsAlpha<-4.3
cbmsAlphaSD<-.1
cbmsSigma<-24.45
cbmsSigmaSD<-.15
jbpdsAlpha<-1.76
jbpdsSigma<-16.44
rwsAlpha<-1.22
rwsSigma<-15.59

#N is the number of years to run the simulation 
#simlength is the length of each simulation-1
N<-1000
budget<-800000
simLength<-12*N+1
index<-seq(from=1,to=simLength-12,by=12)
grandTotals<-as.data.frame(matrix(0,ncol=11,nrow=simLength-1))
totalSummary<-as.data.frame(matrix(NA,ncol=11,nrow=12))
colnames(totalSummary)<-c("month","cost","v1","v2","v3","v4","readiness","cbms","jbpds","rws","overBudget")
colnames(grandTotals)<-colnames(totalSummary)

for(n in index){
  
#Simulation to find costs per NBCRV for a year
    #V1
        #Initialize matrix to capture CBMS 
        V1<-as.data.frame(matrix(data=NA,ncol=22,nrow=13))
        colnames(V1)<-c("month",
                        "cbmsAge","cbmsDraw","cbmsFailed","cbmsCost","cbmsCredit","cbmsInop",
                        "jbpdsAge","jbpdsDraw","jbpdsFailed","jbpdsCost","jbpdsCredit","jbpdsInop",
                        "rwsAge","rwsDraw","rwsFailed","rwsCost","rwsCredit","rwsInop",
                        "routineCosts","vehicleCost","nmc")
        V1$month<-seq(from=1,to=13,by=1)
        V1$routineCosts<-3250
        V1$nmc<-0
        
    #CBMS V1
        #Set up values for first round 
        cbmsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=cbmsAlpha,scale=cbmsSigma)),digits=1)
        V1$cbmsAge<-seq(from=cbmsInit,by=1,to=cbmsInit+12)
        V1$cbmsDraw<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsSigma)
        V1$cbmsFailed<-V1$cbmsAge>V1$cbmsDraw
        V1$cbmsCost<-0
        V1$cbmsInop<-FALSE
        V1$cbmsCredit<-0
        
        #Loop through to test if there are any CBMS Failures.  Fix them that month if budget allows.  
        #Record number of Fails and month.
        for(i in 1:12){
          if(sum(V1$cbmsFailed[(i+1):12]>0)){
            if(V1$cbmsFailed[i]==TRUE){
              V1$cbmsAge[i+1]<-0
              V1$cbmsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
              V1$cbmsDraw[(i+1):13]<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsSigma)
              V1$cbmsFailed[(i+1):13]<-FALSE
              V1$cbmsFailed[(i+1):13]<-V1$cbmsAge[(i+1):13]>V1$cbmsDraw[(i+1):13]
              #determine if high or low cost part failed and multiply by cost
              cbmsType<-rmultinom(n=1,size=1,prob=c(5/7,2/7))
              V1$cbmsCost[i]<-max(cbmsType*c(600000,158199))
              V1$cbmsCredit[i]<-max(cbmsType*c(170112,0))
              V1$cbmsInop[i]<-TRUE
            }
          }
        }
    
    
    #JBPDS V1
    
        #Set up values for first round 
        jbpdsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=jbpdsAlpha,scale=jbpdsSigma)),digits=1)
        V1$jbpdsAge<-seq(from=jbpdsInit,by=1,to=jbpdsInit+12)
        V1$jbpdsDraw<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
        V1$jbpdsFailed<-V1$jbpdsAge>V1$jbpdsDraw
        V1$jbpdsCost<-0
        V1$jbpdsInop<-FALSE
        V1$jbpdsCredit<-0
        
        #Loop through to test if there are any jbpds Failures.  Fix them that month.  
        #Record number of Fails and month.
        for(i in 1:12){
          if(sum(V1$jbpdsFailed[(i+1):12]>0)){
            if(V1$jbpdsFailed[i]==TRUE){
              V1$jbpdsAge[i+1]<-0
              V1$jbpdsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
              V1$jbpdsDraw[(i+1):13]<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
              V1$jbpdsFailed[(i+1):13]<-FALSE
              V1$jbpdsFailed[(i+1):13]<-V1$jbpdsAge[(i+1):13]>V1$jbpdsDraw[(i+1):13]
              #determine if high or low cost part failed and multiply by cost
              jbpdsType<-rmultinom(n=1,size=1,prob=c(.15,.7,.15))
              V1$jbpdsCost[i]<-max(jbpdsType*c(160017,22524,3515))
              V1$jbpdsCredit[i]<-max(jbpdsType*c(98112,12565,0))
              V1$jbpdsInop[i]<-max(jbpdsType*c(TRUE,TRUE,FALSE))
            }
          }
        }
        
    #RWS V1
        
        #Set up values for first round 
        rwsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=rwsAlpha,scale=rwsSigma)),digits=1)
        V1$rwsAge<-seq(from=rwsInit,by=1,to=rwsInit+12)
        V1$rwsDraw<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
        V1$rwsFailed<-V1$rwsAge>V1$rwsDraw
        V1$rwsCost<-0
        V1$rwsInop<-FALSE
        V1$rwsCredit<-0
        
        #Loop through to test if there are any rws Failures.  Fix them that month.  
        #Record number of Fails and month.
        for(i in 1:12){
          if(sum(V1$rwsFailed[(i+1):12]>0)){
            if(V1$rwsFailed[i]==TRUE){
              V1$rwsAge[i+1]<-0
              V1$rwsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
              V1$rwsDraw[(i+1):13]<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
              V1$rwsFailed[(i+1):13]<-FALSE
              V1$rwsFailed[(i+1):13]<-V1$rwsAge[(i+1):13]>V1$rwsDraw[(i+1):13]
              #determine if high or low cost part failed and multiply by cost
              rwsType<-rmultinom(n=1,size=1,prob=c(1/8,7/8))
              V1$rwsCost[i]<-max(rwsType*c(161642,190))
              V1$rwsCredit[i]<-max(rwsType*c(74705,0))
              V1$rwsInop[i]<-max(rwsType*c(TRUE,FALSE))
            }
          }
        }
    
    #summary stats for V1
      cbmsTotal1<-sum(V1$cbmsCost)
      jbpdsTotal1<-sum(V1$jbpdsCost)
      rwsTotal1<-sum(V1$rwsCost)
      #Calculate cumulative total cost by month for V1
      V1$vehicleCost[1]<-V1$cbmsCost[1]-V1$cbmsCredit[1]+
                        V1$jbpdsCost[1]-V1$jbpdsCredit[1]+
                         V1$rwsCost[1]-V1$rwsCredit[1]+V1$routineCosts[i]
      for(i in 2:12){
        V1$vehicleCost[i]<-V1$vehicleCost[i-1]+V1$cbmsCost[i]-V1$cbmsCredit[i]+
                                              V1$jbpdsCost[i]-V1$jbpdsCredit[i]+
                                              V1$rwsCost[i]-V1$rwsCredit[i]+V1$routineCosts[i]
      }
      V1$nmc<-as.logical(V1$cbmsInop+V1$jbpdsInop+V1$rwsInop)

#V2
      #Initialize matrix to capture CBMS 
      V2<-as.data.frame(matrix(data=NA,ncol=22,nrow=13))
      colnames(V2)<-c("month",
                      "cbmsAge","cbmsDraw","cbmsFailed","cbmsCost","cbmsCredit","cbmsInop",
                      "jbpdsAge","jbpdsDraw","jbpdsFailed","jbpdsCost","jbpdsCredit","jbpdsInop",
                      "rwsAge","rwsDraw","rwsFailed","rwsCost","rwsCredit","rwsInop",
                      "routineCosts","vehicleCost","nmc")
      V2$month<-seq(from=1,to=13,by=1)
      V2$routineCosts<-3250
      V2$nmc<-0
      
      #CBMS V2
      #Set up values for first round 
      cbmsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=cbmsAlpha,scale=cbmsSigma)),digits=1)
      V2$cbmsAge<-seq(from=cbmsInit,by=1,to=cbmsInit+12)
      V2$cbmsDraw<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsSigma)
      V2$cbmsFailed<-V2$cbmsAge>V2$cbmsDraw
      V2$cbmsCost<-0
      V2$cbmsInop<-FALSE
      V2$cbmsCredit<-0
      
      #Loop through to test if there are any CBMS Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V2$cbmsFailed[(i+1):12]>0)){
          if(V2$cbmsFailed[i]==TRUE){
            V2$cbmsAge[i+1]<-0
            V2$cbmsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V2$cbmsDraw[(i+1):13]<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsScale)
            V2$cbmsFailed[(i+1):13]<-FALSE
            V2$cbmsFailed[(i+1):13]<-V2$cbmsAge[(i+1):13]>V2$cbmsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            cbmsType<-rmultinom(n=1,size=1,prob=c(5/7,2/7))
            V2$cbmsCost[i]<-max(cbmsType*c(600000,158199))
            V2$cbmsCredit[i]<-max(cbmsType*c(170112,0))
            V2$cbmsInop[i]<-TRUE
          }
        }
      }
      
      
      #JBPDS V2
      
      #Set up values for first round 
      jbpdsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=jbpdsAlpha,scale=jbpdsSigma)),digits=1)
      V2$jbpdsAge<-seq(from=jbpdsInit,by=1,to=jbpdsInit+12)
      V2$jbpdsDraw<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
      V2$jbpdsFailed<-V2$jbpdsAge>V2$jbpdsDraw
      V2$jbpdsCost<-0
      V2$jbpdsInop<-FALSE
      V2$jbpdsCredit<-0
      
      #Loop through to test if there are any jbpds Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V2$jbpdsFailed[(i+1):12]>0)){
          if(V2$jbpdsFailed[i]==TRUE){
            V2$jbpdsAge[i+1]<-0
            V2$jbpdsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V2$jbpdsDraw[(i+1):13]<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
            V2$jbpdsFailed[(i+1):13]<-FALSE
            V2$jbpdsFailed[(i+1):13]<-V2$jbpdsAge[(i+1):13]>V2$jbpdsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            jbpdsType<-rmultinom(n=1,size=1,prob=c(.15,.7,.15))
            V2$jbpdsCost[i]<-max(jbpdsType*c(160017,22524,3515))
            V2$jbpdsCredit[i]<-max(jbpdsType*c(98112,12565,0))
            V2$jbpdsInop[i]<-max(jbpdsType*c(TRUE,TRUE,FALSE))
          }
        }
      }
      
      #RWS V2
      
      #Set up values for first round 
      rwsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=rwsAlpha,scale=rwsSigma)),digits=1)
      V2$rwsAge<-seq(from=rwsInit,by=1,to=rwsInit+12)
      V2$rwsDraw<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
      V2$rwsFailed<-V2$rwsAge>V2$rwsDraw
      V2$rwsCost<-0
      V2$rwsInop<-FALSE
      V2$rwsCredit<-0
      
      #Loop through to test if there are any rws Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V2$rwsFailed[(i+1):12]>0)){
          if(V2$rwsFailed[i]==TRUE){
            V2$rwsAge[i+1]<-0
            V2$rwsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V2$rwsDraw[(i+1):13]<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
            V2$rwsFailed[(i+1):13]<-FALSE
            V2$rwsFailed[(i+1):13]<-V2$rwsAge[(i+1):13]>V2$rwsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            rwsType<-rmultinom(n=1,size=1,prob=c(1/8,7/8))
            V2$rwsCost[i]<-max(rwsType*c(161642,190))
            V2$rwsCredit[i]<-max(rwsType*c(74705,0))
            V2$rwsInop[i]<-max(rwsType*c(TRUE,FALSE))
          }
        }
      }
      
      #summary stats for V2
      cbmsTotal2<-sum(V2$cbmsCost)
      jbpdsTotal2<-sum(V2$jbpdsCost)
      rwsTotal2<-sum(V2$rwsCost)
      #Calculate cumulative total cost by month for V2
      V2$vehicleCost[1]<-V2$cbmsCost[1]-V2$cbmsCredit[1]+
        V2$jbpdsCost[1]-V2$jbpdsCredit[1]+
        V2$rwsCost[1]-V2$rwsCredit[1]+V2$routineCosts[i]
      for(i in 2:12){
        V2$vehicleCost[i]<-V2$vehicleCost[i-1]+V2$cbmsCost[i]-V2$cbmsCredit[i]+
          V2$jbpdsCost[i]-V2$jbpdsCredit[i]+
          V2$rwsCost[i]-V2$rwsCredit[i]+V2$routineCosts[i]
      }
      V2$nmc<-as.logical(V2$cbmsInop+V2$jbpdsInop+V2$rwsInop)
      
      
#V3
      #Initialize matrix to capture CBMS 
      V3<-as.data.frame(matrix(data=NA,ncol=22,nrow=13))
      colnames(V3)<-c("month",
                      "cbmsAge","cbmsDraw","cbmsFailed","cbmsCost","cbmsCredit","cbmsInop",
                      "jbpdsAge","jbpdsDraw","jbpdsFailed","jbpdsCost","jbpdsCredit","jbpdsInop",
                      "rwsAge","rwsDraw","rwsFailed","rwsCost","rwsCredit","rwsInop",
                      "routineCosts","vehicleCost","nmc")
      V3$month<-seq(from=1,to=13,by=1)
      V3$routineCosts<-3250
      V3$nmc<-0
      
      #CBMS V3
      #Set up values for first round 
      cbmsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=cbmsAlpha,scale=cbmsSigma)),digits=1)
      V3$cbmsAge<-seq(from=cbmsInit,by=1,to=cbmsInit+12)
      V3$cbmsDraw<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsSigma)
      V3$cbmsFailed<-V3$cbmsAge>V3$cbmsDraw
      V3$cbmsCost<-0
      V3$cbmsInop<-FALSE
      V3$cbmsCredit<-0
      
      #Loop through to test if there are any CBMS Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V3$cbmsFailed[(i+1):12]>0)){
          if(V3$cbmsFailed[i]==TRUE){
            V3$cbmsAge[i+1]<-0
            V3$cbmsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V3$cbmsDraw[(i+1):13]<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsSigma)
            V3$cbmsFailed[(i+1):13]<-FALSE
            V3$cbmsFailed[(i+1):13]<-V3$cbmsAge[(i+1):13]>V3$cbmsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            cbmsType<-rmultinom(n=1,size=1,prob=c(5/7,2/7))
            V3$cbmsCost[i]<-max(cbmsType*c(600000,158199))
            V3$cbmsCredit[i]<-max(cbmsType*c(170112,0))
            V3$cbmsInop[i]<-TRUE
          }
        }
      }
      
      
      #JBPDS V3
      
      #Set up values for first round 
      jbpdsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=jbpdsAlpha,scale=jbpdsSigma)),digits=1)
      V3$jbpdsAge<-seq(from=jbpdsInit,by=1,to=jbpdsInit+12)
      V3$jbpdsDraw<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
      V3$jbpdsFailed<-V3$jbpdsAge>V3$jbpdsDraw
      V3$jbpdsCost<-0
      V3$jbpdsInop<-FALSE
      V3$jbpdsCredit<-0
      
      #Loop through to test if there are any jbpds Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V3$jbpdsFailed[(i+1):12]>0)){
          if(V3$jbpdsFailed[i]==TRUE){
            V3$jbpdsAge[i+1]<-0
            V3$jbpdsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V3$jbpdsDraw[(i+1):13]<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
            V3$jbpdsFailed[(i+1):13]<-FALSE
            V3$jbpdsFailed[(i+1):13]<-V3$jbpdsAge[(i+1):13]>V3$jbpdsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            jbpdsType<-rmultinom(n=1,size=1,prob=c(.15,.7,.15))
            V3$jbpdsCost[i]<-max(jbpdsType*c(160017,22524,3515))
            V3$jbpdsCredit[i]<-max(jbpdsType*c(98112,12565,0))
            V3$jbpdsInop[i]<-max(jbpdsType*c(TRUE,TRUE,FALSE))
          }
        }
      }
      
      #RWS V3
      
      #Set up values for first round 
      rwsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=rwsAlpha,scale=rwsSigma)),digits=1)
      V3$rwsAge<-seq(from=rwsInit,by=1,to=rwsInit+12)
      V3$rwsDraw<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
      V3$rwsFailed<-V3$rwsAge>V3$rwsDraw
      V3$rwsCost<-0
      V3$rwsInop<-FALSE
      V3$rwsCredit<-0
      
      #Loop through to test if there are any rws Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V3$rwsFailed[(i+1):12]>0)){
          if(V3$rwsFailed[i]==TRUE){
            V3$rwsAge[i+1]<-0
            V3$rwsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V3$rwsDraw[(i+1):13]<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
            V3$rwsFailed[(i+1):13]<-FALSE
            V3$rwsFailed[(i+1):13]<-V3$rwsAge[(i+1):13]>V3$rwsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            rwsType<-rmultinom(n=1,size=1,prob=c(1/8,7/8))
            V3$rwsCost[i]<-max(rwsType*c(161642,190))
            V3$rwsCredit[i]<-max(rwsType*c(74705,0))
            V3$rwsInop[i]<-max(rwsType*c(TRUE,FALSE))
          }
        }
      }
      
      #summary stats for V3
      cbmsTotal3<-sum(V3$cbmsCost)
      jbpdsTotal3<-sum(V3$jbpdsCost)
      rwsTotal3<-sum(V3$rwsCost)
      #Calculate cumulative total cost by month for V3
      V3$vehicleCost[1]<-V3$cbmsCost[1]-V3$cbmsCredit[1]+
        V3$jbpdsCost[1]-V3$jbpdsCredit[1]+
        V3$rwsCost[1]-V3$rwsCredit[1]+V3$routineCosts[i]
      for(i in 2:12){
        V3$vehicleCost[i]<-V3$vehicleCost[i-1]+V3$cbmsCost[i]-V3$cbmsCredit[i]+
          V3$jbpdsCost[i]-V3$jbpdsCredit[i]+
          V3$rwsCost[i]-V3$rwsCredit[i]+V3$routineCosts[i]
      }
      V3$nmc<-as.logical(V3$cbmsInop+V3$jbpdsInop+V3$rwsInop)
      
#V4
      #Initialize matrix to capture CBMS 
      V4<-as.data.frame(matrix(data=NA,ncol=22,nrow=13))
      colnames(V4)<-c("month",
                      "cbmsAge","cbmsDraw","cbmsFailed","cbmsCost","cbmsCredit","cbmsInop",
                      "jbpdsAge","jbpdsDraw","jbpdsFailed","jbpdsCost","jbpdsCredit","jbpdsInop",
                      "rwsAge","rwsDraw","rwsFailed","rwsCost","rwsCredit","rwsInop",
                      "routineCosts","vehicleCost","nmc")
      V4$month<-seq(from=1,to=13,by=1)
      V4$routineCosts<-3250
      V4$nmc<-0
      
      #CBMS V4
      #Set up values for first round 
      cbmsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=cbmsAlpha,scale=cbmsSigma)),digits=1)
      V4$cbmsAge<-seq(from=cbmsInit,by=1,to=cbmsInit+12)
      V4$cbmsDraw<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsSigma)
      V4$cbmsFailed<-V4$cbmsAge>V4$cbmsDraw
      V4$cbmsCost<-0
      V4$cbmsInop<-FALSE
      V4$cbmsCredit<-0
      
      #Loop through to test if there are any CBMS Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V4$cbmsFailed[(i+1):12]>0)){
          if(V4$cbmsFailed[i]==TRUE){
            V4$cbmsAge[i+1]<-0
            V4$cbmsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V4$cbmsDraw[(i+1):13]<-rweibull(n=1,shape=cbmsAlpha,scale=cbmsSigma)
            V4$cbmsFailed[(i+1):13]<-FALSE
            V4$cbmsFailed[(i+1):13]<-V4$cbmsAge[(i+1):13]>V4$cbmsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            cbmsType<-rmultinom(n=1,size=1,prob=c(5/7,2/7))
            V4$cbmsCost[i]<-max(cbmsType*c(600000,158199))
            V4$cbmsCredit[i]<-max(cbmsType*c(170112,0))
            V4$cbmsInop[i]<-TRUE
          }
        }
      }
      
      
      #JBPDS V4
      
      #Set up values for first round 
      jbpdsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=jbpdsAlpha,scale=jbpdsSigma)),digits=1)
      V4$jbpdsAge<-seq(from=jbpdsInit,by=1,to=jbpdsInit+12)
      V4$jbpdsDraw<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
      V4$jbpdsFailed<-V4$jbpdsAge>V4$jbpdsDraw
      V4$jbpdsCost<-0
      V4$jbpdsInop<-FALSE
      V4$jbpdsCredit<-0
      
      #Loop through to test if there are any jbpds Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V4$jbpdsFailed[(i+1):12]>0)){
          if(V4$jbpdsFailed[i]==TRUE){
            V4$jbpdsAge[i+1]<-0
            V4$jbpdsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V4$jbpdsDraw[(i+1):13]<-rweibull(n=1,shape=jbpdsAlpha,scale=jbpdsSigma)
            V4$jbpdsFailed[(i+1):13]<-FALSE
            V4$jbpdsFailed[(i+1):13]<-V4$jbpdsAge[(i+1):13]>V4$jbpdsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            jbpdsType<-rmultinom(n=1,size=1,prob=c(.15,.7,.15))
            V4$jbpdsCost[i]<-max(jbpdsType*c(160017,22524,3515))
            V4$jbpdsCredit[i]<-max(jbpdsType*c(98112,12565,0))
            V4$jbpdsInop[i]<-max(jbpdsType*c(TRUE,TRUE,FALSE))
          }
        }
      }
      
      #RWS V4
      
      #Set up values for first round 
      rwsInit<-round(runif(n=1,min=0,max=qweibull(p=.5,shape=rwsAlpha,scale=rwsSigma)),digits=1)
      V4$rwsAge<-seq(from=rwsInit,by=1,to=rwsInit+12)
      V4$rwsDraw<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
      V4$rwsFailed<-V4$rwsAge>V4$rwsDraw
      V4$rwsCost<-0
      V4$rwsInop<-FALSE
      V4$rwsCredit<-0
      
      #Loop through to test if there are any rws Failures.  Fix them that month.  
      #Record number of Fails and month.
      for(i in 1:12){
        if(sum(V4$rwsFailed[(i+1):12]>0)){
          if(V4$rwsFailed[i]==TRUE){
            V4$rwsAge[i+1]<-0
            V4$rwsAge[(i+1):13]<-seq(from=0,by=1,to=13-(i+1))
            V4$rwsDraw[(i+1):13]<-rweibull(n=1,shape=rwsAlpha,scale=rwsSigma)
            V4$rwsFailed[(i+1):13]<-FALSE
            V4$rwsFailed[(i+1):13]<-V4$rwsAge[(i+1):13]>V4$rwsDraw[(i+1):13]
            #determine if high or low cost part failed and multiply by cost
            rwsType<-rmultinom(n=1,size=1,prob=c(1/8,7/8))
            V4$rwsCost[i]<-max(rwsType*c(161642,190))
            V4$rwsCredit[i]<-max(rwsType*c(74705,0))
            V4$rwsInop[i]<-max(rwsType*c(TRUE,FALSE))
          }
        }
      }
      
      #summary stats for V4
      cbmsTotal4<-sum(V4$cbmsCost)
      jbpdsTotal4<-sum(V4$jbpdsCost)
      rwsTotal4<-sum(V4$rwsCost)
      #Calculate cumulative total cost by month for V4
      V4$vehicleCost[1]<-V4$cbmsCost[1]-V4$cbmsCredit[1]+
        V4$jbpdsCost[1]-V4$jbpdsCredit[1]+
        V4$rwsCost[1]-V4$rwsCredit[1]+V4$routineCosts[i]
      for(i in 2:12){
        V4$vehicleCost[i]<-V4$vehicleCost[i-1]+V4$cbmsCost[i]-V4$cbmsCredit[i]+
          V4$jbpdsCost[i]-V4$jbpdsCredit[i]+
          V4$rwsCost[i]-V4$rwsCredit[i]+V4$routineCosts[i]
      }
      V4$nmc<-as.logical(V4$cbmsInop+V4$jbpdsInop+V4$rwsInop)
      
  #Initialize within simulation totals  
    totalSummary<-as.data.frame(matrix(NA,ncol=11,nrow=12))
    colnames(totalSummary)<-c("month","cost","v1","v2","v3","v4","readiness","cbms","jbpds","rws","overBudget")      
    totalSummary$month<-seq(from=1,to=12,by=1)
    
#Final summary stats for sim
    totalSummary$v1<-as.numeric(V1$nmc[1:12])
    totalSummary$v2<-as.numeric(V2$nmc[1:12])
    totalSummary$v3<-as.numeric(V3$nmc[1:12])
    totalSummary$v4<-as.numeric(V4$nmc[1:12])
  
    for(i in 1:12){
      totalSummary$cost[i]<-V1$vehicleCost[i]+V2$vehicleCost[i] +V3$vehicleCost[i] +V4$vehicleCost[i]     
      totalSummary$readiness[i]<-(4-sum(totalSummary[i,3:6]))/4
      totalSummary$cbms[i]<-V1$cbmsInop[i]+V2$cbmsInop[i]+V3$cbmsInop[i]+V4$cbmsInop[i]
      totalSummary$jbpds[i]<-V1$jbpdsInop[i]+V2$jbpdsInop[i]+V3$jbpdsInop[i]+V4$jbpdsInop[i]
      totalSummary$rws[i]<-V1$rwsInop[i]+V2$rwsInop[i]+V3$rwsInop[i]+V4$rwsInop[i]
      totalSummary$overBudget[i]<-totalSummary$cost[i]>budget
    }

  #Budget Constraint loop
  if(sum(totalSummary$overBudget)>0){
    #Check to see when budget exceeded and then force all inop to remain that way 
    for(m in 3:6){
      if(sum(totalSummary[min(which(totalSummary$overBudget==TRUE)),m])>0){
              totalSummary[(min(which(totalSummary$overBudget==TRUE)):12),m]<-1
      }
    #Recalculate readiness based off of budget criteria
      for(i in 1:12){
        totalSummary$readiness[i]<-(4-sum(totalSummary[i,3:6]))/4
        }
      }   
    }#Closes budget if statement to prevent error when it didn't exceed budget in any month

grandTotals[n:(n+11),]<-totalSummary
}#closes overall for loop

#Creates averages over each month for plotting
plotSumm<-plyr::ddply(grandTotals, c("month"), summarise,
                          Readiness = mean(readiness),
                          se_Readiness   = sd(readiness) / sqrt(length(grandTotals$readiness)),
                          Cost= mean(cost),
                          sd_cost = sd(cost),
                          CBMS= sum(cbms),
                          JBPDS= sum(jbpds),
                          RWS= sum(rws))
plotSumm$Min_Readiness<-min(plotSumm$Readiness)
plotSumm$upperR<-plotSumm$Readiness+2*plotSumm$se_Readiness
plotSumm$lowerR<-plotSumm$Readiness-2*plotSumm$se_Readiness
plotSumm$Cost[12]

min(plotSumm$Min_Readiness)
mean(plotSumm$Readiness)

h<-ggplot(data=plotSumm)
h+geom_line(aes(x=month,y=Readiness))+scale_y_continuous(limits=c(.5,1))+
  geom_ribbon(aes(ymin=lowerR,ymax=upperR,x=month,fill="band"),alpha=.3)+
  scale_x_continuous(labels=c("Oct","Jan","Apr","July","Sep"),breaks=c(1,4,7,10,12))+
  xlab("Month")+ggtitle("Readiness Given $50K Per Vehicle")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(values="red", name="fill")
  

plotSumm$upperC<-plotSumm$Cost+2*plotSumm$sd_cost
plotSumm$lowerC<-plotSumm$Cost-2*plotSumm$sd_cost
plotSumm$lowerC[1:8]<-0

c<-ggplot(data=plotSumm)
c+geom_line(aes(x=month,y=Cost))+
  scale_y_continuous(labels=comma,breaks=c(seq(from=0,to=1000000,by=200000)))+
  scale_x_continuous(labels=c("Oct","Jan","Apr","July","Sep"),breaks=c(1,4,7,10,12))+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_ribbon(aes(ymin=lowerC,ymax=upperC,x=month,fill="band"),alpha=.3)+
  scale_fill_manual(values="gray", name="fill")+
  ggtitle("Expected Cost over the FY (95% CI)")+xlab("Month")+
  ylab("Cost per Fleet in $ (4x Vehicles)")

plotSumm1<-plotSumm
plotSumm2<-plotSumm
plotSumm3<-plotSumm
plotSumm4<-plotSumm
plotSumm5<-plotSumm


plotSumm1$Fund_level<-1
plotSumm2$Fund_level<-2
plotSumm3$Fund_level<-3
plotSumm4$Fund_level<-4
plotSumm5$Fund_level<-5
plotSumms<-rbind(plotSumm1,plotSumm2,plotSumm3,plotSumm4,plotSumm5)
plotSumms$Fund_level<-as.factor(plotSumms$Fund_level)
#Grand Plot
hh<-ggplot(data=plotSumms)
hh+geom_line(aes(x=month,y=Readiness,color=Fund_level))+
  scale_y_continuous(limits=c(.5,1))+
  ylab("Readiness (%)")+
  scale_x_continuous(labels=c("Oct","Jan","Apr","July","Sep"),breaks=c(1,4,7,10,12))+
  xlab("Month")+ggtitle("Readiness Levels for Various Budget Constraints")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_color_brewer(palette="Reds",
                     name="Funding Level",
                     labels=c("Unlimited Funding","$200K / Vehicle","$150K / Vehicle",
                              "$100K / Vehicle","$50K / Vehicle"))+theme_dark()

#run loop with budget set to low
plotSummLow<-plotSumm
#run again with budget set to high
plotSummHigh<-plotSumm
plotSummLow$Budget=0
plotSummHigh$Budget=1
#combine datasets into one for comparison graph
plotSumms2<-rbind(plotSummLow,plotSummHigh)
plotSumms2$Budget<-as.factor(plotSumms2$Budget)

#Plot of cost for both cbms prices
xyz<-ggplot(plotSumms2,aes(x=month,y=Cost))
xyz+geom_line(aes(color=Budget))+
  scale_y_continuous(labels=comma,breaks=c(seq(from=0,to=1600000,by=200000)))+
  scale_x_continuous(labels=c("Oct","Jan","Apr","July","Sep"),breaks=c(1,4,7,10,12))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values="gray", name="fill")+
  ggtitle("Change in Total Cost if CBMS Increases to $600K")+xlab("Month")+
  ylab("Cost per Fleet in $ (4x Vehicles)")+
  scale_color_brewer(palette="Reds",
                     name="CBMS Price",
                     labels=c("$367K Each","$600K Each"))+theme_dark()

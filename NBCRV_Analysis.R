library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(fitdistrplus)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(sjmisc)
library(factoextra)
library(rlist)
library(bnlearn)
library(scales)

#load data
cost<-read.csv("NBCRV/nbcrv_matCost.csv")
credits<-read.csv("NBCRV/nbcrv_credits.csv")
prostat<-read.csv("NBCRV/nbcrv_prostat.csv")
mileage<-read.csv("NBCRV/nbcrv_mileage.csv")

#generate month/year var in cost data
cost$Created.on<-as.Date(cost$Created.on,format="%m/%d/%Y")
cost$Created.on=cost$Created.on+years(2000)
cost$Date=as.character(as.yearmon(cost$Created.on))

#Remove turn in credits for cost df
cost<-filter(cost,Cost>=0)

#Change Description to text
cost$Description<-as.character(cost$Description)
#Change Year to numeric
cost$Year<-as.numeric(cost$Year)

#Gen vehicle factor based on admin number
cost$vehicle<-as.character(cost$AdminNum)
cost$vehicle<-as.factor(substr(cost$vehicle,4,6))

#Add additional data from free parts memo
freeParts<-matrix(ncol=length(cost),nrow=11)
colnames(freeParts)<-colnames(cost)
freeParts<-as.data.frame(freeParts)
freeParts[1,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"LCS Computer",NA,NA,NA,NA,20362,"Sep 2019",NA)
freeParts[2,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"LCS Display",NA,NA,NA,NA,18226,"Sep 2019",NA)
freeParts[3,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"FTS",NA,NA,NA,NA,29866,"Sep 2019",NA)
freeParts[4,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"FL1 Tube",NA,NA,NA,NA,2095.46,"Sep 2019",NA)
freeParts[5,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"T5",NA,NA,NA,NA,1016,"Sep 2019",NA)
freeParts[6,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"Keyboard",NA,NA,NA,NA,1436.88,"Sep 2019",NA)
freeParts[7,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"JSLSCAD SIM",NA,NA,NA,NA,224261,"Sep 2019",NA)
freeParts[8,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"JSLSCAD Scanner",NA,NA,NA,NA,192864,"Sep 2019",NA)
freeParts[9,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"Ground Probe",NA,NA,NA,NA,64734,"Sep 2019",NA)
freeParts[10,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"CBMS Block",NA,NA,NA,NA,331575,"Sep 2019",NA)
freeParts[11,1:18]<-c("FY19",19,NA,NA,NA,NA,NA,NA,NA,NA,"Carrier Cards",NA,NA,NA,NA,50218.35,"Sep 2019",NA)
freeParts$Cost<-as.numeric(freeParts$Cost)

#Combine free parts data with cost data
cost<-rbind(cost,freeParts)

#Add month variable to cost df
cost$month<-month(as.Date(cost$Created.on,format="%y-$m-%d"))
#Add month data to free parts
cost[which(is.na(cost$month)),19]<-c(rep(9,n=11))

#### Humidity #### 
    #Add in humidity data for Schofield Barracks
      humidity<-as.data.frame(matrix(0,nrow=12,ncol=4))
      colnames(humidity)<-c("month","avgHumidity","highHumidity","lowHumidity")
      humidity[,1]<-seq(from=1,to=12,by=1)
      humidity[,2]<-c(76,76,77,72.5,70.5,69,70.5,69.5,71.5,73,75.5,75.5)
      humidity[,3]<-c(87,88,83,83,81,80,82,81,83,85,86,86)
      humidity[,4]<-c(65,64,61,62,60,58,59,58,60,61,65,65)
      cost2<-cost
      cost2<-merge(cost2,humidity,by="month")
      
      #Examine humidity and failure data
        h<-ggplot(data=tblJBPDS,aes(x=month))
        h+geom_path(aes(y=avgHumidity))+
          geom_point(aes(y=(7/16)*N+(1097/16)))+
          scale_y_continuous(sec.axis = sec_axis(~.*(16/7)-(1097/7),name = "Failures"))+
          ylab("Avg Humidity (%)")+
          xlab("Month")+
          scale_x_continuous(labels=c("January","April","July","October"),breaks=c(1,4,7,10))

      

        #Hist showing failures by each month
        hist(cost$Cost[which(cost$CBMS==TRUE)])
        hist(cost$Cost[which(cost$jbpds==TRUE)])
        hist(cost$Cost[which(cost$rws==TRUE)])
        length(which(cost$rws==TRUE&cost$Cost>50000))
        summary(cost$Cost[which(cost$rws==TRUE)])
        
    
    #Lagged humidity
      lagHumidity<-as.data.frame(matrix(0,nrow=12,ncol=4))
      colnames(lagHumidity)<-c("month","avgHumidity2","highHumidity2","lowHumidity2")
      lagHumidity[,1]<-c(2,3,4,5,6,7,8,9,10,11,12,1)
      lagHumidity[,2]<-c(76,76,77,72.5,70.5,69,70.5,69.5,71.5,73,75.5,75.5)
      lagHumidity[,3]<-c(87,88,83,83,81,80,82,81,83,85,86,86)
      lagHumidity[,4]<-c(65,64,61,62,60,58,59,58,60,61,65,65)
      cost<-merge(cost,lagHumidity,by="month")
      
    #Test humidity for relationship with cbms or jbpds ordering
      summary(glm(jbpds~avgHumidity,data=tblJBPDS,family=poisson))
      
      
    #summarize data
    tbl1 <- ddply(cost, c("vehicle", "Year"), summarise,
                   N    = length(Cost),
                   mean = mean(Cost),
                   total=sum(Cost),
                   sd   = sd(Cost),
                   se   = sd / sqrt(N))
    as_tibble(tbl1)
    tbl1$Year<-as.numeric(tbl1$Year)
    #Plot means by vehicle over each FY
    a<-ggplot(data=tbl1[-17,])
    a+geom_path(aes(x=Year,y=total,color=vehicle))+ylab("Total Cost")+
      scale_y_continuous(labels=comma)+ggtitle("Cost per FY by Vehicle")


## #CBMS Failure investigation...
    
    #Get costs in which description contains "CBMS"
    #Initialize CBMS vector
    cost$CBMS<-rep(FALSE,length(cost$Description))
    for (i in 1:length(cost$Description)){
      cost[i,ncol(cost)]<-grepl(x=cost[i,11],pattern="CBMS")
    }
    cbmsFailures<-cost[which(cost$CBMS==TRUE),]
    summarise(cbmsFailures,
              cbmsFailures$FY,
              cbmsFailures$vehicle)
    
    #Get costs for all parts within JBPDS system (jbpds, fts, baws, jca)
    #Initialize jbpds vector
    cost$jbpds<-rep(FALSE,length(cost$Description))
    jbpdsFailures<-c("jbpds","baws","fts","jca","fluid transfer system","power pack")
    for (i in 1:length(cost$Description)){
      cost[i,ncol(cost)]<-str_contains(x=cost[i,11],pattern=jbpdsFailures,
                                       ignore.case=TRUE,
                                       logic="or")
    }
    
    tblJBPDS<-plyr::ddply(cost,c("jbpds","month","vehicle"),summarise,N=length(Cost),mean=mean(Cost),total=sum(Cost))
    tblJBPDS<-tblJBPDS[c(13:23),]
    tblJBPDS<-merge(tblJBPDS,humidity,by=c("month"))
    summary(glm(N~avgHumidity,data=tbl5,family=poisson))
    
    mileageTable<-plyr::ddply(cost,c("jbpds","month","vehicle"),summarise,N=length(Cost),mean=mean(Cost),total=sum(Cost))
    mileageTable<-mileageTable[which(mileageTable$jbpds==TRUE),]
    mileageTable<-merge(mileageTable,humidity,by=c("month"))
    #Regression again while controlling for vehicle
    glmMod2<-glm(N~avgHumidity+vehicle,data=mileageTable,family=poisson)
      #Now adding in mileage control
      mileageTable<-plyr::ddply(cost,c("jbpds","month","vehicle","Year"),summarise,N=length(Cost),mean=mean(Cost),total=sum(Cost))
      mileageTable<-mileageTable[which(mileageTable$jbpds==TRUE),]
      mileageTable<-merge(mileageTable,humidity,by=c("month"))
      mileage$month<-match((substr(mileage$Date,1,3)),month.abb)
      mileage$Year<-as.numeric(substr(mileage$Date,7,8))
      mileageTable<-merge(mileage,mileageTable,by=c("vehicle","month","Year"))
      mileageTable$mileage<-as.numeric(mileageTable$mileage)
      mileageTable$drove<-rep(0,length(mileageTable$month))
      mileageTable[which(mileageTable$mileage>0),13]<-1
      mileageTable$mileageScale<-scale(mileageTable$mileage)
    glmMod3<-glm(N~avgHumidity+vehicle+mileageScale,data=mileageTable,family=poisson)
summary(glmMod3)

    failTable<-plyr::ddply(cost,c("common","month","vehicle","Year"),summarise,N=length(Cost),
                           mean=mean(Cost),total=sum(Cost),mileage=sum(mileage))
    summary(glm(N~mileage+vehicle+month,data=failTable,family=poisson))
        
    
    
    ##RWS ##
    cost$rws<-rep(FALSE,length(cost$Description))
    rwsFailures<-c("rws","M2")
    for (i in 1:length(cost$Description)){
      cost[i,ncol(cost)]<-str_contains(x=cost[i,11],pattern=rwsFailures,
                                       ignore.case=TRUE,
                                       logic="or")
      }
    
    #Routine Failures 
    cost$common<-rep(FALSE,length(cost$Description))
    commonFailures<-c("jbpds","baws","fts","jca","fluid transfer system","power pack",
                      "cbms","rws","M2")
    for (i in 1:length(cost$Description)){
      cost[i,ncol(cost)]<-str_contains(x=cost[i,11],pattern=commonFailures,
                                       ignore.case=TRUE,
                                       logic="or")
    }
    
    tblCommon<-as.data.frame(cost[which(cost$common==FALSE),]%>%
                               group_by(Date)%>%summarise_at(vars(Cost),sum))
    tblCommon$Date<-lubridate::parse_date_time(tblCommon$Date,orders="b-Y")

    c<-ggplot(data=tblCommon)
    commonMean<-round(mean(tblCommon[which(tblCommon$Cost<200000),2]),digits=0)

    c+geom_point(aes(x=Date,y=Cost))+scale_y_continuous(labels=comma)+
      ggtitle("Total Fleet Cost per Month of 'Routine' Expenses")+
      xlab(paste("Mean Cost per Vehicle (excluding Oct 2019) = $",commonMean))

    
    #number of CBMS Failures by each vehicle...and the average cost for each transaction
    cbmsFailures%>%group_by(vehicle)%>%  summarise_at(vars(Cost),mean)
    
    table2<-as.data.frame(cost%>%group_by(Date,vehicle)%>%summarise_at(vars(Type),count))
    
    
    cost$treat<-rep(FALSE,length(cost$Description))
    #not going to use this now
    #for (i in 1:length(cost$Description)){
     # cost[i,ncol(cost)]<-str_contains(x=cost[i,which(colnames(cost)=="Description")],
       #                                pattern=trtMonths,
       #                                ignore.case=TRUE,
        #                               logic="or")
    #}
    cost[(which(cost$vehicle==112&cost$Date=="Sep 2017")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==121&cost$Date=="Sep 2017")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==111&cost$Date=="Jul 2019")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==111&cost$Date=="Jun 2018")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==122&cost$Date=="Dec 2018")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==122&cost$Date=="Dec 2019")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==122&cost$Date=="Nov 2017")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==121&cost$Date=="Nov 2019")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==112&cost$Date=="Oct 2018")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==121&cost$Date=="Oct 2018")),length(cost)]<-TRUE
    cost[(which(cost$vehicle==112&cost$Date=="Oct 2019")),length(cost)]<-TRUE
    
    
    ###Compare months after PM vs months after no PM
    #Remove months in which preventive maintenance is performed
    pm<-cost[which(cost$Type!="PM02"),]
    #Remove $0 costs because they are almost always duplicates and part of a larger workorder
    pm<-pm[which(pm$Cost>0),]
    pm$trt<-as.numeric(pm$treat)
    pmTable1<- ddply(pm, c("Date", "vehicle","trt"), summarise,count=length(Cost))

    
    #Test for sig different means, fail to reject in GLM and OLS
    mod1<-glm(count~trt+vehicle,data=pmTable1,family=poisson)
    summary(mod1)
  #Try again with cost data
    pmTable2<- ddply(pm, c("Date", "vehicle","trt"), summarise,Cost=sum(Cost))
    pmTable2$Cost<-as.numeric(as.character(pmTable2$Cost))
    mod2<-glm(Cost~trt+vehicle,data=pmTable2,family=Gamma)
    summary(mod2)
    
    #Test for equality of variance, p=value is .0001 (variance is 21.3 x higher if no PM)
    var.test(Cost~trt,data=pmTable2)
    
    #how many obs > 50,00
    table3[which(table3$Cost>50000),]
    
    #So the goal of any maintenance estimator is to increase the variance if not completed
    mean(rweibull(n=10000,shape=cbmsMLE_mu,scale=cbmsMLE_scale))
        
#Compare density plots for treated vs untreated
    g<-ggplot(data=pmTable2,aes(y=trt,x=Cost))
    g+geom_boxplot()+ylab("Preventive Maintenance in Prior Month?")+
      xlab("Cost ($)")+
      scale_x_continuous(labels=comma)

    #Initialize failure vector
    
##### not sure any of this works anymore but it's here
    
    commonFailures<-cost%>%filter(common==TRUE)
    summarise(commonFailures,
              commonFailures$FY,
              commonFailures$vehicle)
    #number of common Failures by each vehicle...and the average cost for each transaction
    commonFailures%>%group_by(vehicle)%>% summarise_at(vars(Cost),mean)
    
    
    #create yes/no vector for rws
    cost$rws<-rep(FALSE,length(cost$Description))
    for (i in 1:length(cost$Description)){
      cost[i,ncol(cost)]<-grepl(x=cost[i,11],pattern="RWS")
    }
    
    #Create common indicator
    cost$majComponent<-cost$rws+cost$CBMS+cost$jbpds
    

##### Mileage ######
  mileage$Date<-as.character(mileage$Date)
  mileage$Date<-paste(substr(mileage$Date,1,3),substr(mileage$Date,5,8),sep=" ")
  cost<-merge(mileage,cost,by=c("Date","vehicle"))
  mileage$mileage<-as.numeric(as.character(gsub(",","",mileage$mileage)))

  ###summary stats
  VehicleCosts<-cost%>%group_by(vehicle,Date)%>%summarise_at(vars(jbpds),sum)
  VehicleCosts<-merge(VehicleCosts,mileage,by=c("vehicle","Date"))
  VehicleCosts$mileage<-as.numeric(VehicleCosts$mileage)
 

######## CREDITS ########
#Drop all obs where credit > 0
credit<-credits[which(credits$Received.Credit>0),]
#Reformat dates for manipulation
credit$Turn.In.Date<-as.Date(credit$Turn.In.Date,format="%m/%d/%Y")
credit$Turn.In.Date=credit$Turn.In.Date+years(2000)
credit$Clearing.Date<-as.Date(credit$Clearing.Date,format="%m/%d/%Y")
credit$Clearing.Date=credit$Clearing.Date+years(2000)
#Find turn in credit lag
credit$turnInLag<-as.double(difftime(credit$Clearing.Date,
                     credit$Turn.In.Date,
                     units = "days"))

#Density plot of Turn in Lag
d <- density(credit$turnInLag)
plot(d,main="")

plot(turnInXFit,turnInYFit,col="red")

fg <- fitdist(credit$turnInLag, "gamma")
fw <- fitdist(credit$turnInLag, "weibull")
fe <- fitdist(credit$turnInLag, "exp")
##Ended up just going with an exponential
plot.legend <- "Exponential using MLE"
denscomp(list(fe,fw), legendtext = plot.legend,xlab="Count of Turn In Lag (Days)",
         main="Histogram and Exponential Densities")
fw

cbms<-clean[which(clean$CBMS==TRUE),]
fitW<-fitdist(cbms$days,"weibull")
fitG<-fitdist(cbms$days,"gamma")
denscomp(list(fitW),legendtext=plot.legend,xlab="",main="")

#turn in lag does not seem to be dependent, at all, on cost
mod1<-lm(turnInLag~Received.Credit,credit)
#SCMC (type of parts) are jointly significant in determining turn in lag...
#but overall not one is by itself
mod2<-lm(turnInLag~SCMC,credit)
summary(mod2)

plot(credit$turnInLag,credit$Received.Credit)
filter(credit,credit$turnInLag>100)
credit$mon<-month(credit$Turn.In.Date)
#Turn in month is also significant in determining credit...could be cyclical
mod3<-lm(turnInLag~mon,credit)
summary(mod3)

#get means for turn in credit by month
lagMonMeans<-credit%>%
  group_by(mon)%>%
  summarise_at(vars(turnInLag),mean)
#Plot means by month
plot(credit$mon,credit$turnInLag,ylab="Turn In Lag Time (Days)",xlab="Calendar Month")
points(x=seq(from=1,to=12,by=1),y=unlist(lagMonMeans[2]),col="red",pch=17)

########Start to analyze different types of failures ###########



  #Text analysis of failures
  costText<-paste(cost[1:length(cost$Description),11],sep=" ")
  costText<-Corpus(VectorSource(costText))

    #Returns most frequently used words from description line
    ct<-tm_map(costText,content_transformer(tolower))
    ctm<-TermDocumentMatrix(ct)
    m<-as.matrix(ctm)
    v<-sort(rowSums(m),decreasing=TRUE)
    d<-data.frame(word=names(v),freq=v)
    write.csv(head(d,250),"nbcrv_failures.csv")
    set.seed(3131)
    abcd<-wordcloud(words=d$word,freq=d$freq,min.freq=2,
              max.words=200,random.order=FALSE,rot.per=0.35,
              colors=brewer.pal(8,"Dark2"))

as_tibble(d[c(2,11,20,21,23,32,35,74,75,171),])
    
    ### Common Failures for each FY
    #FY 17
    c16<-cost[cost$Year==17,]
    costText16<-paste(c16[1:length(c16$Description),11],sep=" ")
    costText16<-Corpus(VectorSource(costText16))
    ct16<-tm_map(costText16,content_transformer(tolower))
    ctm16<-TermDocumentMatrix(ct16)
    m16<-as.matrix(ctm16)
    v16<-sort(rowSums(m16),decreasing=TRUE)
    d16<-data.frame(word=names(v16),freq=v16)
    head(d16,20)
    # FY17

######Common failures investigation#########


##Analyze each component 

    
    #Plotting to see humidity and lag humidity by month
    hist(cost[which(cost$jbpds==TRUE),1])
    plot(x=humidity$month,y=humidity$avgHumidity)
    plot(x=lagHumidity$month,y=lagHumidity$avgHumidity2)
    

#Failure means and std
    #Initialize capturing matrix
    failMeans<-as.data.frame(matrix(ncol=3,nrow=length(systemFailures)))
    thisFailed<-rep(FALSE,length(commonFailures))
    colnames(failMeans)<-c("system","mean","stdev")
    failMeans$system<-systemFailures
    
    #Loop through each row of system failures and find mean and sd
    for (j in 1:length(systemFailures)){
      for(i in 1:length(commonFailures$Description)){
        thisFailed[i]<-grepl(x=tolower(commonFailures[i,11]),pattern=failMeans[j,1])
      }
      failMeans[j,2]<-filter(commonFailures[which(thisFailed==TRUE),])%>%summarise_at(vars(Cost),mean)
      failMeans[j,3]<-filter(commonFailures[which(thisFailed==TRUE),])%>%summarise_at(vars(Cost),sd)
    }
    
colnames(d)<-c("system","freq")
failMeans<-merge(failMeans[,c("system","mean","stdev")],
              d[,c("system","freq")])




###### BAYESIAN STUFF #####

    #### Priors ####
    rwsFreqs<-c(1.75,1.75,1.75, 2.75,2.75,3.75,3.75,3.75,4.5,4.5,6.75,6.75,6.75,
                    6.75,7.5,7.5,8,9.25,9.25,10.75)
    rwsFreqs<-12*rwsFreqs
    hist(rwsFreqs,main="RWS Histogram",xlab="Time to Failure (Years)")
    
    jbpdsFreqs<-c(3.75,5,5,6,6.75,6.75,7.75,7.75,7.75,8.5,8.5,8.5,9.25,10,10,
                  10,10.25,10.75,10.75,10.75)
    hist(jbpdsFreqs,main="JBPDS Histogram",xlab="Time to Failure (Years)")
    
    cbmsFreqs<-c(1,1.5,1.5,1.5,2.75,2.75,2.75,3.75,4.75,4.75,4.75,4.75,6,6,6,
                 6.5,7.75,7.75,7.75,9)
    cbmsFreqs<-12*cbmsFreqs
    hist(cbmsFreqs,main="CBMS Histogram",xlab="Time to Failure (Months)",breaks=6)

    
    #Read in cleaned cost and failure data
    clean<-read.csv("cost_RECLEANED.csv")
    clean<-clean[,-1]
    clean<-clean[which(clean$days>0),]
    clean$mons<-clean$days/30
    
    
    cost3<-cost[which(cost$Cost>0),]
    summary(cost3$Cost[which(cost$jbpds==TRUE)])
    cost[which(cost$Cost==331575),]
    
    
    

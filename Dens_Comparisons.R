#### Priors ####
rwsFreqs<-c(1.75,1.75,1.75, 2.75,2.75,3.75,3.75,3.75,4.5,4.5,6.75,6.75,6.75,
            6.75,7.5,7.5,8,9.25,9.25,10.75)
rwsFreqs<-12*rwsFreqs
hist(rwsFreqs,main="RWS Histogram",xlab="Time to Failure (Years)")

jbpdsFreqs<-c(3.75,5,5,6,6.75,6.75,7.75,7.75,7.75,8.5,8.5,8.5,9.25,10,10,
              10,10.25,10.75,10.75,10.75)
jbpdsFreqs<-12*jbpdsFreqs
hist(jbpdsFreqs,main="JBPDS Histogram",xlab="Time to Failure (Years)")

cbmsFreqs<-c(1,1.5,1.5,1.5,2.75,2.75,2.75,3.75,4.75,4.75,4.75,4.75,6,6,6,
             6.5,7.75,7.75,7.75,9)
cbmsFreqs<-12*cbmsFreqs

#CBMS
fitW<-fitdist(cbmsFreqs,distr="weibull",method="mle")
fitG<-fitdist(cbmsFreqs,distr="gamma",method="mle")
fitN<-fitdist(cbmsFreqs,distr="norm",method="mle")
fitsM<-list(fitW,fitG,fitN)
cbmsDens<-denscomp(fitsM,legendtext=c("Weibull","Gamma","Normal"),
         main="Expert Histogram and Fitted MLE Dists for CBMS",
         xlab="Estimated Months until Failure",
         xlim=c(0,150), fitcol=c("orange1", "green", "firebrick1"),
         plotstyle="ggplot")


cbmsDens+theme_dark()+
  geom_line(lwd=.75)+
  scale_x_continuous(breaks=seq(from=0,to=150,by=30))

gofstat(fitsM)


#RWS
fitWr<-fitdist(rwsFreqs,distr="weibull",method="mle")
fitGr<-fitdist(rwsFreqs,distr="gamma",method="mle")
fitNr<-fitdist(rwsFreqs,distr="norm",method="mle")
fitsMr<-list(fitWr,fitGr,fitNr)
rwsDens<-denscomp(fitsMr,legendtext=c("Weibull","Gamma","Normal"),
         main="Expert Histogram and Fitted MLE Dists for RWS",
         xlab="Estimated Months until Failure",
         xlim=c(0,150),
         fitcol=c("orange1", "green", "firebrick1"),
         plotstyle="ggplot")
rwsDens+theme_dark()+geom_line(lwd=.75)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150))

gofstat(fitsMr)


#JBPDS
fitWj<-fitdist(jbpdsFreqs,distr="weibull",method="mle")
fitGj<-fitdist(jbpdsFreqs,distr="gamma",method="mle")
fitNj<-fitdist(jbpdsFreqs,distr="norm",method="mle")
fitsMj<-list(fitWj,fitGj,fitNj)
jbpdsDens<-denscomp(fitsMj,legendtext=c("Weibull","Gamma","Normal"),
         main="Expert Histogram and Fitted MLE Dists for JBPDS",
         xlab="Estimated Months until Failure",
         xlim=c(0,150),fitcol=c("orange1", "green", "firebrick1"),
         xlegend=0,ylegend=.2,plotstyle="ggplot")
jbpdsDens+theme_dark()+geom_line(lwd=.75)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150))

gofstat(fitsMj)



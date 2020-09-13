#!/usr/bin/env Rscript

#library("ggplot2")

#args<-commandArgs("trailingOnly=TRUE")

samples<-3000000
dem<-0.49 
rep<-0.39
route270<-data.frame(State=as.character(0), EVs=as.integer(0), Chance=as.double(0), stringsAsFactors=FALSE)
theScore<-data.frame(EVs=as.integer(0))

dem16<-0.48
rep16<-0.46

total2p<-(dem+rep)
corDem<-(dem/total2p)
corRep<-(rep/total2p)

swing<-corDem - (dem16/(dem16 + rep16))

us2016<-read.csv(file='US.csv', stringsAsFactors = FALSE)

demDiff<-dem-rnorm(samples, dem, 0.03/1.96)
repDiff<--demDiff


#state odds
for (i in 1:nrow(us2016))
{
  stateDiff<-rnorm(samples, 0, 0.02)
  #generate additional factor
  reverse <- 1
  trendFactor<-us2016[i, ]$Trend
  localDemDiff<-rnorm(samples, trendFactor/100, abs(trendFactor/100))
  localRepDiff = -localDemDiff
  demProjection<-us2016[i,]$D16 + swing * 100 + demDiff * 100 + localDemDiff * 100 + stateDiff * 100
  repProjection<-us2016[i,]$R16 - swing * 100 + repDiff * 100 + localRepDiff * 100 - stateDiff * 100
  demVote<-us2016[i,]$Turnout * demProjection/100
  repVote<-us2016[i,]$Turnout * repProjection/100
  demVictoryMargin<-demVote - repVote
  demWin<-(demVictoryMargin > 0) * us2016[i,]$EVs
  theScore<-cbind(theScore, demWin)
  z<-sum(demVictoryMargin > 0)
  if (z > 0) {
    route270<-rbind(route270, c(as.character(us2016[i,]$State), as.integer(us2016[i,]$EVs), as.double(z/samples)))
  }
  cat(as.character(us2016[i,]$State),": Biden has ", z/(samples/100), "% chance of winning\n")
}
route270<-route270[-1,]
theScore<-theScore[-1,]
answers<-rowSums(theScore)
rr<-ecdf(answers)
plot(rr, main="Biden electoral votes", xlab="Electoral votes", ylab="Cumulative probability")
abline(v=270, col="red")
abline(h=0.5, col="blue")
abline(h=0.1, col="green")

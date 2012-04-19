setwd("~/Dropbox/_STATS222/")
week1Xi = read.table(file="myth_data.txt", header = T)
colnames(week1Xi)<-c("Xi.1","Xi.3","Xi.5","W")
head(week1Xi)
week1Xi$theta = (week1Xi$Xi.5 - week1Xi$Xi.1)/4
attach(week1Xi)

cor(W,theta)
cor(week1Xi)

pairs(week1Xi)
pairs(week1Xi, pch = 20)

truereg1<-lm(Xi.5 ~ W + Xi.1)
truereg2<-lm(Xi.5 ~ W + Xi.3)
truereg3<-lm(theta ~ W)

summary(truereg1)
summary(truereg2)
summary(truereg3)
confint(truereg1)
confint(truereg2)
confint(truereg3)

truereg1a = lm(theta ~ W + Xi.1)
truereg2a = lm(theta ~ W + Xi.3)

summary(truereg1a)
summary(truereg2a)
confint(truereg1a)
confint(truereg2a)
#----------------------------------------------------------------

install.packages("reshape", repos = "http://cran.stat.ucla.edu/")
library(reshape)

week1Xi = read.table(file="myth_data2.txt", header = T)
dat.wide = subset(week1Xi, select = c(subject, Xi.1., Xi.3., Xi.5. ))
names(dat.wide)<-c("subject","1","3","5")
head(dat.wide)

dat.molten = melt(dat.wide, id.vars = c("subject"))
head(dat.molten)
dim(dat.molten)
dat.long = cast(dat.molten, subject + variable ~ .)
head(dat.long)
names(dat.long) = c("subject", "time", "Xi")

attach(dat.long)
write.table(dat.long, file="xilong.dat", quote=F)
detach(dat.long)

week1Xi.long<-read.table(file="xilong.dat", header=T)
head(week1Xi.long)

install.packages("lme4")
library(lme4)

xiList = lmList(Xi ~ time |subject, data = week1Xi.long)
summary(xiList)

xyplot(Xi ~ time | subject, type=c("p", "r"), index.cond=function(x,y)
   {coef(lm(y ~ x))[1]}, data=week1Xi.long)
xyplot(Xi ~ time , groups =subject, type=c("r"), index.cond=function(x,y)
   {coef(lm(y ~ x))[1]}, data=week1Xi.long, col = c("black"))

rate <- coef(xiList)[2]
stem(rate[,1])
sort(rate[,1])
fivenum(rate[,1])
week1Xi$theta - rate[,1]
Wt <- tapply(week1Xi$W, week1Xi$subject, mean)
week1Xi.long$WW <- Wt[as.character(week1Xi.long$subject)]
head(week1Xi.long)
plot(Wt, rate[,1], pch=20)

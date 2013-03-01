setwd("/Users/renekizilcec/Dropbox/r-code/STATS/comm372/data/")
library(ggplot2)
library(lme4)
library(plyr)

# define conditions
# -1=disregard; 0=noPIP; 1=PIP
cond1=c(rep(-1, 30*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(-1, 18*rate))
cond2=c(rep(-1, 30*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(-1, 18*rate))

### plot data and fit loess
# plot(data$time,data$eda)
# lines(data$time,predict(loess(eda ~ as.numeric(time), span=0.01, data), data),col=3)
# plot.ts(matrix(subset(data, marker==0)$eda, ncol=8))
# plot.ts(matrix(subset(data, marker==1)$eda, ncol=8))


# Takes trimmed file and returns smoothed and averaged dataframe
processFile = function(fileName,condition,rate=32,length=4*60+49){
    data=read.csv(fileName, stringsAsFactors=F)
    colnames(data)=c("z","y","x","battery","temp","eda","time")
    data$time=strptime(data$time, format="%H:%M:%OS", tz="") #convert time
    data$time=data$time-min(data$time) #make time relative
    data=subset(data, time<=length) #cut off end
    data$marker=condition[1:nrow(data)] #add marker for condition
    data=subset(data, marker!=-1) #cut off slack
    
    newdat=data.frame(eda0=rowMeans(matrix(subset(data, marker==0)$eda, ncol=8)), eda1=rowMeans(matrix(subset(data, marker==1)$eda, ncol=8)))
    newdat$time=seq(0,15,1/32)[1:nrow(newdat)]
    newdat$sm0=predict(loess(eda0 ~ time, span=.2, newdat), newdat) #smooth
    newdat$sm1=predict(loess(eda1 ~ time, span=.2, newdat), newdat) #smooth
    
    dat=newdat[(seq(1,nrow(newdat),rate/2)),] # downsample to 2 Hz
    
    newdat=data.frame(eda=scale(c(dat$sm0,dat$sm1)), marker=c(rep("noPIP",nrow(dat)),rep("PIP",nrow(dat))), time=1:nrow(dat)) # convert to long format
    
    return(newdat)
}


# Put all data in one df
data=rbind(cbind(subj=1,processFile("anita_pip_inter2.csv",cond2)),
cbind(subj=2,processFile("claire_pip_inter2.csv",cond2)),
cbind(subj=3,processFile("kaiping_pip_inter1.csv",cond1)),
cbind(subj=4,processFile("mathias_pip_cond2.csv",cond2)))

# plotting each subject
ggplot(data, aes(time,eda,group=marker))+geom_line(aes(color=marker))+theme_bw()+facet_grid(subj~.)

# aggregating over subjects
agg=ddply(data, .(time,marker), summarize, eda=mean(eda))
ggplot(agg, aes(time,eda,group=marker))+geom_line(aes(color=marker))+theme_bw()

# fit Mixed Effects model
hist(data$eda)
m1=lmer(eda~marker:time+(time|subj), data) # slope only
m2=lmer(eda~marker*time+(time|subj), data) # slope and interc.
anova(m1,m2)
summary(m2)

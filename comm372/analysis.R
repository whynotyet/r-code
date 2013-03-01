setwd("/Users/renekizilcec/Dropbox/r-code/STATS/comm372/data/")

data=read.csv("claire_pip_inter2.csv", stringsAsFactors=F)
colnames(data)=c("z","y","x","battery","temp","eda","time")
rate=32 #sampling rate
length=4*60+49 #video length

data$time=strptime(data$time, format="%H:%M:%OS", tz="") #convert time
data$time=data$time-min(data$time) #make time relative
data=subset(data, time<=length) #cut off end
#data$eda=scale(data$eda)

# define conditions
# -1=disregard; 0=noPIP; 1=PIP
cond1=c(rep(-1, 30*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(-1, 18*rate))[1:nrow(data)]
cond2=c(rep(-1, 30*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(-1, 18*rate))[1:nrow(data)]

### file specific
data$marker=cond2
data=subset(data, marker!=-1)

plot(data$time,data$eda)
lines(data$time,predict(loess(eda ~ as.numeric(time), span=0.01, data), data),col=3)



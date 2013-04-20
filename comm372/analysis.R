setwd("/Users/renekizilcec/Dropbox/r-code/STATS/comm372/data/")
library(ggplot2)
library(lme4)
library(plyr)
library(grid)
library(reshape2)
library(splines)
library(MASS)
# Plot options
opt=list(theme_bw(),theme(legend.position="bottom",legend.key=element_rect(color="white"),legend.key.width=unit(1.5, "cm"),panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),legend.text = element_text(size = 12),strip.background=element_rect(fill=0),strip.text=element_text(face="bold",size=12)),labs(linetype="",x="Time (seconds)", y=expression(paste("Normalized EDA (",mu, "S)"))))

rate=32
# define conditions
# -1=disregard; 0=noPIP; 1=PIP
cond1=c(rep(-1, 30*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(-1, 18*rate))
cond2=c(rep(-1, 30*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(-1, 18*rate))

# #alternative
# cond1_=c(rep(-1, 30*rate), rep(-1, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(-1, 15*rate), rep(-1, 18*rate))
# cond2_=c(rep(-1, 30*rate), rep(-1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(-1, 15*rate), rep(-1, 18*rate))



# Remove blibs
cleanEDA = function(eda,sensitivity=1){
    for(i in 5:(length(eda)-5)){
        ref=seq(i-5,i+5,1)[-6]
        if(abs(eda[i]-median(eda[ref]))>sensitivity*sd(eda[ref])){
            eda[i]=median(eda[ref])
        }
    }
    return(eda)
}


# Takes trimmed file and normalizes for each segment based on last second of previous segment
processFile = function(fileName,condition,rate=32,length=4*60+49){
    data=read.csv(fileName, stringsAsFactors=F)
    colnames(data)=c("z","y","x","battery","temp","eda","time")
    data$time=strptime(data$time, format="%H:%M:%OS", tz="") #convert time
    data$time=data$time-min(data$time) #make time relative
    data=subset(data, time<=length) #cut off end
    data$marker=condition[1:nrow(data)] #add marker for condition (cut in case too long)
    data$marker[is.na(data$marker)]=-1 #add -1 in case too short
    data$eda=cleanEDA(cleanEDA(data$eda))

    for(i in nrow(data):2){
        if(data$marker[i]!=data$marker[i-1]){
            baseline=median(data$eda[(i-1*rate):(i-1)])
            data$eda[i:(i+15*rate-1)]=data$eda[i:(i+15*rate-1)]-baseline
        }
    }
    
    data=subset(data, marker!=-1) #cut off slack
    
    newdat=data.frame(eda0=rowMeans(matrix(subset(data, marker==0)$eda, ncol=8)), eda1=rowMeans(matrix(subset(data, marker==1)$eda, ncol=8)))
    #newdat=data.frame(eda0=rowMeans(matrix(subset(data, marker==0)$eda, ncol=7)), eda1=rowMeans(matrix(subset(data, marker==1)$eda, ncol=7)))
    newdat$time=seq(0,15,1/rate)[1:nrow(newdat)]

    newdat$sm0=newdat$eda0
    newdat$sm1=newdat$eda1

        
    dat=newdat[(seq(1,nrow(newdat),rate/2)),] # downsample to 2 Hz

    newdat=data.frame(eda=scale(c(dat$sm0,dat$sm1)), marker=as.factor(c(rep("No Face",nrow(dat)),rep("Face",nrow(dat)))), time=(1:nrow(dat))/2) # convert to long format
    
    return(newdat)
}

# Takes trimmed file and returns smoothed and averaged dataframe
processFileDelin = function(fileName,condition,rate=32,length=4*60+49,smooth=TRUE){
    data=read.csv(fileName, stringsAsFactors=F)
    colnames(data)=c("z","y","x","battery","temp","eda","time")
    data$time=strptime(data$time, format="%H:%M:%OS", tz="") #convert time
    data$time=data$time-min(data$time) #make time relative
    data=subset(data, time<=length) #cut off end
    data$marker=condition[1:nrow(data)] #add marker for condition
    data$eda=cleanEDA(cleanEDA(data$eda))
    data=subset(data, marker!=-1) #cut off slack
        
    data$eda=lm(eda~I(1:length(eda)), data)$resid #removes linear trend
    
    newdat=data.frame(eda0=rowMeans(matrix(subset(data, marker==0)$eda, ncol=8)), eda1=rowMeans(matrix(subset(data, marker==1)$eda, ncol=8)))
    #newdat=data.frame(eda0=rowMeans(matrix(subset(data, marker==0)$eda, ncol=7)), eda1=rowMeans(matrix(subset(data, marker==1)$eda, ncol=7)))
    newdat$time=seq(0,15,1/rate)[1:nrow(newdat)]
    if(smooth){
        newdat$sm0=predict(loess(eda0 ~ time, span=.2, newdat), newdat) #smooth
        newdat$sm1=predict(loess(eda1 ~ time, span=.2, newdat), newdat) #smooth    
    }else{
        newdat$sm0=newdat$eda0
        newdat$sm1=newdat$eda1
    }
    
    dat=newdat[(seq(1,nrow(newdat),rate/2)),] # downsample to 2 Hz
    
    newdat=data.frame(eda=scale(c(dat$sm0,dat$sm1)), marker=as.factor(c(rep("No Face",nrow(dat)),rep("Face",nrow(dat)))), time=(1:nrow(dat))/2) # convert to long format
    
    return(newdat)
}


# Put all data in one df
data=rbind(cbind(subj=1,processFile("anita_pip_inter2.csv",cond2)),
cbind(subj=2,processFile("claire_pip_inter2.csv",cond2)),
cbind(subj=3,processFile("kaiping_pip_inter1.csv",cond1)),
cbind(subj=4,processFile("mathias_pip_cond2.csv",cond2)),
cbind(subj=5,processFile("soohee_pip_inter1.csv",cond1)),
cbind(subj=6,processFile("cody_pip_inter1.csv",cond1)),
cbind(subj=7,processFile("andreas_pip_inter2.csv",cond2)),
cbind(subj=8,processFile("andrea_pip_inter1.csv",cond1)),
cbind(subj=9,processFile("key_pip_inter2.csv",cond2)),
cbind(subj=10,processFile("jordan_pip_inter1.csv",cond1)))

dataDelin=rbind(cbind(subj=1,processFileDelin("anita_pip_inter2.csv",cond2)),
        cbind(subj=2,processFileDelin("claire_pip_inter2.csv",cond2)),
        cbind(subj=3,processFileDelin("kaiping_pip_inter1.csv",cond1)),
        cbind(subj=4,processFileDelin("mathias_pip_cond2.csv",cond2)),
        cbind(subj=5,processFileDelin("soohee_pip_inter1.csv",cond1)),
        cbind(subj=6,processFileDelin("cody_pip_inter1.csv",cond1)),
        cbind(subj=7,processFileDelin("andreas_pip_inter2.csv",cond2)),
        cbind(subj=8,processFileDelin("andrea_pip_inter1.csv",cond1)),
        cbind(subj=9,processFileDelin("key_pip_inter2.csv",cond2)),
        cbind(subj=10,processFileDelin("jordan_pip_inter1.csv",cond1)))

# plotting each subject
ggplot(dataDelin, aes(time,eda,group=marker))+geom_line(aes(linetype=marker))+facet_grid(subj~.)+opt

# aggregating over subjects
agg=ddply(dataDelin, .(time,marker), summarize, eda=mean(eda))
ggplot(agg, aes(time,eda,group=marker))+geom_line(aes(linetype=marker))+opt+theme(legend.position=c(.8,.9))
ggplot(agg, aes(time,eda,group=marker))+geom_line()+stat_smooth(aes(linetype=marker),method=lm,formula=y~ns(x,6))+opt+theme(legend.position=c(.8,.9))


ggplot(agg, aes(marker,eda))+geom_boxplot()+opt+labs(x="")

temp=ddply(dataDelin, .(subj, marker), summarize, eda=median(eda))
temp$delta=temp[,2]-temp[,3]
t.test(temp[,2],temp[,3],paired=T)
t.test(eda~marker,temp,paired=T)
summary(aov(eda~marker*time+Error(subj), dataDelin))

agg$pos=factor(rep(c("Start","Middle","End"),c(20,20,20)), levels=c("Start","Middle","End"))
ggplot(agg, aes(pos,eda))+geom_boxplot()+facet_grid(.~marker)+opt+labs(x="Stimulus Period (5 seconds each)")
#ggplot(ddply(agg,.(pos,marker), summarize, eda=mean(eda)), aes(pos,eda))+geom_bar(stat="identity")+facet_grid(.~marker)+theme_bw()+labs(x="")
agg$marker=factor(agg$marker, labels=c("PIP just appeared", "PIP just disappeared"))
opt=list(theme_bw(),theme(legend.position=c(.8,.8),legend.key=element_rect(color="white"),legend.key.width=unit(1.2, "cm"),panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),strip.background=element_rect(fill=0),strip.text=element_text(face="bold")))
opt=list(theme_bw(),theme(legend.position="bottom",legend.key=element_rect(color="white"),legend.key.width=unit(1.2, "cm"),panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),strip.background=element_rect(fill=0),strip.text=element_text(face="bold")),labs(linetype="",x="Time (seconds)", y=expression(paste("Normalized EDA (",mu, "S)"))))

ggplot(agg, aes(pos,eda))+geom_boxplot()+facet_grid(.~marker)+opt+labs(x="Stimulus Period (5 seconds each)",title="Arousal from PIP")
ggsave(file="~/Desktop/eda.png", dpi=300, height=5, width=7)

summary(dataDelin)
dataDelin$pos=NA
dataDelin[dataDelin$time>=0&dataDelin$time<=5,]$pos="Start"
dataDelin[dataDelin$time>5&dataDelin$time<=10,]$pos="Middle"
dataDelin[dataDelin$time>10&dataDelin$time<=15,]$pos="End"
dataDelin$pos=factor(dataDelin$pos, levels=c("Start","Middle","End"))

# check for differneces
summary(aov(eda~pos*marker+Error(subj),dataDelin))
TukeyHSD.aov(aov(eda~pos*marker, agg))
# fit Mixed Effects model
hist(data$eda)
m1=lmer(eda~marker:time+time+(time|subj), dataDelin) # slope only
m1=lmer(eda~marker*time+(time|subj), dataDelin) # slope and interc.
m2=lmer(eda~marker+time+I(time^2)+I(time^3)+(time+I(time^2)+I(time^3)|subj), dataDelin)# slope and interc.
m3=lmer(eda~marker*time+marker*I(time^2)+marker:I(time^3)+marker:I(time^4)+(time+I(time^2)|subj), dataDelin)
summary(m3)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# NEW IDEA: predict face/noFace based on time series

prepareFile = function(fileName,condition,rate=32,length=4*60+49){
    data=read.csv(fileName, stringsAsFactors=F)
    colnames(data)=c("z","y","x","battery","temp","eda","time")
    data$time=strptime(data$time, format="%H:%M:%OS", tz="") #convert time
    data$time=data$time-min(data$time) #make time relative
    data=subset(data, time<=length) #cut off end
    data$marker=condition[1:nrow(data)] #add marker for condition
    data$eda=scale(cleanEDA(cleanEDA(data$eda)))
    data=subset(data, marker!=-1) #cut off slack
    data$eda=lm(eda~I(1:length(eda)), data)$resid #removes linear trend
    return(subset(data, select=c(eda,marker,time)))
}

# Compare orders
data3=cbind(ord2=rowMeans(cbind(prepareFile("anita_pip_inter2.csv",cond2)[,1],
            prepareFile("claire_pip_inter2.csv",cond2)[,1],
            prepareFile("mathias_pip_cond2.csv",cond2)[,1],
            prepareFile("andreas_pip_inter2.csv",cond2)[,1],
            prepareFile("key_pip_inter2.csv",cond2)[,1])),
            mark2=prepareFile("andreas_pip_inter2.csv",cond2)[,2],
            ord1=rowMeans(cbind(prepareFile("kaiping_pip_inter1.csv",cond1)[,1],
            prepareFile("soohee_pip_inter1.csv",cond1)[,1],
            prepareFile("cody_pip_inter1.csv",cond1)[,1],
            prepareFile("andrea_pip_inter1.csv",cond1)[,1],
            prepareFile("jordan_pip_inter1.csv",cond1)[,1])),
            mark2=prepareFile("andrea_pip_inter1.csv",cond1)[,2])
plot.ts(data3)

# Put all data in one df
data2=rbind(cbind(subj=1,prepareFile("anita_pip_inter2.csv",cond2)),
           cbind(subj=2,prepareFile("claire_pip_inter2.csv",cond2)),
           cbind(subj=3,prepareFile("kaiping_pip_inter1.csv",cond1)),
           cbind(subj=4,prepareFile("mathias_pip_cond2.csv",cond2)),
           cbind(subj=5,prepareFile("soohee_pip_inter1.csv",cond1)),
           cbind(subj=6,prepareFile("cody_pip_inter1.csv",cond1)),
           cbind(subj=7,prepareFile("andreas_pip_inter2.csv",cond2)),
           cbind(subj=8,prepareFile("andrea_pip_inter1.csv",cond1)),
           cbind(subj=9,prepareFile("key_pip_inter2.csv",cond2)),
           cbind(subj=10,prepareFile("jordan_pip_inter1.csv",cond1)))

summary(data2)
plot.ts(data2$eda)

mod=glm(marker~eda, data2, family=binomial)
summary(mod)
exp(cbind(OR=coef(mod),confint(mod)))
#               OR          2.5 %   97.5 %
# (Intercept) 0.9994803 0.9837884 1.015422
# eda         1.0816448 1.0626318 1.101009
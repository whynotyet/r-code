setwd("/Users/renekizilcec/Dropbox/r-code/STATS/comm372/data/")
library(ggplot2)
library(lme4)
library(plyr)
library(grid)

# Plot options
opt=list(theme_bw(),theme(legend.position="bottom",legend.key=element_rect(color="white"),legend.key.width=unit(1.5, "cm"),panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),legend.text = element_text(size = 12),strip.background=element_rect(fill=0),strip.text=element_text(face="bold",size=12)),labs(linetype="",x="Time (seconds)", y=expression(paste("Normalized EDA (",mu, "S)"))))

rate=32
# define conditions
# -1=disregard; 0=noPIP; 1=PIP
cond1=c(rep(-1, 30*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(-1, 18*rate))
cond2=c(rep(-1, 30*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(-1, 18*rate))

#alternative
cond1_=c(rep(-1, 30*rate), rep(-1, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(-1, 15*rate), rep(-1, 18*rate))
cond2_=c(rep(-1, 30*rate), rep(-1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(0, 15*rate), rep(1, 15*rate), rep(-1, 15*rate), rep(-1, 18*rate))



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
processFile = function(fileName,condition,rate=32,length=4*60+49,smooth=FALSE){
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
processFile = function(fileName,condition,rate=32,length=4*60+49,smooth=TRUE){
    data=read.csv(fileName, stringsAsFactors=F)
    colnames(data)=c("z","y","x","battery","temp","eda","time")
    data$time=strptime(data$time, format="%H:%M:%OS", tz="") #convert time
    data$time=data$time-min(data$time) #make time relative
    data=subset(data, time<=length) #cut off end
    data$marker=condition[1:nrow(data)] #add marker for condition
    data$eda=cleanEDA(cleanEDA(data$eda))
    data=subset(data, marker!=-1) #cut off slack
    
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
data=rbind(cbind(subj=1,processFile("anita_pip_inter2.csv",cond2,smooth=FALSE)),
cbind(subj=2,processFile("claire_pip_inter2.csv",cond2,smooth=FALSE)),
cbind(subj=3,processFile("kaiping_pip_inter1.csv",cond1,smooth=FALSE)),
cbind(subj=4,processFile("mathias_pip_cond2.csv",cond2,smooth=FALSE)),
cbind(subj=5,processFile("soohee_pip_inter1.csv",cond1,smooth=FALSE)),
cbind(subj=6,processFile("cody_pip_inter1.csv",cond1,smooth=FALSE)),
cbind(subj=7,processFile("andreas_pip_inter2.csv",cond2,smooth=FALSE)),
cbind(subj=8,processFile("andrea_pip_inter1.csv",cond1,smooth=FALSE)))

data=rbind(cbind(subj=1,processFile("anita_pip_inter2.csv",cond2)),
           cbind(subj=2,processFile("claire_pip_inter2.csv",cond2)),
           cbind(subj=3,processFile("kaiping_pip_inter1.csv",cond1)),
           cbind(subj=4,processFile("mathias_pip_cond2.csv",cond2)),
           cbind(subj=5,processFile("soohee_pip_inter1.csv",cond1)),
           cbind(subj=6,processFile("cody_pip_inter1.csv",cond1)),
           cbind(subj=7,processFile("andreas_pip_inter2.csv",cond2)),
           cbind(subj=8,processFile("andrea_pip_inter1.csv",cond1)))


# plotting each subject
ggplot(data, aes(time,eda,group=marker))+geom_line(aes(linetype=marker))+facet_grid(subj~.)+opt

# aggregating over subjects
agg=ddply(data, .(time,marker), summarize, eda=mean(eda))
ggplot(agg, aes(time,eda,group=marker))+geom_line(aes(linetype=marker))+opt+theme(legend.position=c(.8,.9))

agg$pos=factor(rep(c("Start","Middle","End"),c(20,20,20)), levels=c("Start","Middle","End"))
ggplot(agg, aes(pos,eda))+geom_boxplot()+facet_grid(.~marker)+opt
#ggplot(ddply(agg,.(pos,marker), summarize, eda=mean(eda)), aes(pos,eda))+geom_bar(stat="identity")+facet_grid(.~marker)+theme_bw()+labs(x="")

# check for differneces
summary(aov(eda~pos*marker, agg))
TukeyHSD.aov(aov(eda~pos*marker, agg))
# fit Mixed Effects model
hist(data$eda)
m1=lmer(eda~marker:time+time+(time|subj), data) # slope only
m2=lmer(eda~marker*time+(time|subj), data) # slope and interc.
anova(m1,m2)
summary(m2)



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
    return(subset(data, select=c(eda,marker,time)))
}

# Put all data in one df
data2=rbind(cbind(subj=1,prepareFile("anita_pip_inter2.csv",cond2)),
           cbind(subj=2,prepareFile("claire_pip_inter2.csv",cond2)),
           cbind(subj=3,prepareFile("kaiping_pip_inter1.csv",cond1)),
           cbind(subj=4,prepareFile("mathias_pip_cond2.csv",cond2)),
           cbind(subj=5,prepareFile("soohee_pip_inter1.csv",cond1)),
           cbind(subj=6,prepareFile("cody_pip_inter1.csv",cond1)),
           cbind(subj=7,prepareFile("andreas_pip_inter2.csv",cond2)),
           cbind(subj=8,prepareFile("andrea_pip_inter1.csv",cond1)))
summary(data2)
plot.ts(data2$eda)

mod=glm(marker~eda, data2, family=binomial)
summary(mod)
exp(cbind(OR=coef(mod),confint(mod)))
#               OR          2.5 %   97.5 %
# (Intercept) 0.9994803 0.9837884 1.015422
# eda         1.0816448 1.0626318 1.101009
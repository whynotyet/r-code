###################################
# STATS 222 Exercise Set (Week 1) #
###################################

### 1. Time1-time2 regressions; Class example 4/2 ###
# a. Push hard on the regression fits shown in the handout (also linked) with standard regression diagnostic tools (residuals etc) to see if you can find anything askew with the time2 on W and time1 regressions (student question). Point is that it is the standard interpretation of the coefficient of W rather than the regression fit that is the issue here.
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

#additional regression diagnostics (see http://www.statmethods.net/stats/rdiagnostics.html)
install.packages("car")
library(car)
#Test outliers --- no outliers
outlierTest(truereg1)
qqPlot(truereg1, main="QQ Plot")
leveragePlots(truereg1)
#Test non-normality --- reasonably normal in distribution
qqPlot(truereg1, main="QQ Plot")
library(MASS)
sresid <- studres(truereg1)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 


# b. Repeat the handout demonstration regressions using the fallible measures (the X's) from the bottom half of the linked data page. The X's are simply error-in-variable versions of the Xi's: X = Xi + error, with error having mean 0 and variance 10. 
week1Xi = read.table(file="myth_data_bottom.txt", header = T)
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

#Test outliers --- no outliers
outlierTest(truereg1)
qqPlot(truereg1, main="QQ Plot")
leveragePlots(truereg1)
#Test non-normality --- reasonably normal in distribution
qqPlot(truereg1, main="QQ Plot")
sresid <- studres(truereg1)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Compare 5-number summaries for the amount of change from the earliest time "1" to the final observation "5" using the "Xi" measurements (upper frame) and the fallible "X" observations (lower frame).
week1Xi = read.table(file="myth_data.txt", header = T)
colnames(week1Xi)<-c("Xi.1","Xi.3","Xi.5","W")
X_true <- week1Xi$Xi.5-week1Xi$Xi.1

week1Xi = read.table(file="myth_data_bottom.txt", header = T)
colnames(week1Xi)<-c("Xi.1","Xi.3","Xi.5","W")
X_diff_below <- week1Xi$Xi.5-week1Xi$Xi.1

fivenum(X_true)
fivenum(X_diff_below)

par(mfrow=c(1,2))
boxplot(X_true, ylim = c(0, 45), main="True Xs")
boxplot(X_diff_below, ylim = c(0, 45), main="Noisy Xs")
#saved as exc1_q1b_boxplots.png


### 2. Captopril and Blood pressure ###
# The file captopril.dat contains the data shown in Section 2.2 of Verbeke, Introduction to Longitudinal Data Analysis, slides. Captopril is an angiotensin-converting enzyme inhibitor (ACE inhibitor) used for the treatment of hypertension. Use the before and after Spb measurements to examine the improvement (i.e. decrease) in blood pressure. Obtain a five-number summary for observed improvement. What is the correlation between change and initial blood pressure measurement? Obtain a confidence interval for the correlation and show the corresponding scatterplot.


# 3. (more challenging). Use mvrnorm to construct a second artificial data example (n=100) mirroring the 4/2 class handout BUT with the correlation between true individual rate of change and W set to .7 instead of 0. Carry out the corresponding regression demonstration.
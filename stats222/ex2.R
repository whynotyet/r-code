###################################
# STATS 222 Exercise Set (Week 2) #
###################################

# 1. Reliability versus precision demonstration
# Consider a population with true change between time1 and time2 distributed Uniform [99,101] and measurement error Uniform [-1, 1]. If you used discrete Uniform in this construction then you could say measurement of change is accurate to 1 part in a hundred.
# Calculate the reliability of the difference score.
# Also try error Uniform [-2,2], accuracy one part in 50.
# A similar demonstration can be found in my Shoe Shopping and the Reliability Coefficient

unidata1<-runif(50,99,101) #how to specify the measurement error?

######################################################
# 2. Regression toward the mean? Galton's data on the heights of parents and their children
# In the "HistData" or "psych" packages reside the "galton" dataset, the primordial regression toward mean example.
install.packages("HistData")
library(HistData)
data(Galton)
# Description: Galton (1886) presented these data in a table, showing a cross-tabulation of 928 adult children born to 205 fathers and mothers, by their height and their mid-parent’s height. A data frame with 928 observations on the following 2 variables. parent Mid Parent heights (in inches) child Child Height. Details: Female heights were adjusted by 1.08 to compensate for sex differences. (This was done in the original data set)
head(Galton)
# Consider "parent" as time1 data and "child" as time2 data and investigate whether these data indicate regression toward the mean according to either definition (metric or standardized)? Refer to Section 4 of the Myths chapter supplement (pagination 61-63) for an assessment of regression toward the mean (i.e. counting up number of subjects satisfying regression-toward-mean).
Galton$id<-1:nrow(Galton)

Galton.molten = melt(Galton, id.vars = c("id"))
Galton.long = cast(Galton.molten, id + variable ~ .)
names(Galton.long) = c("id", "time", "Xi")
attach(Galton.long)
write.table(Galton.long, file="Galton_long.dat", quote=F)
detach(Galton.long)

Galton.long<-read.table(file="Galton_long.dat", header=T)
xyplot(Xi ~ time , groups =id, type=c("r"), index.cond=function(x,y)
{coef(lm(y ~ x))[1]}, data=Galton.long, col = c("black"), strip=F)
#??? How to cut off left and right extrapolation ???#

reg<-lm(child~parent,data=Galton)
summary(reg)
plot(Galton$parent,Galton$child, ylim=c(60,74), xlim=c(60,74))
abline(reg)

# Aside: if you like odd plots, try this (and then look at the docs ?sunflowerplot; this may require the package "car" to be installed on your machine)
with(Galton,
{
sunflowerplot(parent,child, xlim=c(62,74), ylim=c(62,74))
reg <- lm(child ~ parent)
abline(reg)
lines(lowess(parent, child), col="blue", lwd=2)
if(require(car)) {
dataEllipse(parent,child, xlim=c(62,74), ylim=c(62,74), plot.points=FALSE)
}
}) 


######################################################
# 3. Let's look again at the Week1 data, here using the bottom half, the fallible "X" measurements (constructed by adding noise to the Xi measurements).
# a. Follow the Week 2 R-session and obtain the plot showing each subjects data and straight-line fit. Use lmList to obtain the 40 slopes for the straight-line fits. Compare the five-number summary of rates of change for the "X" measurement with that obtained for the perfectly measured "Xi" measurements in the posted R-session.
week1Xi_true = read.table(file="myth_data.txt", header = T)
colnames(week1Xi_true)<-c("Xi.1","Xi.3","Xi.5","W")
week1Xi_true$subject <- 1:nrow(week1Xi_true)
true.wide = subset(week1Xi_true, select = c(subject, Xi.1, Xi.3, Xi.5 ))
names(true.wide)<-c("subject","1","3","5")
true.molten = melt(true.wide, id.vars = c("subject"))
true.long = cast(true.molten, subject + variable ~ .)
names(true.long) = c("subject", "time", "Xi")
attach(true.long)
write.table(true.long, file="true_long.dat", quote=F)
detach(true.long)
true.long<-read.table(file="true_long.dat", header=T)

week1Xi_fall = read.table(file="myth_data_bottom.txt", header = T)
colnames(week1Xi_fall)<-c("Xi.1","Xi.3","Xi.5","W")
week1Xi_fall$subject <- 1:nrow(week1Xi_fall)
fall.wide = subset(week1Xi_fall, select = c(subject, Xi.1, Xi.3, Xi.5 ))
names(fall.wide)<-c("subject","1","3","5")
fall.molten = melt(fall.wide, id.vars = c("subject"))
fall.long = cast(fall.molten, subject + variable ~ .)
fall.long = as.data.frame(fall.long)
names(fall.long) = c("subject", "time", "Xi")
attach(fall.long)
write.table(fall.long, file="fall_long.dat", quote=F)
detach(fall.long)
fall.long<-read.table(file="fall_long.dat", header=T)

attach(week1Xi_fall)
head(week1Xi_fall)
diff = Xi.5 - Xi.1
cor(diff, Xi.1)
cor(diff, W)
cor.test(diff, W)
detach(week1Xi_fall)

xyplot(Xi ~ time | subject, type=c("p", "r"), index.cond=function(x,y)
{coef(lm(y ~ x))[1]}, data=true.long)
xyplot(Xi ~ time | subject, type=c("p", "r"), index.cond=function(x,y)
{coef(lm(y ~ x))[1]}, data=fall.long)

true.list = lmList(Xi ~ time |subject, data = true.long)
true.rate<-coef(true.list)[2]
fivenum(true.rate)
	# 1.095690 3.989401 5.033039 6.336699 8.991205
fall.list = lmList(Xi ~ time |subject, data = fall.long)
fall.rate<-coef(fall.list)[2]
fivenum(fall.rate)
	# 0.5290025  3.9306175  5.1907113  6.1846750 10.6300700

# b. Standardizing is always a bad idea is a good motto for life, especially with longitudinal data. Start out with the "X" data, and standardize (i.e. transform to mean o, var 1) at each of the 3 time points. Note "scale" will do this for you (in wide form). For the standardized data obtain the plot showing each subjects data and straight-line fit. What do you have here?
fall.std<-scale(subset(week1Xi_fall,select= -subject))
fall.std<-as.data.frame(fall.std[1:40,])
fall.std$subject <- 1:40
fall.std.wide = subset(fall.std, select = c(subject, Xi.1, Xi.3, Xi.5 ))
names(fall.std.wide)<-c("subject","1","3","5")
fall.std.molten = melt(fall.std.wide, id.vars = c("subject"))
fall.std.long = cast(fall.std.molten, subject + variable ~ .)
names(fall.std.long) = c("subject", "time", "Xi")
attach(fall.std.long)
write.table(fall.std.long, file="fall_std_long.dat", quote=F)
detach(fall.std.long)
fall.std.long<-read.table(file="fall_std_long.dat", header=T)

xyplot(Xi ~ time | subject, type=c("p", "r"), index.cond=function(x,y)
{coef(lm(y ~ x))[1]}, data=fall.std.long)
	# Overall, there is no change. Each individual change was adjusted downward to achieve this. 

######################################################
# 4. Paired and unpaired samples, continuous vs categorical measurements.
# Let's use again the 40 subjects in the week1 "X" data.
week1Xi_fall = read.table(file="myth_data_bottom.txt", header = T)
colnames(week1Xi_fall)<-c("Xi.1","Xi.3","Xi.5","W")
# a. Measured data. Take the time1 and time5 observations and obtain a 95% Confidence Interval for the amount of change.
attach(week1Xi_fall)
inter = Xi.5-Xi.1
t.test(inter)	#this is paird, same as t.test(Xi.5,Xi.1,paired=T)
	#95%CI = 17.13709 23.34782
barplot(sort(inter), ylab="Amount of change (t5-t1)", col="lightblue", main="Paired vs. unpaired")
abline(h=17.13709, col="red", lwd=3)
abline(h=23.34782, col="red", lwd=3)
text(4, 18, paste("2.5% (paired) = ",17.13709), col="red")
text(4, 22, paste("97.5% (paired) = ",23.34782), col="red")

# Compare the width of that interval with a confidence interval for the difference beween the time5 and time1 means if we were told a different group of 40 subjects was measured at each of the time points (data no longer paired).
t.test(Xi.5, Xi.1, paired=F)
	#95%CI = 16.09929 24.38562 
abline(h=16.09929, col="darkgreen", lwd=3)
abline(h=24.38562, col="darkgreen", lwd=3)
text(4, 15, paste("2.5% (unpaired) = ",16.09929), col="darkgreen")
text(4, 25, paste("97.5% (unpaired) = ",24.38562), col="darkgreen")

# b. Dichotomous data. Instead look at these data with the criterion that a score of 50 or above is a "PASS" and below that is "FAIL". Carry out McNemar's test for the paired dichotomous data, and obtain a 95% CI for the difference between dependent proportions. Compare that confidence interval with the "unpaired" version (different group of 40 subjects was measured at each of the time points) for independent proportions. 
Xi.1d=Xi.1
Xi.1d[which(Xi.1>=50)] = "PASS"
Xi.1d[which(Xi.1<50)] = "FAIL"
Xi.1d=as.factor(Xi.1d)
Xi.5d=Xi.5
Xi.5d[which(Xi.5>=50)] = "PASS"
Xi.5d[which(Xi.5<50)] = "FAIL"
Xi.5d=as.factor(Xi.5d)
mcnemar.test(Xi.5d, Xi.1d)
	#McNemar's chi-squared = 25.037, df = 1, p-value = 5.624e-07


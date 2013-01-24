################################################
######## STATAS 209 (DAVID ROGOSA)      ########
######## HOMEWORK #2                    ########
################################################

# QUESTION 1
set.seed(8)
T_tc=rnorm(100,2,1)
Y_c=rnorm(100,10,1)
ACE=2
FACE=mean(T_tc)
ACE-FACE # BIAS

G=rbinom(100, 1, .5)
control=Y_c[!as.logical(G)]
treat=(Y_c+T_tc)[as.logical(G)]
mean(treat)-mean(control)

prob=sapply(Y_c-10, function(x) ifelse(x>1,1,ifelse(x<(-1),-1,x)) )
Gneq=rbinom(100, 1, abs(prob))
control=Y_c[!as.logical(Gneq)]
treat=(Y_c+T_tc)[as.logical(Gneq)]
mean(treat)-mean(control)



# QUESTION 2
setwd("~/Dropbox/r-code/stats209/")
data=read.table("hw2q2.dat",header=T)[,-1]
attach(data)
cor(X,Y)
m1=lm(Y~Z)
m2=lm(X~Z)
cor.test(residuals(m1), residuals(m2))
plot(residuals(m1), residuals(m2))

install.packages("ppcor")
library(ppcor)
pcor.test(X,Y,Z)
detach(data)
# Partial cor much lower and not sign. different from 0
# Causal question can only be settled with experiment


# QUESTION 3
cole=read.table("http://www-stat.stanford.edu/~rag/stat209/coleman.dat", header=T)
install.packages("multilevel")
library(multilevel)
with(cole, sobel(momed, tverb, vach))[c("Indirect.Effect","SE")]
library(mediation)

plot(with(cole, mediate(model.m=lm(tverb~momed), model.y=lm(vach~momed+tverb), treat="momed", mediator="tverb", boot=T) ) )

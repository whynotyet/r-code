################################################
######## STATS 209 (DAVID ROGOSA)      ########
######## HOMEWORK #1                    ########
################################################

# QUESTION 1
paup=read.table("http://www-stat.stanford.edu/~rag/stat209/yuledoc.dat", header=T)
Dpaup=paup-100
summary(lm(paup~.,Dpaup)) # looks similar
m1=lm(paup~., Dpaup)
m2=lm(paup~outrelief, Dpaup)
anova(m2,m1) # reject null that c=d=0

# QUESTION 2
cole=read.table("http://www-stat.stanford.edu/~rag/stat209/coleman320.dat", header=T)
edreg=lm(vach~., cole)
momreg=lm(momed~ssal+whcol+ses+tverb, cole)
momregadj=lm(vach~residuals(momreg), cole)
coef(edreg)
coef(momregadj)
plot(cole$vach~residuals(momreg))
abline(coef(momregadj))

# QUESTION 3
v1=cole$vach
v2=cole$momed
v3=cole$ses
m1=lm(v1~v2)
m2=lm(v1~v2+v3)
m3=lm(v3~v2)
coef(m1)[2] #LHS
coef(m2)[2]+coef(m3)[2]*coef(m2)[3] #RHS

# QUESTION 4
library(DAAG)
errorsINx(mu=10, n=100, a=0, b=1.5, SD=10, SDyerr=0,timesSDx=1)

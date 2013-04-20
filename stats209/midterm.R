################################################
######## STATS 209 (DAVID ROGOSA)       ########
######## MIDTERM                        ########
################################################
library(plyr)
library(ggplot2)
library(psych)
# QUESTION 1
# part a.
# Let X1=fitness, X2=stress, X3=illness
r12=-.13
r13=-.29
r23=.34
r13.2=(r13-r12*r23)/((1-r12^2)*(1-r23^2))^.5 # = -.264 not much less than -.29
r.con(-.264,373) # 95% CI = [-0.3559992, -0.1669325] #by converting r to Fisher z' and back
# CI does not include 0. No evidence for fitness-illness association to be spurious

# part b.
corPred=matrix(c(1,-.03,.39,-.05,-.03,1,.07,-.23,.39,.07,1,-.13,-.05,-.23,-.13,1),byrow=F,ncol=4)
corResp=matrix(c(-.08,-.16,-.29,.34),ncol=1,byrow=F)
coefs = t(corResp)%*%solve(corPred)
coefs # same as direct effects in Table 5.2. (kleine p.121)

sqrt((corPred[1,1]/(373))*(373 / (373-4))) # unstandardized SE for Excercise
sqrt((corPred[2,2]/(373))*(373 / (373-4))) # unstandardized SE for Hardiness
sqrt((corPred[3,3]/(373))*(373 / (373-4))) # unstandardized SE for Fitness
sqrt((corPred[4,4]/(373))*(373 / (373-4))) # unstandardized SE for Stress

rsq=coefs%*%corResp # this is the Rsquared (not adjusted)
covB = as.numeric(1-rsq)*solve(corPred)
#remeber: standard errors for standardized coeffs are just square roots of the diagonals adjusted for sample size
sqrt((covB[1,1]/(373))*(373 / (373-4))) # standardized SE for Excercise
sqrt((covB[2,2]/(373))*(373 / (373-4))) # standardized SE for Hardiness
sqrt((covB[3,3]/(373))*(373 / (373-4))) # standardized SE for Fitness
sqrt((covB[4,4]/(373))*(373 / (373-4))) # standardized SE for Stress

# c-c' = beta32 * beta13.2 =a*b #where X1=ill,X2=fit,X3=stress
# using Tab 5.2.
mediation=-.11*.29 # = -0.0319
# var(c-c')=b^2 Sa^2 + a^2 Sb^2
sobel=(-.11)^2 * ? + .29^2 *.048677

#----------------------------------------------------------------------
# QUESTION 2
# part 1.
#From the results in Table 3 (use 18-month change from baseline) and Table 5 (use 18-month systolic) estimate the dose-response relation for decrease in systolic blood pressure for each unit decrease in salt intake (as measured by sodium excretion).
-43.9 #difference between trt and ctr salt (18 months)
-2.06 #difference between trt and ctr blood pressure (18 months)
2.06/43.9 # = 0.047 reduction in blood pressure for 1 unit reduction in salt intake

#What strong assumption did you need to make to justify this estimator?
#   I assumed a linear dose-response relation (as opposed to a different functional form).

#Briefly justify that assumption for this study?
2.03/((154.6-103)-(156.4-159.3))    # = 0.03724771 (6months)
1.9/((154.6-100.2)-(156.4-152.1))   # = 0.03792415 (12months)
2.06/((154.6-99.4)-(156.4-146.5))   # = 0.04547461 (18months)
#   The doese-response relation measured after 6 and 12 months are very similar, which points at a linear functional form.
#   However, after 18 months, the relation becomes stronger. Note (!) that there is a mistake in Table 3, the "18-Month change from baseline" value for Control, does not correspond to thte difference between Baseline and 18-Month.


# part 2.
install.packages("mediation")
library(mediation)
data(framing)
table(framing$treat) # check
dim(framing) # and check

boxplot(framing$emo~framing$treat, main="Emo for control/treatment")
boxplot(framing$emo~framing$cong_mesg, main="Emo for Send/NotSend")

#Is there a significant effect of treatment on emo?
summary(lm(emo ~ treat,framing)) # treatment effect significantly reduces anxiety (increases emo) est=1.48, se=0.38, t=3.89

#Is there a significant effect of treatment on cong_mesg?
summary(lm(cong_mesg ~ treat,framing)) # no, treatment has no significant effect on cong_mesg, t=1.62, p=.11

#Estimate the effect on probability of sending congressional message of a unit change in "emo" (negative feelings).
summary(lm(cong_mesg ~ emo,framing)) # the prob. of sending message increases by 6.3% points for each additional unit of emo; not that at emo=0, the intercept (baseline prob.) is out of bound (<0)

#Compare the "path analysis" estimator (see week 3) for the encouragement design with the estimator you used in part 1.
#   The estimator in part 1 is simply a computing two difference-in-difference points and calculating the slope of straight line that passes though both points.
#   The 'path analysis' estimator is based on two regressions BP ~ Salt + Group, and Salt~Group.
#   However, these regressions reveal that the 'path analysis' coefficients are biased. There is support for a zero relation between treat and cong_mesg here (like \tau=0 in class). Still, even under this assumption, the 'p.a.' estimator is biased, due to individual differences in cong_mesg.

#Why do the needed assumptions in this study seem far less reasonable than for the salt example?
#   It seems unlikely that assignment to group (treat) has no effect on cong_mesg because people are exposed to media stories in both conditions, which likely raises their awareness of current issues either way (ctr and trt) and thereby increase prob. of sending message. (however, this is not supported by the regression above)
#   An important individual difference that in this study is gender, given that changes in "emo" are likely to different between gender. (Females more emotional than males?) 
boxplot(framing$emo~framing$gender) # well, doesn't look like it in this plot

#Repeat these questions just for the females in the study. Find anything different?
summary(lm(emo ~ treat,subset(framing,gender=="female"))) # significant and similar to above, 1.16 increase in emo (anxiety reduction) due to treatment
summary(lm(cong_mesg ~ treat,subset(framing,gender=="female"))) # NS t=0.14
summary(lm(cong_mesg ~ emo,subset(framing,gender=="female"))) # significant 5.6% points prob. increase associated with additional unit emo. Similar to above.


#----------------------------------------------------------------------
# QUESTION 3
install.packages("mlmRev")
library(mlmRev)
data(Exam)

# check if 'schavg' is really the school mean of 'standLRT'
ddply(Exam, .(school,schavg), summarize, mean(standLRT))

# part 1 a.
summary(lm(normexam~standLRT, Exam)) # indiv. level regression
agg=ddply(Exam, .(school,schavg), summarize, normexam=mean(normexam))
summary(lm(normexam~schavg, agg)) # group level regression
0.595057-0.883722 # = -0.288665 aggregation bias: difference in individual and group slope coefficient

# part 1 b.
summary(lm(normexam~standLRT+schavg, Exam)) # contextual model
0.354017 # contextual effect: increase in normexam for unit increase in school LRT score "controlling" for indiv. score

# part 2 c.
coed=subset(Exam, type=="Mxd") # extract coed schools
table(coed[coed$school==47,"sex"]) # only 1 female, not really coed
table(coed[coed$school==43,"sex"]) # only 1 male, not really coed

# SFYS regression: outcome ~ sex |for each school
regc=lmList(normexam~sex|school, data=coed)
summary(coef(regc)) # males do worse on average by 0.2693
boxplot(coef(regc)) # outlier slope detected, but leaving it in; looks like a significant gender effect

# Multilevel (very basic, each school has own inter./slope)
# Level 1: normexam_{ij} ~ a_{0i} + a_{1i} sex_{ij} + e_{ij}
# Level 2: a_{0i} = b_{00} + e_{0i}
#          a_{1i} = b_{10} + e_{1i}
# Combined: normexam_{ij} ~ b_{00} + b_{10} sex_{ij} + [e_{ij}+e_{0i}+e_{1i}]
lmer(normexam~sex+(sex|school), coed)
# b_{00} intercept (female score avg): .03311 (slightly above SFYS)
# b_{10} gender effect -.2614 (se=.04163) for males (similar to SFYS)

# part 2 d.
cor.test(as.numeric(coed$intake),as.numeric(coed$sex)) #weak sig. correlation between intake and gender
prop.table(table(coed$intake,coed$sex),1) # more males for higher intake
prop.test(table(coed$intake,coed$sex)) # sig. evidence for unequal proportion
cor.test(as.numeric(coed$intake),coed$normexam) # strong & sig. correlation between intake band and score (no surprise...)

# Multilevel (intercept/'slope' depend on intake)
# Level 1: normexam_{ij} ~ a_{0i} + a_{1i} sex_{ij} + e_{ij}
# Level 2: a_{0i} = b_{00} + b_{01} intake_i + e_{0i}
#          a_{1i} = b_{10} + b_{11} intake_i + e_{1i}
# Combined: normexam_{ij} ~ b_{00} + b_{01} intake_i + b_{10} sex_{ij} + b_{11} intake_i:sex_{ij} + [e_{ij}+e_{0i}+e_{1i}]
m1=lmer(normexam~sex*intake+(sex|school), coed)
m1
# b_{00} intercept (intake=bottom 25%,sex=F): 0.62380
# b_{01} intake effects on intercept: very significant!
# b_{10} gender effect/gap (for male): -0.11479
# b_{11} intake effect on gender effect/gap: small effect size, and not significant fro mid50%

# Maybe better model with only intercept depending on intake
m2=lmer(normexam~sex+intake+(sex|school), coed)
anova(m1,m2) # simpler model favored by BIC
# Level 1: normexam_{ij} ~ a_{0i} + a_{1i} sex_{ij} + e_{ij}
# Level 2: a_{0i} = b_{00} + b_{01} intake_i + e_{0i}
#          a_{1i} = b_{10} + e_{1i}
# Combined: normexam_{ij} ~ b_{00} + b_{01} intake_i + b_{10} sex_{ij} + [e_{ij}+e_{0i}+e_{1i}]
m2
# b_{00} intercept (intake=bottom 25%,sex=F): 0.65753
# b_{01} intake effects on intercept: very significant! higher intake band associated with reduction in normalizd examscore
# b_{10} gender effect/gap (for male): -0.18774

# Looks like intake band is important in explaining exam scores. Judging by effect size and significance level intake seems even more important than gender gap, though gender remains a significant predictor in the model.

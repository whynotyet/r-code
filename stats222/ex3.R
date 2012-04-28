###################################
# STATS 222 Exercise Set (Week 3) #
###################################

# 1. Early Education data (From Bates and Willett-Singer). Week 3 class example, Data on early childhood cognitive development described in Doug Bates talk materials (pdf pages 49-52). Obtain these data from the R-package "mlmRev" or the Willett-Singer book site (in our week 1 intro links). Data are in long form and consist of 3 observations 58 treatment and 45 control children; see the Early entry in the mlmRev package docs.
install.packages("mlmRev")
library("mlmRev")
data(Early)
head(Early)
# Produce the plot of individual trajectories shown pdf p.49, Bates talk. (note:Bates does connect-the-dots, we have done straight-line fit, your choice). Show five-number summaries of rates of impovement in cognitive scores for treatment and control groups. Develop and fit the fm12 lmer model shown in Bates pdf p.50 (note fm12 allows trt to effect rates of improvement but not level;). Interpret results.
xyplot(cog ~ age | id, type=c("p", "r"), index.cond=function(x,y)
{coef(lm(y ~ x))[1]}, data=Early)

#how do I get the rates of improvlement of difference (data manipulation) ?

Early1 <- within(Early , tos <- age -0.5)
fm12 <- lmer(cog ~ tos+trt:tos+(tos|id), Early1, verbose=TRUE)
#what is the verbose output?
#how to interpret the summary? Corr = -1 ?
summary(fm12)
# 	Linear mixed model fit by REML 
# 	Formula: cog ~ tos + trt:tos + (tos | id) 
# 	Data: Early1 
# 	AIC  BIC logLik deviance REMLdev
# 	2379 2405  -1182     2371    2365
# 	Random effects:
# 		Groups   Name        Variance Std.Dev. Corr   
# 	id       (Intercept) 	166.418  12.9003         
# 	tos         			 10.484   3.2379  -1.000 
# 	Residual              	75.537   8.6912         
# 	Number of obs: 309, groups: id, 103
# 	
# 	Fixed effects:
# 				Estimate Std. Error t value
# 	(Intercept)  120.783      1.824   66.23
# 	tos          -22.470      1.494  -15.04
# 	tos:trtY       7.646      1.447    5.28
# 	
# 	Correlation of Fixed Effects:
# 				(Intr) tos   
# 	tos      -0.687       
# 	tos:trtY  0.000 -0.545


######################################################
# 2. Ramus Data example; see links in week 3 Class data examples. Use lmList to obtain the 20 OLS fits, with the initial time set to 8 years of age, i.e. intercepts are fits for the time of initial measurement (not t=0). Fit the lmer model for the collection of growth curves (using initial time = 8); verify that fixed effects are the sample means (over persons) of the lmList intercepts and slopes. Verify that the random effects variance for "age" (i.e. slopes) is the method-of-moments estimate for Var(theta). Compare the random effect estimates (ranef) which borrow strength for each subject with the OLS estimates from lmList (c.f. Bates Chap 4 discussion of sleepstudy data)
ramusfull  = read.table(file="http://www-stat.stanford.edu/~rag/stat209/ramuslmedat", header = T)
head(ramusfull)
ramus.list = lmList(ramus ~ age| subj , data=ramusfull)
#lmList does not work with I(age-8). How to set initial time to 8 otherwise?

ramus.lm = lmer(ramus ~ I(age-8) + (age|subj), data=ramusfull)
# 	Linear mixed model fit by REML 
# 	Formula: ramus ~ I(age - 8) + (age | subj) 
# 	Data: ramusfull 
# 	AIC   BIC logLik deviance REMLdev
# 	246.2 260.5 -117.1      234   234.2
# 	Random effects:
# 	Groups   Name        Variance Std.Dev. Corr   
# 	subj     (Intercept) 92.06228 9.59491         
# 	age          1.20348 1.09703  -0.966 
# 	Residual              0.19345 0.43983         
# 	Number of obs: 80, groups: subj, 20
# 	
# 	Fixed effects:
# 				Estimate Std. Error t value
# 	(Intercept)  48.6755     0.5696   85.46
# 	I(age - 8)    1.8660     0.2606    7.16
# 	
# 	Correlation of Fixed Effects:
# 		(Intr)
# 	I(age - 8) -0.222

mean(coef(ramus.list)[,1])
# 	33.7475 NOT THE SAME, BECAUSE HERE LMER AGE=8, BUT LMLIST AGE=0
mean(coef(ramus.list)[,2])
# 	1.866 SAME AS FIXED EFFECT ESTIMATE
var(coef(ramus.list)[,2])
#	1.358236

######################################################
# 3. Continue problem 3a from week 2 with the "X" week 1 data. Produce a boxplot of the (40) individual rates of change and a scatterplot of the rates of change against the background, exogenous variable (W). Follow the week 3 NC example, pdf pages 3,4 of the plots link.


######################################################
# 4. Brain example. Take the extended model (p.3 on link) and derive the indicated combined model (either version). Also do a parameter interpretation listing as done for the base model (pdf p.2 link)
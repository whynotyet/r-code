###################################
# STATS 222 Exercise Set (Week 4) #
###################################

# 1. lmer/lme vs lm  Consider the sleepstudy and Ramus examples, collections of growth trajectories with no exogenous variable. Fitting the lmer models with Formula: Reaction ~ 1 + Days + (1 + Days | Subject) or Formula: ramus ~ I(age - 8) + (age | subj) has motivated the student question, what is going on here beyond what lm would do? So let's look at what lm would do in these examples. Verify (or disprove) the assertion that the fixed effects from lmer, which we have seen are the averages of the individual fit parameter estimates (i.e. lmList), and therefore the coefficients of the average growth curve are identical to the fit from lm (which ignores the existence of individual trajectories). Compare the results of the lm and lmer analyses for these two data sets.




######################################################
# 2. Vocabulary learning data from test results on file in the Records Office of the Laboratory School of the University of Chicago. Source D R Bock, MSMBR. The data consist of scores, obtained from a cohort of pupils at the eigth through eleventh gade level on alternative forms of the vocabulary section of the Cooperative Reading Tests." There are 64 students in all, 36 male, 28 female (ordered) each with four equally spaced observations (test scores). Wide form of these data are in BOCKwide.dat and I kindly also made a long-form version BOCKlong.dat . Construct the usual collection of individual trajectory display (either connect-the-dots or compare to a straight-line). Obtain the means (over persons) and plot the group growth curve, also do separately by gender. Does there appear to be curvature (i.e. deceleration in vocabulary skill growth)? Construct an lmer model with the individual growth curve a quadratic function of grade (year), most convenient to use uncorrelated predictors grade - mean(grade) and (grade - mean(grade))^2. In the level II model allow each of the three paraters of the individual quadratic curves to differ by gender. Fit the lmer model and interpret the fixed and random effects you obtain. Compare the results with a lmer model in which the individual trajectories are straight-line. Use the anova model comparison functionality in R (e.g. anova(modLin, modQuad) to test whether the quadratic function for individual growth produces a better model fit.


######################################################
# 3. Orange tree extras. Take the fixed effects from the orangre tree nlmer model, "m1" in the class materials, as the parameters of the "average" growth curve for this group of tress. Plot that logistic growth curve. Compare the fixed effects from nlmer to the results from nls for these data. More challenging Try to superimpose the group logistic curve )(above) onto the plots of the individual tree trajectories.



######################################################
# 4. Asymptotic regression, SSasymp slide (pdf p.5 of Bates slides). Data are from Neter-Wasserman text in file CH13TA04.txt. The outcome variable is manufacturing relative efficiency (RelEff) over 90 weeks duration for two different locations. Plot the RelEff outcome against week for the two locations. Use the SSasymp function for a nlmer fit (or nls if needed) to see whether the asymptote differs for the two locations.



######################################################
# 5. Autocorrelation example. Take the week 1 data and add AR(1) errors to the "Xi" observations. Assess the consequences for analyzing the collection of grwoth curves. Post of a worked out example of consequences.




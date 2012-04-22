###################################
# STATS 222 Exercise Set (Week 2) #
###################################

# 1. Reliability versus precision demonstration
# Consider a population with true change between time1 and time2 distributed Uniform [99,101] and measurement error Uniform [-1, 1]. If you used discrete Uniform in this construction then you could say measurement of change is accurate to 1 part in a hundred.
# Calculate the reliability of the difference score.
# Also try error Uniform [-2,2], accuracy one part in 50.
# A similar demonstration can be found in my Shoe Shopping and the Reliability Coefficient




######################################################
# 2. Regression toward the mean? Galton's data on the heights of parents and their children
# In the "HistData" or "psych" packages reside the "galton" dataset, the primordial regression toward mean example.
# Description: Galton (1886) presented these data in a table, showing a cross-tabulation of 928 adult children born to 205 fathers and mothers, by their height and their mid-parent’s height. A data frame with 928 observations on the following 2 variables. parent Mid Parent heights (in inches) child Child Height. Details: Female heights were adjusted by 1.08 to compensate for sex differences. (This was done in the original data set)
# Consider "parent" as time1 data and "child" as time2 data and investigate whether these data indicate regression toward the mean according to either definition (metric or standardized)? Refer to Section 4 of the Myths chapter supplement (pagination 61-63) for an assessment of regression toward the mean (i.e. counting up number of subjects satisfying regression-toward-mean).
# Aside: if you like odd plots, try this (and then look at the docs ?sunflowerplot; this may require the package "car" to be installed on your machine)
# 
# with(Galton,
# {
# sunflowerplot(parent,child, xlim=c(62,74), ylim=c(62,74))
# reg <- lm(child ~ parent)
# abline(reg)
# lines(lowess(parent, child), col="blue", lwd=2)
# if(require(car)) {
# dataEllipse(parent,child, xlim=c(62,74), ylim=c(62,74), plot.points=FALSE)
# }
# }) 
# 





######################################################
# 3. Let's look again at the Week1 data, here using the bottom half, the fallible "X" measurements (constructed by adding noise to the Xi measurements).
# a. Follow the Week 2 R-session and obtain the plot showing each subjects data and straight-line fit. Use lmList to obtain the 40 slopes for the straight-line fits. Compare the five-number summary of rates of change for the "X" measurement with that obtained for the perfectly measured "Xi" measurements in the posted R-session.
# Solution provided for problem 3a
# b. Standardizing is always a bad idea is a good motto for life, especially with longitudinal data. Start out with the "X" data, and standardize (i.e. transform to mean o, var 1) at each of the 3 time points. Note "scale" will do this for you (in wide form). For the standardized data obtain the plot showing each subjects data and straight-line fit. What do you have here?





######################################################
# 4. Paired and unpaired samples, continuous vs categorical measurements.
# Let's use again the 40 subjects in the week1 "X" data.
# a. Measured data. Take the time1 and time5 observations and obtain a 95% Confidence Interval for the amount of change. Compare the width of that interval with a confidence interval for the difference beween the time5 and time1 means if we were told a different group of 40 subjects was measured at each of the time points (data no longer paired).
# b. Dichotomous data. Instead look at these data with the criterion that a score of 50 or above is a "PASS" and below that is "FAIL". Carry out McNemar's test for the paired dichotomous data, and obtain a 95% CI for the difference between dependent proportions. Compare that confidence interval with the "unpaired" version (different group of 40 subjects was measured at each of the time points) for independent proportions. 



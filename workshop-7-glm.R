#### Sparrows ####

# Read and examine the data
# 
# 1. Read the data from the file and inspect the first few lines to make sure 
# it was read correctly.

sparrows <- read.csv("data-raw/songsparrow.csv", stringsAsFactors = F)

# 2. We’ll be comparing survival probabilities among different years. To this end, 
# make sure that year is a categorical variable in your data frame.
str(sparrows)
sparrows$year <- as.factor(sparrows$year)


# 3. Plot survival against tarsus length of female sparrows. Use a method to 
# reduce the overlap of points (the response variable is discrete) to see the patterns
# more clearly.
unique(sparrows$sex) # all are female

plot(jitter(sparrows$survival) ~ sparrows$tarsus, 
     xlab = "tarsus length(mm)",
     ylab = "survival",
     pch = 16)


# 4. Examine the plot. Can you visualize a trend? Use a smoothing method to see if any 
# trend is present (most methods won’t constrain the curve to lie between 0 and 1, 
# but at least you’ll get an idea).

plot(jitter(sparrows$survival) ~ sparrows$tarsus, 
     xlab = "tarsus length(mm)",
     ylab = "survival",
     pch = 16)
lines(lowess(sparrows$tarsus, sparrows$survival))
# The lowess method calculates a smooth curve that estimates the trend in the data 
# (rather like a running mean). Is it similar to the glm fitted curve?


# Fitting a GLM:

# 1. The response variable is binary. What probability distribution is appropriate to
# describe the error distribution around a model fit? What is an appropriate link 
# function?

# family = binomial(link="logit")

# 2. Fit a generalized linear model to the data on survival and tarsus length.
z <- glm(survival ~ tarsus, family = binomial(link="logit"), data = sparrows)

# 3. Use visreg to visualize the model fit. You should see a straight line, as in a 
# simple linear regression. Interpret this linear relationship. What does it describe? 
# Don’t worry too much about the interpretation of the dots in this figure (they are 
# the “working values” of the response variable on the logit scale from the last 
# iteration of the behind-the-scenes fitting procedure).

# On the scale of the linear predictor, an increase in tarsus length leads to decreases 
# in probability of survival. This visualization shows the model in "imaginary space".
library(visreg)
visreg(z)

# 4. Obtain the estimated regression coefficients for the fitted model. What is the 
# interpretation of these coefficients? On a piece of paper, write down the complete 
# formula for the model shown in the visreg plot.
fitted(z)
coef(z)

# y = 24.63612 - 1.25779 * x, where y is survival and x is tarsus length.

# 5. Redraw the plot of survival against tarsus length. Now add the fitted logistic 
# regression curve to your plot. What quantity is described by the curve?

# The quantity described is the probability of "success" in this case, survival.

plot(jitter(sparrows$survival, amount = 0.02) ~ sparrows$tarsus, 
     xlab = "tarsus length(mm)",
     ylab = "survival",
     pch = 16)

yhat <- fitted(z)

lines(yhat[order(sparrows$tarsus)] ~ sparrows$tarsus[order(sparrows$tarsus)])

zhat <- predict(z, se.fit = TRUE)       # result on logit or log scale
zupper <- zhat$fit + 1.96 * zhat$se.fit
zlower <- zhat$fit - 1.96 * zhat$se.fit

yupper <- exp(zupper)/(1 + exp(zupper)) # for logit link
ylower <- exp(zlower)/(1 + exp(zlower))

# or
# yupper <- exp(zupper)                   # for log link
# ylower <- exp(zlower)

lines(yupper[order(sparrows$tarsus)] ~ sparrows$tarsus[order(sparrows$tarsus)], lty = 2)
lines(ylower[order(sparrows$tarsus)] ~ sparrows$tarsus[order(sparrows$tarsus)], lty = 2)

# OR just use visreg:
visreg(z, xvar = "tarsus", ylim = range(sparrows$survival), scale = "response")  # fit on the original scale
points(jitter(sparrows$survival, amount = 0.02) ~ sparrows$tarsus,
       pch = 16)

visreg(z)
?visreg

# 6. Use the coefficients to calculate the predicted survival probability of a song 
# sparrow having tarsus length 20.5 mm*. Does the result agree with your plot of the 
# fitted regression curve?

coef(z)
24.63612 - 1.25779 * 20.5 # No, because this is still on the logit scale.
plogis(24.63612 - 1.25779 * 20.5)

# 7. The ratio -intercept/slope estimates the point at which probability of survival is 
# changing most rapidly. In toxicology this point is known as the LD50. Calculate this 
# value** and compare it visually with the fitted curve. Does it agree? Finally, the 
# slope of the curve at a given value for the explanatory variable 
# x is b * p(x) * ( 1 – p(x) ), where b is the slope coefficient and 
# p(x) is the predicted probability of survival at that x.

library(MASS)
confint(z, level = 0.95) # approximate 95% confidence intervals
dose.p(z, p = 0.50)      # LD50 for a dose-response curve

# The ratio of intercept/slope is highest at x = 19.58683. This is the tarsus length
# value for which probability of survival is changing most rapidly. 

# 8. Calculate the likelihood-based 95% confidence interval for the logistic regression 
# coefficients.

confint(z, level = 0.95)


# 9. The summary(z) output for the regression coefficients also includes z values and 
# P-values. What caution would you take when interpreting these P-values? Use a more 
# accurate method to test the null hypothesis of zero slope.

summary(z)

# Note that P-values in the summary() output are based on a normal approximation and are 
# not accurate for small to moderate sample sizes -- use the log likelihood ratio test 
# instead (see below).

anova(z, test = "Chisq")

# 10. If time permits, add year to your logistic regression model (make sure that year
# is a categorical variable in your data set). Never mind the interaction between year
# and tarsus length. Plot the resulting curves. Is there any evidence of a difference 
# among years in the relationship between survival and tarsus length in these data? Use 
# lsmeans to calculate the model estimates of mean survival in each year.


#### Horseshoe Crabs ####

# 1. Read the data from the file. View the first few lines of data to make sure it was 
# read correctly. Use the str command to see the variables and groups.

crabs <- read.csv("data-raw/satellites.csv", stringsAsFactors = F)
str(crabs)

# 2. Plot the number of satellites against the width of the carapace, a measure of 
# female body size. Fit a smooth curve to examine the trend.
plot (nsatellites ~ width.cm, 
      data = crabs,
      pch = 16)

lines(lowess (crabs$width.cm, crabs$nsatellites))

# Fit a generalized linear model

# 1. Fit a model to the relationship between the number of satellites and the width 
# of the female carapace. What type of variable is the number of satellites? 
# What probability distribution might be appropriate to describe the error distribution 
# around a model fit? What is the appropriate link function?

z1 <- glm (nsatellites ~ width.cm, family = poisson (link="log"), data = crabs)

hist(crabs$nsatellites) 

# 2. Fit a generalized linear model to the relationship between number of satellite 
# males and female carapace width. Use visreg to examine the relationship on the 
# transformed scale, including confidence bands. Don’t worry about the data points 
# (they are “working values” for the response variable from the last iteration of model 
# fitting, which glm uses behind the scenes to fit the model on the transformed scale).

library(visreg)
visreg(z1)


# 3. Plot the data on the original scale, and add the glm model fit to your plot. 
# Why is it curvilinear?

plot(crabs$nsatellites ~ crabs$width.cm, 
     xlab = "carapace width (cm)",
     ylab = "number of satellites",
     pch = 16)

yhat1 <- fitted(z1)

lines(yhat1[order(crabs$width.cm)] ~ crabs$width.cm[order(crabs$width.cm)])

zhat1 <- predict(z1, se.fit = TRUE)       # result on logit or log scale
zupper1 <- zhat1$fit + 1.96 * zhat1$se.fit
zlower1 <- zhat1$fit - 1.96 * zhat1$se.fit

yupper1 <- exp(zupper1)                   # for log link
ylower1 <- exp(zlower1)

lines(yupper1[order(crabs$width.cm)] ~ crabs$width.cm[order(crabs$width.cm)], lty = 2)
lines(ylower1[order(crabs$width.cm)] ~ crabs$width.cm[order(crabs$width.cm)], lty = 2)

# 4. Extract the estimated regression coefficients from your model object. What is the
# interpretation of these coefficients? On a piece of paper, write down the complete 
# formula for your fitted model.

coef(z1)

# These are the coefficients for the linear model fit in the imaginary predictor scale. 
# y = -3.03047572 + 0.1640451 * x

# 5. Calculate the likelihood-based 95% confidence interval for the regression 
# coefficients. The most useful estimate is that for the slope, since exp(slope) 
# represents the multiple to the response variable accompanying a 1-unit change 
# in the explanatory variable. Convert the lower and upper confidence limits for 
# the slope to the original scale to obtain the confidence interval for the multiple.

library(MASS)
confint(z1, level = 0.95)

exp(coef(z1)[2])


# 6. Test the relationship between number of satellite males and female carapace width. Notice how small the P-value is for the null hypothesis test for the slope. I’m afraid that this is a little optimistic. Why? Read on.

# 7. When you extracted the regression coefficients from your model object, you probably saw the following line of output: “(Dispersion parameter for poisson family taken to be 1)”. What are we really assuming here?

# 8. If you did not want to rely on this assumption (or you wanted to estimate the dispersion parameter), what option is available to you? Refit a generalized linear model without making the assumption that the dispersion parameter is 1. Save the results in a new glm object so that you can compare your results with the previous fit.

# 9. Extract and examine the coefficients of the new glm model object. Examine the estimated dispersion parameter. Is it close to 1? On this basis, which of the two glm fits to the same data would you regard as the more reliable?

# 10. How do the regression coefficients of this new fit compare with the estimates from the earlier model fit? How do the standard errors compare? Why are they larger this time?

# 11. Compare the visreg plot of the current model to that of the earlier fit. What difference do you notice?

# 12. Redo the test of significance for the slope of the relationship between number of satellite mates and female carapace width. Remember to use the F test rather than the likelihood ratio test in the anova command. How do the results compare with those from the previous fit?

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
# 

# 10. If time permits, add year to your logistic regression model (make sure that year
# is a categorical variable in your data set). Never mind the interaction between year
# and tarsus length. Plot the resulting curves. Is there any evidence of a difference 
# among years in the relationship between survival and tarsus length in these data? Use 
# lsmeans to calculate the model estimates of mean survival in each year.

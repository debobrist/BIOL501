bmr <- read.csv('data-raw/bmr.csv')

# 1. Plot the data. Is the relationship between mass and metabolic rate linear on a 
# log scale?

plot (log(bmr$bmr.w) ~ log(bmr$mass.g))

# Yes

# 2. Fit a linear model to the log-transformed data (original data are not on the log 
# scale). What is the estimate of slope?

bmr.mod1 <- lm(log(bmr$bmr.w) ~ log(bmr$mass.g))
summary(bmr.mod1)

# 0.736

# 3. Produce a 95% confidence interval for the estimate of slope. 
# Does the interval include either of the candidate values for the scaling parameter β?

confint(bmr.mod1)
# CI for slope = 0.711222 : 0.76186. Theory based on optimization of hydrodynamic flows 
# through the circulation system predicts that the exponent should be β = 3/4.

# 4. Add the best-fit regression line to the plot in (1).

# 5. Now compare the fits of the two candidate models to the data. To accomplish this you need to force a regression line having a specified slope through the (log-transformed) data. See the end of the part on simple linear regression in the “Fit a linear model” section of the “Fit model” tab at the R tips page.

# 6. Replot the data indicating the relationship between log(M) and log(BMR). Add to this plot the best-fit line having slope 3/4. Repeat this for the slope 2/3. By eye, which line appears to fit the data best?

# 7. Compare the residual sum of squares of the two models you fit in (5). Which has the smaller value? Do these values agree with your visual assessment of your plots in (6)?

# 8. Calculate the log-likelihood of each model fitted in (5). Which has the higher value?

# 9. Calculate AIC for the two models, and the AIC difference*. By this criterion, which model is best? How big is the AIC difference?

# 10. In general terms, what does AIC score attempt to measure?
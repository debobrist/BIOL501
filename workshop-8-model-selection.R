#### BMR  ####

bmr <- read.csv ('data-raw/bmr.csv')

# 1. Plot the data. Is the relationship between mass and metabolic rate linear on a 
# log scale?

plot (log (bmr$bmr.w) ~ log (bmr$mass.g))

# Yes

# 2. Fit a linear model to the log-transformed data (original data are not on the log 
# scale). What is the estimate of slope?

bmr.mod1 <- lm (log (bmr$bmr.w) ~ log (bmr$mass.g))
summary (bmr.mod1)

# 0.736

# 3. Produce a 95% confidence interval for the estimate of slope. 
# Does the interval include either of the candidate values for the scaling parameter β?

confint (bmr.mod1)
# CI for slope = 0.711222 : 0.76186. Theory based on optimization of hydrodynamic flows 
# through the circulation system predicts that the exponent should be β = 3/4.

# 4. Add the best-fit regression line to the plot in (1).
abline (bmr.mod1)

# 5. Now compare the fits of the two candidate models to the data. To accomplish this 
# you need to force a regression line having a specified slope through the 
# (log-transformed) data. See the end of the part on simple linear regression 
# in the “Fit a linear model” section of the “Fit model” tab at the R tips page.

bmr.mod2 <- lm (log (bmr$bmr.w) ~ 1 + offset ((3/4) * log (bmr$mass.g)))

bmr.mod3 <- lm (log (bmr.w) ~ 1 + offset ((2/3) * log (mass.g)),
                data = bmr) 

# 6. Replot the data indicating the relationship between log(M) and log(BMR). 
# Add to this plot the best-fit line having slope 3/4. Repeat this for the slope 2/3. 
# By eye, which line appears to fit the data best?

plot (log (bmr$bmr.w) ~ log (bmr$mass.g),
      pch = 16,
      xlab = "mass (g)",
      ylab = "bmr (watts)")

abline (a = summary(bmr.mod2)$coefficients[1,1], 
        b = 3/4,
        col = "seagreen",
        lwd = 2)

abline (a = summary(bmr.mod3)$coefficients[1,1],
        b = 2/3,
        col = "coral", 
        lwd = 2)

# The line with slope 3/4 fits the data better the line with slope 2/3.

# 7. Compare the residual sum of squares of the two models you fit in (5). 
# Which has the smaller value? Do these values agree with your visual assessment of 
# your plots in (6)?

anova (bmr.mod2)
anova (bmr.mod3)

# The model with slope 3/4 has a lower residual sum of squares - this is good because
# this is the model we determined (visually) fit the data better, so should have smaller
# residuals.

# 8. Calculate the log-likelihood of each model fitted in (5). Which has the higher value?

logLik (bmr.mod2) # -14.79 this is the higher log likelihood.
logLik (bmr.mod3) # -26.65

# 9. Calculate AIC for the two models, and the AIC difference*. By this criterion, 
# which model is best? How big is the AIC difference?

AIC (bmr.mod2) # 33.58269
AIC (bmr.mod3) # 57.3186

AIC (bmr.mod3) - AIC (bmr.mod2) # Delta AIC is 23.73591, so we can confidently say that
# model 2 is better than model 3.

# 10. In general terms, what does AIC score attempt to measure?

# The AIC score tells you the relative goodness of fit of the models you are comparing.

# 11. Calculate the Akaike weights of the two models**. Which has the higher weight of 
# evidence in its favor? These weights would be used in Multimodel Inference (such as 
# model averaging), which we won’t go into in this course. The weights should sum to 1. 
# (They are sometimes interpreted as the posterior probability that the given model is 
# the “best” model, assuming that the “best” model is one of the set of models being 
# compared, but this interpretation makes assumptions that we won’t go into right now.)


x <- c(AIC(bmr.mod2), AIC(bmr.mod3)) # stores AIC values in a vector
delta <- x - min(x)               # AIC differences

L <- exp(-0.5 * delta)            # relative likelihoods of models
w <- L/sum(L)                     # Akaike weights

# 9.999930e-01, 7.011488e-06

# 12. Summarize the overall findings. Do both models have some support, according to 
# standard criteria, or does one of the two models have essentially no support?

# The 2nd model (bmr.mod3, in this case) essentially has no support. bmr.mod2 has nearly
# all of the weight, and since the deltaAIC between the two models was ~23, we know that 
# the 2nd model has no support.

# 13. Why is it not possible to compare the two models using a conventional log-likelihood
# ratio test?

# They are not nested.

# Optional: Both theories mentioned earlier predict that the relationship between 
# basal metabolic rate and body mass will conform to a power law — in other words that 
# the relationship between log(BMR) and log(M) will be linear. Is the relationship 
# linear in mammals? Use AIC to compare the fit of a linear model fitted to the 
# relationship between log(BMR) and log(M) with the fit of a quadratic regression of 
# log(BMR) on log(M) (a model in which both log(M) and (log(M))2 are included as terms). 
# Don’t force a slope of 2/3 or 3/4. Plot both the linear and quadratic regression curves
# with the data. Which model has the most support? Which has the least? On the basis of 
# this analysis, does the relationship between basal metabolic rate and body mass in 
# mammals conform to a power law?




#### Bird abundance ####
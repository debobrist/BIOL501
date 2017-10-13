# Plot lions data:
lions <- read.csv("data-raw/lions.csv")

plot(lions$age ~ lions$black,
     xlab = "Proportion of Black on Nose",
     ylab = "Age",
     pch = 16)
lionsmod1 <- lm(lions$age ~ lions$black)
abline(lionsmod1,
       lwd = 2)

# Check model assumptions, confidence intervals, etc.:
summary(lionsmod1)

coef(lionsmod1) # B0= 0.879, B1 = 10.647

confint(lionsmod1)

anova(lionsmod1)

plot(lionsmod1)

# Plot data without last data point (oldest lion - high leverage)

plot(lions$age[lions$age != 13.1] ~ lions$black[lions$age != 13.1 ],
     xlab = "Proportion of Black on Nose",
     ylab = "Age",
     pch = 16)
lionsmod2 <- lm(lions$age[lions$age != 13.1] ~ lions$black[lions$age != 13.1])
abline(lionsmod2,
       lwd = 2)


# Check model assumptions, confidence intervals, etc.:
summary(lionsmod2)

coef(lionsmod2) # B0= 0.879, B1 = 10.647

confint(lionsmod2)

anova(lionsmod2)

plot(lionsmod2)


#### Prediction #### 

# 1. Display the data once again in a scatter plot. Add the regression line.

plot(lions$age ~ lions$black,
     xlab = "Proportion of Black on Nose",
     ylab = "Age",
     pch = 16)

lionsmod1 <- lm(age ~ black, data = lions)

xpoints <- range(lions$black)

yhat <- predict(lionsmod1, newdata = data.frame(black = xpoints))

lines(yhat ~ xpoints)

# 2. Add confidence bands to the scatter plot. These are confidence limits for the 
# prediction of mean of lion age at each value of the explanatory variable. 
# You can think of these as putting bounds on the most plausible values for the 
# “true” or population regression line. Note the spread between the upper and lower 
# limits and how this changes across the range of values for age.

xpoints <- seq(min(lions$black), max(lions$black), length.out = 100) 
ypoints <- data.frame(predict(lionsmod1, newdata = data.frame(black = xpoints), 
                              interval = "confidence", level = 0.95)) 
lines(ypoints$lwr ~ xpoints, lty = 2) 
lines(ypoints$upr ~ xpoints, lty = 2)

# 3. Add prediction intervals to the scatter plot. These are confidence limits for 
# the prediction of new individual lion ages at each value of the explanatory variable. 
# Whereas confidence bands address questions like “what is the mean age of lions whose 
# proportion black in the nose is 0.5 ?”, prediction intervals address questions like 
# “what is the age of that new lion over there, which has a proportion black in the 
# nose of 0.5 ?”.

ypoints2 <- data.frame(predict(lionsmod1, newdata = data.frame(black = xpoints), 
                              interval = "prediction", level = 0.95))
lines(ypoints$lwr ~ xpoints, lty = 2, col = "blue")
lines(ypoints$upr ~ xpoints, lty = 2, col = "blue")

# 4. Examine the confidence bands and prediction intervals. Is the prediction of mean 
# lion age from black in the nose relatively precise? Is prediction of individual lion 
# age relatively precise? Could this relationship be used in the field to age lions?

# Yes, prediction is relatively precise and maybe could be used to age lions in the field, 
# especially for younger lions.

# 5. Optional: use the visreg command to visualize the fit of the regression model to 
# the data, including the 95% confidence bands. You’ll need to install the visreg 
# package if you haven’t already done so, and then load it before you start to use it.

library(visreg)

visreg(lionsmod1)

#### Effects of light treatment on circadian rhythms ####

# 1. Read the data from the file.
knees <- read.csv("data-raw/knees.csv")

# 2. View the first few lines of data to make sure it was read correctly.
head(knees)

# 3. Determine whether the categorical variable “treatment” is a factor. 
# If not a factor, convert treatment to a factor using the factor command. 
# This will be convenient when we fit the linear model.
class(knees$treatment)

# 4. Use the levels command on the factor variable “treatment” to see how R has 
# ordered the different treatment groups. The order will be alphabetical, by default. 
# Conveniently, you will find that the control group is listed first in the 
# alphabetical sequence. (As you are about to analyze these data with a linear model
# in R, can you think of why having the control group first in the order is convenient?)
levels(knees$treatment)

# 5. To get practice, change the order of the levels so that the “knee” treatment group
# is second in the order, after “control”, and the “eyes” group is listed third.
levels(knees$treatment) <- c("control", "knee", "eyes")
levels(knees$treatment)

# 6. Plot the phase shift data, showing the individual data points in each treatment 
# group.

stripchart(shift ~ treatment, data = knees, vertical = TRUE, method = "jitter",
           pch = 16, col = 'red')

#### Fit the model! ####

# 1. Fit a linear model to the light treatment data. Store the output in an lm object.

k <- lm(shift ~ treatment, data = knees)

# 2. Create a graphic that illustrates the fit of the model to the data. 
# In other words, include the predicted (fitted) values to your plot.

stripchart(fitted(k) ~ treatment, vertical = TRUE, add = TRUE, pch="------",
           method = "jitter", data = knees)

visreg(k)
visreg(k, points.par = list(cex = 1.2, col = "red"))

# 3. Use plot to check whether the assumptions of linear models are met in this case.
# Examine the plots. Are there any potential concerns? There are several options 
# available to you if the assumptions are not met (transformations, robust regression
# methods, etc.) but we don’t seem to need them in this case.

plot(k)

# 4. Remember from lecture that R represents the different levels of the categorical 
# variable using dummy variables. To peek at this behind-the-scenes representation, 
# use the model.matrix command on the model object from your linear model fit in step 
# (1). The output should have a column of 1’s for the intercept and two additional 
# columns representing two of the three levels of the explanatory variable. 
# Why is one level left out? Which level is the one not represented by a dummy variable?*

model.matrix(k)

# 5. Using the lm model object, obtain the parameter estimates (coefficients) along 
# with standard errors. Examine the parameter estimates. If you’ve done the analysis 
# correctly, you should see the three coefficients. Rounded, they are -0.308, -0.027, 
# and -1.24. What do each of these coefficients represent — what is being estimated 
# by each value?**

coef(k) 
# each of the estimates represents the value of the intercept.

# 6. The P-values associated with the three coefficients are generally invalid. 
# Why? Under what circumstance might one of the P-values be valid?
# The p-values are representing "Is the estimate different from the control" not 
# is the estimate different from zero.

# 7. Obtain 95% confidence intervals for the three parameters.
confint(k)

# 8. Test the effect of light treatment on phase shift with an ANOVA table. 
# Note that the output will also include an R2 value. This is loosely interpretable 
# as the “percent of the variance in phase shift that is explained by treatment.”
anova(k)

# 9. Produce a table of the treatment means using the fitted model object, along 
# with standard errors and confidence intervals. Why are these values not the same 
# as those you would get if you calculated means and SE’s separately on the data from
# each treatment group?

library(lsmeans)
lsmeans(k, "treatment")


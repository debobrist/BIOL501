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

# Because these are the least squared means based on the fitted model. The model fits
# the model where there is the smallest squared distance between observed and fitted values.

#### Part 3 ####
# Fruitflies data: The linear model will have longevity as the response variable, 
# and two explanatory variables: treatment (categorical) and thorax length (numerical; 
# representing body size). The goal will be to compare differences in fly longevity 
# among treatment groups, correcting for differences in thorax length. Correcting for
# thorax length will possibly improve the estimates of treatment effect. The method is 
# also known as analysis of covariance, or ANCOVA.

# 1. Read data: 

fruitflies <- read.csv("data-raw/fruitflies.csv")

# 2. View the first few lines of data to make sure it was read correctly.

head(fruitflies)

# 3. Determine whether the categorical variable “treatment” is a factor. If not a factor, 
# convert treatment to a factor. This will be convenient when we fit the linear model.

class(fruitflies$treatment) # it is a factor.

# 4. Use the “levels” command on the factor variable “treatment” to see how R has 
# ordered the different treatment groups (should be alphabetically).

levels(fruitflies$treatment)

# 5. Change the order of the categories so that a sensible control group is first in 
# the order of categories. Arrange the order of the remaining categories as you see fit.
fruitflies$treatment <- factor(fruitflies$treatment, 
                     levels = c("no females added",
                                "1 virgin female",
                                "8 virgin females",
                                "1 pregnant female", 
                                "8 pregnant females"))

# Optional: This repeats an exercise from the graphics workshop. Create a scatter plot, 
# with longevity as the response variable and body size (thorax length) as the 
# explanatory variable. Use a single plot with different symbols (and colors too, 
# if you like) for different treatment groups. Or make a multipanel plot using the 
# lattice or ggplot2 package
names(fruitflies)
plot(longevity.days ~ thorax.mm, 
     data = fruitflies, 
     col = treatment)

library(ggplot2)
library(cowplot)

ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days, color = treatment))+
  geom_point(size = 2)

ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days, color = treatment))+
  geom_point(data = subset(fruitflies, treatment == "no females added"))

ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days, color = treatment))+
  geom_point(data = subset(fruitflies, treatment == "1 virgin female"))

ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days, color = treatment))+
  geom_point(data = subset(fruitflies, treatment == "8 virgin females"))

ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days, color = treatment))+
  geom_point(data = subset(fruitflies, treatment == "1 pregnant female"))

ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days, color = treatment))+
  geom_point(data = subset(fruitflies, treatment == "8 virgin females"))


# Fit a linear model

# 1. Fit a linear model to the fly data, including both body size (thorax length) and 
# treatment as explanatory variables. Place thorax length before treatment in the model 
# formula. Leave out the interaction term for now — we’ll assume for now that there is 
# no interaction between the explanatory variables thorax and treatment.

flymod1 <- lm(longevity.days ~ thorax.mm + treatment, data = fruitflies)

# 2. Use plot to check whether the assumptions of linear models are met in this case. 
# Are there any potential concerns? If you have done the analysis correctly, you will 
# see that the variance of the residuals is not constant, but increases with increasing 
# fitted values. This violates the linear model assumption of equal variance of residuals.

par(mfrow = c(2, 2))
plot(flymod1)

# data appears heteroscedastic - no constant variance of residuals

# 3. Attempt to fix the problem identified in step (3) using a log-transformation of the
# response variable. Refit the model and reapply the graphical diagnostic tools to check 
# assumptions. Any improvements? (To my eye the situation is improved but the issue has 
# not gone away entirely.) Let’s continue anyway with the log-transformed analysis.

flymod2 <- lm(log(longevity.days) ~ thorax.mm + treatment, data = fruitflies)
plot(flymod2) # better but not great

# 4. Visualize the fit of the model to the data using the visreg package. Try two 
# different possibilities. In the first, plot the fit of the response variable to thorax 
# length separately for each treatment group. In the second, plot the fit of the data to 
# treatment, conditioning on the value of the covariate (thorax length).
par(mfrow = c (1, 1))

library(visreg)

# In separate plots
visreg(flymod2, xvar = "thorax.mm", by = "treatment", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"))

# All in one plot!
visreg(flymod2, xvar = "thorax.mm", by = "treatment", whitespace = 0.5, overlay = TRUE, 
       band = FALSE, points.par = list(cex = 1.1))

visreg(flymod2, xvar = "treatment", by = "thorax.mm", whitespace = 0.5, overlay = TRUE, 
       band = FALSE, points.par = list(cex = 1.1))

# 5. Obtain the parameter estimates and standard errors for the fitted model. 
# Interpret the parameter estimates. What do they represent*? Which treatment group 
# differs most from the control group?
summary(flymod2)

# treatment with 8 virgin females differs most from the control group. You can see this
# in the plot above as well. 

# 6. Obtain 95% confidence intervals for the treatment and slope parameters.

confint(flymod2)

# 7. Test overall treatment effects with an ANOVA table. Interpret each significance 
# test — what exactly is being tested?

anova(flymod2)
# Thorax length has a statistically significant effect on longevity, and so does treatment.
# These explanatory variables are being tested against the null hypothesis that they
# do not affect longevity. 

# 8. Refit the model to the data but this time reverse the order in which you entered 
# the two explanatory variables in the model. Test the treatment effects with an ANOVA 
# table. Why isn’t the table identical to the one from your analysis in (7)**?

flymod3 <- lm(longevity.days ~ treatment + thorax.mm, data = fruitflies)
anova(flymod3)

# model terms are being fit sequentially. 

# 9. Our analysis so far has assumed that the regression slopes for different treatment
# groups are the same. Is this a valid assumption? We have the opportunity to investigate
# just how different the estimated slopes really are. To do this, fit a new linear model 
# to the data, but this time include an interaction term between the explanatory 
# variables.

flymod4 <- lm(longevity.days ~ thorax.mm * treatment, data = fruitflies)

# 10. The parameters will be more complicated to interpret in the model including an 
# interaction term, so lets skip this step. Instead, go right to the ANOVA table to test
# the interaction term using the new model fit. Interpret the result. Does it mean 
# that the interaction term really is zero?

anova(flymod4) # Interaction not statistically significant.

# 11. Another way to help assess whether the assumption of no interaction is a sensible 
# one for these data is to determine whether the fit of the model is “better” when an 
# interaction term is present or not, and by how much. We will learn new methods later 
# in the course to determine this, but in the meantime a simple measure of model fit 
# can be obtained using the adjusted R2 value. The ordinary R2 measures the fraction of 
# the total variation in the response variable that is “explained” by the explanatory 
# variables. This, however, cannot be compared between models that differ in the number 
# of parameters because fitting more parameters always results in a larger R2, even if 
# the added variables are just made-up random numbers. To compare the fit of models 
# having different parameters, use the adjusted R2 value instead, which takes account 
# of the number of parameters being fitted. Use the summary command on each of two fitted
# models, one with and the other without an interaction term, and compare their adjusted 
# R2 values. Are they much different? If not, then maybe it is OK to assume that any 
# interaction term is likely small and can be left out.

summary(flymod3) # R-squared is ~ 0.65
summary(flymod4) # R-squared is 0.65

# It is probably safe to assume that the interaction is not important.
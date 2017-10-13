# Fitting linear mixed effects models: 

# 1. Read the data from the file.
flycatchers <- read.csv("data-raw/flycatcher.csv")

# 2. View the first few lines of data to make sure it was read correctly.
head(flycatchers)

# 3. Create a plot showing the pair of measurements for each individual flycatcher.
# Use a plot that allows you to see the difference between repeat measurements 
# made on the same individual. Is there evidence of measurement variability?

interaction.plot(x.factor = flycatchers$year, trace.factor = flycatchers$bird,
                 response = flycatchers$patch)

# Fitting a linear mixed-effects model: 

# 1. Fit a linear mixed-effects model to the data, treating the individual 
# birds as the random groups.
# Note: The two measurements on each bird were taken in successive years of 
# the study. For simplicity here, do not include “year” as a factor in the 
# model.
# (OK, if you really want to try including “year” as a fixed factor in the 
# model, go ahead. Just make sure to convert year to a factor in R so it 
# is not treated as a numeric variable. Recalculate repeatability with this 
# model as described in steps (2) and (3) below. How is the interpretation 
# of repeatability changed?)

library(nlme)

mod1 <- lme(patch ~ 1, random = ~ 1|bird, data = flycatchers) 

# avoid mydata$y and mydata$B notation

# 2. Extract parameter estimates from the saved lme object (the command is 
# the same one we used with lm to get the coefficients table). 
# Inspect the output for the random effects and for the fixed effect 
# (never mind the AIC BIC etc line, we’ll cover that topic later in 
# the course).

coef(mod1)
summary(mod1)

# 3. In the output, examine the standard deviations (“StdDev”) 
# for the random effects. There should be two standard deviations: 
# one for “(Intercept)” and one for “Residual”. This is because the mixed 
# effects model has two sources of random variation: variation among repeat
# measurements within birds, and true variation among birds in their patch 
# lengths. Which of these two sources corresponds to “(Intercept)” and which
# to “Residual”?

# The true variation among measurements among birds is explained in the 
# (Intercept) std dev. Variation within birds in their patches is
# the residual std dev.


# 4. Also examine the output for the fixed effect results. The only fixed 
# effect in the model formula is the grand mean of all the patch length 
# measurements. It is called “(Intercept)”, but don’t confuse with the 
# intercept for the random effects. The fixed effect output gives you the 
# estimate of the grand mean and a standard error. Notice how the fixed 
# effect output provides estimates of means, whereas the random effects 
# output provides estimates of variances (or standard deviations).

summary(mod1)

# 5. Extract the variance components from the fitted model and estimate the 
# repeatability of patch length from year to year*.

# Repeatability is "the proportion of variance in a character that
# occurs among rather than within individuals". It can be thought of as 
# the correlation between repeated measurements made on the same individuals.

# Repeatability = var(among) / (var(among) + var(within))

VarCorr(mod1)
var.among <- as.numeric(VarCorr(mod1)[1,1])
var.within <- as.numeric(VarCorr(mod1)[2,1])

r <- var.among / (var.among + var.within)
r # 0.776

# 6. Interpret the measure of repeatability obtained in the previous step. 
# If the repeatability you obtained is less than 1.0, what is the source of 
# the variation among measurements within individuals? Is it measurement 
# error alone?

# In this case, 78% of the variation can be attributed to differences between
# (among) different birds. This means that the remaining 22% can be attributed
# to either the year effect or just measurement error. It's impossible to 
# tease these two apart.

# 7. Produce a plot of residuals against fitted values. Notice anything odd?
# There sees to be a slightly positive trend. This isn’t a mistake, but 
# results from “shrinkage” of the best linear unbiased predictors (BLUPs).
# Consult the lecture notes and the Rtips pages (see the repeatability 
# example under “Fit a linear mixed-effects model”) for additional 
# information on what is happening. (Optional: To confirm what is happening, 
# plot the original data in a stripchart and superimpose the lme fitted 
# (predicted) values).

plot(mod1)
          
stripchart(flycatchers$patch ~ flycatchers$year, 
           vertical = TRUE, 
           pch = 1)

stripchart(fitted(mod1) ~ flycatchers$year, 
           vertical = TRUE, 
           add = TRUE, 
           pch = "---",
           cex = 2,
           col = "red")

plot(flycatchers$patch ~ flycatchers$bird)

points(fitted(mod1) ~ flycatchers$bird, 
       col = "red",
       pch = 16,
       cex = 0.75)

# Part 2: Goldfish - Read and examine the data

# 1. Read the data from the file, and view the first few lines to make sure
# it was read correctly.
goldfish <- read.csv("data-raw/goldfish.csv")
head(goldfish)
str(goldfish)

# 2. Use an interaction plot to compare the responses of individual fish 
# across the different light wavelength treatments.
interaction.plot(x.factor = goldfish$wavelength, 
                 trace.factor = goldfish$fish,
                 response = goldfish$sensitivity)

# 3. What type of experimental design was used?* This will determine the 
# linear mixed model to use when fitting the data.

#*It is a “subjects-by-treatment” repeated measures design, since every 
# fish is measured repeatedly, once under each treatment. It would be 
# analyzed the same way as a randomized complete block design (think of 
# the individual fish as “blocks”).


# Fit linear mixed effects model to goldfish data:

# 1. Fit a linear mixed-effects model to the data.

mod2 <- lme(sensitivity ~ wavelength, random = ~ 1|fish, data = goldfish)
summary(mod2)


# 2. Plot the fitted (predicted) values**. A visreg() plot is generally 
# preferred to an interaction.plot(), since it shows the data (although you 
# can overly overlay the data onto an interaction.plot in an extra step). 
# Note that the predicted lines for different fish are parallel — this is 
# because you have fit a model without an interaction term (including an 
# interaction term is not possible with these data because there is only 
# one measurement per fish per wavelength).
library(visreg)
visreg(mod2)

# 3. Compare the predicted values from (2) with the observed data. 
# The difference between the predicted and observed values are the 
# residuals, whose variance is also estimated by lme.
plot(goldfish$sensitivity ~ goldfish$wavelength)
points(fitted(mod2) ~ goldfish$wavelength,
       col = "blue", 
       pch = 16)

# 4. What assumptions are you making in (1)? Create a plot of residuals 
# against fitted values to check one of these assumptions.

# Assumptions: 
# - Variation	within	groups	follows	a	normal	distribution	with	
# equal	variance among	groups.
# - Groups	are	randomly	sampled	from a “population”	of	groups 
# (i.e.,	are	independent	and	sampled	without	bias).
# - Group	effects have a	normal	distribution.
# - Replicates	within	groups	are	also	randomly	sampled	
# (i.e.	independent and	sampled	without	bias).
# - No	carry-over	between	repeated	measurements	on	the	same	subject.
# - Sphericity:	the	variances	of	the	differences	between	all	pairs	of	
# factor	levels	are	equal

plot(mod2) 

# 5. Extract parameter estimates from the saved lme object. Inspect the 
# results for the fixed effects. The coefficients given have the same 
# interpretation as in the case of a categorical variable analyzed using 
# lm (arbitrarily, the group “nm426” is set as the “control”).
summary(mod2)
# Value gives you the difference in intercepts at different wavelengths.
# slope is always 0 in this case. 

# 6. Inspect the output for the random effects. Once again we have two 
# sources of random error in our mixed effects model. 
# What are they? Which of them corresponds to the “(Intercept)” and which 
# to the “Residual” in the output? Notice that the estimated standard 
# deviation for one of the sources of variation is very small.

# Error corresponding to (Intercepts): Variation between fish.
# Error corresponding to Residual: Variation within measurements in each fish
# (within fish - measurement error, effect of wavelength on each fish)


# 7. Generate the model-based estimates of the mean sensitivities for each 
# wavelength.


# 8. Generate the ANOVA table for the lme object. What effects are tested 
# here, the random effects or the fixed effects? Interpret the ANOVA 
# results.
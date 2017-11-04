# Script for DR in Quantitative Methods - Assignment 2
# Debora Obrist

# Read in data from: Are there indirect fitness benefits of female extra-pair reproduction? 
# Lifetime reproductive success of within-pair and extra-pair offspring
# by Sardell, R.J., Arcese, P., Keller, L.F. & Reid, J.M. 
# Published in American Naturalist, 2012
sosp <- read.csv ("data-raw/sosp.csv")

# Look at structure of data:
str (sosp)

# Change epstatus into a factor:
sosp$epstatus <- as.factor (sosp$epstatus)

# Visualize data:

# First, look at LRShatched ~ epstatus:
stripchart (sosp$LRShatched ~ sosp$epstatus,
            vertical = TRUE,
            xlab = "status",
            ylab = "number of offspring hatched",
            pch = 16,
            method = "jitter",
            xaxt = "n")

axis (1, at = 1:2, labels = c("within-pair young", "extra-pair young"))

# Next, look at LRShatched ~ sex:
stripchart (sosp$LRShatched ~ sosp$sex,
            vertical = TRUE,
            xlab = "sex",
            ylab = "number of offspring hatched",
            pch = 16,
            method = "jitter",
            xaxt = "n")

axis (1, at = 1:2, labels = c("female", "male"))

# Next, look at LRShatched ~ year:
stripchart (sosp$LRShatched ~ sosp$natalyr,
            vertical = TRUE,
            xlab = "sex",
            ylab = "number of offspring hatched",
            pch = 16,
            method = "jitter")


# Load required packages: 
library (lme4)
library (visreg)

# First, determine what type of linear model is most appropriate. 

# Response: LRShatched
# Explanatory:
#   Possible fixed: epstatus, sex
#   Possible random: year, natal brood, social parent pair

# What is the probability distribution of the response variable?
hist (sosp$LRShatched)

# The response variable is not normally distributed, so we have to analyze it with a generalized linear mixed-effects model.
# It is count data. It is right-skewed, discrete, and bounded between 0 and infinity. That narrows it down to either Poisson
# or negative-binomial distribution.

# If variance = mean, use Poisson. 
# If variance > mean, use negative-binomial.

var  (sosp$LRShatched) # 30.71
mean (sosp$LRShatched) # 1.77


# Fit model: 

# sospmod <- glmer (sosp$LRShatched ~ sosp$epstatus + as.factor (sosp$natalyr) + sosp$sex + (1 | sosp$parentpairID) + (1|sosp$broodcode), 
#                         family = negative.binomial (link = "log"))


# Apparently, you can't use glmer to fit negative binomially distributed data. 
# I'm going to take it back a step, and for now, ignore the fact that there are both fixed and random effects, 
# that the variance > the mean, and that the data is probably zero-inflated. I will just fit a simple glm with variables
# I'm interested in.

# Fit global model:
sospmod1 <- glm (LRShatched ~ epstatus + natalyr + sex, 
                 family = poisson (link = "log"), 
                 data = sosp)

# See parameter estimates:
coef(sospmod1)

# Visualize fit:
stripchart (sosp$LRShatched ~ sosp$epstatus,
            vertical = TRUE,
            xlab = "status",
            ylab = "number of offspring hatched",
            pch = 16,
            method = "jitter",
            xaxt = "n")

axis (1, at = 1:2, labels = c ("within-pair young", "extra-pair young"))

# Use fitted() to generate the predicted values corresponding to data points on the original scale.
yhat1 <- fitted (sospmod1)

# Plot linear model: Y = number of offspring hatched values predicted by model, X = extra-pair status.
lines (yhat1 [order (sosp$epstatus)] ~ sosp$epstatus [order (sosp$epstatus)])

# Add confidence intervals: 

# First, approximate confidence intervals:
zhat1 <- predict (sospmod1, se.fit = TRUE)       
zupper1 <- zhat1$fit + 1.96 * zhat1$se.fit
zlower1 <- zhat1$fit - 1.96 * zhat1$se.fit

# Convert to original scale:
yupper1 <- exp (zupper1)
ylower1 <- exp (zlower1)

# Add lines to plot:
lines (yupper1 [order(sosp$epstatus)] ~ sosp$epstatus [order (sosp$epstatus)], lty = 2)
lines (ylower1 [order(sosp$epstatus)] ~ sosp$epstatus [order (sosp$epstatus)], lty = 2)

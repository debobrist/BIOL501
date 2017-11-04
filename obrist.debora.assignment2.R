# Script for DR in Quantitative Methods - Assignment 2
# Debora Obrist

# Read in data from: Are there indirect fitness benefits of female extra-pair reproduction? 
# Lifetime reproductive success of within-pair and extra-pair offspring
# by Sardell, R.J., Arcese, P., Keller, L.F. & Reid, J.M. 
# Published in American Naturalist, 2012
sosp <- read.csv ("data-raw/sosp.csv")

# Load required packages: 
library (lme4)
library (visreg)

# Look at structure of data:
str (sosp)

# Change epstatus into a factor:
sosp$epstatus <- as.factor (sosp$epstatus)

# It makes more sense to me to have a binary factor in the form of  0s and 1s, not 1s and 2s.

# Changing from females = 1, males = 2, to females = 0, males = 1.
sosp$sex[sosp$sex == 1] <- 0
sosp$sex[sosp$sex == 2] <- 1

# Visualize data:

# First, look at LRShatched ~ epstatus:
par (bty = "l")

stripchart (sosp$LRShatched ~ sosp$epstatus,
            vertical = TRUE,
            xlab = "status",
            ylab = "number of offspring hatched",
            pch = 16,
            method = "jitter",
            xaxt = "n",
            cex.lab = 1.5,
            cex.axis = 1.25)

axis (1, 
      at = 1:2, 
      labels = c("within-pair", "extra-pair"))

# Next, look at LRShatched ~ sex:
stripchart (sosp$LRShatched ~ sosp$sex,
            vertical = TRUE,
            xlab = "sex",
            ylab = "number of offspring hatched",
            pch = 16,
            method = "jitter",
            xaxt = "n",
            cex.lab = 1.5,
            cex.axis = 1.25)

axis (1, 
      at = 1:2, 
      labels = c("female", "male"))

# First, determine what type of linear model is most appropriate. 

# What happens if you just fit a multiple linear regression model? 

sospmod <- lm (sosp$LRShatched ~ sosp$epstatus + sosp$sex)

# Plot all 4 model diagnostic plots at once:
par (mfrow = c (2, 2))
plot (sospmod)

# Things don't look great. 

# Let's take a closer look at our parameters: 

# Response: LRShatched
# Explanatory:
#   Possible fixed: epstatus, sex
#   Possible random: natalyr, parentpairID, season, broodcode

# Return to regular plotting pane (1 plot at a time):
par (mfrow = c (1, 1))

# What is the probability distribution of the response variable?
hist (sosp$LRShatched,
      main = "",
      xlab = "Number of offspring hatched",
      col = "gray")

# The response variable is not normally distributed, so we have to analyze it with a generalized linear mixed-effects model.
# It is count data. It is right-skewed, discrete, and bounded between 0 and infinity. That narrows it down to either Poisson
# or negative-binomial distribution.

# If variance = mean, use Poisson. 
# If variance > mean, use negative-binomial.

var  (sosp$LRShatched) # 30.71
mean (sosp$LRShatched) # 1.77


# Fit model: 

# sospmod1 <- glmer (sosp$LRShatched ~ sosp$epstatus + sosp$sex + (1 | sosp$parentpairID) + (1|sosp$broodcode) + (1 | as.factor (sosp$natalyr)), 
#                         family = negative.binomial (link = "log"))


# Apparently, you can't use the glmer function to fit negative binomially distributed data. 

# I'm going to take it back a step, and ignore the fact that there is some pseudoreplication, 
# probably a random effect of year, and that the variance > the mean. 
# I will just fit a simple glm with explanatory variables I'm interested in - the extra-pair status, 
# and sex of the bird.


# Fit model with quasipoisson error distribution to see if data overdispersed:
sospmod2 <- glm (LRShatched ~ epstatus + sex, 
                 family = quasipoisson (link = "log"), 
                 data = sosp)

summary (sospmod2)

# Dispersion parameter for quasipoisson family taken to be 16.51793 - definitely overdispersed!

# Is this the best model?

# What if sex isn't important in determining lifetime reproductive success, 
# only epstatus?
sospmod3 <- glm (LRShatched ~ epstatus, 
                 family = quasipoisson (link = "log"), 
                 data = sosp)

# What if epstatus doesn't matter, and sex best explains variations in 
# lifetime reproductive success?
sospmod4 <- glm (LRShatched ~ sex,
                 family = quasipoisson (link = "log"),
                 data = sosp)

# What if neither of these two are important, and the best model to explain 
# lifetime reproductive success is the null model? This model tests the data in 
# relation to the intercept - a straight line for the mean.
sospmod.null <- glm (LRShatched ~ 1,
                 family = quasipoisson (link = "log"),
                 data = sosp)

# Let's compare these models using an AIC framework:
x <- c (AIC (sospmod.null), AIC (sospmod2), AIC (sospmod3), AIC (sospmod4))

# Returns all NA values - apparently you can't calculate likelihood with quasi models,
# and therefore, by definition, you can't calculate AIC. Since the only thing that changes
# in the quasi model vs the regular Poisson is the variance, I'm going to make these same
# 4 models with family = Poisson to see if one is better than the rest. 

# Both explanatory variables:
sospmod2.p <- glm (LRShatched ~ epstatus + sex, 
                 family = poisson (link = "log"), 
                 data = sosp)


# epstatus only:
sospmod3.p <- glm (LRShatched ~ epstatus, 
                 family = poisson (link = "log"), 
                 data = sosp)

# sex only:
sospmod4.p <- glm (LRShatched ~ sex,
                 family = poisson (link = "log"),
                 data = sosp)

# null: 
sospmod.null.p <- glm (LRShatched ~ 1,
                     family = poisson (link = "log"),
                     data = sosp)

# Let's compare these models using an AIC framework:
aic.table <- AIC (sospmod.null.p, sospmod2.p, sospmod3.p, sospmod4.p)

x <- c (AIC (sospmod.null.p), AIC (sospmod2.p), AIC (sospmod3.p), AIC (sospmod4.p))

# Calculate delta AICs:
x <- c (AIC (sospmod.null.p), AIC (sospmod2.p), AIC (sospmod3.p), AIC (sospmod4.p))
delta <- x - min(x)

# Add to table: 

aic.table$delta <- delta

# Relative likelihoods of models:
L <- exp(-0.5 * delta)   

# Add Akaike weights to table:
aic.table$weight <- L/sum(L)

aic.table

# So, sospmod2.p (with both sex and epstatus) has the most weight, so is the best model. 
# It is more than 2 delta AIC units better than the next top model, 
# sospmod3.p (epstatus only)

# I'm now going to go back to using sospmod2 - the version of the full model with 
# quasipoisson distribution. Again, this model essentially the same as the one I just
# determined to be the best, but more conservative with it's error estimates because of the
# overdispersed data.

# See parameter estimates on log-scale:
coef(sospmod2)

# See back-transformed parameter estimates:
exp(coef(sospmod2))

# What do these parameter estimates mean, biologically? 

# If epstatus = 0, and sex = 0, how many offspring? (this is just intercept)
# Female, within-pair
exp(coef (sospmod2)[1] + coef (sospmod2)[2] * 0 + coef (sospmod2)[3] * 0)

# If epstatus = 1, and sex = 0, how many offspring? 
# Female, extra-pair
exp(coef (sospmod2)[1] + coef (sospmod2)[2] * 1 + coef (sospmod2)[3] * 0)

# If epstatus = 1, and sex = 1, how many offspring? 
# Male, extra-pair
exp(coef (sospmod2)[1] + coef (sospmod2)[2] * 1 + coef (sospmod2)[3] * 1)

# If epstatus = 0, and sex = 1, how many offspring? 
# Male, within-pair
exp(coef (sospmod2)[1] + coef (sospmod2)[2] * 0 + coef (sospmod2)[3] * 1)


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
yhat1 <- fitted (sospmod2)

# Plot linear model: Y = number of offspring hatched values predicted by model, X = extra-pair status.
lines (yhat1 [order (sosp$epstatus)] ~ sosp$epstatus [order (sosp$epstatus)])

# Add confidence intervals: 

# First, approximate confidence intervals:
zhat1 <- predict (sospmod2, se.fit = TRUE)       
zupper1 <- zhat1$fit + 1.96 * zhat1$se.fit
zlower1 <- zhat1$fit - 1.96 * zhat1$se.fit

# Convert to original scale:
yupper1 <- exp (zupper1)
ylower1 <- exp (zlower1)

# Add lines to plot:
lines (yupper1 [order(sosp$epstatus)] ~ sosp$epstatus [order (sosp$epstatus)], lty = 2)
lines (ylower1 [order(sosp$epstatus)] ~ sosp$epstatus [order (sosp$epstatus)], lty = 2)



# 1. View the data and examine it closely. The data set is not large, but it has aspects in 
# common with larger compilations used in meta-analyses, and raises many of the same questions. 
# First, there more entries than published studies. Repeated entries from the same study 
# represent different nearby populations, or measurements taken in different years from the same 
# population. Can the 15 effect sizes in the table therefore be considered independent? 
# Should we average all the values from the same population, or from the same study, before 
# continuing? Welcome to meta-analysis.

library(SDMTools)
library(meta)
library(dplyr)

sparrows <- read.csv ("data-raw/sparrow.csv")

# 2. For the purposes of this exercise, let’s treat the 15 effect sizes as though they are 
# independent and belong to a single fixed class. I hope this does not shock you.

# 3. Create a simple scatter or “funnel” plot depicting the relationship between effect size 
# and sample size for the house sparrow data. Include a dashed line in the plot for r = 0. 
# What pattern would you expect to see if there was publication bias? Is such a pattern evident 
# in your plot? 
names(sparrows)

plot (r ~ n,
      data = sparrows,
      pch = 16)

# Statistical analysis of correlations begins by converting r to the Fisher’s z-scale.
# The reason is that on the z-scale, the sampling distribution for the correlation is 
# approximately normal and the standard error is independent of z. The transformation is
# z = 0.5 ln((1 + r)/(1 – r)), or equivalently, z = tanh-1(r)

z <- 0.5 * log ((1 + sparrows$r)/(1 - sparrows$r))
sparrows <- mutate (sparrows, Fisher.z = z)

# 4. A convenient feature of the Fisher z is that the approximate standard error depends only
# on the sample size n (i.e., number of birds) used in each study,
# SEz = 1/√(n – 3).

SE.z <- 1 / sqrt (sparrows$n - 3)
sparrows <- mutate (sparrows, SE.z = SE.z)

# Fixed effects: 

# 1. To estimate the mean effect size, we will weight each correlation according to the 
# magnitude of its sampling variance, which takes into account the different sample sizes of 
# the studies. Each weight is the inverse of the squared standard error. Calculate the weights
# for the sparrow data. Notice how the weights vary with sample size.

weight <- 1 / ((SE.z) ^ 2)
sparrows <- mutate (sparrows, weight = weight)

# 2. Fit the model by calculating the weighted mean, z, of the z-transformed correlations. 
# R will calculate a weighted mean if you ask it. The result is your estimate of the true
# mean effect size. In what way does it differ from the unweighted mean of the z-transformed 
# correlations?

w.mean <- weighted.mean (x = sparrows$Fisher.z, w = sparrows$weight)

# 3. Calculate the standard error of the weighted mean. This standard error measures 
# uncertainty of the estimate of true effect size.

SE.w.mean <- sqrt (1 / sum (sparrows$weight))

# 4. Calculate an approximate 95% confidence interval for the mean of the transformed effect
# sizes using the normal approximation.

qnorm(1 - 0.05/2) # How to obtain the critical value.

lower.ci <-  w.mean - 1.96 * SE.w.mean
upper.ci <-  w.mean + 1.96 * SE.w.mean
c (lower.ci, upper.ci)

# 5. Convert your estimated mean effect size from (2) back to the untransformed correlation, 
# to obtain the mean effect size r. This requires back-transforming,*r = tanh(z) or,
# equivalently, r = (e2z – 1)/(e2z + 1).

r <- tanh (w.mean) # 0.4617

# 6. Add a horizontal line indicating the mean effect size to your funnel plot created in the
# previous section.

plot (r ~ n,
      data = sparrows,
      pch = 16)
abline (h = r,
        col = "black")

# 7. Apply the same back-transformation to the lower and upper limits of your confidence 
# interval in (4) to yield the 95% confidence interval for the mean correlation coefficient.

lower.ci.r <- tanh (lower.ci)
upper.ci.r <- tanh (upper.ci)

c (lower.ci.r, upper.ci.r)

abline (h = lower.ci.r,
        lty = 2,
        col = "grey")

abline (h = upper.ci.r, 
        lty = 2, 
        col = "grey")


# Random effects; 

# 1. To fit the random effects model we need to estimate the variance among the 
# system-specific effect sizes, Τ2 (“tau squared”). One way to estimate it involves 
# calculating the heterogeneity among the observed effect sizes (Q), and then “correcting” 
# by subtracting the within-study sampling variance. The correction is needed because the 
# variance among the observed effect sizes among studies is inflated by within-study sampling 
# error. To begin, calculate Q, the weighted heterogeneity among the observed z values.

Q <- sum (sparrows$weight * (sparrows$Fisher.z - w.mean) ^ 2)

# 2. Then estimate Τ2 by subtraction, being careful not to allow a negative value (since Τ2 
# is a variance, which can’t be negative).*

N <- 15 # we are assuming independent studies, even though this isn't true.

t2 <- (Q - (N - 1)) / (sum (sparrows$weight) - sum (sparrows$weight ^ 2) / sum (sparrows$weight))

# 3. Using Τ2, calculate new weights for the effect sizes of each study under the random effects model. 
# Examine these new weights w’ and compare them to the weights w under the fixed effects model. How are they different?
# Is as much weight given to large-sample studies, relative to small-sample studies, in the random effects model as in 
# the fixed effects model?

sparrows <- mutate (sparrows, new.weight = 1 / ((sparrows$SE.z) ^ 2) + t2)

# 4. Calculate the weighted mean effect size z under the random effects model. The procedure is the same as that used before for the fixed effects model except that here we will use the new weights w’ calculated in the previous step. Back-transform to get the estimated mean correlation r.** Add the estimated mean correlation to your funnel plot. Compare your result to the effect size estimated under the fixed effects model. Is it the same?
# 5. Calculate the standard error (SE) of the mean z. The formula is the same as that in the fixed-effects model except that here we will use the new weights.
# 6. Calculate the 95% confidence interval for the mean effect size under the random effects model. Is the confidence interval narrower, about the same, or wider than that calculated under the fixed effects model? Why?# 7. Finally, back-transform to get the lower and upper limits of the 95% confidence interval for the mean correlation r. ***
# 7. Optional: make a function. Now that you’ve done the hard work of programming R to calculate effect sizes for random and fixed effects models, why don’t you try to make a function that would carry it out on any set of data? There are a few notes on how to write a function on the “Vector” tab of the R tips page.
# 8. Optional: Use the meta package to repeat these meta-analysis calculations and draw forest and funnel plots. See the “Meta-analysis” tab at the R tips web site for help.

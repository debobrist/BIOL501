# https://www.zoology.ubc.ca/~bio501/R/workshops/workshops-likelihood/

# Log-likelihood ratio test. "reduced" = null hypothesis. 
G <- 2 * (loglikefull - loglikereduced)

# G is also known as the deviance. Under null, G has chi-sq dist.

#### Warmup ####

# We'll start by getting familiar with the commands in R to 
# calculate probabilities.

# 1. The probability of heads in a coin toss is 0.5. If you flip
# a coin 10 times, what is the probability of obtaining exactly 
# 5 heads and 5 tails?

dbinom (5, size = 10, prob = 0.5) # 0.246


# 2. The fraction of human babies born who are boys is about 0.512.
# If 20 newborn babies are randomly sampled, what is the 
# probability that exactly 10 are boys?

dbinom (10, size = 20, prob = 0.51) # 0.175


# 3. Plot the entire probability distribution for the number of 
# boys in families having six children. Assume the probability 
# that any one child is a boy is 0.512.

x <- dbinom(c(0:6), 
            size = 6, 
            prob = 0.512)

plot(x ~ c(0:6),
     type = "l")

# 4. If mortality is independent of age, then the probability of surviving X 
# years after birth, and then dying in the X + 1st year, will follow a 
# geometric distribution. X is any integer from 0 to infinity. If the 
# probability of dying in any given year is 0.1, what fraction of individuals 
# are expected to survive 10 years and then die in their 11th year?*

dgeom (10, prob = 0.1) # This means that they lived 10 years, and died in the 11th year. 0.03487

# 5. Refer to the previous question. If the probability of death in any give year is
# 0.1, what fraction of individuals die before they reach their 6th birthday?**

dgeom (5, prob = 0.1) + 
  dgeom (4, prob = 0.1) + 
  dgeom (3, prob = 0.1) + 
  dgeom (2, prob = 0.1) + 
  dgeom (1, prob = 0.1) + 
  dgeom (0, prob = 0.1)

# 0.468559

# 6. In an environment where prey are randomly distributed, the search time between 
# discovered prey items will follow an exponential distribution. Imagine an environment 
# in which the mean search time between prey items is 0.5 hours. What is the probability 
# density corresponding to a search time of 2 hours?***

x1 <- dexp (2, rate = 1 / 0.5)

# 7. Refer to the previous problem. Create a line plot of the exponential probability 
# curve over most the range of possible values for search time between items (e.g., over 
# the range of values between 0 and 5 hours).

# For this, the area under the curve is the probability of capturing a prey item within
# search time (x).

x2 <- dexp (seq (0, 5, 0.1), rate = 1 / 0.5)
plot(x2 ~ seq (0, 5, 0.1), 
     type = "l",
     ylab = "Probability Density", 
     xlab = "Search Time")

# Y is probability of finding a prey item within time (x).
x3 <- pexp (seq (0, 5, 0.1), rate = 1 / 0.5)
plot(x3 ~ seq (0, 5, 0.1), 
     type = "l",
     ylab = "Probability", 
     xlab = "Search Time")

#### Illegal Tender ####

# 1. Generate a vector that includes a range of possible values 
# for the population proportion p, from 0.01 to 0.99 in increments of 0.01.

p <- seq (0.01, 0.99, 0.01)

# 2. Given the dollar bill data above, calculate the log-likelihood of each 
# value for p.

logLik <- dbinom (46, size = 50, p = p, log = TRUE)
ill.tender <- data.frame(Proportion = p, Loglikelihood = logLik)

# 3. Create a line plot of the log-likelihood against the range of values for p. 
# What is the resulting curve called? Can you see approximately the value of p 
# corresponding to the highest point of the curve? What is this value called?

plot(ill.tender$Loglikelihood ~ ill.tender$Proportion, 
     type = "l")

# This is a log-likelihood curve 

# p value with highest likelihood is 0.92. This is the MLE.
ill.tender$Proportion[ill.tender$Loglikelihood == max(ill.tender$Loglikelihood)]

# 4. To get closer to this value, repeat steps (1) to (3) using a narrower range 
# of values for p surrounding the highest point in the curve.

p1 <- seq (0.80, 0.99, 0.001)
logLik <- dbinom (46, size = 50, p = p1, log = TRUE)
ill.tender <- data.frame(Proportion = p1, Loglikelihood = logLik)
plot(ill.tender$Loglikelihood ~ ill.tender$Proportion, 
     type = "l")

# 5. Use your results to determine the maximum likelihood estimate of the proportion
# of US 1-dollar bills contaminated with cocaine.

pHat <- ill.tender$Proportion[ill.tender$Loglikelihood == max(ill.tender$Loglikelihood)]
# 0.92

# 6. Provide a likelihood-based 95% confidence interval for the population proportion.*
#   *0.823 < p < 0.975

lower <- max (p1 [p1 <= pHat & logLik <= max (logLik) - 1.92] )
upper <- min (p1 [p1 >= pHat & logLik <= max (logLik) - 1.92] )
c (lower = lower, upper = upper)

# 0.823 < p < 0.975

#### Counting Elephants ####

# Using five genetic markers, they generated a unique DNA fingerprint for 
# every elephant encountered in this way. Over the first seven days of collecting 
# they identified 27 elephant individuals. Refer to these 27 elephants as marked. 
# Over the next eight days they sampled 74 individuals, of which 15 had been previously
# marked. Refer to these 15 elephants as recaptured. We would like to use these numbers 
# to estimate the total number of elephants in the park.

# The number of recaptured (i.e., previously marked) individuals X in the second sample 
# should have a hypergeometric distribution with parameters k (the size of the second
# sample of individuals), m (total number of marked individuals in the population), 
# and n (total number of unmarked individuals in the population).

# 1. Using the appropriate command in R for the hypergeometric distribution, 
# calculate the maximum likelihood estimate for the total number of elephants in the park. 
# Note that the total number is n + m, where n is the unknown parameter to be estimated.
# Note also that only integer values for n are allowed, and that n cannot be smaller 
# than k - X, the observed number of unmarked individuals in the second sample.*

elephant <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter20/chap20e4ConservationScoop.csv"))
head(elephant)

# Quantities needed for the likelihood calculations.
m <-  27     # size of first sample (total marked individuals)
n2 <- 74     # size of second sample
Y <-  15     # number of recaptures in second sample

# Choose a range of values of N to try. N is the (unknown) total number of individuals 
# in the population. N must be at least n2 + m - Y (86, in the current example).

N <-  86:250 # The values of N to try

# Log-likelihoods! 
like <- dhyper(Y, m, N - m, n2)
logLike <- dhyper(Y, m, N - m, n2, log = TRUE)
logLike

# Estimate of number of elephants in park:
Nhat <- N[logLike == max(logLike)]
Nhat # 133 elephants

plot(logLike ~ N, lwd = 2, las = 1, type = "l", bty = "l", 
     xlab = "Population size estimate, N", ylab = "Log-likelihood")
abline(logLike[N == Nhat], 0, lty = 2)

# 2. Calculate a likelihood-based 95% confidence interval for the total number of 
# elephants.**  

lower <- max(N[N <= Nhat & logLike <= max(logLike) - 1.92])
upper <- min(N[N >= Nhat & logLike <= max(logLike) - 1.92])
c(lower = lower, upper = upper)

# 104 to 193


#### Left-handed flowers #### 

# 1. Calculate the log-likelihood curve and the maximum-likelihood estimate 
# of the proportion of left-handed flowers (to the nearest hundredth). Use the data 
# in the data frame, rather than summaries of the frequencies of left- and right-handed 
# flowers, to calculate the likelihoods (practice with this approach will help you 
# later below).

flowers <- read.csv("data-raw/plantain.csv")

nrow(flowers[flowers$hand == "left", ]) # 6
summary(flowers$hand) 

# Expected MLE: 6/27

loglik.flowers <- dbinom (6, 27, p = p, log = TRUE)

flowers2 <- data.frame(Proportion = p, Loglikelihood = loglik.flowers)

plot(flowers2$Loglikelihood ~ flowers2$Proportion,
     type = "l")

pHat2 <- flowers2$Proportion[flowers2$Loglikelihood == max(flowers2$Loglikelihood)]

pHat2

#0.22

# 2. Use the same approach to calculate the likelihood-based 95% confidence interval 
# of the population proportion.

lower <- max (p [p <= pHat2 & loglik.flowers <= max (loglik.flowers) - 1.92] )
upper <- min (p [p >= pHat2 & loglik.flowers <= max (loglik.flowers) - 1.92] )
c (lower = lower, upper = upper)

# 0.09 < p < 0.40

# 3. We can compare the fits of two models to these same data, to test the null 
# hypothesis that the proportion of left-handed flowers in the cross is 1/4 (i.e., 
# the proportion predicted by the simplest genetic model for flower handedness). 
# To begin, obtain the log-likelihood corresponding to the maximum likelihood 
# estimate of the proportion of left-handed flowers. This represents the fit of 
# the "full" model to the data. This model estimated one parameter from the data 
# (p, estimated using maximum likelihood).

mle.flowers <- flowers2$Loglikelihood[flowers2$Loglikelihood == max(flowers2$Loglikelihood)]

# 4. Now obtain the log-likelihood of the value for p specified by the null hypothesis. 
# This represents the fit of the "reduced" model to the data. This reduced model 
# estimated zero parameters from the data (instead, p was specified by the null
# hypothesis).

loglik.null <- dbinom (6, 27, p = 0.25, log = TRUE)
loglik.flowers <- dbinom (6, 27, p = 0.22, log = TRUE)

# 5. Calculate the G statistic for the log-likelihood ratio test*. To obtain a P-value 
# for the test, calculate the tail probability from the χ2 distribution as follows,

G <- 2 * (loglik.flowers - loglik.null)

1 - pchisq(G, 1) 

# where df is the degrees of freedom, calculated as the difference between the
# two models in the number of parameters estimated from the data.

# 6. Optional: How similar is the result from your log-likelihood ratio test to that 
# from an ordinary χ2 goodness of fit test? Analyze the same data using the chisq.test 
# command in R and comment on the outcome.

?chisq.test


# Workshop 9 - Bayesian Methods

# 1. Using the hypergeometric distribution, calculate the likelihood of each of a range of 
# possible values for the number of elephants in the Park. Note that the total number of 
# elephants is n + m, and that m is known (m = 27). Note also that only integer values for
# n are allowed, and that n cannot be smaller than k – X, the observed number of unmarked 
# individuals in the second sample.

elephant <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter20/chap20e4ConservationScoop.csv"))
head(elephant)

# Quantities needed for the likelihood calculations.
m <-  27     # size of first sample (total marked individuals)
k <- 74     # size of second sample
X <-  15     # number of recaptures in second sample
Y <- k - X # min number of of elephants in population 
N <-  86:250 # Possible population size.

# Log-likelihoods! 
like <- dhyper(X, m, N - m, k)
logLike <- dhyper(X, m, N - m, k, log = TRUE)

# 2. Find the value of n that maximizes the likelihood. Add m to this number to obtain 
# the maximum likelihood estimate for population size.*

# Estimate of number of elephants in park:
Nhat <- N[logLike == max(logLike)]
Nhat # 133 elephants

plot(logLike ~ N, lwd = 2, las = 1, type = "l", bty = "l", 
     xlab = "Population size estimate, N", ylab = "Log-likelihood")
abline(logLike[N == Nhat], 0, lty = 2)

# 3. Calculate the likelihood-based 95% confidence interval for the total number of 
# elephants.**

lower <- max(N[N <= Nhat & logLike <= max(logLike) - 1.92])
upper <- min(N[N >= Nhat & logLike <= max(logLike) - 1.92])
c(lower = lower, upper = upper)

# 4. Decide on a minimum and maximum possible value for n, the number of unmarked 
# elephants in the Park (or n + m, the total number, if that’s how you’re calculating 
# the likelihoods). The minimum n can be as small as 0 but the likelihood will be 0 for 
# values smaller than k – X, so it is practical to set the smallest n to something greater
# or equal to k – X. For this first exercise don’t set the maximum too high. We’ll 
# explore what happens later when we set the maximum to be a very large number.

# Min: 59
# Max: 150

# 5. Create a vector of n values (or n + m values) containing all the integers between
# and including the minimum and maximum values that you decided in (4).

n.ellies <- seq(60, 400, by = 1)

# 6. Calculate the likelihoods of each of these values for n. Plot the likelihoods 
# against n. (We need the likelihoods rather than the log-likelihoods for the posterior
# probability calculations.)

Lik2 <- dhyper(X, m, n.ellies, k)
plot (Lik2 ~ n.ellies)

# 7. Create a vector containing the prior probabilities for each of the possible values 
# for n that you included in your vector in (5). If you are using a flat prior then the 
# vector will be the same length as your n vector, and each element will be the same 
# constant. Plot the prior probabilities against n. If all is OK at this stage then the 
# plot should show a flat line. Also, confirm that the prior probabilities sum to 1.

flat.prior <- rep((1/(length(ellies))), length(ellies))

plot(flat.prior ~ ellies)

sum(flat.prior) # Sum = 1.

# 8. Using your two vectors from (6) and (7), calculate the posterior probabilities of 
# all the possible values of n (or of n + m) between your minimum and maximum values. 
# After your calculation, confirm that the posterior probabilities sum to 1. Plot the 
# posterior probabilities against n (or n + m). Compare with the shape of the likelihood
# curve.

# post <- prior * likelihood / p(data)
post <- (Lik2 * flat.prior) / sum(flat.prior * Lik2)

plot (Lik2 ~ n.ellies)

points (flat.prior ~ n.ellies,
        pch = 16, 
        col = "green")

points (post ~ n.ellies,
      pch = 16,
      cex = 0.5,
      col = "blue")

sum(post)

# 9. What is the most probable value of n + m, given the data? Compare this with your 
# previous maximum likelihood estimate.*
Nhat2 <- n.ellies [post == max (post)]
Nhat2 + 27 # 133 elephants

# 10. Calculate the 95% credible interval for n (or n + m, the total population size).** 
# The procedure for finding the lower and upper limits of the credible interval is a bit 
# like that for likelihood. The idea is illustrated in the figure below. Starting from the
# highest point of the posterior probability, slide a horizontal line downward until you 
# reach a point at which the corresponding values for the parameter (indicated below by 
# the dashed vertical lines) bracket an area of 0.95 under the curve.

plot (post ~ n.ellies,
      pch = 16, 
      cex = 0.5)

# First, order the posterior probabilities from highest to lowest
post.ordered <- post [order (post, decreasing = TRUE)]

# Remember to order the corresponding n values the same way
n.ordered <- n.ellies [order (post, decreasing=TRUE)]

# Obtain the cumulative sum of the posterior probabilities from lowest to highest
post.cumsum <- cumsum (post.ordered)

# Finally, find n corresponding to a cumulative posterior probability of 0.95. 
range (n.ordered [post.cumsum <= 0.95]) + m

# 11. Compare your 95% credible interval for population size with the approximate 
# likelihood-based 95% confidence interval. Which interval is narrower? Also compare the 
# interpretations of the two intervals. How are they different? Are they compatible? 
# Which makes the most sense to you? Why?

# 12. Repeat the procedures (4)-(10) but using a much larger value for the maximum 
# possible population size. How is your credible interval affected by the increase in 
# the maximum value of the posterior probability distribution?

n.ellies2 <- seq(60, 100000, by = 1)

Lik3 <- dhyper(X, m, n.ellies2, k)

flat.prior2 <- rep((1/(length(n.ellies2))), length(n.ellies2))

post2 <- (Lik3 * flat.prior2) / sum(flat.prior2 * Lik3)

Nhat3 <- n.ellies2 [post2 == max (post2)]

Nhat3 + 27

# Credible intervals:
post.ordered2 <- post2 [order (post2, decreasing = TRUE)]

# Remember to order the corresponding n values the same way
n.ordered2 <- n.ellies2 [order (post2, decreasing=TRUE)]

# Obtain the cumulative sum of the posterior probabilities from lowest to highest
post.cumsum2 <- cumsum (post.ordered2)

# Finally, find n corresponding to a cumulative posterior probability of 0.95. 
range (n.ordered2 [post.cumsum2 <= 0.95]) + m



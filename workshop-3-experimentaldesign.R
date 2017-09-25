# Random sampling warm-up
# To begin, get some practice sampling categorical and normally-distributed data from a 
# population. Consult the “Simulate data” section of the “Plan” tab on the R tips 
# pages for help.


# 1. Sample 20 observations from a population having two groups of individuals, 
# “infected” and “uninfected”, in equal proportions. 
# Use a table to show the frequencies of the two categories obtained.

pop <- sample(c("infected","uninfected"), size=20, replace=TRUE, prob=c(.5, .5))
table(pop)

# 2. Repeat the previous step five times to convince yourself that the outcome 
# varies from sample to sample.

pop <- sample(c("infected","uninfected"), size=20, replace=TRUE, prob=c(.5, .5))
table(pop)

pop <- sample(c("infected","uninfected"), size=20, replace=TRUE, prob=c(.5, .5))
table(pop)

pop <- sample(c("infected","uninfected"), size=20, replace=TRUE, prob=c(.5, .5))
table(pop)

pop <- sample(c("infected","uninfected"), size=20, replace=TRUE, prob=c(.5, .5))
table(pop)

pop <- sample(c("infected","uninfected"), size=20, replace=TRUE, prob=c(.5, .5))
table(pop)

# 3. Sample 18 individuals from a population having two groups of individuals, 
# “mated” and “unmated”, where the proportion mated in the population is 0.7. 
# Summarise the frequencies in a table.

pop2 <- sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7, .3))
table(pop2)

# 4. Repeat the previous step five times to convince yourself that the outcome 
# varies from sample to sample.

pop2 <- sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7, .3))
table(pop2)

pop2 <- sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7, .3))
table(pop2)

pop2 <- sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7, .3))
table(pop2)

pop2 <- sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7, .3))
table(pop2)

pop2 <- sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7, .3))
table(pop2)

# 5. Sample 30 observations from a normally-distributed population having mean 0 
# and standard deviation 2. Plot the results in a histogram.

data <- rnorm(30, mean = 0, sd = 2)
hist(data)

# 6. Repeat the following 5 times and calculate the mean each time: sample 30 
# observations from a normally-distributed population having mean 0 and standard 
# deviation 2. Convince yourself that the sample mean is different each time.

data2 <- rnorm(30, mean = 0, sd = 2)
mean(data2)

data2 <- rnorm(30, mean = 0, sd = 2)
mean(data2)

data2 <- rnorm(30, mean = 0, sd = 2)
mean(data2)

data2 <- rnorm(30, mean = 0, sd = 2)
mean(data2)

data2 <- rnorm(30, mean = 0, sd = 2)
mean(data2)

#### Part 2 ####

# 1. Randomly sample n = 10 females from a population having equal numbers of “successes”
# (females who choose males of her own species) and “failures” (females who choose 
# males of the other species). 
# What was the proportion of successes in your sample?

spiders <- sample(c("same_sp","dif_sp"), size=10, replace=TRUE, prob=c(.5, .5))
table(spiders)

# 2. Using the data from step 1, calculate an approximate 95% confidence interval for 
# the population proportion of successes. Use the Agresti-Coull method in the 
# binom package in R, which you will need to install if you haven’t already done so.

install.packages("binom", dependencies = TRUE) # install once only
library(binom)                                 # load before using

# To obtain the 95% confidence interval, use the binom.confint function explained below. The argument x is the number of “successes” in your generated sample (number of females who chose males of her own species) and n is the sample size (number of females tested).

myCI <- binom.confint(length(spiders[spiders == "same_sp"]), 
                      length(spiders), method = "ac")  # gets the confidence interval
print(myCI)                                 # shows the results
myCI$lower                                  # 0.167
myCI$upper                                  # 0.688

# Repeat steps 1 and 2 five times and keep a record of the confidence intervals 
# you obtained. What was the lowest value for the span of the confidence interval 
# from the 5 samples?

spiders1 <- sample(c("same_sp","dif_sp"), size=10, replace=TRUE, prob=c(.5, .5))
myCI1 <- binom.confint(length(spiders1[spiders1 == "same_sp"]), 
                      length(spiders1), method = "ac")  # gets the confidence interval

print(myCI1)                                 # shows the results
myCI1$lower                                  # -0.004
myCI1$upper                                  # 0.42

# 4. You can speed up the effort if you create a for loop in R that automatically repeats 
# steps 1 and 2 as many times as you decide. See the "Loop" tab on the R tips page. 
# A loop that repeats ten times would look something like the following. The "i" in this 
# loop is a counter, starting at 1 and increasing by 1 each time the commands in the loop 
# are executed. Don't forget to include a command inside the loop to print each result.


for(i in 1:10){
  t <- sample(c("same_sp","dif_sp"), size=10, replace=TRUE, prob=c(.5, .5))
  myCI <- binom.confint(length(t[t == "same_sp"]), length(t), method = "ac")
  print(myCI)
  print(myCI$lower)
  print(myCI$upper)
  print(myCI$upper - myCI$lower)
}

# 5. Increase the sample size to n = 20 and run the loop from step 4 again.
# How much narrower are the confidence interval spans? Are the spans adequate?

for(i in 1:10){
  t <- sample(c("same_sp","dif_sp"), size=20, replace=TRUE, prob=c(.5, .5))
  myCI <- binom.confint(length(t[t == "same_sp"]), length(t), method = "ac")
  print(myCI)
  print(myCI$lower)
  print(myCI$upper)
  print(myCI$upper - myCI$lower)
}

# 6. By modifying the sample size and re-running the loop a bunch of times, 
# find a sample size (ballpark, no need to be exact at this point) that usually 
# produces a confidence interval having a span no greater than 0.2. 
# This would be the span of a confidence interval that had, e.g., a lower limit 
# of 0.4 and an upper limit of 0.6. Surely this would be convincing evidence that 
# the mate preference really was weak.

t <- rep(0, 10)

for(i in 1:10){
  spiders <- sample(c("same_sp","dif_sp"), size=95, replace=TRUE, prob=c(.5, .5))
  myCI <- binom.confint(length(spiders[spiders == "same_sp"]), length(spiders), method = "ac")
  t[i] <- (myCI$upper - myCI$lower)
}
t


# 7. Given the results of step 6, you would now have some design options before you. 
# Is the sample size n that your simulation indicated was needed to generate a 
# confidence interval of span 0.2 realistic? In other words, would an experiment with so
# many female spiders (and so many males) be feasible? 
# If the answer is yes, great, get started on your experiment! 
# If the answer is no, the sample size required is unrealistically large, 
# then you have some decisions to make:

# Forget all about doing the experiment. (Consider a thesis based on theory instead.)
# Revise your concept of what represents a "narrow" confidence interval. Maybe a confidence interval for p spanning, say, 0.3 to 0.7 (a span of 0.4) would be good enough to allow you to conclude that the preference was "not strong". This would not require as big a sample size as a narrower interval.
# Repeat the above procedures to find a sample size that usually gives a confidence interval having a span of 0.4 or less.

t <- rep(0, 10)

for(i in 1:10){
  spiders <- sample(c("same_sp","dif_sp"), 
                    size=20, 
                    replace=TRUE, 
                    prob=c(.5, .5))
  myCI <- binom.confint(length(spiders[spiders == "same_sp"]), 
                        length(spiders), 
                        method = "ac")
  t[i] <- (myCI$upper - myCI$lower)
}
t

# You only need ~ 20 female spiders to give confidence intervals of less than 0.4.
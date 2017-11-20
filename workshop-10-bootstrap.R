# Bootstrap estimation

# The bootstrap is a computer-intensive method used to calculate standard errors for an 
# estimate and to obtain approximate confidence intervals for a population parameter. 
# It works well in most situations except when sample size is small. The basic procedure is:
  
# 1. Take a random sample of the data, sampling with replacement
# 2. Compute the statistic on the bootstrap replicate obtained in (1)
# 3. Repeat steps (1) and (2) many times (e.g., B = 10000), saving the result each time
# 4. Plot the B bootstrap replicate estimates to visualize the shape of the sampling 
# distribution
# 5. Calculate the standard deviation of the B bootstrap replicate estimates to obtain the 
# standard error of the estimate

antilles <- read.csv ("data-raw/antilles.csv")

# 1. What shape is the frequency distribution of estimated immigration dates? Use a graph 
# to display it.

hist (antilles$immigration.date)

# 2. What are the mean and median dates of immigration, in millions of years? Why are the
# two values so different?

mean (antilles$immigration.date) # 8.66
median (antilles$immigration.date) # 3.5

# because the data is zero - inflated / right skewed.

# 3. Obtain a single bootstrap replicate of the immigration dates and plot the results. 
# How different is the frequency distribution from that of the data?

antilles.boot <- sample (antilles$immigration.date, replace = TRUE)

hist (antilles.boot, breaks = 10)

mean(antilles.boot)
median(antilles.boot)

# It is different.

# 4. Write a short loop to generate 10000 bootstrap replicate estimates for the sample 
# median immigration date. It is a good idea to begin by using only 10 replicates until 
# your loop is tested and found to be working smoothly. Store the resulting medians in a 
# vector.

x.stats <- data.frame ("median" = rep (0,10000))

for (i in 1:nrow (x.stats)){
  x.boot <- sample (antilles$immigration.date, replace = TRUE)
  x.stats$median [i] <- median(x.boot)
}

# 5. Plot the frequency distribution of your results from (4). What does this frequency
# distribution estimate?

hist (x.stats$median)

median (x.stats$median)
mean (x.stats$median)

# 6. Using your results in (4) to calculate a standard error for the sample median*.

sd (x.stats$median)

# 7. Most of the familiar estimators of population parameters, such as sample mean and 
# variance, are unbiased, which means that the mean of its sampling distribution equals 
# the parameter value being estimated. For example, the mean of the sampling distribution 
# of the sample mean is μ, the very parameter being estimated. The sample mean is an
# unbiased estimator. However, some estimators are biased, and the bootstrap is often used 
# to estimate bias. Is the median of immigration dates biased? Calculate the mean of the 
# bootstrap replicate estimates of the median immigration date to estimate the bias**.

mean (x.stats$median) # 4.48
median (antilles$immigration.date) # 3.5

# Because the mean of bootstrapped medians is more than the median,
# we can see that the bootstrap tends to overestimate the true median.


# 8. Use the percentile method (check the quantile() function) to generate an approximate 
# 95% confidence for the median***.

CI <- quantile (x.stats$median, c(0.025, 0.975))

CI # 1.8 to 11.1


# 9. Apply the boot package to generate a more accurate bootstrap confidence interval for 
# the median using the BCa method.****

install.packages ("boot")
library (boot)

boot.median <- function (x, i){
  boot.median <- median (x [i])
}

z <- boot (antilles$immigration.date, boot.median, R = 2000)

boot.ci (z, type = "bca")
boot.ci (z, type = "perc")


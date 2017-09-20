library(plyr)
mammals <- read.csv("data-raw/mammals.csv", na.strings="")

# Carry out the following inspections of the data
# 
# Use the head function to view the first few lines of the data frame on the screen. 
# You’ll see that every row represents the data for a different mammal species.

head(mammals)

# For the two most interesting character variables, tabulate the frequency of 
# cases in each group (remember that the category “NA” in continent stands for 
# North America, not missing data).
levels(mammals$continent)[1]<-"AF"

count(mammals, "continent")
count(mammals, "status")

# can also use table (mammals$continent), but gives output in different form.


# You’ll notice in the frequency table for the variable “continent” that there’s a 
# typo in the data. One case is shown as having the continent “Af” rather than “AF”. 
# Fix this using the command line in R.

# Fixing this before assigning continents: levels(mammals$continent)[1]<-"AF"


# Create a new variable in the mammal data frame: the log (base 10) of body mass. 
# (See “Work with data frames” on the R Tips “Data frame” page if you need help with this.)

mammals <- mutate(mammals, log.mass.grams = log10(mass.grams))



#### Visualizing frequency distributions ####

# 1. Which continent has the greatest number of mammal species? Which has the least? 
# Use a simple bar graph to find out.

barplot((count(mammals, "continent")$freq), 
        names = continents$continent, 
        las = 2)

# 2. Redo the bar graph using the cex.names option to control the size
# of the category names (this might be needed to ensure that there is 
# room for them all.

barplot((count(mammals, "continent")$freq), 
        names = continents$continent, 
        las = 2, 
        cex.names = 0.5)

# 3. Redo the bar graph in color. Add a label to the y axis.

barplot((count(mammals, "continent")$freq), 
        names = continents$continent, 
        ylab = "Number of Mammal Species",
        las = 2, 
        cex.names = 0.8,
        col = terrain.colors(7))

# 4. Redo the bar graph to increase the limit of the y-axis to 1500 species. 
# (The result might not be immediately evident in the axis labeling because by default 
# R applies internal rules to make graphs “pretty”. Try increasing the limit to 1600 or 
# 1700 and see what happens.)

barplot((count(mammals, "continent")$freq), 
        names = continents$continent, 
        ylab = "Number of Mammal Species",
        ylim = c(0, 1600),
        las = 2, 
        cex.names = 0.8,
        col = terrain.colors(7))

# 5. The plot categories are listed in alphabetical order by default, which is 
# arbitrary and makes the visual display less efficient than other possibilities. 
# Redo the bar graph with the continents appearing in order of decreasing numbers of species.

barplot(sort((count(mammals, "continent")$freq), decreasing = TRUE),  
        names.arg = c("INSULAR","AF","EA", "SA", "NA", "AUS", "OCEANIC"), 
        ylab = "Number of Mammal Species",
        ylim = c(0, 1600),
        las = 2, 
        cex.names = 0.8,
        col = terrain.colors(7))

# 6. Generate a histogram of the body masses of mammal species. 
# How informative is that?!

hist(mammals$mass.grams)

# 7. Generate a histogram of log body mass. Is this more informative? 
# Morphological data commonly need a log-transformation when analyzing.

hist(mammals$log.mass.grams)

# 8. Redo the previous histogram but use the breaks option to force a bin width of 2 units 
# (i.e., generate breaks between 0 and 10 by 2 units). How much detail is lost? 
# (note: if you used log rather than log10 to create your variable of log body mass 
#  you will need to use breaks between 0 and 20).

hist(mammals$log.mass.grams, breaks = seq(from = 0, to = 10, by = 2), right = FALSE)

# 9. Redo the previous histogram but vary the bin width. 
# Try a bin width of of 1; then try 0.5; and then 0.1. Which bin width is superior?

hist(mammals$log.mass.grams, breaks = seq(from = 0, to = 10, by = 0.5), right = FALSE)

# 10. Redo the histogram, but display probability density instead of frequency.

hist(mammals$log.mass.grams, prob = TRUE, right = FALSE)

# 11. How does the frequency distribution of log body mass depart from a normal distribution? 
# Answer by visual examination of the histogram you just created. Now answer by examining a normal 
# quantile plot instead. Which display is more informative?

# It is positively skewed. 

qqnorm(mammals$log.mass.grams)


# 12. Redo the normal quantile plot but use the option pch="." to use a smaller plotting symbol.

qqnorm(mammals$log.mass.grams, pch = 1, cex = 0.75)

# 13. If time permits, redraw the histogram of log body mass and superimpose a normal density 
# curve to assess help detect deviations from normality.

hist(mammals$log.mass.grams, breaks = seq(from = 0, to = 10, by = 0.5), right = FALSE)

m <- mean(mammals$log.mass.grams, na.rm = TRUE)
s <- sd(mammals$log.mass.grams, na.rm = TRUE)
xpts <- seq(from = min(mammals$log.mass.grams, na.rm=TRUE), to = max(mammals$log.mass.grams, na.rm = TRUE), length.out = 101)

lines(dnorm(xpts, mean=m, sd=s) ~ xpts, col="red", lwd=2)
# This didn't work. 


#### Visualizing associations between variables ####

# 1. Use a box plot to compare the distribution of body sizes (log scale most revealing) 
# of mammals having different extinction status. Are extinct mammals similar to, larger than, 
# or smaller than, extant mammals? (You may need to use the cex.axis option to shrink the labels
# so that they all fit on the graph).


# 2. Examine the previous box plot. How do the shapes of the body size distributions 
# compare between extinct and extant mammals?


# 3. Redo the previous box plot but make box width proportional to the square root of sample size. 
# Add a title to the plot.


# 4. Use the tapply command to calculate the median log body mass of each extinction-status group of mammals. 
# Check that these are consistent with the box plot results.

# 5. Calculate the mean of log body mass of each extinction-status mammal group. 
# Why is the mean log size of extant mammals larger than the median, but the mean log size for 
# extinct mammals smaller than the median?


# 6. Create a two-way frequency table (contingency table) describing the frequencies of mammal 
# species in different extinction status groups on different continents. Which continent has seen the most extinctions? 
# Which continent has the greatest number of extinctions relative to the number of extant species?


# 7. Draw a mosaic plot illustrating the relative frequencies of mammal species in different extinction status 
# groups on different continents. Try switching the order of the variables in the mosaicplot command to change the 
# explanatory and response variable. Which continent has experienced the greatest number of extinctions relative to 
# total numbers of species? (A mosaic plot is perhaps not ideal for these data because the frequencies are so small 
# for some categories, such as “introduction”. In this case R also has difficulties squeezing in the labels. 
# Perhaps this is a case in which a table is superior to a graph.)

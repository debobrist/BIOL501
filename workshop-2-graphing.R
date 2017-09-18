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

continents <- count(mammals, "continent")
status <- count(mammals, "status")
# can also use table (mammals$continent), but gives output in different form.


# You’ll notice in the frequency table for the variable “continent” that there’s a 
# typo in the data. One case is shown as having the continent “Af” rather than “AF”. 
# Fix this using the command line in R.

mammals$continent <- toupper(mammals$continent)

# Create a new variable in the mammal data frame: the log (base 10) of body mass. 
# (See “Work with data frames” on the R Tips “Data frame” page if you need help with this.)

mammals <- mutate(mammals, log.mass.grams = log10(mass.grams))



#### Visualizing frequency distributions ####

# 1. Which continent has the greatest number of mammal species? Which has the least? 
# Use a simple bar graph to find out.

barplot(continents$freq, 
        names = continents$continent, 
        las = 2)

# 2. Redo the bar graph using the cex.names option to control the size
# of the category names (this might be needed to ensure that there is 
# room for them all.

barplot(continents$freq, 
        names = continents$continent, 
        las = 2, 
        cex.names = 0.8)

# 3. Redo the bar graph in color. Add a label to the y axis.

barplot(continents$freq, 
        names = continents$continent, 
        ylab = "Number of Mammal Species",
        las = 2, 
        cex.names = 0.8,
        col = terrain.colors(7))

# 4. Redo the bar graph to increase the limit of the y-axis to 1500 species. 
# (The result might not be immediately evident in the axis labeling because by default 
# R applies internal rules to make graphs “pretty”. Try increasing the limit to 1600 or 
# 1700 and see what happens.)

barplot(continents$freq, 
        names = continents$continent, 
        ylab = "Number of Mammal Species",
        ylim = c(0, 1600),
        las = 2, 
        cex.names = 0.8,
        col = terrain.colors(7))

# 5. The plot categories are listed in alphabetical order by default, which is 
# arbitrary and makes the visual display less efficient than other possibilities. 
# Redo the bar graph with the continents appearing in order of decreasing numbers of species.
A <- factor(levels = c("INSULAR", "AF", "EA", "SA", "NA", "AUS", "OCEANIC"))

barplot(sort(continents$freq, decreasing = TRUE), 
        names = continents$continent, 
        ylab = "Number of Mammal Species",
        ylim = c(0, 1600),
        las = 2, 
        cex.names = 0.8,
        col = terrain.colors(7))


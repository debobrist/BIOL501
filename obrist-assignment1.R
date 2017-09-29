# Assignment #1: Remake a poor graph in R.

# Data from Figure 4 in Price et al., 2013 - Prey Selectivity of Fraser River Sockeye 
# Salmon during Early Marine Migration in British Columbia

# Data obtained by extracting points from graph using a free web plot digitizer app: 
# http://arohatgi.info/WebPlotDigitizer/app/?

# Read in data: 
data.2009  <- read.csv("data-raw/price-2009.csv")
data.2010 <- read.csv("data-raw/price-2010.csv")

# View data: 
head(data.2009)
head(data.2010)

# Rename columns:
names(data.2009) <- c("migration_day", "foraging_success")
names(data.2010) <- c("migration_day", "foraging_success")

# Insert column for year in each data frame:
library(dplyr)
data.2009 <- mutate(data.2009, year = 2009)
data.2010 <- mutate(data.2010, year = 2010)

# Combine data frame from 2009 and 2010.
data <- rbind(data.2009, data.2010)

# Plot the data: 

# First, check range of x and y values to determine xlim and ylim:
range(data$migration_day) # from 8.07 to ~ 49.09
range(data$foraging_success) # from 0.125 to 6.8

# Make plot without differentiating between years:
plot(data$foraging_success ~ data$migration_day, 
     pch = 16, # make points closed, coloured circles
     xlab = "Migration day", # label the axes
     ylab = "Foraging success (%)",
     cex.lab = 1.2, # make axis labels bigger
     las = 1, # turn axis tick labels horizontally
     xlim = c(5, 50), # make x and y limits based on ranges determined above
     ylim = c(0, 7))

# Add linear regression line:
abline(lm(data$foraging_success ~ data$migration_day))


# Make plot with points for 2009 to start.
plot(data$foraging_success[data$year == 2009] ~ data$migration_day[data$year == 2009], 
     col = "black", # make points black
     pch = 16, # make points closed, coloured circles
     xlab = "Migration day", # label the axes
     ylab = "Foraging success (%)",
     cex.lab = 1.2, # make axis labels bigger
     las = 1, # turn axis tick labels horizontally
     xlim = c(5, 50), # make x and y limits based on ranges determined above
     ylim = c(0, 7))

# Add linear regression line for 2009, make it black.
abline(lm(data$foraging_success[data$year == 2009] ~ 
            data$migration_day[data$year == 2009]),
       col = "black")

# Overlay points for 2010, but make these gray: 
points(data$foraging_success[data$year == 2010] ~ data$migration_day[data$year == 2010], 
     col = "gray", 
     pch = 16)

# Add linear regression line for 2010, make it gray.
abline(lm(data$foraging_success[data$year == 2010] ~ 
            data$migration_day[data$year == 2010]),
       col = "gray", 
       lwd = 1)

# Add a legend to show which colours correspond to which years.
legend("topright",
       c("2009", "2010"),
       col = c("black", "gray"),
       pch = 16,
       bty = "n")

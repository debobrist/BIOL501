# Assignment #1: Remake a poor graph in R.

# Data from Figure 4 in Price et al., 2013 - Prey Selectivity of Fraser River Sockeye Salmon during
# Early Marine Migration in British Columbia

# Data obtained using: 

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


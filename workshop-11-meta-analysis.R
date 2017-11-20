# 1. View the data and examine it closely. The data set is not large, but it has aspects in 
# common with larger compilations used in meta-analyses, and raises many of the same questions. 
# First, there more entries than published studies. Repeated entries from the same study 
# represent different nearby populations, or measurements taken in different years from the same 
# population. Can the 15 effect sizes in the table therefore be considered independent? 
# Should we average all the values from the same population, or from the same study, before 
# continuing? Welcome to meta-analysis.

sparrows <- read.csv ("data-raw/sparrow.csv")

# 2. For the purposes of this exercise, let’s treat the 15 effect sizes as though they are 
# independent and belong to a single fixed class. I hope this does not shock you.
# 3. Create a simple scatter or “funnel” plot depicting the relationship between effect size 
# and sample size for the house sparrow data. Include a dashed line in the plot for r = 0. 
# What pattern would you expect to see if there was publication bias? Is such a pattern evident 
# in your plot?
# Statistical analysis of correlations begins by converting r to the Fisher’s z-scale.
# The reason is that on the z-scale, the sampling distribution for the correlation is 
# approximately normal and the standard error is independent of z. The transformation is
# z = 0.5 ln((1 + r)/(1 – r)), or equivalently, z = tanh-1(r)

# 4. A convenient feature of the Fisher z is that the approximate standard error depends only
# on the sample size n (i.e., number of birds) used in each study,
# SEz = 1/√(n – 3).
# 1. Read the data from the file and inspect the variables. Most of the species are 
# classified into 6 “ecomorph” categories according to ecological similarity, especially 
# on the basis of the type of perch used. Species not belonging to any specific category are 
# labeled “Unique”.
anolis <- read.csv ("data-raw/anolis.convergence.csv")

# 2. Examine associations among variables using graphs. Which variables are most strongly 
# correlated with one another? Positively or negatively?
plot (SVLength ~ FemurLength, data = anolis) # strongly correlated
plot (TibiaLength ~ FemurLength, data = anolis)
plot (FootLamellae ~ HandLamellae, data = anolis)

plot (anolis)
v <- cov(anolis [ , 4:14])
diag (v)

# It looks like almost all variables are strongly positively correlated. 

# 3. Carry out a principal components analysis on the Anolis species mean measurements.

any (is.na (anolis)) # There are no NAs
# z <- prcomp( ~ x1 + x2 + x3, data = mydata) # formula method
z <- prcomp (anolis [ , 4:14])                # shortcut, indicating columns

# 4. Examine the proportion of variance explained by each principal component. What 
# proportion of the variance is accounted for by the first principal component? 
# By the first two? How many components are needed to capture 99% of the total variance 
# among species?

biplot (z, cex = 0.7)
biplot (z, cex = 0.7, choices = c(3,4)) # Pulls out 3rd and 4th pcs.

summary (z)
# PC1 = 93.22 % of variance
# PC2 = 3.426 %
# PC3 = 2.17%
# PC4 = 0.531% 
# The first 4 pcs capture 99% of the total variance among species.

# 5. What are eigenvalues? Create a scree plot to visualize the magnitudes of the 
# eigenvalues.
# Eigenvalues are numbers that tell you the amount of variance in the data in the direction
# of the corresponding eigenvector.
screeplot (z, type = "lines") # Can see amount of variation due to each variable.

# 6. Create a biplot to visualize the contribution of traits to the first two principal 
# components. Which traits contribute most to PC1? Which vary most with PC2?
biplot (z, cex = 0.7, choices = c (1,2)) # Pulls out 1st and 2nd pcs.

# In this case, all traits contribute substantially to PC1. Hand lamellae and tail length
# contribute most to PC2.

# 7. What are eigenvectors? Examine and interpret the eigenvectors for the first two 
# principal components. Which variables contribute the most to the first two principal 
# components? Can any of the principal components be interpreted as a general “size” 
# variable? Which one? How much of the variance does it account for?

# Eigenvectors are also called "loadings" and they describe the direction of pull of the
# eigenvalue. 

z$rotation [ , c (1, 2)] # eigenvectors (with trait loadings)
# Since PC1 explains 93% of the variation, it makes sense that the eigenvectors for PC1
# show that all traits pull in the same direction. PC2 explains much less of the variation,
# and some traits (Femur length, tibia length, foot length, toe length, and tail length),
# pull in the opposite direction of SV length, humuerus, radius, and finger length.


# 8. Compare the eigenvectors for the first two principal components to the biplot you 
# generated for these same two components. Do you see a correspondence?

# 9. Do the other eigenvectors have as straightforward an interpretation? For example, 
# which traits contribute to principal components 3 and 4?

# 10. Save the scores for the first four principal components, the measurements of every individual along these principal axes, into the Anolis data frame.
# 11. Illustrate the range of phenotypes on the four islands of the Greater Antilles in a scatter plot of the first two principal components. (If you use the option pch = Island in your plot command, the symbol plotted for each species will be the first letter of the name of the island on which it occurs. Make sure that “Island” is a character, not a factor) Do all islands have the same range of phenotypes? Jamaica is the youngest island and has the fewest species. How does the range of phenotypes on this island compare with that on the older islands?
# 12. Re-plot the first two principal components, but this time use different symbols for the different ecomorphs. The principal components analysis is blind to the ecomorphs of the species, yet species of the same ecomorph tend to cluster together. What does this imply about the body forms of species utilizing similar ecological resources?
# 13. On the basis of the differences observed, and your earlier interpretation of the eigenvectors, which ecomorph seems to have the largest overall body size? Which ecomorphs are the smallest?
# 14. On the basis of the differences observed, and your earlier interpretation of the eigenvectors, which ecomorphs likely have the longest tails, relative to other dimensions? Which likely have the shortest tails? Confirm your answer by comparing the mean tail lengths of the different ecomorphs.
# Optional: Repeat the principal components analysis but this time use the correlation matrix rather than the covariance matrix (i.e., standardize all the traits). How are the results changed? Look especially at the change to the importance of the lamellae traits to the eigenvectors.
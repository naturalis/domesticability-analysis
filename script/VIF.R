## IMPORT PACKAGES
library(usdm)
library(caret)
library(corrplot)


## BEGIN VALUES
cat("The first correlation matrix is a visualization of the dataset without the removal of any traits.
    There is a lot of collinearity present. The VIF analysis will take care of this.")
startPredictors <- dataset[,c(8:78)]
matrix <- cor(startPredictors, use = "pairwise.complete.obs")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.5)


## CHECK CORRELATION MATRIX
# The corrplot package is used to visualize the collinearity between the traits,
# using the correlation matrix. With the findCorrelation() function, the columns
# with the highest collinearity are found. By visualizing the matrix, the
# outcome of the findCorrelation function can be easily checked. At the
# beginning of every round the VIF function is run, to check if the results are
# still infinite.
cutoffValues <- c(.9, .8, .7, .6, .5, .46)
for(cut in cutoffValues){
  matrix <- cor(startPredictors, use = "pairwise.complete.obs")
  delColumn <- findCorrelation(matrix, cutoff = cut, verbose = FALSE, exact=FALSE)
  startPredictors <- startPredictors[,-c(delColumn)]
}
vif(startPredictors)


## INSERT ADULTBODYMASS
# The set of predictors is declared for the VIF analysis, and a last check for
# collinearity using the visualized matrix is done. During the first round the
# AdultBodyMass was removed due to a high collinearity with other traits. However, because
# Adultbodymass is hypothesized to be a biologically relevant trait, it will be reinserted in this step.
vifPredictors <- cbind(dataset$X5.1_AdultBodyMass_g, startPredictors)
colnames(vifPredictors)[1] <- "X5.1_AdultBodyMass_g"

# Check VIF -> Adultbodymass is highly correlated with the other traits.
vif(vifPredictors)

# Check correlation matrix
matrix <- cor(vifPredictors, use = "pairwise.complete.obs")
cat("The second corrplot is a visualization of the dataset after cutoff values are implemented and 
    adultbodymass is added.")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.7)

# Remove highest collinearity trait after adult body mass to see if it brings the value below 10
vifPredictors$AVGFoodConsumption <- NULL

# Check VIF -> normal values (<10), we are good to go
vif(vifPredictors)


## FINAL PREDICTOR SET
# Final set of predictors, containing:  X5.1_AdultBodyMass_g, X1.1_ActivityCycle,
# X6.1_DietBreadth, X12.1_HabitatBreadth, X15.1_LitterSize,
# X21.1_PopulationDensity_n_km2, X10.2_SocialGrpSize, AVGFoodConsumption, Sociality,
# SocialHierarchy, NumMales, MatingSystem, YearRoundBreeding, DevelopmentStrategy,
# Horns_Antlers, Lifespan, NaturalPredators, AVGMovingSpeed, AVGTravelDistance,
# Aspect, ClayPercentage, PETWettestQuarter, OrganicCarbon, bio18

predictors <- vifPredictors

## FINAL VALUES
cat("The third correlation matrix is our final product after removing all highly correlated traits.")
matrix <- cor(predictors, use = "pairwise.complete.obs")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.5)

## REMOVE VARIABLES
rm(vifPredictors, startPredictors, matrix, cut, cutoffValues, delColumn)

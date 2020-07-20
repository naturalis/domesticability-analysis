# IMPORT PACKAGES
library(usdm)
library(caret)
library(corrplot)

## BEGIN VALUES
cat("The correlation matrix without removing any traits. There is a lot of collinearity present.")
startPredictors <- dataset[,c(8:78)]
matrix <- cor(startPredictors, use = "pairwise.complete.obs")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.5)
## CHECK CORRELATION MATRIX
# The corrplot package is used to visualize the collinearity between the traits,
# using the correlation matrix. With the findCorrelation function, the columns
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
# Adultbodymass is hypothesized to be an important trait, it will be reinserted in this step.
vifPredictors <- cbind(dataset$X5.1_AdultBodyMass_g, startPredictors)
colnames(vifPredictors)[1] <- "X5.1_AdultBodyMass_g"
# Check VIF -> Adultbodymass is highly correlate with the other traits, so it cannot be used.
vif(vifPredictors)
# Check correlation matrix
matrix <- cor(vifPredictors, use = "pairwise.complete.obs")
#corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.7)
# Remove highest collinearity trait with the help of the matrix
vifPredictors$X5.1_AdultBodyMass_g <- NULL
# Check VIF -> normal values, we are good to go
vif(vifPredictors)

## FINAL PREDICTOR SET
# Final set of predictors, containing: X1.1_ActivityCycle,
# X6.1_DietBreadth, X12.1_HabitatBreadth, X15.1_LitterSize,
# X21.1_PopulationDensity_n_km2, X10.2_SocialGrpSize, AVGFoodConsumption, Sociality,
# SocialHierarchy, NumMales, MatingSystem, YearRoundBreeding, DevelopmentStrategy,
# Horns_Antlers, Lifespan, NaturalPredators, AVGMovingSpeed, AVGTravelDistance,
# Aspect, ClayPercentage, PETWettestQuarter, OrganicCarbon, bio18

predictors <- vifPredictors
## FINAL VALUES
cat("The correlation matrix after removing all unnecessary traits")
matrix <- cor(predictors, use = "pairwise.complete.obs")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.5)
## REMOVE VARIABLES
rm(vifPredictors, startPredictors, matrix, cut, cutoffValues, delColumn)

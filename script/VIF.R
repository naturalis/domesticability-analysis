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
# AdultBodyMass is removed due to a high collinearity with other traits. The
# bodymass is an important trait, so it will be reinserted in this step.
vifPredictors <- cbind(dataset$X5.1_AdultBodyMass_g, startPredictors)
colnames(vifPredictors)[1] <- "X5.1_AdultBodyMass_g"

# Check VIF -> Still infinite values
vif(vifPredictors)

# Check correlation matrix
matrix <- cor(vifPredictors, use = "pairwise.complete.obs")
#corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.7)

# Remove highest collinearity trait with the help of the matrix
vifPredictors$PETDriestQuarter <- NULL

# Check VIF -> normal values
vif(vifPredictors)


## VIF ANALYSIS
# During the VIF analysis traits are removed using the VIF values: the highest
# values are removed until the remaining traits have a VIF of 10 or lower.

# Round1 - Results: remove bio18
vif(vifPredictors)
vifPredictors$bio18 <- NULL

# Round2 - Results: remove AVGFoodConsumption
vif(vifPredictors)
vifPredictors$AVGFoodConsumption <- NULL

# Round3 - Results: remove NumMales
vif(vifPredictors)
vifPredictors$NumMales <- NULL

# Round4 - Results: all VIFs under 10
vif(vifPredictors)


## FINAL PREDICTOR SET
# Final set of predictors, containing: X5.1_AdultBodyMass_g, X1.1_ActivityCycle,
# X6.1_DietBreadth, X12.1_HabitatBreadth, X15.1_LitterSize,
# X21.1_PopulationDensity_n_km2, X10.2_SocialGrpSize, Sociality,
# SocialHierarchy, MatingSystem, YearRoundBreeding, DevelopmentStrategy,
# Horns_Antlers, Lifespan, NaturalPredators, AVGMovingSpeed, AVGTravelDistance,
# Aspect, ClayPercentage, PETWettestQuarter, OrganicCarbon
predictors <- vifPredictors


## FINAL VALUES
cat("The correlation matrix after removing all necessary traits. Almost all of the collinearity is removed\n(except for the collinearity with AdultBodyMass).")
matrix <- cor(predictors, use = "pairwise.complete.obs")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.5)


## REMOVE VARIABLES
rm(vifPredictors, startPredictors, matrix, cut, cutoffValues, delColumn)

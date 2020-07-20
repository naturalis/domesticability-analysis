# IMPORT PACKAGES
library(usdm)
library(caret)
library(corrplot)

## BEGIN VALUES
cat("The correlation matrix without removing any traits. There is a lot of collinearity present.")
startPredictors <- dataset[,c(8:76)]
matrix <- cor(startPredictors, use = "pairwise.complete.obs")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.5)


## CHECK CORRELATION MATRIX
# The corrplot package is used to visualize the collinearity between the traits,
# using the correlation matrix. With the findCorrelation function, the columns
# with the highest collinearity are found. By visualizing the matrix, the
# outcome of the findCorrelation function can be easily checked. At the
# beginning of every round the VIF function is run, to check if the results are
# still infinite. A cutoff of .58 gives us a set of variables with VIF values below 10

#After being unable to find reliable data on Habitat/Diet breadth and subsequently removing 
#them from the dataset, a few steps changed in the analysis. This analysis previously had a cutoff of .46, 
#which changed to .58 after the removal of Habitat/Diet Breadth. This cutoff value gives us a set of 
#variables with VIF values below 10.
cutoffValues <- c(.9, .8, .7, .6, .58)

for(cut in cutoffValues){
  matrix <- cor(startPredictors, use = "pairwise.complete.obs")
  delColumn <- findCorrelation(matrix, cutoff = cut, verbose = FALSE, exact=FALSE)
  startPredictors <- startPredictors[,-c(delColumn)]
}

vif(startPredictors)


## INSERT ADULTBODYMASS
# The set of predictors is decided for the VIF analysis, and a last check for
# collinearity using the visualized matrix is done. However, AdultBodyMass is hypothesized to 
#be an important trait, so it is reinserted in this step to check if it has a VIF value below 10
vifPredictors <- cbind(dataset$X5.1_AdultBodyMass_g, startPredictors)
colnames(vifPredictors)[1] <- "X5.1_AdultBodyMass_g"

# Check VIF -> Inserting Adult Body Mass is not successful as we are given a VIF value of 18.010486 
vif(vifPredictors)

# Check correlation matrix
matrix <- cor(vifPredictors, use = "pairwise.complete.obs")
corrplot(matrix, type="lower", order = "hclust", tl.pos = "l", tl.col = "black", tl.cex = 0.7)

# Remove highest collinearity trait with the help of the matrix, which is AdultBodyMass (18.010486)
vifPredictors$X5.1_AdultBodyMass_g <- NULL

# Check VIF -> normal values and ultimately our final VIF values for this analysis
vif(vifPredictors)


## VIF ANALYSIS
#EDIT: The below stage of the VIF analysis is no longer required after the removal of Habitat/Diet Breadth. 
#The removal of these traits changed the cutoff values and influenced the VIF values of the variables 
#in our dataset. I will keep this section for historical purposes, perhaps to be deleted at a later time.

## Trait removal:
#During the VIF analysis traits are removed using the VIF values: the highest
#values are removed until the remaining traits have a VIF of 10 or lower.

# Round1 - Results: remove bio18
#vif(vifPredictors)
#vifPredictors$bio18 <- NULL

# Round2 - Results: remove AVGFoodConsumption
#vif(vifPredictors)
#vifPredictors$AVGFoodConsumption <- NULL

# Round3 - Results: remove NumMales
#vif(vifPredictors)
#vifPredictors$NumMales <- NULL


#Results: all VIFs under 10
#vif(vifPredictors)





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



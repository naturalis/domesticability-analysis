## LOAD PACKAGES
library(phylolm)


## PROCESS DATASET
# A dataset without missing values is made. The X12.1_HabitatBreadth and
# X6.1_DietBreadth are removed, due to there being not enough data for the
# domesticated species. It contains the Domestication trait, and also the
# CanonicalNames to match the data with the tree. The tree tips that aren't in
# the final data are dropped.
predictors$X6.1_DietBreadth <- NULL
predictors$X12.1_HabitatBreadth <- NULL

modelData <- cbind(dataset$CanonicalName, dataset$Domestication,predictors)
names(modelData)[names(modelData)=="dataset$Domestication"] <- "Domestication"
names(modelData)[names(modelData)=="dataset$CanonicalName"] <- "CanonicalName"

# Only domesticated species REMOVE LATER
#domData <- modelData[modelData$Domestication == 1,]

# Only the rows without any missing values are selected
modelData <- modelData[complete.cases(modelData),]

# Dropping species from the tree
dropTips <- setdiff(tree$tip.label, modelData$CanonicalName)
modelTree <- drop.tip(tree, dropTips)


## DEPENDENT VARIABLE
# The dependent variable should be in a binary state. This means that the
# Domestication trait needs to be converted to 0-1 instead of the 1-2 it's now.
# The 'wild' state (2) will be converted to the zero state (0). This way 'wild'
# appears as 0 and 'domesticated' appears as 1.
modelData$Domestication[modelData$Domestication==2] <- 0


## FORMULAS
# The input formulas are defined, which are going to be used as input for the
# phyloglmstep function.

# Formula with all the predictors
# Results:  Current model: 
#           AIC(k=2): 28.3458557864183
formula <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

## MODEL SELECTION

#Using the phyloglmstep
phyloglmstep(formula, starting.formula = NULL, data=modelData, phy=modelTree, 
             method= "logistic_MPLE", direction = "forward", trace = 2, 
             btol = 36.7462, log.alpha.bound = 4, start.beta=NULL, 
             start.alpha=NULL, boot = 0, full.matrix = TRUE, k=2)

## FINAL MODEL
# The final formula for the model constructed by the phyloglmstep function
finalFormula <- Domestication ~ 1 + X5.1_AdultBodyMass_g + DevelopmentStrategy + Horns_Antlers + AVGMovingSpeed + AVGTravelDistance


rm(dropTips, formula, modelData, modelTree)



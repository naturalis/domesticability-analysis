## LOAD PACKAGES
library(phylolm)


## PROCESS DATASET
# A dataset without any missing values is made. It contains the Domestication
# trait, and also the CanonicalNames to match the data with the tree. The tree
# tips that aren't in the final data, are dropped.

modelData <- cbind(dataset$CanonicalName, dataset$Domestication,predictors)
names(modelData)[names(modelData)=="dataset$Domestication"] <- "Domestication"
names(modelData)[names(modelData)=="dataset$CanonicalName"] <- "CanonicalName"

# Only domesticated species REMOVE LATER
#domData <- modelData[modelData$Domestication == 1,]

# Only the rows without any missing values are selected
modelData <- modelData[complete.cases(modelData),]
#test <- modelData[rowSums(is.na(modelData)) == 1,]

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

# Formula1: All predictors
# Results: Current model: Domestication ~ 1 + X15.1_LitterSize + Horns_Antlers + NaturalPredators + AVGMovingSpeed - AIC(k=2): 19.2464091190541
formula1 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula2: Without X5.1_AdultBodyMass_g
# Results: 
formula2 <- Domestication ~ X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula3: Without X1.1_ActivityCycle
# Results: 
formula3 <- Domestication ~ X5.1_AdultBodyMass_g + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula4: Without X6.1_DietBreadth
# Results: 
formula4 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula5: Without X12.1_HabitatBreadth
# Results: 
formula5 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula6: Without X15.1_LitterSize
# Results: 
formula6 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula7: Without X21.1_PopulationDensity_n_km2
# Results: 
formula7 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula8: Without X10.2_SocialGrpSize
# Results: 
formula8 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula9: Without Sociality
# Results: 
formula9 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + SocialHierarchy + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula10: Without SocialHierarchy
# Results: 
formula10 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + MatingSystem + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula11: Without MatingSystem
# Results: 
formula11 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + YearRoundBreeding + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula12: Without YearRoundBreeding
# Results: 
formula12 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + DevelopmentStrategy + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon

# Formula13: Without DevelopmentStrategy
# Results: 
formula13 <- Domestication ~ X5.1_AdultBodyMass_g + X1.1_ActivityCycle + X6.1_DietBreadth + X12.1_HabitatBreadth + X15.1_LitterSize + X21.1_PopulationDensity_n_km2 + X10.2_SocialGrpSize + Sociality + SocialHierarchy + MatingSystem + YearRoundBreeding + Horns_Antlers + Lifespan + NaturalPredators + AVGMovingSpeed + AVGTravelDistance + Aspect + ClayPercentage + PETWettestQuarter + OrganicCarbon


### MODEL SELECTION
# Using the phyloglmstep.
phyloglmstep(formula1, starting.formula = NULL, data=modelData, phy=modelTree, 
             method="logistic_MPLE", direction = "forward", trace = 2, 
             btol = 30, log.alpha.bound = 4, start.beta=NULL, 
             start.alpha=NULL, boot = 0, full.matrix = TRUE, k=2)


rm(dropTips)

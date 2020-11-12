
# Load packages and data --------------------------------------------------

library(tidyverse) #data tidying and visualization
library(stats) #glm function
library(glmmTMB) #dataset
library(lme4) #mixed model package
library(MuMIn) #model selection
library(DHARMa) #model diagnostics

#load salamanders dataset
data(Salamanders)


# Data explorations -------------------------------------------------------

#look at this dataset
str(Salamanders)

#look at response variable distribution
hist(Salamanders$count)

#if we are interested in count as a response variable
#and Water temp and cover as predictor variables,
#explore the data relationships between these 
#variables
Salamanders %>%
  dplyr::select(cover, Wtemp, count) %>%
  pivot_longer(cols = cover:Wtemp,
               names_to = "predictor",
               values_to = "value") %>%
  ggplot(aes(x = value, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~predictor, scales = "free") +
  theme_bw()


# Fit a GLM ---------------------------------------------------------------

#Fit a generalized linear model
#with a Poisson distribution for 
#count data (look at distribution table 
#in R Markdown for review of distributions
#and data types)
model <- glm(count ~ cover + Wtemp,
             data = Salamanders,
             family = "poisson")

#assess the summary of this model
summary(model)
#particular interest are the values
#for each predictor Estimate and 
#Pr > |z| values. Estimate
#gives a "slope" of the relationship, while 
#Pr > |z| is a p-vaue for that predictor


# Model selection with AICc ------------------------------------------------

#this call also gives us the AIC value for this
#model, which is used to compare different models
#remember, 
#1. trying to minimize AIC, 
#2. AIC has a "reward" portion for explaining variance in the data while
#having a "penalty" portion for adding more variables to explain the data
#3. AIC can only be used to compare models of the same underlying data
#4. an AIC difference of 2 is a good rule of thumb for model selection

#for small ecological datasets we want to use AICc instead
#AICc adds extra penalties for adding more predictors to the model

#MuMIn package has an AICc() function
AICc(model)

#to do the next function, dredge(), need to make sure our model
#fits to the whole dataset, so add an na.action argument. 
#(this is not always necessary, but a good troubleshooting
#step if the dredge() function does not work)
model <- glm(count ~ cover + Wtemp,
             data = Salamanders,
             family = "poisson",
             na.action = "na.fail")

#use dredge to compare models
dredge(model)

#Multiple ways to deal with this - could go with lowest AIC (a good 
#appraoch), or take the model within the top 2 AIC with the 
#simplest structure

best_model <- glm(count ~ cover,
                  data = Salamanders,
                  family = "poisson")


# Model diagnostics -------------------------------------------------------

#does this dataset actually fit the distribution we have 
#specified for this model?
#we'll be using functions from the DHARMa package

simulationOutput <- simulateResiduals(fittedModel = best_model,
                                      plot = T)

testDispersion(simulationOutput)

#This dataset does not meet the assumptions of a Poisson distribution
#Look at the Rmarkdown document for troubleshooting steps that you migth
#perform from here. 

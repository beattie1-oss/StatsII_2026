##################
#### Stats II ####
##################

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("tidyverse", "ggplot2","readxl"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


###############################
#### Tutorial 2: GLMs ####
###############################
####Theory####
# Logic: use maximum likelihood estimation, take derivatives and try to minimise it, be confident that it is the best combination of parameters t
# this then helps us find the best relationship
# lm() is special type of glm() => both logic estimation approach arrives at the best set of parameters
iris
lm(data = iris, Sepal.Length ~ Sepal.Width + Species) #linear
glm(data = iris, Sepal.Length ~ Sepal.Width + Species, family = "gaussian")
#DIfference is GLM doesnt get R^2
# Get AIC/BIC/Log Likelihood - pnalty terms fail to explain
# If all Values = 0 perfect model
# CLose to 0 much of variation explained by model
# Higher deviance = Bad
# Do not draw conclusions, try new model, use these values to compare
#There is no threshold, it is a comparitive (the size depends on units)
# AIC/BIC penalty term increasing model complexity (new terms) simple is better. Is predictive power improving?
# Log likelihood value -
# Deviance = how much info you failed to capture
#Logit regression. Use log odds to convert binomial, as it is a infity scale which is a straight line
# Log of odds also converts model from multiplicative to an additive relationships



#Binary 
# Logistic regression = binary outcomes
# Must be bounded between 0 and 1 (i.e. True/False, Yes/No)
# We need a distribution that falls between 0 and 1
#Getting log of odds, which is related to parameters(beta valus) in linear form
# Link function = how the distribution is related to my predictors using a linear relationship

#Using Logistic regression


# -106 = log odds 
# Vs using response, get p straight away

# WHen use glms have to be cautious of the value to be in interpretive first, 
# have to take the transformation

# Linear vs Logit Regression



#####Exercises ####
# In today's tutorial, we'll begin to explore GLMs
#     1. Import/wrangle data
#     2. Execute lm() and glm() of RQ
#     3. Compare models

#### Case study
# We're interested in central bank governors, specifically their occupational turnover, for almost all countries in the world starting from the year 1970

#### Create the dataset
# For this task, we first need data.
# 1. Go to https://kof.ethz.ch/en/data/data-on-central-bank-governors.html and download the data on Central Bank Governors
# https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/central_bank_governors/cbg_turnover_v23upload.xlsx
# 2. Gather necessary variables
#    codewdi: Country code or name
#    year
#    time to regular turnover	
#    regular turnover dummy	
#    irregular turnover dummy	
#    legal duration

# MAKE SURE THERE AREN'T MISSING VALUES!

# Now, you've got your dataset

#### Import the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in uour libraries

## loading the data
data_raw <- read_excel("cbg_turnover_v23upload.xlsx", sheet = "data v2023")
str(data_raw)

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 
data <- data_raw %>%
      select(codewdi,
             country,
             year,
             `time to regular turnover`,
             `regular turnover dummy`,
             `irregular turnover dummy`,
             `legal duration`
      ) %>% 
  mutate(
    codewdi = as.factor(codewdi),
    country = as.factor(country),
    year = as.integer(year),
    time_to_regular_turnover = as.integer(`time to regular turnover`),
    regular_turnover_dummy  = as.integer(`regular turnover dummy`),
    irregular_turnover_dummy  = as.integer(`irregular turnover dummy`),
    legal_duration  = as.integer(`legal duration`),
  ) %>%
  drop_na()

data <- data %>%
  select(
    codewdi,
    country,
    year,
    time_to_regular_turnover,
    regular_turnover_dummy,
    irregular_turnover_dummy,
    legal_duration 
  )
        
#Unique = what values taken are there any outliers
unique(data$codewdi)
unique(data$country) # 163
unique(data$year) #60ish
unique(data$time_to_regular_turnover) #get
unique(data$regular_turnover_dummy)
unique(data$irregular_turnover_dummy)
unique(data$legal_duration)

bad_codes <- c(-999, -666, -555, -535, -881)
data <- data %>%
  mutate(
  across(
    c(year, time_to_regular_turnover, regular_turnover_dummy, irregular_turnover_dummy, legal_duration
      ),
    ~ replace(., . %in% bad_codes, NA)
  )
)

#Check turned to NA
unique(data$codewdi)
unique(data$country) # 163
unique(data$year) #60ish
unique(data$time_to_regular_turnover) #scale variable
unique(data$regular_turnover_dummy) # 1 if governmer completed tenure and change in position
unique(data$irregular_turnover_dummy) #ideally mutally exclusively
unique(data$legal_duration)

data %>% drop_na()
  
#### Descriptive patterns in turnover
# Compute the average turnover rate (mean of turnover) by country over the full sample period

country_turnover <- data %>%
  group_by(country) %>%
  summarize(
    avg_turnover = mean(irregular_turnover_dummy), #average irregular turnover , likelihood that governer gets turned over
    n = n()
  )
#0-1 format therefore
#av_tn over = 0.38 probability that CB goivernor turnover irregular. 0.5. 
# Higher rate = in that country the CB govt turnover before regular tenure 
# n = total number of observations we have for country

# (a) Which five countries have the highest average turnover rates?
country_turnover %>% 
  arrange(desc(avg_turnover)) %>%
  slice(1:5) #recieve first five
# In almost 44% of years Bolivia experienced an irregular replacement 
  
# (b) Which five have the lowest average turnover rates?
country_turnover %>% 
  arrange(avg_turnover) %>%
  slice(1:5) #recieve first five
  
# (c) Plot the distribution of country‑level average turnover rates (e.g. histogram or density) 
#     Briefly comment on whether high turnover is concentrated in a small set of countries

ggplot()

####  Estimate a linear probability model (LPM) with OLS:
  
# (a) Fit lm() with:
  # Outcome: irregular turnover dummy
  # Covariates: 
  #   time to regular turnover	
  #   legal duration
#If time to turnover is less, the previos govt spent some time
# Higher time to turnover, recently saw a change in governer
lmp <-
  lm(irregular_turnover_dummy ~ time_to_regular_turnover + legal_duration , data = data)

summary(lmp)
# B1
#If time to turnover is less than the changes of having an irregular turnover is less
# If time till tiu
# New government are mre likely to turnover than an old goveror

# B1
# If increase legal duration assocaited with decrease in irregular turnover,
# More legal duraction possible more l


# (b) For a “typical” observation  (e.g. median time to regular turnover & legal duration), compute the predicted probability

typical <- data.frame(
  time_to_regular_turnover <- mean()
)  
predict(lpm, newdata = typical)
#mean of all countires

# (c) Identify at least one observation for which lm() prediction is below 0 or above 1 and explain why such predictions are problematic for a probability
data$lmp

# Using the full sample, construct a plot of predicted probability of turnover vs time to regular turnover:

  

#### Baseline logistic regression
  
# Estimate a logistic regression with governor turnover as the binary outcome and same covariates using glm(family = "binomial")
logit <- glm(
  irregular_turnover_dummy ~ time_to_regular_turnover + legal_duration , data = data, family = "binomial"
)  
summary(logit)
# (a) Report coefficient estimates and standard errors

# (b) Interpret the sign of each coefficient in terms of how they affect the probability of turnover
#Sign does not change

# (c) For the same “typical” observation used above, compute the predicted probability of turnover (type = "response"), and compare it to the lm() prediction

#### Compare lm() and glm()  

# (a) Use the lm() to compute fitted values across the observed range of time to regular turnoner, holding legal duration at median value

# (b) Use the logit model to compute fitted probabilities for the same legal duration values

# (c) Plot both curves on the same graph (e.g. blue for lm(), red for glm()) 
  
#### Country heterogeneity and fixed effects

# (a) Introduce country fixed effects into the logit specification using dummy variables 

# (b) Compare the estimated coefficients with and without country fixed effects. How does controlling for unobserved country characteristics affect the relationships w/ turnover?
  
# (c) What kinds of country‑specific factors might be absorbed by these fixed effects in this context

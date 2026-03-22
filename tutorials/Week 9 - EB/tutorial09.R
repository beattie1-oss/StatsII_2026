#######################
# Tutorial 9: Poisson #
#######################

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("dplyr", "ggplot"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 

# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

# Make sure your data are in the correct format.
str(long_data)

# (a) Examine the distribution of the response variable. 
table(long_data)
mean(long_data$art)
var(long_data$art)
# Does least-squares linear regression appear a promising strategy for these data?
# Do we meet the OLS assumptions?
hist(long_data$art)

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
model_lm <- lm(art~., data = long_data)
summary(model_lm)

# Do we meet assumptions for Poisson?

# No we do not Q-Q resuiduals
model_poisson <- glm(art ~., data = long_data, family = poisson())
summary(model_poisson)

coeffs<- coefficients(model_poisson)
xvalues <- sort(long_data$ment)
means <- exp(coeffs[1] + coeffs[3]*xvalues)
plot(long_data$ment, long_data$art)
lines(xvalues, means, lty = 2, col = "red")

# What conclusions would you draw from this analysis (i.e. interpret your estimated coefficients)?

# expec


# What is the predicted number of articles for a married male PhD researcher with 1 child at 2-rated institute whose PhD supervisor published 5 articles?
predict(model_poisson, newdata = )
# Plot predictions vs count.
# Calculate pseudo R squared.
1 - model_poisson$deviance / model_poisson$null.deviance #10% deviance explained, some predictors against no predictros
# Calculate RMSE.
rmse <- sqrt(mean((long_data$art - pred)^2)) #
# On averge how far off are my prediction?
# Model predictions are odf by 1.8 articles on avg
#its an absolute measure not 

# Should we add an interaction for gender with our covariates?
model_int <- glm(art ~ fem * (mar + kid5 + phd + ment),
                 data = long_data,
                 family = poisson)

anova(model_poisson, model_int, test = "LRT")
#so not going ahead with the interactive model. relationship explained by set of preditors is explained in an additive fassion not 

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account?
library(AER)
dispersiontest(model_poisson)
#Null is true dispersion is less than or eqal to 1. Pvalue evidence to reject the null hypothesis
# and conclude that the data is overdispered


library("pscl")
model_zip <- zeroinfl(art ~., data = long_data, dist = "poisson")
summary(model_zip)
#Have count model coefficients then have zero inflation model coefficients
#in this case y=1 for non publishers. what does it take to publish
#Zero how probable is it that wer are in the zero world
#How prob


# Is there evidnece for overdispersion? Run quasi modek
quasi_mode <- glm(art ~ fem + mar + kid5 + phd + ment,
                  data = long_data,
                  family = quasipoisson())
summary(quasi_mode)


long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

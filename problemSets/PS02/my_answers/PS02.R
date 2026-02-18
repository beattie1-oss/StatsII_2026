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

lapply(c("tidyverse", "ggplot2", "readr", "readxl", "purrr", "ggridges", "ggdist", "xtable"),  pkgTest)


# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

str(climateSupport)
table(is.na(climateSupport)) #check for missingness
 
contrasts(climateSupport$countries) <- contr.treatment #quick method to convert factor into set of dummys
contrasts(climateSupport$sanctions) <- contr.treatment #autoadapt to one for each type

m_add <-  glm(
  choice ~ countries + sanctions, #additive 
  data = climateSupport,
  family = binomial(link = "logit") #specify link types
)
summary(m_add)
stargazer::stargazer(m_add,
                     type = "latex", #output code for use in latex
                     title = "Impact of Number of Countries and Sanctions on Choice",
                     covariate.labels = c("Countries 80 of 192", "Countries 160 of 192", "Sanctions 5%", "Sanctions 10%", "Sanctions 20%"),
                     dep.var.labels = "Choice on policy support")

m_null <- glm(
  choice ~ 1, #remove all predictors and replace it with 1 i.e. no slopes
  data = climateSupport,
  family = binomial(link = "logit") #logit link still
)

anova(m_null, m_add, test = "LRT") #global LRT test against null with no covariates


#####################
# Problem 3
#####################
m_int <-  glm( #interaction model version
  choice ~ countries + sanctions + countries*sanctions, #adding in interaction term
  data = climateSupport,
  family = binomial(link = "logit")
)

anova(m_add, m_int, test = "LRT") #partial LRT test

stargazer::stargazer(m_int,
                     type = "latex", #output code for use in latex
                     title = "Impact of Number of Countries and Sanctions on Choice with Interaction",
                     dep.var.labels = "Choice on policy support")



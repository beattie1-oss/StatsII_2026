##############################################
# Tutorial 5 
# Ordered and Multinomial Logistic Regression
##############################################

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

lapply(c("MASS", "nnet", "ggplot2", "dplyr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This data set is analyzed by Long (1997).  The response variable has four ordered categories:
# Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement

# "A working mother can establish just as warm and secure a relationship with her children as a mother who does not work."

# The explanatory variables are:
# the year of the survey (1977 or 1989),
# the gender of the respondent,
# the race of the respondent (white or non-white),
# the respondent's age, and
# the prestige of the respondent's occupation (a quantitative variable)

# load data
workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# Re-label your outcome, gender, race, and year so they are legible

# i.e. "SD", "D", "A", "SA" to "Strongly Disagree", "Disagree", "Agree", "Strongly Agree" 
# 0,1 to "Non-white", "White"
# and "Year1977", "Year1989" to "1977", "1989"
workingMoms$attitude <- factor(workingMoms$attitude,
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly Agree"))
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "White"))
workingMoms$year <- factor(workingMoms$year,
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))

ftable(xtabs(~ gender + year + attitude, data = workingMoms))
prop.table(table(workingMoms$gender, workingMoms$attitude), 1)

# Plot prestige (y-axis) by your outcome (x-axis) by gender ~ year
plot(gender ~ year, prestige, data = workingMoms)
ggplot(data = workingMoms, aes(x = attitude, y = prestige, colour = attitude))+
  geom_boxplot() +
  geom_jitter(alpha = 0.4) +
  scale_x_discrete(labels = function(x){sub("\\s", "\n", x)})+
  theme_minimal(base_family = "CMU Sans Serif")+
  facet_grid(gender ~ year) +
  scale_color_discrete(palette = "Set2")+
  labs(title = "Prestige by Outcome (Attitude)",
       x = "Attitude",
       y = "Prestige",
       color = "Attitude") +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey60")
  )

# a) Perform an ordered (proportional odds) logistic regression
ord.log <- polr(attitude ~ ., data = workingMoms, Hess = TRUE)
summary(ord.log)

# Calculate a p value
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p-value" = p))


# Calculate confidence intervals
(ci <- confint(ord.log))

# Convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

# How do we interpret these coefficients?
#odds of being in higher category the odds of being in a higher supportive decreases by 1-0.97
# for every addiotnal expect the odds of being in a higher category to increase in about 7%
#older people are les

#if hgihes percentage matches it is a highly accurate model

# b) fit a multinomial logit model
# with Strongly Disagree as reference level for the outcome
#set refrence category

workingMoms$attitude <- relevel(workingMoms$attitude, ref = "Strongly Disagree")


# run model
mult.log <- multinom(attitude ~ ., data = workingMoms)
summary(mult.log)
#for each categories has fifferent predictors
# what are the odds of being in disagree given the reference category
# each predictor has different coefficients for each category

# Interpretation:
#A one year increase in age decreases the log odds of agreeeing by .0035


# get p values
exp(coef(mult.log))
# how do we interpret these coefficients?
z <- 
  
# calculate predicted probabilities to help our interpretation
pp <- data.frame(fitted(mult.log))

  
# c) Consider gender as an interaction with your other predictors
# i.e. consider that possibility that gender interacts with the other explanatory variables in influencing the response variable
mult.log.int <- multinom(attitude ~ gender *., data = workingMoms)
summary(mult.log.int)

z.int <- summary(mult.log.int)$coefficients/summary(mult.log.int)$sta
(p.int <- (1-pnorm(abs(z.int), 0, 1)) * 2)
# What do you find?
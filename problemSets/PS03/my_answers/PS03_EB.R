## Ellen Beattie PS03 Stats II #

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

lapply(c("nnet", "MASS", "dplyr", "stargazer", "xtable", 
         "modelsummary", "lmtest", "broom", "knitr", "AER"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

#create new column category depending on GDPWdiff
summary(gdp_data$GDPWdiff)
unique(gdp_data$GDPWdiff)

gdp_data <- gdp_data %>% 
  mutate( 
    GDPWdiff_cat = case_when(
      GDPWdiff > 0 ~ "Positive", #if diff positve label
      GDPWdiff < 0 ~ "Negative",
      TRUE ~ "No Change" #if zero  be no change
    ),
    GDPWdiff_cat = factor(GDPWdiff_cat, levels = c("No Change", "Positive", "Negative"))
  ) #as we set up the baseline level is no change

table(gdp_data$GDPWdiff_cat) #how many are in each


# Unordered Multinomial Logit
unordered_log <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp_data)
summary(unordered_log)
stargazer(unordered_log ,
          type = "latex", #output code for use in latex
          title = "Unordered Multinomial Logit",
          covariate.labels = c("REG", "OIL"),
          dep.var.caption = "DV")

# Predicted Probabilities - since 2 binary covariates 4 possible combinations
newdata <- expand.grid(REG = c(0,1), OIL = c(0,1)) # i.e combos of (0,0), (0,1), (1,0), (1,1)
pp <- predict(unordered_log, newdata = newdata, type = "probs")
round_pp <- round(pp, 4)
pp_table <- cbind(newdata, round_pp)
pp_table





# Ordered Multinomial Logit
#Relevel the categories
gdp_data$GDPWdiff_cat <- factor(
  gdp_data$GDPWdiff_cat,
  levels = c("Negative", "No Change", "Positive"),
  ordered = TRUE #specify this is now an ordered factor
)

levels(gdp_data$GDPWdiff_cat) #double check
is.ordered(gdp_data$GDPWdiff_cat) #TRUE

#Run ordered log reg
ordered_log <- polr(GDPWdiff_cat ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ordered_log)
stargazer(ordered_log,
          type = "latex", #output code for use in latex
          title = "Ordered Multinomial Logit",
          covariate.labels = c("REG (Democracy)", "OIL (Dominant Oil Exporter)"),
          dep.var.caption = "DV")


# Predicted Probabilities - since 2 binary covariates 4 possible combinations
pp <- predict(ordered_log, newdata = newdata, type = "probs") 
round_pp <- round(pp, 4)
pp_table <- cbind(newdata, round_pp)
pp_table

 




# Problem 2 #####################
# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
sum(is.na(mexico_elections)) #sum nas
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
    data = mexico_elections,
    family = poisson())
summary(poisson_model)
stargazer(poisson_model,
          type = "latex",
          title = "Poisson Regression on the Number of times PAN visited a district",
          covariate.labels = c("Competitive District", "Marginality", "PAN-Affiliated Governor"),
          dep.var.labels = "Number of PAN Visits")
dispersiontest(poisson_model) #cannot reject the null that true dispersion is greater than 1


# Wald Test on Competitive District
wt <-lmtest::coeftest(poisson_model) #get z test statistics and p-values
wt

#Predict 
round(predict(poisson_model, 
              newdata = data.frame(competitive.district = 1, 
                                   marginality.06 = 0, 
                                   PAN.governor.06 = 1), 
              type = "response"), 4)

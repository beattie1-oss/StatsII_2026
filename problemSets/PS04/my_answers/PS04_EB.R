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

lapply(c("nnet", "MASS", "survival", "eha", "tidyverse", "ggfortify", "stargazer", "sampleSelection", "texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on child mortality by mother's age and child gender
data("child")

child_surv <- with(child, Surv(enter, exit, event)) #survival function for cox
cox <- coxph(child_surv ~ m.age + sex, data = child) #fit proportional hazard model
summary(cox)

stargazer(cox, type = "latex", digits = 4)
drop1(cox, test = "Chisq")


#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")

# Base Heck
base_heck <- lm(originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged, 
                data = subset(disaster_data, binContribution == 1)) #linear model only observing when contributionis 1
summary(base_heck)
stargazer(base_heck, type = "text", title = "Base Heckman (OLS)")

#Heckman equation- addr selection bias
heck <- heckit( 
  selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
  outcome = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
  data = disaster_data
)
summary(heck)

# Note Assistance from ChatGPT was used to format the table into desired combo linear and heckit
# Base Heckit text reg model
base_coefs <- coef(base_heck)
base_ses   <- sqrt(diag(vcov(base_heck)))
base_pvals <- 2 * (1 - pnorm(abs(base_coefs / base_ses)))

base_tr <- createTexreg(
  coef.names = names(base_coefs),
  coef = base_coefs,
  se = base_ses,
  pvalues = base_pvals, 
  gof.names = c("N"),
  gof = nobs(base_heck)
)

#Full Heckit Texreg model
# All coefficients and standard errors
all_coef <- coef(heck)
all_se   <- sqrt(diag(vcov(heck)))

# Number of selection coefficients
sel_vars <- all.vars(formula(heck$call$selection))[-1] #drop dependent variable
n_sel <- length(sel_vars) + 1

# Number of outcome coefficients (excluding lambda)
out_vars <- all.vars(formula(heck$call$outcome))[-1] #and again
n_out <- length(out_vars) + 1

# Selection coefficients
sel_coef <- all_coef[1:n_sel]
sel_se   <- all_se[1:n_sel]

# Outcome coefficients: only the actual outcome variables, exclude lambda
out_coef <- all_coef[(n_sel+1):(n_sel+n_out)]
out_se   <- all_se[(n_sel+1):(n_sel+n_out)]

# Labels
coef_names <- c(names(sel_coef), paste0("O: ", names(out_coef))) #as in lecture distinguish outcome
coefs      <- c(sel_coef, out_coef) #selecting both selection and outcome coefficient
ses        <- c(sel_se, out_se) #and standard errors

#Goodness of Fit measures
gof_names <- c("N", "Sigma", "Rho") #add on goodness of fit metrics
gof_vals  <- c(nobs(heck), heck$sigma, heck$rho) #select from the hecked

#P-values
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
names(p_vals) <- coef_names

# Full Heckit textreg
heck_tr <- createTexreg(
  coef.names = coef_names,
  coef = coefs,
  se = ses,
  pvalues = p_vals,
  gof.names = gof_names,
  gof = gof_vals
)

#Combine together to make one output table
texreg(list(base_tr, heck_tr),
       custom.model.names = c("Base Heckman", "Full Heckman"),
       use.packages = FALSE,
       booktabs = TRUE,
       digits = 4,
       caption = "Base and Full Heckman on Disaster Relief",
       label = "tab:heckman_comparison",
       float.env = "table",
       stars = c(0.05, 0.01, 0.001))



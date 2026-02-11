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

lapply(c("stargazer", "broom"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

ks_test <- function(input, tol = 1e-12) { #tol = diff that breaks loop i.e precision
  
  ECDF <- ecdf(input)  
  empiricalCDF <- ECDF(input) #create cdf from input to compare to normal
  D <- max(abs(empiricalCDF - pnorm(input))) #test statistic
  n <- length(input) #sample size
  
  x <- sqrt(n) * D #scaled test statistic for large n
  
  if (x == 0) { #if x 0 probability, no difference between distribution, p-value is 1
    p <- 1
  } else if (x > 1.94) { #large x tail
    c <- 2.000071 + 0.331 / sqrt(n) + 1.409 / n #shortcut approx in MTW
    p <- 2 * exp(-c * x^2)  
  } else { 
    p <- 0 #initialise series at 0
    i <- 1 #index starts at 1
    repeat {
      term <- (2 * (-1)^(i - 1) * exp( -2 * i^2 * x^2)) #Limiting form formula from MWT
      p <- p + term #loop over i's  and sum term each time
      if (abs(term) < tol) break #breaking condition if the increment is small enough
      i <- i + 1 #if not loop through next iteration
    }
  }
  
  p <- max(min(p,1), 0)  #make sure p in correct range (bounded w 0-1)
  
  return(list("Test Statistic D" = D, "P-Value" = p)) #print results
}

set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)
ks_test(data) #implement function on data


ks.test(data, "pnorm") #built in version, testing data against normal 



##################### 
# Problem 2
#####################

set.seed (123)
data2 <- data.frame(x = runif(200, 1, 10))
data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5)


# Plot and visualise the distribution
library(ggplot2)
ggplot(data2, aes(x = y)) + geom_density()
ggplot(data2, aes(x = x, y = y)) + geom_point()


##  From Scratch - Create linear log likelihood function ##
linear_lik <- function(y, X, theta) { #output, input, theta = vector of unknown parms
  n      <- nrow(X) #size of sample
  k      <- ncol(X) # number of parameters to estimate
  beta   <- theta[1:k] # vector of coefficients 
  sigma2 <- theta[k+1]^2 # variance
  e      <- y - X%*%beta #errors with mean of Xbeta (as normal distribution)
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl) #as optim function finds min reverse log likelihood
}

# Run MLE using linear likelihood and data
linear_MLE <- optim(fn=linear_lik,  #using scratch function
                    y =data2$y,  #outcome var
                    X =cbind(1, data2$x),  #input var plus 1's for intercept
                    par=c(1,1,1),  #parameter starting points
                    hessian=TRUE, #find standard errors
                    method="BFGS")

linear_MLE$par # see the parameters for intercept, x and sigma2

#Find Standard Errors
H <- linear_MLE$hessian #extract the hessian matrix
cov_matrix <- solve(H) #compute inverse matrix H^-1 (hessian for neg. log likelihood so just take inverse)
se <-sqrt(diag(cov_matrix)) #taking the square root of the covariance matrix for ses
se

par <- c(0.13983, 2.72655,-1.43907) #parameters from model
se <- c(0.25141, 0.04136, 0.07191) #se's
names(par) <- c("Intercept", "x", "Sigma2")
comb <- paste0(par, "\n(", se, ")") #combined in format usual regression
comb #see format of 'estimate (se)'

linear_MLE_output <- data.frame(Parameter = names(par), #make table
      Estimate = comb, #put in estimates as combines above
      stringsAsFactors = FALSE)
xtable::xtable(linear_MLE_output)  #create latex compatible table


## OLS LM
LM <- lm(data2$y~data2$x) #fit linear regression model using OLS
stargazer(LM,
          type = "latex",
          title = "OLS Linear Regession lm")


# GLS Gaussian
GLM_gaussian <- glm(data2$y~data2$x, family = "gaussian") #estimate using MLE with iterataley reweighted least squares
stargazer(GLM_gaussian,
          type = "latex",
          title = "GLM gaussian")

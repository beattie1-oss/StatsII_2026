##################
#### Stats II ####
##################

###############################
#### Tutorial 1: Refresher ####
###############################

# Today's tutorial is a refesher of the R skills we learnt in the first semester.
#     1. Importing data
#     2. Wrangling data
#     3. Analysing data
#     4. Communicating

#### Case study
# A fictional think-tank, the Alliance of Wealthy People who Dislike Tax, has asked
# you to study the relationship between tax, regulation and GDP per capita. They believe
# that countries with low tax and light regulation are wealthier, and they want you to 
# prove it using statistics!:

# We're going to use variables such as: 
# Ease of doing business rank (1=most business-friendly regulations): IC.BUS.EASE.XQ
# GDP per capita (current US$): NY.GDP.PCAP.CD
# "tax revenue (% of GDP)": GC.TAX.TOTL.GD.ZS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#### Importing the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in our libraries.

library(tidyverse)
library(stargazer)

## loading the data
data <- read_csv('tutorial01_data.csv', #must be underscore
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations)` = col_double(),
                   `GDP per capita (current US$)` = col_double(),
                   `Tax revenue (% of GDP)` = col_double()))
str(data) #understand changes to variable data type has suceeded or nah

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type.
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 
  
# 1. First, let's have a look at our data object. Use the functions we learned from last
#    term. 
head(data)
summary(data)
ls(data) #lists column names

# 2. Let's drop the rows and columns we don't need.
# We only have one year, so the two cols related to year can be dropped; also, we only
# really need one col for country name, so let's drop country code too.
data <- data %>% #subset data
  select(-(starts_with('Time')),-('Country Code'))
ls(data) #check that it worked

# 3. Let's also get rid of the variable code in square brackets
#If there were codes
names(data) <- sub("\\[.*","",names(data))

  #hint: try using the function sub() with the regexp " \\[.*"
  
#### Analysing the data
# Now that we have a dataset in the desired format, we can proceed to the analysis.

# 1. Let's perform some preliminary descriptive analysis using our visualisation skills.
#    Try using ggplot to create a plot of scatter showing GDP p/c vs Tax revenue. Add a
#    simple linear regression line.
library(ggplot2)
update_theme(theme_minimal())
theme_minimal()
ggplot(data = data, mapping = aes(y=`GDP per capita (current US$)`, x = `Tax revenue (% of GDP)`)) +
  geom_point() +
  geom_smooth(method = 'lm')
       
  
# 2. Now let's try the same using GDP p/c vs Ease of Doing Business.
ggplot(data = data, mapping = aes(y=`GDP per capita (current US$)`, x = `Ease of doing business rank (1=most business-friendly regulations)`)) +
  geom_point() +
  geom_smooth(method = 'lm')

#higher the ranking the higher GDP per caputa

# 3. And, for the sake of argument, let's see what the relationship is between Tax and
#    Ease of Doing Business.

ggplot(data = data, mapping = aes(y=`Tax revenue (% of GDP)`, x = `Ease of doing business rank (1=most business-friendly regulations)`)) +
  geom_point() +
  geom_smooth(method = 'lm')

# 4. Let's think for a minute before we perform the multivariate regression: what kind
#    of interaction are we seeing with these three plots?


# 5. Now let's run a regression!

formula <- `GDP per capita (current US$)` ~ `Tax revenue (% of GDP)` + `Ease of doing business rank (1=most business-friendly regulations)`
reg1 <- lm(data=data, formula = formula)
summary(reg1)

# How do we interpret these results?
#Tax revenue 0.1 p indicating some evidence supporting its association with GDP

#### Communicating
# The final task is to communicate our results. We're going to do this in pdf format 
# using latex, and then upload our results to github, just as we would with a problem
# set!

# 1. Visualisation
# We want a good visualisation of our results, including a title. We've seen that Ease 
# of Doing Business doesn't seem to have a very significant effect (statistically or
# substantively), so let's plot GDP vs Tax, and include Ease of Doing Business as
# either a size or alpha variable to our scatter points. Use the "export" option in the
# plots window to create a pdf of the plot below. Save it in the same folder as your 
# latex template.

pdf('T1_Plot.pdf')
data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, 
             `GDP per capita (current US$)`, 
             alpha = `Ease of doing business rank (1=most business-friendly regulations)`)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'GDP per capita and Tax Revenue (2019)')
dev.off()

# 2. Regression table
# We'll use stargazer to create the latex code for our regression table. Clear your 
# console, then run the code below.

stargazer(reg1)

# Now all we need is to update the latex template and upload the pdf to github!
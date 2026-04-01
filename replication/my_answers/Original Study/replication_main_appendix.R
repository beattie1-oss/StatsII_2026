#############################################################################################
##### R code for article: Can Warm Behavior Mitigate the Negative Effect of Unfavorable #####
##### Governmental Decisions on Citizens' Trust? (JEPS) #####################################
#############################################################################################

### Journal: Journal of Experimental Political Science
### Author: Frederik Godt Hansen
### Contact: frederik@ps.au.dk

### Loading required packages ###
library(haven)
library(ggpubr)
library(dplyr)

##### Reading data set and save as a new data frame called "data" ########
##### Choose the place on your computer where you have saved #############
##### the data set for this article (uploaded to Dataverse as "data")#####

data <- read_dta(".../data.dta")

### Recoding ###
levels(data$warmth_treatment)[levels(data$warmth_treatment)=="0"] <- "Low warmth"
levels(data$warmth_treatment)[levels(data$warmth_treatment)=="1"]   <- "High warmth"

###############################
##### Manipulation checks #####
###############################

### Perceived favorability ###

data %>% 
group_by(of_treatment)%>%
get_summary_stats(perc_favorability, type = "mean_sd")

pairwise.t.test(data$perc_favorability, data$of_treatment, p.adjust.method="none")

### Perceived warmth ###

data %>% 
group_by(warmth_treatment)%>%
get_summary_stats(perc_warmth, type = "mean_sd")

pairwise.t.test(data$perc_warmth, data$warmth_treatment, p.adjust.method="none")

##################################################
##### Main analysis of experiment -- Table 1 #####
##################################################

### Model 1 ###
reg_trust_bureaucrat <- lm(trust_b ~ of_treatment + warmth_treatment, data=data)
summary(reg_trust_bureaucrat)

### Model 2 ###
reg_trust_municipality <- lm(trust_m ~ of_treatment + warmth_treatment, data=data)
summary(reg_trust_municipality)

### Model 3 ###
int_trust_bureaucrat <- lm(trust_b ~ of_treatment*warmth_treatment, data=data)
summary(int_trust_bureaucrat)

### Model 4 ###
int_trust_municipality <- lm(trust_m ~ of_treatment*warmth_treatment, data=data)
summary(int_trust_municipality)

######################################################
##### Barplot of interaction effects -- Figure 1 ##### 
######################################################

### Helper function ### 

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

### Using the treatments as factor variables ###

data$warmth_treatment <- as.factor(data$warmth_treatment)
data$of_treatment <- as.factor(data$of_treatment)

### Mean of "trust in bureaucrat" by experimental group ### 

sum1 <- summarySE(data, measurevar="trust_b", groupvars=c("of_treatment", "warmth_treatment"), na.rm=T)
sum1

### Panel A of Figure 1 ###

p <- ggplot(sum1, aes(x=of_treatment, y=trust_b, fill=of_treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=trust_b-ci, ymax=trust_b+ci),
                  width=.05, position=position_dodge(.9)) + theme_bw() +
	labs(x="", y="Trust in bureaucrat") + 
	scale_fill_grey(start=0.6, end=0.3, name="", labels=c("Unfavorable", "Favorable")) +
	scale_x_discrete(labels=c("0" = "Unfavorable outcome", "1" = "Favorable outcome")) +
	theme(legend.position="none")
	

plot1 <- p + facet_grid(cols=vars(warmth_treatment)) + annotate("text", x=1.5, y=.67, label="***") +
annotate("segment", x = 1, xend = 2, y = .66, yend = .66, colour = "black")
plot1

### Mean of "trust in municipality" by experimental group ###

sum2 <- summarySE(data, measurevar="trust_m", groupvars=c("of_treatment", "warmth_treatment"), na.rm=T)
sum2

### Panel B of Figure 1 ###

p1 <- ggplot(sum2, aes(x=of_treatment, y=trust_m, fill=of_treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=trust_m-ci, ymax=trust_m+ci),
                  width=.05, position=position_dodge(.9)) + theme_bw() + 
	labs(fill="OF treatment", x="", y="Trust in municipality") + 
	scale_fill_grey(start=0.6, end=0.3, name="", labels=c("Unfavorable", "Favorable")) + 
	scale_x_discrete(labels=c("0" = "Unfavorable outcome", "1" = "Favorable outcome")) +
	theme(legend.position="none")

plot2 <- p1 + facet_grid(cols=vars(warmth_treatment)) + annotate("text", x=1.5, y=.67, label="***") +
annotate("segment", x = 1, xend = 2, y = .66, yend = .66, colour = "black") 
plot2

### Final Figure 1 -- combining Panel A and Panel B ###

figure1 <- ggarrange(plot1, plot2, labels=c("A", "B"), ncol = 1, nrow = 2)
figure1 

#pdf("fig1.pdf")
#figure1
#dev.off()

#############################################################################
################################## APPENDIX #################################
#############################################################################

#######################################
##### Power analysis -- Figure C1 #####
#######################################

library(pwr)

# range of correlations
r <- seq(.1,.5,.01)
nr <- length(r)

# power values
p <- seq(.8,.1)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.t.test(n = NULL, d = r[j],
    sig.level = .05, power = p[i],
    alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
p <- plot(xrange, yrange, type="n",
  xlab="Minimum detectable effect size",
  ylab="Sample Size per experimental group" )

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
p + abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
   col="grey89")
title("Sample Size Estimation")


###############################################
##### Descriptive statistics -- Table D1 ######
###############################################

mean(data$gender)
sd(data$gender)

summary(as.factor(data$age))

mean(data$ideology, na.rm=T)
sd(data$ideology, na.rm=T)

summary(as.factor(data$education_rc))

mean(data$pubsector_pref, na.rm=T)
sd(data$pubsector_pref, na.rm=T)


###################################################################
##### Test of randomization/balance in covariates -- Table D2 #####
###################################################################

### Gender ###
data %>% 
group_by(treatment_groups)%>%
get_summary_stats(gender, type = "mean_sd")

pairwise.t.test(data$gender, data$treatment_groups, p.adjust.method="none")

### Age ###
data %>% 
group_by(treatment_groups)%>%
get_summary_stats(age_1, type = "mean_sd")

pairwise.t.test(data$age_1, data$treatment_groups, p.adjust.method="none")

### Education ###
data %>% 
group_by(treatment_groups)%>%
get_summary_stats(edu, type = "mean_sd")

pairwise.t.test(data$edu, data$treatment_groups, p.adjust.method="none")

### Ideology ###
data %>% 
group_by(treatment_groups)%>%
get_summary_stats(ideology, type = "mean_sd")

pairwise.t.test(data$ideology, data$treatment_groups, p.adjust.method="none")

### Public sector preference ###
data %>% 
group_by(treatment_groups)%>%
get_summary_stats(pubsector_pref, type = "mean_sd")

pairwise.t.test(data$pubsector_pref, data$treatment_groups, p.adjust.method="none")

####################################################################
##### Heterogenous effects: "user" of elderly care -- Table E1 #####
####################################################################

### Model 1 ###
user1 <- lm(trust_b ~ warmth_treatment*user + gender + as.factor(age) +
as.factor(education_rc) + ideology + pubsector_pref + of_treatment, data=data)

summary(user1)

### Model 2 ###
user2 <- lm(trust_m ~ warmth_treatment*user + gender + as.factor(age) +
as.factor(education_rc) + ideology + pubsector_pref + of_treatment, data=data)

summary(user2)

### Model 3 ###
user3 <- lm(trust_b ~ of_treatment*user + gender + as.factor(age) +
as.factor(education_rc) + ideology + pubsector_pref + of_treatment + warmth_treatment, data=data)

summary(user3)

### Model 4 ###

user4 <- lm(trust_m ~ of_treatment*user + gender + as.factor(age) +
as.factor(education_rc) + ideology + pubsector_pref + of_treatment + warmth_treatment, data=data)

summary(user4)


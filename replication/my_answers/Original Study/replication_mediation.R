####################################################################################################
##### R code for causal mediation analysis (Appendix F) of article: Can Warm Behavior Mitigate #####
##### the Negative Effect of Unfavorable Governmental Decisions on Citizens' Trust? (JEPS) #########
####################################################################################################

### Journal: Journal of Experimental Political Science
### Author: Frederik Godt Hansen
### Contact: frederik@ps.au.dk

#################################################################################
##### Causal mediation analysis: perceived fairness as mediator -- Table F1 #####
#################################################################################

### Loading required packages ###

library(haven)
library(dplyr)
library(mediation)

### Reading data set and save as a new data frame called "data" ######
### Choose the place on your computer where you have saved ###########
### the data set for this article (uploaded to Dataverse as "data")###

data <- read_dta(".../data.dta")

data$age <- as.factor(data$age)
data$education_rc <- as.factor(data$education_rc)


###################################################################################
##### Causal mediation analysis: perceived competence as mediator -- Table F1 #####
###################################################################################

### Trust in bureaucrat as outcome ###

### Estimation of mediation ###
mdata1 <- data %>% dplyr::select(trust_b, warmth_treatment, perc_fairness, 
gender, age, education_rc, ideology, pubsector_pref)
mdata1 <- na.omit(mdata1)

### Model 1 in Table F1 ###
med.fit1 <- lm(perc_fairness ~ warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data = mdata1)
summary(med.fit1)

### Model 2 in Table F1 ###
out.fit1 <- lm(trust_b ~ perc_fairness + warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data=mdata1)
summary(out.fit1)

### Trust in municipality as outcome ###

### Estimation of mediation ###
mdata2 <- data %>% dplyr::select(trust_m, warmth_treatment, perc_fairness,
gender, age, education_rc, ideology, pubsector_pref)
mdata2 <- na.omit(mdata2)

### Model 3 in Table F1 ###
med.fit2 <- lm(perc_fairness ~ warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data = mdata2)
summary(med.fit2)

### Model 4 in Table F1 ###
out.fit2 <- lm(trust_m ~ perc_fairness + warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data=mdata2)
summary(out.fit2)

###################################################################################
##### Causal mediation analysis: perceived competence as mediator -- Table F2 #####
###################################################################################

### Trust in bureaucrat as outcome ###

# Estimation of mediation
mdata3 <- data %>% dplyr::select(trust_b, warmth_treatment, perc_competence,
gender, age, education_rc, ideology, pubsector_pref)
mdata3 <- na.omit(mdata3)

### Model 1 in Table F2 ###
med.fit3 <- lm(perc_competence ~ warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data=mdata3)
summary(med.fit3)

### Model 2 in Table F2 ###
out.fit3 <- lm(trust_b ~ perc_competence + warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data=mdata3)
summary(out.fit3)

### Trust in municipality as outcome ###

### Estimation of mediation ###
mdata4 <- data %>% dplyr::select(trust_m, warmth_treatment, perc_competence,
gender, age, education_rc, ideology, pubsector_pref)
mdata4 <- na.omit(mdata4)

### Model 3 in Table F2 ### 
med.fit4 <- lm(perc_competence ~ warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data=mdata4)
summary(med.fit4)

### Model 4 in Table F2 ###
out.fit4 <- lm(trust_m ~ perc_competence + warmth_treatment + gender + age + 
education_rc + ideology + pubsector_pref, data=mdata4)
summary(out.fit4)

####################
##### Table F3 #####
####################

### Results for upper left corner of Table F3 ###
set.seed(20210325)
med.out.fair_trustb <- mediate(med.fit1, out.fit1, treat = "warmth_treatment", 
mediator = "perc_fairness", sims = 1000, boot = TRUE)

summary(med.out.fair_trustb)

### Results for lower left panel of Table F3 ###
set.seed(20210325)
med.out.fair_trustm <- mediate(med.fit2, out.fit2, treat = "warmth_treatment", 
mediator = "perc_fairness", sims = 1000, boot = TRUE)

summary(med.out.fair_trustm)

### Results for upper right panel of Table F3 ###
set.seed(20210325)
med.out.comp_trustb <- mediate(med.fit3, out.fit3, treat ="warmth_treatment",
mediator = "perc_competence", sims = 1000, boot = TRUE)

summary(med.out.comp_trustb)

### Results for lower right panel of Table F3 ###
set.seed(20210325)
med.out.comp_trustm <- mediate(med.fit4, out.fit4, treat ="warmth_treatment",
mediator = "perc_competence", sims = 1000, boot = TRUE)

summary(med.out.comp_trustm)

#####################
##### Figure F1 #####
#####################

### Upper left panel of Figure F1 ###
plot(med.out.fair_trustb)

### Results for upper right panel of Figure F1 ###
plot(med.out.fair_trustm)

### Results for lower left panel of Figure F1 ###
plot(med.out.comp_trustb)

### Results for lower right panel of Figure F1 ###
plot(med.out.comp_trustm)

### Combining plots for Figure F1 ###

#pdf("mediationplots.pdf")

par(mar=c(3,4,3,1.5))
par(mfrow=c(2,2))

plot(med.out.fair_trustb, xlim=c(-0.10, 0.15), main = "Trust in bureaucrat") 
mtext(3, text= "Mediator: perceived fairness", cex = .8)

plot(med.out.fair_trustm, xlim=c(-0.10, 0.15), main = "Trust in municipality")
mtext(3, text= "Mediator: perceived fairness", cex = .8)

plot(med.out.comp_trustb, xlim=c(-0.10, 0.15), main = "Trust in bureaucrat")
mtext(3, text= "Mediator: perceived competence", cex = .8)

plot(med.out.comp_trustm, xlim=c(-0.10, 0.15), main = "Trust in municipality")
mtext(3, text= "Mediator: perceived competence", cex = .8)

#dev.off()

#####################
##### Figure F2 #####
#####################

### Estimation of sensitivity ###
set.seed(20210325)
sens.out.fair_trustb <- medsens(med.out.fair_trustb, rho.by=0.01, effect.type="indirect", sims=1000)
summary(sens.out.fair_trustb)

### Left panel of Figure F2 ###
plot(sens.out.fair_trustb)

### Estimation of sensitivity ###
set.seed(20210325)
sens.out.fair_trustm <- medsens(med.out.fair_trustm, rho.by=0.01, effect.type="indirect", sims=1000)

summary(sens.out.fair_trustm)

### Right panel of Figure F2 ###
plot(sens.out.fair_trustm)

### Combining plots for Figure F2 ###

#pdf("sensitivityplot1.pdf")
par(mar=c(4,4,3,1.5))
par(mfrow=c(1,2))

plot(sens.out.fair_trustb, main = "Trust in bureaucrat") 
mtext(3, text= "Mediator: perceived fairness", cex = .8)

plot(sens.out.fair_trustm, main = "Trust in municipality")
mtext(3, text= "Mediator: perceived fairness", cex = .8)

#dev.off()

#####################
##### Figure F3 #####
#####################

### Estimation of sensitivity ###
set.seed(20210325)
sens.out.comp_trustb <- medsens(med.out.comp_trustb, rho.by = 0.01, effect = "indirect", sims = 1000)
summary(sens.out.comp_trustb)

### Results for left panel of Figure F3 ###
plot(sens.out.comp_trustb)

### Estimation of sensitivity ###
set.seed(20210325)
sens.out.comp_trustm <- medsens(med.out.comp_trustm, rho.by = 0.01, effect = "indirect", sims = 1000)
summary(sens.out.comp_trustm)

### Results for right panel of Figure F3 ###
plot(sens.out.comp_trustm)

### Combining plots for Figure F3 ###

#pdf("sensitivityplot2.pdf")
par(mar=c(4,4,3,1.5))
par(mfrow=c(1,2))

plot(sens.out.comp_trustb, main = "Trust in bureaucrat")
mtext(3, text= "Mediator: perceived competence", cex = .8)

plot(sens.out.comp_trustm, main = "Trust in municipality")
mtext(3, text= "Mediator: perceived competence", cex = .8)

#dev.off()





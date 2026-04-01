##### R code for article: Can Warm Behavior Mitigate the Negative Effect of Unfavorable #####
##### Governmental Decisions on Citizens' Trust? (JEPS) #####################################
#### Ellen Beattie Replication Project
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
lapply(c("stargazer", "modelsummary","dplyr","tidyverse", "ggplot2", "ggpubr", "readr", "readxl", "showtext", "purrr", 
        "xtable", "ggrepel", "extrafont", "WDI", "ggtext", "haven", "knitr", "tidybayes",
         "forecats", "patchwork", "tidytext", "gutenbergr", "pwr", "emmeans",
        "ordbetareg","brms", "cmdstanr", "texreg", "effectsize", "broom", "broom.mixed", "kableExtra"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



### Loading data
data <- read_dta("data.dta")

### Recoding ###
levels(data$warmth_treatment)[levels(data$warmth_treatment)=="0"] <- "Low warmth" #rename
levels(data$warmth_treatment)[levels(data$warmth_treatment)=="1"]   <- "High warmth"
colnames(data) # Columns

data %>%
  dplyr::select(caseid, treatment_groups, of_treatment, warmth_treatment, trust_b, trust_m, gender) %>%
  dplyr::slice_head(n = 10) %>%
  knitr::kable(format = "latex", booktabs = TRUE)
sort(unique(data$trust_b)) # Explore outcome variable
sort(unique(data$trust_m))

###############################
##### Manipulation checks #####
###############################

### Perceived favorability ###

data %>% 
group_by(of_treatment)%>% #0 for unfavourable, 1 for favourble
get_summary_stats(perc_favorability, type = "mean_sd")

pairwise.t.test(x = data$perc_favorability, g = data$of_treatment, p.adjust.method="none")
#grouped by treatment type of favourability condition are the means statistically different


### Perceived warmth ###

data %>% 
group_by(warmth_treatment)%>%
get_summary_stats(perc_warmth, type = "mean_sd")

pairwise.t.test(x = data$perc_warmth,g = data$warmth_treatment, p.adjust.method="none")
# is the warmth manipulation statistically overall perceived warmth difference 




##################################################
##### Main analysis of experiment -- Table 1 #####
##################################################

reg_trust_bureaucrat <- lm(trust_b ~ of_treatment + warmth_treatment, data=data)
cohens_d(trust_b ~ of_treatment, data = data) #matching papers 

reg_trust_municipality <- lm(trust_m ~ of_treatment + warmth_treatment, data=data)
cohens_d(trust_m ~ of_treatment, data = data)

int_trust_bureaucrat <- lm(trust_b ~ of_treatment*warmth_treatment, data=data)

int_trust_municipality <- lm(trust_m ~ warmth_treatment*warmth_treatment, data=data)

orignal_reg_models <- list(reg_trust_bureaucrat, reg_trust_municipality, int_trust_bureaucrat, int_trust_municipality)

# Print to Latex
stargazer(orignal_reg_models,
          title = "Experimental Results",
          covariate.labels = c("Outcome favorability (OF)", 
                               "Warmth",
                               "OF * Warmth"),
          dep.var.labels = c("\\shortstack{Trust \\\\ Bureaucrat}", "\\shortstack{Trust \\\\ Municipality}",
                             "\\shortstack{Trust \\\\ Bureaucrat}", "\\shortstack{Trust \\\\ Municipality}"),
          type = "latex")


######################################################
##### Barplot of interaction effects -- Figure 1 ##### 
#####################################################
### Helper function ### 
#essentially splits the group and calculates N, mean,se,sd, ci stats for each of them 
summarySE <- function(data=NULL, 
                      measurevar, groupvars=NULL, na.rm=FALSE,
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
    datac <- rename(datac, c("mean" = measurevar)) # Rename the "mean" column  
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
                  width=.05, position=position_dodge(.9)) + theme_bw(base_family = "CMU Sans Serif") +
	labs(x="", y="Trust in bureaucrat") + 
	scale_fill_manual(values = c("#c83131", "#4571cf"), name="", labels=c("Unfavorable", "Favorable")) +
	scale_x_discrete(labels=c("0" = "Unfavorable outcome", "1" = "Favorable outcome")) +
	theme(legend.position="none")

p
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
                  width=.05, position=position_dodge(.9)) + theme_bw(base_family = "CMU Sans Serif") + 
	labs(fill="OF treatment", x="", y="Trust in municipality") + 
	scale_fill_manual(values = c("#c83131", "#4571cf"), name="", labels=c("Unfavorable", "Favorable")) + 
	scale_x_discrete(labels=c("0" = "Unfavorable outcome", "1" = "Favorable outcome")) +
	theme(legend.position="none")

p1
plot2 <- p1 + facet_grid(cols=vars(warmth_treatment)) + annotate("text", x=1.5, y=.67, label="***") +
annotate("segment", x = 1, xend = 2, y = .66, yend = .66, colour = "black") 
plot2

### Final Figure 1 -- combining Panel A and Panel B ###
figure1 <- ggarrange(plot1, plot2, labels=c("A", "B"), ncol = 1, nrow = 2)
figure1 

ggsave(figure1, filename = "Figures/figure1.pdf", device = cairo_pdf,
       width = 6, height = 6, units = "in", bg = "transparent")







############################################ 196
############## My Contribution ##################
###########################################

### Transform data to original scale value
data <- data %>%
  mutate(
    trust_B = as.integer(round(trust_b * 10)),
    trust_M = as.integer(round(trust_m * 10))
  )

all_model_names <-c("\\shortstack{Trust \\\\ Bureaucrat}", "\\shortstack{Trust \\\\ Municipality}",
                    "\\shortstack{Trust \\\\ Bureaucrat}", "\\shortstack{Trust \\\\ Municipality}")

########################################
####### Re-scaled OLS #################
########################################
reg_ols_B <- lm(trust_B ~ of_treatment + warmth_treatment, data=data)
reg_ols_M <- lm(trust_M ~ of_treatment + warmth_treatment, data=data)
int_ols_B <- lm(trust_B ~ of_treatment*warmth_treatment, data=data)
int_ols_M <- lm(trust_M ~ of_treatment*warmth_treatment, data=data)

ols_models <- list(reg_ols_B, reg_ols_M, int_ols_B, int_ols_M)

# OLS Coefficients #
stargazer(ols_models,
          title = "Experimental Results - OLS Originally Coded",
          covariate.labels = c("Outcome favorability (OF)", 
                               "Warmth",
                               "OF * Warmth"),
          dep.var.labels = all_model_names,
          type = "latex")


##################################
####### Ordered Logit ############
###################################
reg_logit_B <- MASS::polr(as.factor(trust_B) ~ of_treatment + warmth_treatment, data=data, Hess = TRUE)
reg_logit_M <- MASS::polr(as.factor(trust_M) ~ of_treatment + warmth_treatment, data=data, Hess = TRUE)
int_logit_B <- MASS::polr(as.factor(trust_B) ~ of_treatment * warmth_treatment, data=data, Hess = TRUE)
int_logit_M <- MASS::polr(as.factor(trust_M) ~ of_treatment * warmth_treatment, data=data, Hess = TRUE)

logit_models <- list(reg_logit_B, reg_logit_M, int_logit_B, int_logit_M)
class(reg_logit_B)


## Ordered Multinomia Coeffients ##
texreg(logit_models,
       custom.model.names = all_model_names,
       caption = "Experimental Results - Ordinal Logit Models",
       include.cutpoints = FALSE, #just want the coefficients
       float.pos = "htbp",
       single.row = TRUE,
       digits = 3,
       use.packages = FALSE) #copy paste into latex, manually remove intercept section for cleanliness


## Ordered Multinomial Predicted probabilities / values ##
data <- data %>%
  mutate(
    of_treatment = factor(of_treatment, levels = c(0,1)),
    warmth_treatment = factor(warmth_treatment, levels = c(0,1))
  )

newdata <- expand.grid(
  of_treatment = factor(c("0","1"), levels = c("0","1")),
  warmth_treatment = factor(c("0","1"), levels = c("0","1"))
) %>%
  mutate(
    group = case_when(
      of_treatment == "0" & warmth_treatment == "0" ~ "Unfavourable / Low Warmth",
      of_treatment == "1" & warmth_treatment == "0" ~ "Favourable / Low Warmth",
      of_treatment == "0" & warmth_treatment == "1" ~ "Unfavourable / High Warmth",
      of_treatment == "1" & warmth_treatment == "1" ~ "Favourable / High Warmth"
    )
  )

#Helper function to get predicted probabilities and trust values per group 
get_pred_probs <- function(model, label) {
  # get predicted probabilities
  probs <- predict(model, newdata = newdata, type = "probs")
  probs_df <- as.data.frame(probs) # convert matrix to data frame and keep trust category names
 
  df <- cbind(newdata, probs_df) %>%  # combine with newdata
    mutate(model = label)
  
  categories <- as.numeric(colnames(probs_df))  # compute expected trust
  df$pred_trust <- rowSums(probs_df * matrix(categories, nrow=nrow(probs_df), ncol=length(categories), byrow=TRUE))
  
  return(df)
}

#Join models pp together
pred_all <- bind_rows(
  get_pred_probs(reg_logit_B, "reg_logit_B"),
  get_pred_probs(reg_logit_M, "reg_logit_M"),
  get_pred_probs(int_logit_B, "int_logit_B"),
  get_pred_probs(int_logit_M, "int_logit_M")
)

# Predicted values
pred_trust_table <- pred_all %>%
  dplyr::select(model, group, pred_trust) %>%
  pivot_wider(names_from = model, values_from = pred_trust) %>%
  dplyr::arrange(factor(group, levels = c(
    "Unfavourable / Low Warmth",
    "Favourable / Low Warmth",
    "Unfavourable / High Warmth",
    "Favourable / High Warmth"
  )))

colnames(pred_trust_table) <- c("Group", "Trust B", "Trust M", "Trust B (int)", "Trust M (int)")

# Export table of predicted average 
xtable(pred_trust_table, digits = 2, caption = "Predicted Trust by Experimental Group and Model")

pred_trust_plot <- pred_trust_table %>%
  pivot_longer(
    cols = -Group,
    names_to = "Model",
    values_to = "Predicted_Trust")

OMR_pred_trust <- ggplot(pred_trust_plot, aes(x = Group, y = Predicted_Trust, fill = Model)) +
  geom_col(position = "dodge") +   # sidebyside bars per group
  scale_y_continuous(breaks = 1:7) +
  labs(x = "Experimental Group",y = "Predicted Trust (0-10 Scale)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_family = "CMU Sans Serif") +
  theme(axis.text.x = element_text(size = 10, angle = 20, hjust = 0.5),
        legend.position = "top") 

ggsave(OMR_pred_trust, filename = "Figures/OMR_pred_trust.pdf", device = cairo_pdf,
       width = 8, height = 6, units = "in", bg = "transparent")


############################################## 331
############# Ordered Beta #################
############################################

library(cmdstanr)
library(brms)
library(ordbetareg)
cmdstanr::cmdstan_path()
set.seed(123) 

reg_beta_b <- ordbetareg::ordbetareg(trust_b ~ of_treatment + warmth_treatment, data = data)
reg_beta_m <- ordbetareg::ordbetareg(trust_m ~ of_treatment + warmth_treatment, data = data)
int_beta_b <- ordbetareg::ordbetareg(trust_b ~ of_treatment * warmth_treatment, data = data)
int_beta_m <- ordbetareg::ordbetareg(trust_m ~ of_treatment * warmth_treatment, data = data)

beta_models <- list(reg_beta_b, reg_beta_m,  int_beta_b, int_beta_m)

# Ordered Beta Regression Coefficients
summary(reg_beta_b) ## difficulties with exporting so just c+p
summary(reg_beta_m)
summary(int_beta_b)
summary(int_beta_m)


# Predicted means from models
extract_means <- function(model, outcome_name) {
  newdata <- expand.grid(of_treatment = c(0,1), warmth_treatment = c(0,1))
  
  preds <- posterior_epred(model, newdata = newdata) #using posteriod prevws
  preds <- as.matrix(preds)
  colnames(preds) <- c("of0_warmth0", "of1_warmth0", "of0_warmth1", "of1_warmth1")
  df <- as.data.frame(preds)
  
  summarise_group <- function(x, prefix) {
    x <- as.numeric(x)
    tibble(
      !!paste0(prefix, "_mean") := mean(x, na.rm = TRUE),
      !!paste0(prefix, "_low")  := quantile(x, 0.025, na.rm = TRUE),
      !!paste0(prefix, "_high") := quantile(x, 0.975, na.rm = TRUE)
    )
  }
  result <- bind_cols(
    summarise_group(df$of0_warmth0, "of0_warmth0"),
    summarise_group(df$of1_warmth0, "of1_warmth0"),
    summarise_group(df$of0_warmth1, "of0_warmth1"),
    summarise_group(df$of1_warmth1, "of1_warmth1")
  ) %>%
    dplyr::mutate(outcome = outcome_name) %>%
    dplyr::select(outcome, everything())
  
  return(result)
}


#Applying Function to find predicted means of models  #
means_list <- list(
  "Trust Bureaucrat" = extract_means(reg_beta_b, "Trust Bureaucrat"),
  "Trust Municipality" = extract_means(reg_beta_m, "Trust Municipality"),
  "Trust Bureaucrat (int)" = extract_means(int_beta_b, "Trust Bureaucrat (int)"),
  "Trust Municipality (int)" = extract_means(int_beta_m, "Trust Municipality (int)")
)

# Mini Function for formatting mean and cI's
format_ci <- function(mean, low, high) {
  sprintf("%.2f [%.2f, %.2f]", mean, low, high)
}

# Convert each tibble into long format with treatment labels
means_long <- lapply(names(means_list), function(name) {
  df <- means_list[[name]]
  df_long <- df %>%
    dplyr::select(-outcome) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("treatment", ".value"),
      names_pattern = "(.*)_(mean|low|high)"
    ) %>%
    mutate(model = name) %>%
    mutate(display = format_ci(mean, low, high))
  
  df_long
}) %>% bind_rows()

# Create comparison table with the predicted means in long form
comparison_table <- means_long %>%
  dplyr::select(treatment, model, display) %>%
  pivot_wider(names_from = model, values_from = display) %>%
  dplyr::arrange(treatment)

#Neaten the experimengt group labels
comparison_table <- comparison_table %>%
  mutate(
    treatment = recode(treatment,
                       "of0_warmth0" = "Unfavourable / Low Warmth",
                       "of1_warmth0" = "Favourable / Low Warmth",
                       "of0_warmth1" = "Unfavourable / High Warmth",
                       "of1_warmth1" = "Favourable / High Warmth")
  )

#Export Table
kable(comparison_table, format = "latex", booktabs = TRUE)



#Plot
# Clean labels first in means long this time
means_long <- means_long %>%
  mutate(
    treatment = recode(treatment,
                       "of0_warmth0" = "Unfavourable / Low Warmth",
                       "of1_warmth0" = "Favourable / Low Warmth",
                       "of0_warmth1" = "Unfavourable / High Warmth",
                       "of1_warmth1" = "Favourable / High Warmth"
    )
  )

# Group by outcome for plotting comparison
means_long <- means_long %>%
  mutate(
    outcome = case_when(
      str_detect(model, "Bureaucrat") ~ "Trust in Bureaucrat", #detect string from statemtn
      str_detect(model, "Municipality") ~ "Trust in Municipality"
    ),
    model_type = case_when(
      model %in% c("Trust Bureaucrat (int)", "Trust Municipality (int)") ~ "Interaction model",
      TRUE ~ "Main effects model"
    )
  )

#Predicted Mean Plot
betas_predicted_means <- ggplot(means_long, aes(x = treatment, y = mean, color = model_type)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = position_dodge(width = 0.4),
                width = 0.2) +
  facet_wrap(~ outcome, nrow = 2) +
  labs(
    x = "Treatment Group",
    y = "Predicted Mean Trust (0-1 Scale)",
    color = "Model type"
  ) +
  theme_minimal(base_family = "CMU Sans Serif") +
  theme(legend.position = "top",
    axis.text.x = element_text(angle = 20, hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )
betas_predicted_means
#Save plot
ggsave(betas_predicted_means, filename = "Figures/betas_predicted_means.pdf", device = cairo_pdf,
       width = 8, height = 6, units = "in", bg = "transparent")






## Ordered Betas Marginal Effect of Warmth
calc_warmth_marginal <- function(model, outcome_label) {
  
  newdata <- expand.grid(of_treatment = c(0,1), warmth_treatment = c(0,1))
  preds <- posterior_epred(model, newdata = newdata)  # draws each conditions
  
  # marginal effect is warmth=1 minus warmth=0 for each of_treament condition
  n_draws <- nrow(preds) 
  diff_0 <- preds[,2] - preds[,1]  # diff w1-w0 for OF=0
  diff_1 <- preds[,4] - preds[,3]  # diff w1-w0 for OF=1
  
  df <- data.frame(
    of_treatment = c(0,1),
    mean = c(mean(diff_0), mean(diff_1)),
    low  = c(quantile(diff_0, 0.025), quantile(diff_1, 0.025)),
    high = c(quantile(diff_0, 0.975), quantile(diff_1, 0.975)),
    outcome = outcome_label
  )
  return(df)
}

#Bind the rows are marginal effect calc for different models
me_warmth <- bind_rows(
  calc_warmth_marginal(reg_beta_b, "Trust Bureaucrat"),
  calc_warmth_marginal(reg_beta_m, "Trust Municipality"),
  calc_warmth_marginal(int_beta_b, "Trust Bureaucrat (Interaction)"),
  calc_warmth_marginal(int_beta_m, "Trust Municipality (Interaction)")
)

# Format CI to compact it
me_warmth <- me_warmth %>%
  mutate(effect_ci = sprintf("%.2f [%.2f, %.2f]", mean, low, high))

# Wide format to have c
me_warmth_wide <- me_warmth %>%
  dplyr::select(of_treatment, outcome, effect_ci) %>%
  pivot_wider(names_from = outcome, values_from = effect_ci) %>%
  arrange(of_treatment)

me_warmth_wide %>%
  kable(format = "latex", booktabs = TRUE,
        caption = "Marginal Effects of Warmth Treatment by OF Treatment and Model") 






#Plot Marginal Effects of Warmth
betas_me_warmth <- ggplot(me_warmth, aes(x = of_treatment, y = mean)) +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "skyblue", alpha = 0.2) +  # CI 
  geom_line(color = "skyblue", size = 1) +    # vis change direction                                 
  geom_point(color = "skyblue", size = 2) +  #points for the mean                               
  scale_x_continuous(breaks = c(0,1), labels = c("0", "1")) +
  ylab("Marginal Effect of Warmth") +
  xlab("OF Treatment") +
  facet_wrap(~ outcome, ncol = 2) + #for each model                                            
  theme_minimal(base_family = "CMU Sans Serif") +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) 


betas_me_warmth
#Save plot
ggsave(betas_me_warmth, filename = "Figures/betas_me_warmth.pdf", device = cairo_pdf,
       width = 8, height = 6, units = "in", bg = "transparent")

#########################################


#Mean summary plot for experimental groups
summary_combined <- sum1 %>%
  select(of_treatment, warmth_treatment, trust_b) %>%
  left_join(sum2 %>% select(of_treatment, warmth_treatment, trust_m),
            by = c("of_treatment", "warmth_treatment")
  )
summary_final <- summary_combined %>%
  mutate(group = case_when(
    of_treatment == 0 & warmth_treatment == 0 ~ "Unfavourable / Low Warmth",
    of_treatment == 1 & warmth_treatment == 0 ~ "Favourable / Low Warmth",
    of_treatment == 0 & warmth_treatment == 1 ~ "Unfavourable / High Warmth",
    of_treatment == 1 & warmth_treatment == 1 ~ "Favourable / High Warmth"
  )) %>% select(group, trust_b, trust_m) %>%
  dplyr::rename(`Experimental Group` = group,`Trust b` = trust_b,`Trust m` = trust_m
  )

kable(summary_final, format = "latex", booktabs = TRUE, digits = 3)

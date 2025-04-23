# Install and load the following packages
library (tidyverse)
library (nlme)
library (lme4)
library (emmeans)
library (car)

# Import the suckling_data dataset
# Update the file path based on where you have saved the datafile
suckling_data <- read.csv("./data/suckling_data.csv")

# Correct variables
suckling_data$dam <- as.factor(suckling_data$dam)
suckling_data$calf_id <- as.factor(suckling_data$calf_id)
suckling_data$dam_present <- as.factor(suckling_data$dam_present)
suckling_data$other_calves<- as.factor(suckling_data$other_calves)
suckling_data$heifer_calf <- as.factor(suckling_data$heifer_calf)
suckling_data$calf_age <- as.numeric(suckling_data$calf_age)
suckling_data$allosuckling <- as.factor(suckling_data$allosuckling)
suckling_data$study <- as.factor(suckling_data$study)

# For all analyses, filter for suckling bouts
suckling_data_2 <- suckling_data %>%
  filter(behavior == "suckling_bout")

# Set global contrast options prior to running all models
# We will use contr.sum for running models to obtain F-statistics and P-values
# We will use contr.treatment for estimates and SE reported in tables
options(contrasts = c("contr.sum", "contr.poly"))
options(contrasts = c("contr.treatment", "contr.poly"))

# COW-DRIVEN SYSTEM ------------------------------------------------------------
## a) Total SB (no bouts/d) ----------------------------------------------------

# First, we need to sum the number of SB per calf and age
# We will also sum the total suckling time for the next model
all_daily <- suckling_data_2 %>%
  group_by(calf_id, calf_age) %>%
  summarise(total_sb = n(),
            total_min_suckling = (sum(event_length_s))/60,
            heifer_calf = first(heifer_calf),
            study = first(study))

cow_sb_total <- lmer(total_sb ~ calf_age + heifer_calf + (1|calf_id),
                    data = all_daily %>% filter(study == "cow-driven"))

# Model estimates and SE
# Make sure to run model with contr.treatment first (line 30)
# This way, model estimates will be easiest to interpret, with each
# factor level compared to a baseline level
summary(cow_sb_total)

# Main effects
# Make sure to re-run model with contr.sum first (line 29)
# This way, we can specify a Type 3 sums of squares test and obtain
# p-values that are correct
Anova(cow_sb_total, type = 3, test = "F")

# Check residuals for final model
qqnorm(residuals(cow_sb_total))
qqline(residuals(cow_sb_total))
plot(cow_sb_total)

# Least square means of fixed effects
emmeans(cow_sb_total, pairwise ~ heifer_calf)

# Calculate the intra-class correlation coefficient (ICC)
# This is calculated by first extracting the variance of random effects and residual variance from the model output,
# then dividing the variance of random effects by the sum of the two variances.
cow_sb_total_ICC <- (0.653/(0.653+1.454))
print(cow_sb_total_ICC, 2)

## b) SB duration (s/bout) -----------------------------------------------------
cow_sb_duration <- lmer(event_length_s ~ calf_age + allosuckling  + heifer_calf + (1|calf_id),
                       data = suckling_data_2 %>% filter(study == "cow-driven"))

summary(cow_sb_duration) #Remember to run line 30 & model first
Anova(cow_sb_duration, type = 3, test = "F") #Remember to run line 29 & model first

# Check residuals
qqnorm(residuals(cow_sb_duration))
qqline(residuals(cow_sb_duration))
plot(cow_sb_duration)

# Least square means of fixed effects
emmeans(cow_sb_duration, pairwise ~ heifer_calf)
emmeans(cow_sb_duration, pairwise ~ allosuckling)

# ICC
cow_sb_duration_ICC <- (9301)/(9301+71315)
print(cow_sb_duration_ICC, 2)

## c) Total suckling time (min/d) ------------------------------------------- 
cow_suckling_min_total <- lmer(total_min_suckling ~ calf_age + heifer_calf + (1|calf_id),
                              data = all_daily %>% filter(study == "cow-driven"))

summary(cow_suckling_min_total) #Remember to run line 30 & model first
Anova(cow_suckling_min_total, type = 3, test = "F") #Remember to run line 29 & model first

# Check residuals for final model
qqnorm(residuals(cow_suckling_min_total))
qqline(residuals(cow_suckling_min_total))
plot(cow_suckling_min_total)

# Least square means of fixed effects
emmeans(cow_suckling_min_total, pairwise ~ heifer_calf)
        
# ICC
cow_suckling_total_ICC <- (56.83/(56.83 +217.87))
print(cow_suckling_total_ICC, 2)

## d) Allosuckling (1/0) -------------------------------------------------------
cow_as_glmm <- glmer(allosuckling ~ calf_age + other_calves + birth_bw + heifer_calf + (1|calf_id),
                     family = binomial, 
                     data = suckling_data_2 %>% filter(study == "cow-driven"))

summary(cow_as_glmm) #Remember to run line 30 & model first

# ICC
# Variance of the random effects divided by the variance plus ((pi^2)/3)
ICC_cow_as_glmm = (0.04363)/((0.04363)+((pi^2)/3))
print(ICC_cow_as_glmm, 1)

# Calculating odds ratios (OR)
# OR do not depend on the contrast option specified, but individual levels (i.e., odds at 3 wk) do
# However, we have reported all allosuckling models using contr.treatment, so we will use those
# estimates for the calculation of odds

# calf_age: exp(0.12377) = 1.131756
# The odds of allosuckling increase by 1.13 with each week in calf age.
# 
# Odds at 3 weeks:  Eta = -2.08842+(0.12377*3)  Eta = -1.71711   e^(-1.71711)   odds = 0.1795844
# Odds at 6 weeks:  Eta = -2.08842+(0.12377*6)  Eta = -1.3458    e^(-1.3458)    odds = 0.2603314
# Odds at 9 weeks:  Eta = -2.08842+(0.12377*9)  Eta = -0.97449   e^(-0.97449)   odds = 0.3773848
# Odds at 12 weeks: Eta = -2.08842+(0.12377*12) Eta = -0.60318   e^(-0.60318)   odds = 0.5470692
# Odds at 15 weeks: Eta = -2.08842+(0.12377*15) Eta = -0.23187   e^(-0.23187)   odds = 0.7930492
#
# OR between week 3 and 15: 0.7930492/0.1795844 = 4.416025
# 
# heifer_calf:0(male)   Eta = -2.08842+(0.42402*0)  Eta = -2.08842   e^(-2.08842)   odds = 0.1238827
# heifer_calf:1(female) Eta = -2.08842+(0.42402*1)  Eta = -1.6644    e^(-1.6644)    odds = 0.1893042
# Female calves have 1.53 (0.1893042/0.1238827) times the odds of allosuckling than male calves.
# 
# other_calves:0        Eta = -2.08842+(5.13574*0)  Eta = -2.08842   e^(-2.08842)   odds = 0.1238827
# other_calves:1        Eta = -2.08842+(5.13574*1)  Eta = 3.04732    e^(3.04732)    odds = 21.05883
# The presence of other suckling calves results in 170 times higher odds of allosuckling than if no calves are present.
# 
# birth_bw: exp(-0.04265) = 0.9582467
# The odds of cross-suckling decrease by 0.96 with every kg increase in bw at birth.

## e) Allosuckling with dam present (1/0) --------------------------------------
# Run the exact same model, but filter for only events with the dam presence
cow_as_dam_present <- glmer(allosuckling ~ calf_age + other_calves + birth_bw  + heifer_calf + (1|calf_id),  
                            family = binomial, 
                            data = suckling_data_2 %>% filter(study == "cow-driven" & dam_present == "1"))

summary(cow_as_dam_present) #Remember to run line 30 & model first

# ICC
# Variance of the random effects divided by the variance plus ((pi^2)/3)
ICC_cow_glmm = (1.023)/((1.023)+((pi^2)/3))
print(ICC_cow_glmm, 2)

# CALF-DRIVEN SYSTEM -----------------------------------------------------------
## a) Total SB (no bouts/d) -------------------------------------------------------
calf_sb_total <- lmer(total_sb ~ calf_age + (1|calf_id),
                     data = all_daily %>% filter(study == "calf-driven"))

summary(calf_sb_total) #Remember to run line 30 & model first
Anova(calf_sb_total, type = 3, test = "F") #Remember to run line 29 & model first

# Check residuals for final model
qqnorm(residuals(calf_sb_total))
qqline(residuals(calf_sb_total))
plot(calf_sb_total)

# Least square means of fixed effects
emmeans(calf_sb_total, ~ calf_age, at = list(calf_age=c(3,6,9,12))) 

# ICC
calf_sb_total_ICC <- (0.9217/(0.9217+1.1404))
print(calf_sb_total_ICC, 2)

## b) SB duration (s/bout) -----------------------------------------------------
calf_sb_duration <- lmer(event_length_s ~ calf_age + allosuckling + (1 | calf_id),
                        data = suckling_data_2 %>% filter(study == "calf-driven"))

summary(calf_sb_duration) #Remember to run line 30 & model first
Anova(calf_sb_duration, type = 3, test = "F") #Remember to run line 29 & model first

# Check residuals
qqnorm(residuals(calf_sb_duration))
qqline(residuals(calf_sb_duration))
plot(calf_sb_duration) 

# Least square means of fixed effects
emmeans(calf_sb_duration, ~ calf_age:allosuckling, at = list(calf_age=c(3,6,9,12))) 
emmeans(calf_sb_duration, pairwise ~ allosuckling)

# Intra-class correlation coefficient (ICC)
# This is calculated by first extracting the variance of random effects and residual variance from the model output,
# then dividing the variance of random effects by the sum of the two variances.
calf_sb_duration_ICC <- (19409/(19409+56832))
print(calf_sb_duration_ICC, 2)

## c) Total suckling time (min/d) ------------------------------------------- 
calf_suckling_min_total <- lmer(total_min_suckling ~ calf_age + (1 | calf_id),
                               data = all_daily %>% filter(study == "calf-driven"))

summary(calf_suckling_min_total) #Remember to run line 30 & model first
Anova(calf_suckling_min_total, type = 3, test = "F") #Remember to run line 29 & model first

# Check residuals for final model
qqnorm(residuals(calf_suckling_min_total))
qqline(residuals(calf_suckling_min_total))
plot(calf_suckling_min_total)

# Least square means of fixed effects
emmeans(calf_suckling_min_total, ~ calf_age, at = list(calf_age=c(3,6,9,12)))

# ICC
calf_suckling_total_ICC <- (36.14/(36.14+79.96))
print(calf_suckling_total_ICC, 2)

## d) Allosuckling (1/0) -------------------------------------------------------
calf_as_glmm <- glmer(allosuckling ~ calf_age + other_calves + birth_bw + (1|calf_id), 
                      family = binomial, 
                      data = suckling_data_2 %>% filter(study == "calf-driven"))

summary(calf_as_glmm) #Remember to run line 30 & model first

# ICC
# Variance of the random effects divided by the variance plus ((pi^2)/3)
ICC_calf_as_glmm = (2.175)/((2.175)+((pi^2)/3))
print(ICC_calf_as_glmm, 2)

# Calculating OR
# calf_age: exp(0.22188) = 1.248422
# The odds of cross-suckling increase by 1.25 with each week in calf age.
# 
# Odds at 3 weeks:  Eta = -6.31513+(0.22188*3)  Eta = -5.64949   e^(-5.64949)   odds = 0.003519311
# Odds at 6 weeks:  Eta = -6.31513+(0.22188*6)  Eta = -4.98385   e^(-4.98385)   odds = 0.006847648
# Odds at 9 weeks:  Eta = -6.31513+(0.22188*9)  Eta = -4.31821   e^(-4.31821)   odds = 0.01332371
# Odds at 12 weeks: Eta = -6.31513+(0.22188*12) Eta = -3.65257   e^(-3.65257)   odds = 0.02592442
#
# OR between week 3 and 12: 0.02592442/0.003519311 = 7.366334
# 
# other_calves:0        Eta = -6.31513+(4.94644*0)  Eta = -6.31513   e^(-6.31513)   odds = 0.001808731
# other_calves:1        Eta = -6.31513+(4.94644*1)  Eta = -1.36869   e^(-1.36869)   odds = 0.2544401
# The presence of other suckling calves results in 141 times higher odds of cross-suckling than if no calves are present.
# 
# birth_bw: exp(0.02659) = 1.026947
# The odds of cross-suckling decrease by 1.0 with every kg increase in bw at birth
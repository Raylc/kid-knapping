# load required libraries
library(lme4)
library(tidyverse)
library(lmerTest)
library(patchwork)
library(ggpubr)
library(glmulti)

# import data
ind_level <- read.csv("data/individual__data_by_participant.csv", fileEncoding="UTF-8-BOM")
core_level <- read.csv("data/lithic_data_by_core.csv", fileEncoding="UTF-8-BOM")
  
# Approach 1: Single model

# Data preparation
ind_level0 <- ind_level %>%
  select(Gender, Grip_Strength_kg, MRT_Percent_Correct, Fitts_movement_time_avg_ms,Participant_Number,BFI_O,BFI_C,BFI_A,BFI_E,BFI_N)
df<- left_join(core_level, ind_level0, by = "Participant_Number")
df_no_NA <- na.omit(df)
df_no_NA <- df_no_NA %>%
  mutate(
    Participant_Number = factor(Participant_Number),
    Core_ID = factor(Core_ID),
    Condition = factor(Condition),
    Gender = factor(Gender)
  )

# Handle missing data (example: multiple imputation)
# You might need mice or other packages for proper handling
# This is just a placeholder - actual implementation depends on your data
# df <- df %>%
#   mutate(across(c(Grip_Strength_kg, MRT_Percent_Correct, Fitts_movement_time_avg_ms), 
#                 ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Build multilevel model
# 
# quantity_complete<- lmer(Quantity ~   Day + 
#                            # Level 2 (individual-level) predictors
#                            Condition + Gender + Grip_Strength_kg + 
#                            MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                            BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
#                            # Individual level Random effects (nested structure)
#                            (1 | Participant_Number),
#                        data = df_no_NA,
#                        REML = FALSE)
# 
quantity_complete <- lmer(Quantity ~ Day +
                            # Level 2 (individual-level) predictors
                            Condition + Gender + Grip_Strength_kg +
                            MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                            BFI_O + BFI_C + BFI_A + BFI_E + BFI_N +
                            # Random effects
                            (1 | Participant_Number),
                          data = df_no_NA,
                          REML = FALSE)
# rma.glmulti <- function(formula, data, ...)
#   rma.mv(formula, vi, random = ~ 1 | Participant_Number, data=data, method="ML", ...)
# #the multimodal selection
# lmer_fit <- function(formula, data) {
#   lmer(update(formula, ". ~ ."),  # Preserve random effects
#        data = data, 
#        REML = FALSE)
# }
# 
# lmer_fit <- function(formula, data) {
#   # Convert formula to character to manipulate it
#   base_formula <- as.character(formula(quantity_complete)[2])  # Get right side of base formula
#   random_part <- "(1 | Participant_Number)"  # Define random effect to keep
#   
#   # Get the new fixed effects from glmulti
#   new_fixed <- as.character(formula)[2]
#   
#   # Combine new fixed effects with preserved random effect
#   full_formula <- as.formula(paste("Quantity ~", new_fixed, "+", random_part))
#   
#   lmer(full_formula,
#        data = data,
#        REML = FALSE)
# }
# quantity_multi <- glmulti(formula = quantity_complete,data = df_no_NA, # use the model with built as a starting point
#                           level = 2, #1=just look at main effects, 2=look at main and interraction effects
#                           method="g", crit="aic", minsize = 0, maxsize = 7, fitfunc = rma.glmulti) 
# quantity_multi <- glmulti(formula = quantity_complete, data = df_no_NA, # use the model with built as a starting point
#                           level = 2, #1=just look at main effects, 2=look at main and interraction effects
#                           method="g", crit="aic", minsize = 0, maxsize = 7, minK = 0, maxK = -1)

lmer.glmulti <- function (formula, data, random, ...) {
  lmer(paste(deparse(formula), random), data = data)
}

quantity_multi <- glmulti(y =Quantity ~ Day + 
          # Level 2 (individual-level) predictors
          Condition + Gender + Grip_Strength_kg + 
          MRT_Percent_Correct + Fitts_movement_time_avg_ms +
          BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
          Condition*Gender*Day+Grip_Strength_kg*Condition*Gender+
          MRT_Percent_Correct*Condition*Gender+Fitts_movement_time_avg_ms*Condition*Gender,
        data = df_no_NA,
        random = '+(1 | Participant_Number)',
        level = 1,
        method = 'h',
        crit = 'aicc',
        marginality = TRUE,
        fitfunc = lmer.glmulti)

quality_multi <- glmulti(y =Quality ~ Day + 
                            # Level 2 (individual-level) predictors
                            Condition + Gender + Grip_Strength_kg + 
                            MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                            BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
                          data = df_no_NA,
                          random = '+(1 | Participant_Number)',
                          level = 1,
                          method = 'h',
                          crit = 'aicc',
                          marginality = TRUE,
                          fitfunc = lmer.glmulti)

economy_multi <- glmulti(y =Economy ~ Day + 
                           # Level 2 (individual-level) predictors
                           Condition + Gender + Grip_Strength_kg + 
                           MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                           BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
                         data = df_no_NA,
                         random = '+(1 | Participant_Number)',
                         level = 1,
                         method = 'h',
                         crit = 'aicc',
                         marginality = TRUE,
                         fitfunc = lmer.glmulti)

print(quantity_multi) #all models
print(quality_multi) #all models
print(economy_multi)
summary(quantity_multi@objects[[1]]) #best model
summary(quality_multi@objects[[1]]) #best model
summary(economy_multi@objects[[1]]) #best model


model1 <- lmer(Quantity ~ 
                # Level 1 (core-level) predictors
                Day + 
                
                # Level 2 (individual-level) predictors
                Condition + Gender + Grip_Strength_kg + 
                MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                
                # Individual level Random effects (nested structure)
                (1 | Participant_Number),
              
              data = df_no_NA)

# View model summary
summary(model1)

###########################
model1 <- lmer(Quantity ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Grip_Strength_kg + 
                 MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                 
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number) +Day*Condition*Gender,
               
               data = df_no_NA)

# View model summary
summary(model1)



model1 <- lmer(Quantity ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
                 
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number) +Day*Condition*Gender,
               
               data = df_no_NA)

# View model summary
summary(model1)



model1 <- lmer(Quality ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
                 
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number) +Day*Condition*Gender,
               
               data = df_no_NA)

# View model summary
summary(model1)


model1 <- lmer(Economy ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
                 
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number) +Day*Condition*Gender,
               
               data = df_no_NA)

# View model summary
summary(model1)
##########################
# Build multilevel model
model2 <- lmer(Quality ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Grip_Strength_kg + 
                 MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                 
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)

# View model summary
summary(model2)

# Build multilevel model
model3 <- lmer(Economy ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Grip_Strength_kg + 
                 MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                 
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)

# View model summary
summary(model3)



# Approach 2: Seperate model with AIC comparison
# Model comparison for Quality outcome
modelquality1 <- lmer(Quality ~ Day + (1 | Participant_Number), data = df_no_NA)
modelquality2 <- lmer(Quality ~ Condition + Gender + (1 | Participant_Number), data = df_no_NA)
modelquality3 <- lmer(Quality ~ Grip_Strength_kg +  MRT_Percent_Correct + Fitts_movement_time_avg_ms + (1 | Participant_Number), data = df_no_NA)

# Create comparison table
aic_table <- AIC(model2,modelquality1, modelquality2, modelquality3)
aic_table[order(aic_table$AIC), ]
summary(modelquality1)
summary(modelquality2)
summary(modelquality3)

# Model comparison for Quality outcome
modelquantity1 <- lmer(Quantity ~ Day + (1 | Participant_Number), data = df_no_NA)
modelquantity2 <- lmer(Quantity ~ Condition + Gender + (1 | Participant_Number), data = df_no_NA)
modelquantity3 <- lmer(Quantity ~ Grip_Strength_kg +  MRT_Percent_Correct + Fitts_movement_time_avg_ms + (1 | Participant_Number), data = df_no_NA)

# Create comparison table
aic_table <- AIC(model1,modelquantity1, modelquantity2, modelquantity3)
aic_table[order(aic_table$AIC), ]
summary(modelquality1)
summary(modelquality2)
summary(modelquality3)

# Model comparison for Quality outcome
modelEconomy1 <- lmer(Economy ~ Day + (1 | Participant_Number), data = df_no_NA)
modelEconomy2 <- lmer(Economy ~ Condition + Gender + (1 | Participant_Number), data = df_no_NA)
modelEconomy3 <- lmer(Economy ~ Grip_Strength_kg +  MRT_Percent_Correct + Fitts_movement_time_avg_ms + (1 | Participant_Number), data = df_no_NA)

# Create comparison table
aic_table <- AIC(model3,modelEconomy1, modelEconomy2, modelEconomy3)
aic_table[order(aic_table$AIC), ]
summary(modelEconomy1)
summary(modelEconomy2)
summary(modelEconomy3)



# Alternative comparison using MuMIn package
# install.packages("MuMIn")
# library(MuMIn)
# model_full <- lmer(Quality ~ AgeGroup + Gender + Personality + MotorAccuracy + 
#                      MentalRotation + GripStrength + (1|ID), data = data_wide,
#                    REML = FALSE)
# model_null <- lmer(Quality ~ 1 + (1|ID), data = data_wide, REML = FALSE)
# model.sel(model_null, model1, model2, model3, model4, model_full)




modelb51 <- lmer(Quantity ~ 
                 # BIG FIVE
                 BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
                 
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)

# View model summary
summary(modelb51)

modelb52 <- lmer(Quality ~ 
                   # BIG FIVE
                   BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
                   
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(modelb52)

modelb53 <- lmer(Economy ~ 
                   # BIG FIVE
                   BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
                   
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(modelb53)

#############################################################
# Replication of SPSS visualization 
## fig 1 Change in average a) Quantity (PC1), b) Quality (PC2), and c) Economy (PC3) factor scores over training in children and adults. Error bars indicate 95% confidence intervals.
summary_Quantity <- core_level %>%
  group_by(Condition, Day) %>%
  summarise(
    Mean_Quantity = mean(Quantity),
    Std_Error = sd(Quantity) / sqrt(n())
  )
f1a<-ggplot(summary_Quantity, aes(x = Day, y = Mean_Quantity, color = Condition)) +
  geom_line(aes(group = Condition)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quantity - 1.96 *Std_Error, ymax = Mean_Quantity + 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Training Session", y = "Mean Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_Quality <- core_level %>%
  group_by(Condition, Day) %>%
  summarise(
    Mean_Quality = mean(Quality),
    Std_Error = sd(Quality) / sqrt(n())
  )
f1b<-ggplot(summary_Quality, aes(x = Day, y = Mean_Quality, color = Condition)) +
  geom_line(aes(group = Condition)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quality - 1.96 *Std_Error, ymax = Mean_Quality + 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Training Session", y = "Mean Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_Economy <- core_level %>%
  group_by(Condition, Day) %>%
  summarise(
    Mean_Economy = mean(Economy),
    Std_Error = sd(Economy) / sqrt(n())
  )
f1c<-ggplot(summary_Economy, aes(x = Day, y = Mean_Economy, color = Condition)) +
  geom_line(aes(group = Condition)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Economy - 1.96 *Std_Error, ymax = Mean_Economy + 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Training Session", y = "Mean Economy") +
  theme_minimal(base_size = 14)

patchwork <- f1a + f1b + f1c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.1 confidence interval.png", width = 18,
                height = 5, bg = "white", dpi = 600)


## fig 2 Effects of age and gender on average knapping a) Quantity (PC1), b) Quality (PC2), and c) Economy (PC3) over the entire study. 
ind_level1<- ind_level %>% filter(Gender!="NB")
level_order <- c('Child', 'Adult') 

summary_Quantity1 <- ind_level1 %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Quantity = mean(Quantity_Average),
    Std_Error = sd(Quantity_Average) / sqrt(n())
  )
f2a<-ggplot(summary_Quantity1, aes(x = factor(Age.Group, level = level_order), y = Mean_Quantity, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quantity - 1.96 *Std_Error, ymax = Mean_Quantity + 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mean Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_Quality1 <- ind_level1 %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Quality = mean(Quality_Average),
    Std_Error = sd(Quality_Average) / sqrt(n())
  )
f2b<-ggplot(summary_Quality1, aes(x = factor(Age.Group, level = level_order), y = Mean_Quality, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quality - 1.96 *Std_Error, ymax = Mean_Quality + 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mean Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_Economy1 <- ind_level1 %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Economy = mean(Economy_Average),
    Std_Error = sd(Economy_Average) / sqrt(n())
  )
f2c<-ggplot(summary_Economy1, aes(x = factor(Age.Group, level = level_order), y = Mean_Economy, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Economy - 1.96 *Std_Error, ymax = Mean_Economy + 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mean Economy") +
  theme_minimal(base_size = 14)

patchwork <- f2a + f2b + f2c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.2 age-gender cf.png", width = 18,
                height = 5, bg = "white", dpi = 600)




## fig 2 Effects of age and gender on average knapping a) Quantity (PC1), b) Quality (PC2), and c) Economy (PC3) over the entire study. 
ind_level1<- ind_level %>% filter(Gender!="NB")
level_order <- c('Child', 'Adult') 

summary_Quantity1 <- ind_level1 %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Quantity = mean(Quantity_Average),
    Std_Error = sd(Quantity_Average) / sqrt(n())
  )
f2a<-ggplot(summary_Quantity1, aes(x = factor(Age.Group, level = level_order), y = Mean_Quantity, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quantity -Std_Error, ymax = Mean_Quantity +Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mean Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_Quality1 <- ind_level1 %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Quality = mean(Quality_Average),
    Std_Error = sd(Quality_Average) / sqrt(n())
  )
f2b<-ggplot(summary_Quality1, aes(x = factor(Age.Group, level = level_order), y = Mean_Quality, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quality -Std_Error, ymax = Mean_Quality + Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mean Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_Economy1 <- ind_level1 %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Economy = mean(Economy_Average),
    Std_Error = sd(Economy_Average) / sqrt(n())
  )
f2c<-ggplot(summary_Economy1, aes(x = factor(Age.Group, level = level_order), y = Mean_Economy, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Economy -Std_Error, ymax = Mean_Economy +Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mean Economy") +
  theme_minimal(base_size = 14)

patchwork <- f2a + f2b + f2c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.2 age-gender se.png", width = 18,
                height = 5, bg = "white", dpi = 600)






## fig 3 Effects of age and gender on individual a) grip strength, b) mental rotation, and c) motor accuracy
ind_level1<- ind_level %>% filter(Gender!="NB")
level_order <- c('Child', 'Adult') 

summary_Grip <- ind_level1 %>% drop_na(Grip_Strength_kg) %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Quantity = mean(Grip_Strength_kg),
    Std_Error = sd(Grip_Strength_kg) / sqrt(n())
  )
f3a<-ggplot(summary_Grip, aes(x = factor(Age.Group, level = level_order), y = Mean_Quantity, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quantity -Std_Error, ymax = Mean_Quantity +Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Grip strength (kg)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_mental <- ind_level1 %>% drop_na(MRT_Percent_Correct) %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Quality = mean(MRT_Percent_Correct),
    Std_Error = sd(MRT_Percent_Correct) / sqrt(n())
  )
f3b<-ggplot(summary_mental, aes(x = factor(Age.Group, level = level_order), y = Mean_Quality, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quality -Std_Error, ymax = Mean_Quality + Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mental rotation test (%correct)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_fitts <- ind_level1 %>%drop_na(Fitts_movement_time_avg_ms) %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Economy = mean(Fitts_movement_time_avg_ms),
    Std_Error = sd(Fitts_movement_time_avg_ms) / sqrt(n())
  )
f3c<-ggplot(summary_fitts, aes(x = factor(Age.Group, level = level_order), y = Mean_Economy, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Economy -Std_Error, ymax = Mean_Economy +Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Fitts test average time (ms)") +
  theme_minimal(base_size = 14)

patchwork <- f3a + f3b + f3c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.3 age-gender-motor-cog se.png", width = 18,
                height = 5, bg = "white", dpi = 600)



## fig 4 Relationship of average knapping Quantity to individual a) grip strength, b) mental rotation, and c) motor accuracy. Panel a) also includes a Loess fit line (50% of points, Epanechnikov kernel) to illustrate the changing relationship at approximately 30 kg.
f4a<-ggplot(ind_level, aes(x = Grip_Strength_kg, y = Quantity_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  geom_smooth(method = "loess", aes(group = 1),se = FALSE, colour="black", linetype = "dotdash", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Grip strength (kg) (n=46)", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f4b<-ggplot(ind_level, aes(x = MRT_Percent_Correct, y = Quantity_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Mental rotation test (%correct) (n=49)",y = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)

f4c<-ggplot(ind_level, aes(x = Fitts_movement_time_avg_ms, y = Quantity_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Fitts test average time (ms) (n=48)", y = NULL) +
  theme_minimal(base_size = 14) + labs(color='Age group') 
# +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)

patchwork <- f4a + f4b + f4c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.4 quantity-cog.png", width = 18,
                height = 5, bg = "white", dpi = 600)



## fig 5 Relationship of average knapping Quality to individual a) grip strength, b) mental rotation, and c) motor accuracy. Panel a) also includes a Loess fit line (50% of points, Epanechnikov kernel) to illustrate the changing relationship at approximately 30 kg.
f5a<-ggplot(ind_level, aes(x = Grip_Strength_kg, y = Quality_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Grip strength (kg) (n=46)", y = "Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f5b<-ggplot(ind_level, aes(x = MRT_Percent_Correct, y = Quality_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Mental rotation test (%correct) (n=49)",y = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)

f5c<-ggplot(ind_level, aes(x = Fitts_movement_time_avg_ms, y = Quality_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  geom_smooth(method = "loess", aes(group = 1),se = FALSE, colour="black", linetype = "dotdash", linewidth = 0.5)+
  labs(x = "Fitts test average time (ms) (n=46)", y = NULL) +
  theme_minimal(base_size = 14) + labs(color='Age group') 
# +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)

patchwork <- f5a + f5b + f5c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.5 quality-cog.png", width = 18,
                height = 5, bg = "white", dpi = 600)

## fig 6 Relationship of average knapping Economy to individual a) grip strength, b) mental rotation, and c) motor accuracy. Panel a) also includes a Loess fit line (50% of points, Epanechnikov kernel) to illustrate the changing relationship at approximately 30 kg.
f6a<-ggplot(ind_level, aes(x = Grip_Strength_kg, y = Economy_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Grip strength (kg) (n=46)", y = "Economy") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f6b<-ggplot(ind_level, aes(x = MRT_Percent_Correct, y = Economy_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Mental rotation test (%correct) (n=49)",y = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)

f6c<-ggplot(ind_level, aes(x = Fitts_movement_time_avg_ms, y = Economy_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Fitts test average time (ms) (n=48)", y = NULL) +
  theme_minimal(base_size = 14) + labs(color='Age group') 
# +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)

patchwork <- f6a + f6b + f6c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.6 economy-cog.png", width = 18,
                height = 5, bg = "white", dpi = 600)


## fig 7 Relationship of average knapping Quantity to a) Quality and b) Economy, and of c) Economy to change in Quantity over training. Note that correlations are only present in the adult sample. 
f7a<-ggplot(ind_level, aes(x = Quality_Average, y = Quantity_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(data=subset(ind_level,Age.Group=="Adult"),method = "lm", se = FALSE, colour="darkred", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Quality", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f7b<-ggplot(ind_level, aes(x = Economy_Average, y = Quantity_Average, color = Age.Group)) +
  geom_point() +
  geom_smooth(data=subset(ind_level,Age.Group=="Adult"),method = "lm", se = FALSE, colour="darkred", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Economy",y = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)

f7c<-ggplot(ind_level, aes(x = Economy_Average, y = Quantity.Change.over.Training, color = Age.Group)) +
  geom_point() +
  geom_smooth(data=subset(ind_level,Age.Group=="Adult"),method = "lm", se = FALSE, colour="darkred", linewidth = 0.5)+
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Economy", y = "Quantity increase over training") +
  theme_minimal(base_size = 14) + labs(color='Age group') 
# +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)

patchwork <- f7a + f7b + f7c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.7 economy-quantity-quality.png", width = 18,
                height = 5, bg = "white", dpi = 600)
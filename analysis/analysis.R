# load required libraries
library(lme4)
library(tidyverse)
library(lmerTest)
library(patchwork)
library(ggpubr)
library(glmulti)
library(performance)
library(see)
library(rempsyc)
# import data
ind_level <- read.csv("data/individual__data_by_participant.csv", fileEncoding="UTF-8-BOM")
core_level <- read.csv("data/lithic_data_by_core.csv", fileEncoding="UTF-8-BOM")
  

# Data preparation
ind_level0 <- ind_level %>%
  select(Age.in.years,Gender, Grip_Strength_kg, MRT_Percent_Correct, Fitts_movement_time_avg_ms,Participant_Number,BFI_O,BFI_C,BFI_A,BFI_E,BFI_N)
df<- left_join(core_level, ind_level0, by = "Participant_Number")
df_no_NA <- na.omit(df)
df_no_NA <- df_no_NA %>%
  mutate(
    Participant_Number = factor(Participant_Number),
    Core_ID = factor(Core_ID),
    Condition = factor(Condition),
    Gender = factor(Gender)
  )

#RQ1 Determine if there are age/gender group-level difference and if training affect the performance.
##Quantity model
model1 <- lmer(Quantity ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Day*Condition*Gender +
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)
summary(model1)

##Quality model
model2 <- lmer(Quality ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Day*Condition*Gender +
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)
summary(model2)

##Economy model
model3 <- lmer(Economy ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Day*Condition*Gender +
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)
summary(model3)

#function that allows model selection involving conditions and interactions
lmer.glmulti <- function (formula, data, random, ...) {
  lmer(paste(deparse(formula), random), data = data)
}

#RQ2 How do different motor-cognitive traits predict the learning processes?

quantity_cogmotor <- glmulti(y =Quantity ~
                               # Level 2 (individual-level) predictors
                               Condition+
                               Grip_Strength_kg + 
                               MRT_Percent_Correct + Fitts_movement_time_avg_ms,
                             data = df_no_NA,
                             random = '+(1 | Participant_Number)',
                             level = 2,
                             method = 'h',
                             crit = 'aicc',
                             marginality = TRUE,
                             fitfunc = lmer.glmulti)
saveRDS(quantity_cogmotor, "data/quantity_cogmotor.rds")

quality_cogmotor <- glmulti(y =Quality ~
                              # Level 2 (individual-level) predictors
                              Condition+
                              Grip_Strength_kg + 
                              MRT_Percent_Correct + Fitts_movement_time_avg_ms,
                            data = df_no_NA,
                            random = '+(1 | Participant_Number)',
                            level = 2,
                            method = 'h',
                            crit = 'aicc',
                            marginality = TRUE,
                            fitfunc = lmer.glmulti)
saveRDS(quality_cogmotor, "data/quality_cogmotor.rds")


economy_cogmotor <- glmulti(y =Economy ~
                              # Level 2 (individual-level) predictors
                              Condition+
                              Grip_Strength_kg + 
                              MRT_Percent_Correct + Fitts_movement_time_avg_ms,
                            data = df_no_NA,
                            random = '+(1 | Participant_Number)',
                            level = 2,
                            method = 'h',
                            crit = 'aicc',
                            marginality = TRUE,
                            fitfunc = lmer.glmulti)
saveRDS(economy_cogmotor, "data/economy_cogmotor.rds")
print(quantity_cogmotor) #all models
print(quality_cogmotor) #all models
print(economy_cogmotor)
summary(quantity_cogmotor@objects[[1]]) #quantity #1 model
summary(quantity_cogmotor@objects[[2]]) #quantity #2 model


summary(quality_cogmotor@objects[[1]]) #quality #1 model
summary(quality_cogmotor@objects[[2]]) #quality #2 model


summary(economy_cogmotor@objects[[1]]) #economy #1 model
summary(economy_cogmotor@objects[[2]]) #economy #2 model

#RQ3 How do different personality traits predict the learning processes?
quantity_big5 <- glmulti(y =Quantity ~
                            # Level 2 (individual-level) predictors
                            Condition+
                            BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
                          data = df_no_NA,
                          random = '+(1 | Participant_Number)',
                          level = 2,
                          method = 'h',
                          crit = 'aicc',
                          marginality = TRUE,
                          fitfunc = lmer.glmulti)
saveRDS(quantity_big5, "data/quantity_big5.rds")

quality_big5 <- glmulti(y =Quality ~
                           # Level 2 (individual-level) predictors
                           Condition+
                           BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
                         data = df_no_NA,
                         random = '+(1 | Participant_Number)',
                         level = 2,
                         method = 'h',
                         crit = 'aicc',
                         marginality = TRUE,
                         fitfunc = lmer.glmulti)
saveRDS(quality_big5, "data/quality_big5.rds")


economy_big5 <- glmulti(y =Economy ~
                          # Level 2 (individual-level) predictors
                          Condition+
                          BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
                        data = df_no_NA,
                        random = '+(1 | Participant_Number)',
                        level = 2,
                        method = 'h',
                        crit = 'aicc',
                        marginality = TRUE,
                        fitfunc = lmer.glmulti)
saveRDS(economy_big5, "data/economy_big5.rds")
print(quantity_big5) #all models
print(quality_big5) #all models
print(economy_big5)
summary(quantity_big5@objects[[1]]) #quantity #1 model
summary(quantity_big5@objects[[2]]) #quantity #2 model
summary(quantity_big5@objects[[3]]) #quantity #2 model


summary(quality_big5@objects[[1]]) #quality #1 model
summary(quality_big5@objects[[2]]) #quality #2 model
summary(quality_big5@objects[[3]]) #quality #3 model
summary(quality_big5@objects[[4]]) #quality #4 model

summary(economy_big5@objects[[1]]) #economy #1 model
summary(economy_big5@objects[[2]]) #economy #2 model
summary(economy_big5@objects[[3]]) #economy #3 model


#############################################################
# Visualization 
## Fig 1 
summary_Quantity <- df_no_NA %>%
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

summary_Quality <- df_no_NA %>%
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

summary_Economy <- df_no_NA %>%
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

## Fig.2
summary_Quantity1 <- df_no_NA %>%
  group_by(Participant_Number) %>%
  summarise(
    Mean_Quantity = mean(Quantity),
    MRT_Percent_Correct = mean(MRT_Percent_Correct),
    Condition=first(Condition)
  )
summary_Quality1 <- df_no_NA %>%
  group_by(Participant_Number) %>%
  summarise(
    Mean_Quality = mean(Quality),
    Grip_Strength_kg = mean(Grip_Strength_kg),
    Condition=first(Condition)
  )
f2a<-ggplot(summary_Quantity1, aes(x = MRT_Percent_Correct, y = Mean_Quantity, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Mental rotation test (%correct)", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


f2b<-ggplot(summary_Quality1, aes(x = Grip_Strength_kg, y = Mean_Quality, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Grip strength (kg)", y = "Quality") +
  theme_minimal(base_size = 14)
patchwork <- f2a + f2b
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.2 significant cog-motor.png", width = 12,
                height = 5, bg = "white", dpi = 600)

## Fig.3
summary_Quantity2 <- df_no_NA %>%
  group_by(Participant_Number) %>%
  summarise(
    Mean_Quantity = mean(Quantity),
    BFI_A = mean(BFI_A),
    Condition=first(Condition)
  )
summary_Economy2 <- df_no_NA %>%
  group_by(Participant_Number) %>%
  summarise(
    Mean_Economy = mean(Economy),
    BFI_C = mean(BFI_C),
    Condition=first(Condition)
  )
f3a<-ggplot(summary_Quantity2, aes(x = BFI_A, y = Mean_Quantity, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = Condition),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Agreeableness", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


f3b<-ggplot(summary_Economy2, aes(x = BFI_C, y = Mean_Economy, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Conscientiousness ", y = "Economy") +
  theme_minimal(base_size = 14)
patchwork <- f3a + f3b
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.3 significant BIG FIVE.png", width = 12,
                height = 5, bg = "white", dpi = 600)
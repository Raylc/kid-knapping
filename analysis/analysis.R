# load required libraries
library(lme4)
library(tidyverse)
library(lmerTest)
library(patchwork)
# import data
ind_level <- read.csv("data/individual__data_by_participant.csv", fileEncoding="UTF-8-BOM")
core_level <- read.csv("data/lithic_data_by_core.csv", fileEncoding="UTF-8-BOM")
  
# Approach 1: Single model

# Convert to wide format for separate outcomes
data_wide <- data %>% 
  tidyr::pivot_wider(names_from = Tool, values_from = Score)

quality_model <- lmer(Quality ~ Condition + Gender + Personality + MotorAccuracy + 
                        MentalRotation + GripStrength + scale(Day) + 
                        (1 + scale(Day) | ID), 
                      data = core_level)

# Multilevel model for Quality outcome (random intercept by ID)
model_quality <- lmer(Quality ~ AgeGroup + Gender + Personality + MotorAccuracy + 
                      MentalRotation + GripStrength + (1|ID), data = data_wide)

# Repeat for Quantity and Economy outcomes
model_quantity <- lmer(Quantity ~ AgeGroup + Gender + Personality + MotorAccuracy + 
                       MentalRotation + GripStrength + (1|ID), data = data_wide)
model_economy <- lmer(Economy ~ AgeGroup + Gender + Personality + MotorAccuracy + 
                      MentalRotation + GripStrength + (1|ID), data = data_wide)

# View results
summary(model_quality)




# Approach 2: Seperate model with AIC comparison
# Model comparison for Quality outcome
model1 <- lmer(Quality ~ AgeGroup + Gender + (1|ID), data = data_wide)
model2 <- lmer(Quality ~ Personality + MotorAccuracy + (1|ID), data = data_wide)
model3 <- lmer(Quality ~ MentalRotation + GripStrength + (1|ID), data = data_wide)
model4 <- lmer(Quality ~ AgeGroup + MotorAccuracy + GripStrength + (1|ID), data = data_wide)

# Create comparison table
aic_table <- AIC(model1, model2, model3, model4)
aic_table[order(aic_table$AIC), ]


# Alternative comparison using MuMIn package
# install.packages("MuMIn")
# library(MuMIn)
# model_full <- lmer(Quality ~ AgeGroup + Gender + Personality + MotorAccuracy + 
#                      MentalRotation + GripStrength + (1|ID), data = data_wide,
#                    REML = FALSE)
# model_null <- lmer(Quality ~ 1 + (1|ID), data = data_wide, REML = FALSE)
# model.sel(model_null, model1, model2, model3, model4, model_full)
  
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
  geom_errorbar(aes(ymin = Mean_Quantity - - 1.96 *Std_Error, ymax = Mean_Quantity + - 1.96 *Std_Error), width = 0.2) +
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
  geom_errorbar(aes(ymin = Mean_Quality - - 1.96 *Std_Error, ymax = Mean_Quality + - 1.96 *Std_Error), width = 0.2) +
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
  geom_errorbar(aes(ymin = Mean_Economy - - 1.96 *Std_Error, ymax = Mean_Economy + - 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Training Session", y = "Mean Economy") +
  theme_minimal(base_size = 14)

patchwork <- f1a + f1b + f1c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.1 confidence interval.png", width = 18,
                height = 5, bg = "white", dpi = 600)


## fig 2 Effects of age and gender on average knapping a) Quantity (PC1), b) Quality (PC2), and c) Economy (PC3) over the entire study. 
summary_Quantity1 <- ind_level %>%
  group_by(Age.Group, Gender) %>%
  summarise(
    Mean_Quantity = mean(Quantity_Average),
    Std_Error = sd(Quantity_Average) / sqrt(n())
  )
f2a<-ggplot(summary_Quantity1, aes(x = Age.Group, y = Mean_Quantity, color = Gender)) +
  geom_line(aes(group = Gender)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quantity - - 1.96 *Std_Error, ymax = Mean_Quantity + - 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("purple", "orange")) +
  labs(x = "Age group", y = "Mean Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
f2a
summary_Quality1 <- core_level %>%
  group_by(Condition, Day) %>%
  summarise(
    Mean_Quality = mean(Quality),
    Std_Error = sd(Quality) / sqrt(n())
  )
f2b<-ggplot(summary_Quality, aes(x = Day, y = Mean_Quality, color = Condition)) +
  geom_line(aes(group = Condition)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Quality - - 1.96 *Std_Error, ymax = Mean_Quality + - 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Training Session", y = "Mean Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

summary_Economy1 <- core_level %>%
  group_by(Condition, Day) %>%
  summarise(
    Mean_Economy = mean(Economy),
    Std_Error = sd(Economy) / sqrt(n())
  )
fcc<-ggplot(summary_Economy, aes(x = Day, y = Mean_Economy, color = Condition)) +
  geom_line(aes(group = Condition)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Economy - - 1.96 *Std_Error, ymax = Mean_Economy + - 1.96 *Std_Error), width = 0.2) +
  scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Training Session", y = "Mean Economy") +
  theme_minimal(base_size = 14)

patchwork <- f1a + f1b + f1c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.1 confidence interval.png", width = 18,
                height = 5, bg = "white", dpi = 600)
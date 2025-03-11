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

#######RQ1 Determine if there are age/gender group-level difference and if training affect the performance.
model1 <- lmer(Quantity ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Day*Condition*Gender +
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)

# View model summary
summary(model1)
# plot(check_residuals(model1))
# plot(check_normality(model1))
# plot(check_heteroscedasticity(model1))

model2 <- lmer(Quality ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Day*Condition*Gender +
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)

# View model summary
summary(model2)



model3 <- lmer(Economy ~ 
                 # Level 1 (core-level) predictors
                 Day + 
                 # Level 2 (individual-level) predictors
                 Condition + Gender + Day*Condition*Gender +
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)

# View model summary
summary(model3)


#######RQ2 How do different motor-cognitive traits predict the learning processes?
# # Group by participant and check if they have all 5 days
# complete_attendance <- df_no_NA %>%
#   group_by(Participant_Number) %>%
#   summarise(days_attended = n_distinct(Day)) %>%
#   filter(days_attended < 5)
# 
# # Print participants who missed at least one day
# print(complete_attendance)
# 
# df_no_NA_ct<-df_no_NA  %>%  filter(Participant_Number!=12 & Participant_Number!=27)
# 
# # Calculate separate averages for Quality, Quantity, and Economy
# result <- df_no_NA_ct %>%
#   group_by(Participant_Number, Day) %>%
#   summarise(
#     Avg_Quality = mean(Quality, na.rm = TRUE),
#     Avg_Quantity = mean(Quantity, na.rm = TRUE),
#     Avg_Economy = mean(Economy, na.rm = TRUE)
#   ) %>%
#   ungroup()
# 
# 
# # Filter for Day 1 and Day 5, then calculate differences
# diff_df <- result %>%
#   filter(Day %in% c(1, 5)) %>%  # Keep only Day 1 and Day 5
#   group_by(Participant_Number) %>%
#   summarise(
#     Diff_Quality = Avg_Quality[Day == 5] - Avg_Quality[Day == 1],
#     Diff_Quantity = Avg_Quantity[Day == 5] - Avg_Quantity[Day == 1],
#     Diff_Economy = Avg_Economy[Day == 5] - Avg_Economy[Day == 1]
#   ) %>%
#   ungroup()
# 
# 
# df_no_NA_ct0 <- df_no_NA_ct %>%
#   select(Condition,Gender, Grip_Strength_kg, MRT_Percent_Correct, Fitts_movement_time_avg_ms,Participant_Number,BFI_O,BFI_C,BFI_A,BFI_E,BFI_N)
# df_learning<- left_join(diff_df, df_no_NA_ct0, by = "Participant_Number")
# df_learning <- df_learning %>%
#   distinct(Diff_Quality, Diff_Quantity, Diff_Economy, .keep_all = TRUE)
# 
# 
# model1_delta <- lm(Diff_Quantity ~ 
#                      Grip_Strength_kg + 
#                      MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                      BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                    
#                    data = df_learning)
# 
# # View model summary
# summary(model1_delta)
# 
# 
# model2_delta <- lm(Diff_Quality ~ 
#                  Grip_Strength_kg + 
#                  MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                    BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                
#                data = df_learning)
# 
# # View model summary
# summary(model2_delta)
# 
# 
# model3_delta <- lm(Diff_Economy ~ 
#                      Grip_Strength_kg + 
#                      MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                      BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                    
#                    data = df_learning)
# 
# # View model summary
# summary(model3_delta)
# 
# 
# 
# 
# # Calculate separate averages for Quality, Quantity, and Economy
# result1 <- df_no_NA_ct %>%
#   group_by(Participant_Number) %>%
#   summarise(
#     Avg_Quality = mean(Quality, na.rm = TRUE),
#     Avg_Quantity = mean(Quantity, na.rm = TRUE),
#     Avg_Economy = mean(Economy, na.rm = TRUE)
#   ) %>%
#   ungroup()
# df_average<- left_join(result1, df_no_NA_ct0, by = "Participant_Number")
# df_average <- df_average %>%
#   distinct(Avg_Quality, Avg_Quantity, Avg_Economy, .keep_all = TRUE)
# 
# 
# df_average_child <- df_average %>% filter(Condition=="Child")
# 
# model1_average <- lm(Avg_Quantity ~ 
#                      Grip_Strength_kg + 
#                      MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                      BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                    
#                    data = df_average_child)
# 
# # View model summary
# summary(model1_average)
# 
# 
# model2_average <- lm(Avg_Quality ~ 
#                      Grip_Strength_kg + 
#                      MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                      BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                    
#                    data = df_average)
# 
# # View model summary
# summary(model2_average)
# 
# 
# model3_average <- lm(Avg_Economy ~ 
#                      Grip_Strength_kg + 
#                      MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                      BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                    
#                    data = df_average)
# 
# # View model summary
# summary(model3_average)
# 
# df_no_NA_child <- df_no_NA %>% filter(Condition=="Child")
# 
# 

model100 <- lmer(Quantity ~ 
                  Grip_Strength_kg + 
                 MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                 # Individual level Random effects (nested structure)
                 (1 | Participant_Number),
               
               data = df_no_NA)

# View model summary
summary(model100)

model101 <- lmer(Quantity ~ 
                   BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(model101)

model102 <- lmer(Quantity ~ 
                   BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+ BFI_O*Condition+
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(model102)







model200 <- lmer(Quality ~ 
                   Grip_Strength_kg + 
                   MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(model200)

model201 <- lmer(Quality ~ 
                   BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(model201)

model300 <- lmer(Economy ~ 
                   Grip_Strength_kg + 
                   MRT_Percent_Correct + Fitts_movement_time_avg_ms +
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(model300)

model301 <- lmer(Economy ~ 
                   BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)

# View model summary
summary(model301)


######model selection involving conditions and interactions

lmer.glmulti <- function (formula, data, random, ...) {
  lmer(paste(deparse(formula), random), data = data)
}

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
# 

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


##########checking relationship between three outcome variables.

model000 <- lmer(Quantity ~ 
                   Quality + Economy+
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)


summary(model000)

model0001 <- lmer(Quantity ~ 
                   Quality + Economy+ Condition*Quality*Economy+
                   # Individual level Random effects (nested structure)
                   (1 | Participant_Number),
                 
                 data = df_no_NA)


summary(model0001)


model0002 <- lmer(Quantity ~ 
                    Condition*Quality+
                    # Individual level Random effects (nested structure)
                    (1 | Participant_Number),
                  
                  data = df_no_NA)


summary(model0002)

model0003 <- lmer(Quantity ~ 
                    Condition*Economy+
                    # Individual level Random effects (nested structure)
                    (1 | Participant_Number),
                  
                  data = df_no_NA)


summary(model0003)

model0004 <- lmer(Quantity ~ 
                    Condition*Quality*Economy+
                    # Individual level Random effects (nested structure)
                    (1 | Participant_Number),
                  
                  data = df_no_NA)


summary(model0004)
# 
# ##########children##############
# 
# 
# model100 <- lmer(Quantity ~ 
#                    Grip_Strength_kg + 
#                    MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA_child )
# 
# # View model summary
# summary(model100)
# 
# model101 <- lmer(Quantity ~ 
#                    BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA_child )
# 
# # View model summary
# summary(model101)
# 
# model200 <- lmer(Quality ~ 
#                    Grip_Strength_kg + 
#                    MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA_child )
# 
# # View model summary
# summary(model200)
# 
# model201 <- lmer(Quality ~ 
#                    BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA_child )
# 
# # View model summary
# summary(model201)
# 
# model300 <- lmer(Economy ~ 
#                    Grip_Strength_kg + 
#                    MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA_child )
# 
# # View model summary
# summary(model300)
# 
# model301 <- lmer(Economy ~ 
#                    BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA_child )
# 
# # View model summary
# summary(model301)
# 
# 
# 
# 
# 
# 
# 
# # Approach 1: AICC-based model selection
# ## main effect only
# lmer.glmulti <- function (formula, data, random, ...) {
#   lmer(paste(deparse(formula), random), data = data)
# }
# 
# quantity_multi <- glmulti(y =Quantity ~ Day + 
#           # Level 2 (individual-level) predictors
#           Condition + Gender + Grip_Strength_kg + 
#           MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#           BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
#           Condition*Gender*Day+Grip_Strength_kg*Condition*Gender+
#           MRT_Percent_Correct*Condition*Gender+Fitts_movement_time_avg_ms*Condition*Gender+
#           BFI_O*Condition*Gender+ BFI_C*Condition*Gender+BFI_A*Condition*Gender+
#             BFI_E*Condition*Gender+BFI_N*Condition*Gender,
#         data = df_no_NA,
#         random = '+(1 | Participant_Number)',
#         level = 1,
#         method = 'h',
#         crit = 'aicc',
#         marginality = TRUE,
#         fitfunc = lmer.glmulti)
# 
# quality_multi <- glmulti(y =Quality ~ Day + 
#                             # Level 2 (individual-level) predictors
#                             Condition + Gender + Grip_Strength_kg + 
#                             MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                             BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                           data = df_no_NA,
#                           random = '+(1 | Participant_Number)',
#                           level = 1,
#                           method = 'h',
#                           crit = 'aicc',
#                           marginality = TRUE,
#                           fitfunc = lmer.glmulti)
# 
# economy_multi <- glmulti(y =Economy ~ Day + 
#                            # Level 2 (individual-level) predictors
#                            Condition + Gender + Grip_Strength_kg + 
#                            MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                            BFI_O + BFI_C + BFI_A + BFI_E + BFI_N,
#                          data = df_no_NA,
#                          random = '+(1 | Participant_Number)',
#                          level = 1,
#                          method = 'h',
#                          crit = 'aicc',
#                          marginality = TRUE,
#                          fitfunc = lmer.glmulti)
# saveRDS(quantity_multi, "data/quantity_multi.rds")
# saveRDS(quality_multi, "data/quality_multi.rds")
# saveRDS(economy_multi, "data/economy_multi.rds")
# 
# print(quantity_multi) #all models
# print(quality_multi) #all models
# print(economy_multi)
# summary(quantity_multi@objects[[1]]) #quantity #1 model
# summary(quantity_multi@objects[[2]]) #quantity #2 model
# summary(quantity_multi@objects[[3]]) #quantity #3 model
# summary(quantity_multi@objects[[4]]) #quantity #4 model
# 
# summary(quality_multi@objects[[1]]) #quality #1 model
# summary(quality_multi@objects[[2]]) #quality #2 model
# summary(quality_multi@objects[[3]]) #quality #3 model
# 
# summary(economy_multi@objects[[1]]) #economy #1 model
# summary(economy_multi@objects[[2]]) #economy #2 model
# summary(economy_multi@objects[[3]]) #economy #3 model
# summary(economy_multi@objects[[4]]) #economy #4 model
# summary(economy_multi@objects[[5]]) #economy #5 model
# 
# ## interaction effect excluding big five
# 
# quantity_int <- glmulti(y =Quantity ~ Day + 
#                             # Level 2 (individual-level) predictors
#                             Condition + Gender + Grip_Strength_kg + 
#                             MRT_Percent_Correct + Fitts_movement_time_avg_ms,
#                           data = df_no_NA,
#                           random = '+(1 | Participant_Number)',
#                           level = 2,
#                           method = 'h',
#                           crit = 'aicc',
#                           marginality = TRUE,
#                           fitfunc = lmer.glmulti)
# saveRDS(quantity_int, "data/quantity_int.rds")
# 
# quality_int <- glmulti(y =Quality ~ Day + 
#                            # Level 2 (individual-level) predictors
#                            Condition + Gender + Grip_Strength_kg + 
#                            MRT_Percent_Correct + Fitts_movement_time_avg_ms,
#                          data = df_no_NA,
#                          random = '+(1 | Participant_Number)',
#                          level = 2,
#                          method = 'h',
#                          crit = 'aicc',
#                          marginality = TRUE,
#                          fitfunc = lmer.glmulti)
# saveRDS(quality_int, "data/quality_int.rds")
# 
# economy_int <- glmulti(y =Economy ~ Day + 
#                            # Level 2 (individual-level) predictors
#                            Condition + Gender + Grip_Strength_kg + 
#                            MRT_Percent_Correct + Fitts_movement_time_avg_ms,
#                          data = df_no_NA,
#                          random = '+(1 | Participant_Number)',
#                          level = 1,
#                          method = 'h',
#                          crit = 'aicc',
#                          marginality = TRUE,
#                          fitfunc = lmer.glmulti)
# saveRDS(economy_int, "data/economy_int.rds")
# 
# print(quantity_int) #all models
# print(quality_int) #all models
# print(economy_int)
# summary(quantity_int@objects[[1]]) #quantity #1 model
# summary(quantity_int@objects[[2]]) #quantity #2 model
# 
# 
# summary(quality_int@objects[[1]]) #quality #1 model
# summary(quality_int@objects[[2]]) #quality #2 model
# summary(quality_int@objects[[3]]) #quality #3 model
# summary(quality_int@objects[[4]]) #quality #4 model
# 
# summary(economy_int@objects[[1]]) #economy #1 model
# summary(economy_int@objects[[2]]) #economy #2 model
# summary(economy_int@objects[[3]]) #economy #3 model
# 
# #########three-way interaction terms added##########
# 
# quantity_complete <- lmer(Quantity ~ Day + 
#                             # Level 2 (individual-level) predictors
#                             Condition + Gender + Grip_Strength_kg + 
#                             MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                             BFI_O + BFI_C + BFI_A + BFI_E + BFI_N+
#                             Condition*Gender*Day+Grip_Strength_kg*Condition*Gender+
#                             MRT_Percent_Correct*Condition*Gender+Fitts_movement_time_avg_ms*Condition*Gender+
#                             BFI_O*Condition*Gender+ BFI_C*Condition*Gender+BFI_A*Condition*Gender+
#                             BFI_E*Condition*Gender+BFI_N*Condition*Gender +
#                             # Random effects
#                             (1 | Participant_Number),
#                           data = df_no_NA)
# 
# summary(quantity_complete)
# 
# 
# quality_complete <- lmer(Quality ~ Day +
#                             # Level 2 (individual-level) predictors
#                             Condition + Gender + Grip_Strength_kg +
#                             MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                             BFI_O + BFI_C + BFI_A + BFI_E + BFI_N +
#                             # Three-way interaction effect terms
#                             Condition*Gender*Day+
#                             Condition*Gender*Grip_Strength_kg+
#                             Condition*Gender*MRT_Percent_Correct +
#                             Condition*Gender*Fitts_movement_time_avg_ms +
#                            BFI_O*Condition*Gender+ BFI_C*Condition*Gender+BFI_A*Condition*Gender+
#                            BFI_E*Condition*Gender+BFI_N*Condition*Gender +
#                             # Random effects
#                             (1 | Participant_Number),
#                           data = df_no_NA)
# 
# summary(quality_complete)
# 
# economy_complete <- lmer(Economy ~ Day +
#                             # Level 2 (individual-level) predictors
#                             Condition + Gender + Grip_Strength_kg +
#                             MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                             BFI_O + BFI_C + BFI_A + BFI_E + BFI_N +
#                             # Three-way interaction effect terms
#                             Condition*Gender*Day+
#                             Condition*Gender*Grip_Strength_kg+
#                             Condition*Gender*MRT_Percent_Correct +
#                             Condition*Gender*Fitts_movement_time_avg_ms +
#                            BFI_O*Condition*Gender+ BFI_C*Condition*Gender+BFI_A*Condition*Gender+
#                            BFI_E*Condition*Gender+BFI_N*Condition*Gender +
#                             # Random effects
#                             (1 | Participant_Number),
#                           data = df_no_NA)
# 
# summary(economy_complete)
# 
# 
# 
# 
# model1 <- lmer(Quantity ~ 
#                 # Level 1 (core-level) predictors
#                 Day + 
#                 
#                 # Level 2 (individual-level) predictors
#                 Condition + Gender + Grip_Strength_kg + 
#                 MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                 
#                 # Individual level Random effects (nested structure)
#                 (1 | Participant_Number),
#               
#               data = df_no_NA)
# 
# # View model summary
# summary(model1)
# 
# ###########################
# model1 <- lmer(Quantity ~ 
#                  # Level 1 (core-level) predictors
#                  Day + 
#                  
#                  # Level 2 (individual-level) predictors
#                  Condition + Gender + Grip_Strength_kg + 
#                  MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                  
#                  # Individual level Random effects (nested structure)
#                  (1 | Participant_Number) +Day*Condition*Gender,
#                
#                data = df_no_NA)
# 
# # View model summary
# summary(model1)
# 
# 
# 
# model1 <- lmer(Quantity ~ 
#                  # Level 1 (core-level) predictors
#                  Day + 
#                  
#                  # Level 2 (individual-level) predictors
#                  Condition + Gender + BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
#                  
#                  # Individual level Random effects (nested structure)
#                  (1 | Participant_Number) +Day*Condition*Gender,
#                
#                data = df_no_NA)
# 
# # View model summary
# summary(model1)
# 
# 
# 
# model1 <- lmer(Quality ~ 
#                  # Level 1 (core-level) predictors
#                  Day + 
#                  
#                  # Level 2 (individual-level) predictors
#                  Condition + Gender + BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
#                  
#                  # Individual level Random effects (nested structure)
#                  (1 | Participant_Number) +Day*Condition*Gender,
#                
#                data = df_no_NA)
# 
# # View model summary
# summary(model1)
# 
# 
# model1 <- lmer(Economy ~ 
#                  # Level 1 (core-level) predictors
#                  Day + 
#                  
#                  # Level 2 (individual-level) predictors
#                  Condition + Gender + BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
#                  
#                  # Individual level Random effects (nested structure)
#                  (1 | Participant_Number) +Day*Condition*Gender,
#                
#                data = df_no_NA)
# 
# # View model summary
# summary(model1)
# ##########################
# # Build multilevel model
# model2 <- lmer(Quality ~ 
#                  # Level 1 (core-level) predictors
#                  Day + 
#                  
#                  # Level 2 (individual-level) predictors
#                  Condition + Gender + Grip_Strength_kg + 
#                  MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                  
#                  # Individual level Random effects (nested structure)
#                  (1 | Participant_Number),
#                
#                data = df_no_NA)
# 
# # View model summary
# summary(model2)
# 
# # Build multilevel model
# model3 <- lmer(Economy ~ 
#                  # Level 1 (core-level) predictors
#                  Day + 
#                  
#                  # Level 2 (individual-level) predictors
#                  Condition + Gender + Grip_Strength_kg + 
#                  MRT_Percent_Correct + Fitts_movement_time_avg_ms +
#                  
#                  # Individual level Random effects (nested structure)
#                  (1 | Participant_Number),
#                
#                data = df_no_NA)
# 
# # View model summary
# summary(model3)
# 
# 
# 
# # Approach 2: Seperate model with AIC comparison
# # Model comparison for Quality outcome
# modelquality1 <- lmer(Quality ~ Day + (1 | Participant_Number), data = df_no_NA)
# modelquality2 <- lmer(Quality ~ Condition + Gender + (1 | Participant_Number), data = df_no_NA)
# modelquality3 <- lmer(Quality ~ Grip_Strength_kg +  MRT_Percent_Correct + Fitts_movement_time_avg_ms + (1 | Participant_Number), data = df_no_NA)
# 
# # Create comparison table
# aic_table <- AIC(model2,modelquality1, modelquality2, modelquality3)
# aic_table[order(aic_table$AIC), ]
# summary(modelquality1)
# summary(modelquality2)
# summary(modelquality3)
# 
# # Model comparison for Quality outcome
# modelquantity1 <- lmer(Quantity ~ Day + (1 | Participant_Number), data = df_no_NA)
# modelquantity2 <- lmer(Quantity ~ Condition + Gender + (1 | Participant_Number), data = df_no_NA)
# modelquantity3 <- lmer(Quantity ~ Grip_Strength_kg +  MRT_Percent_Correct + Fitts_movement_time_avg_ms + (1 | Participant_Number), data = df_no_NA)
# 
# # Create comparison table
# aic_table <- AIC(model1,modelquantity1, modelquantity2, modelquantity3)
# aic_table[order(aic_table$AIC), ]
# summary(modelquality1)
# summary(modelquality2)
# summary(modelquality3)
# 
# # Model comparison for Quality outcome
# modelEconomy1 <- lmer(Economy ~ Day + (1 | Participant_Number), data = df_no_NA)
# modelEconomy2 <- lmer(Economy ~ Condition + Gender + (1 | Participant_Number), data = df_no_NA)
# modelEconomy3 <- lmer(Economy ~ Grip_Strength_kg +  MRT_Percent_Correct + Fitts_movement_time_avg_ms + (1 | Participant_Number), data = df_no_NA)
# 
# # Create comparison table
# aic_table <- AIC(model3,modelEconomy1, modelEconomy2, modelEconomy3)
# aic_table[order(aic_table$AIC), ]
# summary(modelEconomy1)
# summary(modelEconomy2)
# summary(modelEconomy3)
# 
# 
# 
# # Alternative comparison using MuMIn package
# # install.packages("MuMIn")
# # library(MuMIn)
# # model_full <- lmer(Quality ~ AgeGroup + Gender + Personality + MotorAccuracy + 
# #                      MentalRotation + GripStrength + (1|ID), data = data_wide,
# #                    REML = FALSE)
# # model_null <- lmer(Quality ~ 1 + (1|ID), data = data_wide, REML = FALSE)
# # model.sel(model_null, model1, model2, model3, model4, model_full)
# 
# 
# 
# 
# modelb51 <- lmer(Quantity ~ 
#                  # BIG FIVE
#                  BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
#                  
#                  # Individual level Random effects (nested structure)
#                  (1 | Participant_Number),
#                
#                data = df_no_NA)
# 
# # View model summary
# summary(modelb51)
# 
# modelb52 <- lmer(Quality ~ 
#                    # BIG FIVE
#                    BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
#                    
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA)
# 
# # View model summary
# summary(modelb52)
# 
# modelb53 <- lmer(Economy ~ 
#                    # BIG FIVE
#                    BFI_O +  BFI_C + BFI_A + BFI_E + BFI_N+
#                    
#                    # Individual level Random effects (nested structure)
#                    (1 | Participant_Number),
#                  
#                  data = df_no_NA)
# 
# # View model summary
# summary(modelb53)

#############################################################
# Replication of SPSS visualization 
## fig 1 Change in average a) Quantity (PC1), b) Quality (PC2), and c) Economy (PC3) factor scores over training in children and adults. Error bars indicate 95% confidence intervals.
# summary_Quantity <- core_level %>%
#   group_by(Condition, Day) %>%
#   summarise(
#     Mean_Quantity = mean(Quantity),
#     Std_Error = sd(Quantity) / sqrt(n())
#   )
# f1a<-ggplot(summary_Quantity, aes(x = Day, y = Mean_Quantity, color = Condition)) +
#   geom_line(aes(group = Condition)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Quantity - 1.96 *Std_Error, ymax = Mean_Quantity + 1.96 *Std_Error), width = 0.2) +
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Training Session", y = "Mean Quantity") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# summary_Quality <- core_level %>%
#   group_by(Condition, Day) %>%
#   summarise(
#     Mean_Quality = mean(Quality),
#     Std_Error = sd(Quality) / sqrt(n())
#   )
# f1b<-ggplot(summary_Quality, aes(x = Day, y = Mean_Quality, color = Condition)) +
#   geom_line(aes(group = Condition)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Quality - 1.96 *Std_Error, ymax = Mean_Quality + 1.96 *Std_Error), width = 0.2) +
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Training Session", y = "Mean Quality") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# summary_Economy <- core_level %>%
#   group_by(Condition, Day) %>%
#   summarise(
#     Mean_Economy = mean(Economy),
#     Std_Error = sd(Economy) / sqrt(n())
#   )
# f1c<-ggplot(summary_Economy, aes(x = Day, y = Mean_Economy, color = Condition)) +
#   geom_line(aes(group = Condition)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Economy - 1.96 *Std_Error, ymax = Mean_Economy + 1.96 *Std_Error), width = 0.2) +
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Training Session", y = "Mean Economy") +
#   theme_minimal(base_size = 14)
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



## new MAIN FIGURE 2##
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

## new MAIN FIGURE 3##
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



## MAIN FIGURE 2##
summary_Quantity1 <- df_no_NA %>%
  group_by(Participant_Number) %>%
  summarise(
    Mean_Quantity = mean(Quantity),
    Fitts_movement_time_avg_ms = mean(Fitts_movement_time_avg_ms),
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
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f2b<-ggplot(summary_Quantity1, aes(x = Fitts_movement_time_avg_ms, y = Mean_Quantity, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Fitts test average time (ms)", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)

f2c<-ggplot(summary_Quality1, aes(x = Grip_Strength_kg, y = Mean_Quality, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Grip strength (kg)", y = "Quality") +
  theme_minimal(base_size = 14)
patchwork <- f2a + f2b + f2c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.2 significant cog-motor.png", width = 18,
                height = 5, bg = "white", dpi = 600)

## MAIN FIGURE 3##
summary_Quantity2 <- df_no_NA %>%
  group_by(Participant_Number) %>%
  summarise(
    Mean_Quantity = mean(Quantity),
    Mean_O = mean(BFI_O),
    Mean_A = mean(BFI_A),
    Mean_N = mean(BFI_N),
    Condition=first(Condition)
  )
summary_Economy2 <- df_no_NA %>%
  group_by(Participant_Number) %>%
  summarise(
    Mean_Economy = mean(Economy),
    Mean_O = mean(BFI_O),
    Condition=first(Condition)
  )
f3a<-ggplot(summary_Quantity2, aes(x = Mean_O, y = Mean_Quantity, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Openness", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f3b<-ggplot(summary_Quantity2, aes(x = Mean_A, y = Mean_Quantity, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Agreeableness", y = "Quantity") +
  theme_minimal(base_size = 14)
# +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)
f3c<-ggplot(summary_Quantity2, aes(x = Mean_N, y = Mean_Quantity, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Neuroticism", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

f3d<-ggplot(summary_Economy2, aes(x = Mean_O, y = Mean_Economy, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, linetype = "dotdash", linewidth = 0.5)+
  # scale_color_manual(values = c("darkred", "deepskyblue")) +
  labs(x = "Openness", y = "Economy") +
  theme_minimal(base_size = 14)
patchwork <- f3a + f3b + f3c +f3d
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/Fig.3 significant personality.png", width = 12,
                height = 10, bg = "white", dpi = 600)




















#######Supplmentary figures###############

## fig 2 Effects of age and gender on average knapping a) Quantity (PC1), b) Quality (PC2), and c) Economy (PC3) over the entire study. 
# ind_level1<- ind_level %>% filter(Gender!="NB")
# level_order <- c('Child', 'Adult') 
# 
# summary_Quantity1 <- ind_level1 %>%
#   group_by(Age.Group, Gender) %>%
#   summarise(
#     Mean_Quantity = mean(Quantity_Average),
#     Std_Error = sd(Quantity_Average) / sqrt(n())
#   )
# f2a<-ggplot(summary_Quantity1, aes(x = factor(Age.Group, level = level_order), y = Mean_Quantity, color = Gender)) +
#   geom_line(aes(group = Gender)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Quantity - 1.96 *Std_Error, ymax = Mean_Quantity + 1.96 *Std_Error), width = 0.2) +
#   scale_color_manual(values = c("purple", "orange")) +
#   labs(x = "Age group", y = "Mean Quantity") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# summary_Quality1 <- ind_level1 %>%
#   group_by(Age.Group, Gender) %>%
#   summarise(
#     Mean_Quality = mean(Quality_Average),
#     Std_Error = sd(Quality_Average) / sqrt(n())
#   )
# f2b<-ggplot(summary_Quality1, aes(x = factor(Age.Group, level = level_order), y = Mean_Quality, color = Gender)) +
#   geom_line(aes(group = Gender)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Quality - 1.96 *Std_Error, ymax = Mean_Quality + 1.96 *Std_Error), width = 0.2) +
#   scale_color_manual(values = c("purple", "orange")) +
#   labs(x = "Age group", y = "Mean Quality") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# summary_Economy1 <- ind_level1 %>%
#   group_by(Age.Group, Gender) %>%
#   summarise(
#     Mean_Economy = mean(Economy_Average),
#     Std_Error = sd(Economy_Average) / sqrt(n())
#   )
# f2c<-ggplot(summary_Economy1, aes(x = factor(Age.Group, level = level_order), y = Mean_Economy, color = Gender)) +
#   geom_line(aes(group = Gender)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Economy - 1.96 *Std_Error, ymax = Mean_Economy + 1.96 *Std_Error), width = 0.2) +
#   scale_color_manual(values = c("purple", "orange")) +
#   labs(x = "Age group", y = "Mean Economy") +
#   theme_minimal(base_size = 14)
f2a<-ggplot(df_no_NA, aes(x = Grip_Strength_kg, y = Quantity)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, colour="darkred", linetype = "dotdash", linewidth = 0.5)+
  labs(x = "Grip strength (kg)", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f2b<-ggplot(df_no_NA, aes(x = MRT_Percent_Correct, y = Quantity)) +
  geom_point(aes(color = Condition)) +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  labs(x = "Mental rotation test (%correct)", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

f2c<-ggplot(df_no_NA, aes(x = Fitts_movement_time_avg_ms, y = Quantity)) +
  geom_point(aes(color = Condition)) +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  labs(x = "Fitts test average time (ms)", y = "Quantity") +
  theme_minimal(base_size = 14)

patchwork <- f2a + f2b + f2c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/SFig.2 quantity cog-motor.png", width = 18,
                height = 5, bg = "white", dpi = 600)



## fig 3 Effects of age and gender on individual a) grip strength, b) mental rotation, and c) motor accuracy
# ind_level1<- ind_level %>% filter(Gender!="NB")
# level_order <- c('Child', 'Adult') 
# 
# summary_Grip <- ind_level1 %>% drop_na(Grip_Strength_kg) %>%
#   group_by(Age.Group, Gender) %>%
#   summarise(
#     Mean_Quantity = mean(Grip_Strength_kg),
#     Std_Error = sd(Grip_Strength_kg) / sqrt(n())
#   )
# f3a<-ggplot(summary_Grip, aes(x = factor(Age.Group, level = level_order), y = Mean_Quantity, color = Gender)) +
#   geom_line(aes(group = Gender)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Quantity -Std_Error, ymax = Mean_Quantity +Std_Error), width = 0.2) +
#   scale_color_manual(values = c("purple", "orange")) +
#   labs(x = "Age group", y = "Grip strength (kg)") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# summary_mental <- ind_level1 %>% drop_na(MRT_Percent_Correct) %>%
#   group_by(Age.Group, Gender) %>%
#   summarise(
#     Mean_Quality = mean(MRT_Percent_Correct),
#     Std_Error = sd(MRT_Percent_Correct) / sqrt(n())
#   )
# f3b<-ggplot(summary_mental, aes(x = factor(Age.Group, level = level_order), y = Mean_Quality, color = Gender)) +
#   geom_line(aes(group = Gender)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Quality -Std_Error, ymax = Mean_Quality + Std_Error), width = 0.2) +
#   scale_color_manual(values = c("purple", "orange")) +
#   labs(x = "Age group", y = "Mental rotation test (%correct)") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# summary_fitts <- ind_level1 %>%drop_na(Fitts_movement_time_avg_ms) %>%
#   group_by(Age.Group, Gender) %>%
#   summarise(
#     Mean_Economy = mean(Fitts_movement_time_avg_ms),
#     Std_Error = sd(Fitts_movement_time_avg_ms) / sqrt(n())
#   )
# f3c<-ggplot(summary_fitts, aes(x = factor(Age.Group, level = level_order), y = Mean_Economy, color = Gender)) +
#   geom_line(aes(group = Gender)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = Mean_Economy -Std_Error, ymax = Mean_Economy +Std_Error), width = 0.2) +
#   scale_color_manual(values = c("purple", "orange")) +
#   labs(x = "Age group", y = "Fitts test average time (ms)") +
#   theme_minimal(base_size = 14)
f3a<-ggplot(df_no_NA, aes(x = Grip_Strength_kg, y = Quality)) +
  geom_point(aes(color = Condition)) +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  labs(x = "Grip strength (kg)", y = "Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f3b<-ggplot(df_no_NA, aes(x = MRT_Percent_Correct, y = Quality)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Mental rotation test (%correct)", y = "Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

f3c<-ggplot(df_no_NA, aes(x = Fitts_movement_time_avg_ms, y = Quality)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  # geom_smooth(method = "loess", aes(group = 1),se = FALSE, colour="darkred", linetype = "dotdash", linewidth = 0.5)+
  labs(x = "Fitts test average time (ms)", y = "Quality") +
  theme_minimal(base_size = 14)

patchwork <- f3a + f3b + f3c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/sFig.3 quality cog-motor.png", width = 18,
                height = 5, bg = "white", dpi = 600)
# patchwork <- f3a + f3b + f3c
# patchwork + plot_annotation(tag_levels = 'a')
# ggplot2::ggsave("figure/Fig.3 age-gender-motor-cog se.png", width = 18,
#                 height = 5, bg = "white", dpi = 600)
# 


## fig 4 Relationship of average knapping Quantity to individual a) grip strength, b) mental rotation, and c) motor accuracy. Panel a) also includes a Loess fit line (50% of points, Epanechnikov kernel) to illustrate the changing relationship at approximately 30 kg.
# f4a<-ggplot(ind_level, aes(x = Grip_Strength_kg, y = Quantity_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   geom_smooth(method = "loess", aes(group = 1),se = FALSE, colour="black", linetype = "dotdash", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Grip strength (kg) (n=46)", y = "Quantity") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)
# 
# f4b<-ggplot(ind_level, aes(x = MRT_Percent_Correct, y = Quantity_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Mental rotation test (%correct) (n=49)",y = NULL) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)
# 
# f4c<-ggplot(ind_level, aes(x = Fitts_movement_time_avg_ms, y = Quantity_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Fitts test average time (ms) (n=48)", y = NULL) +
#   theme_minimal(base_size = 14) + labs(color='Age group') 
# # +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)
f4a<-ggplot(df_no_NA, aes(x = Grip_Strength_kg, y = Economy)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Grip strength (kg)", y = "Economy") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f4b<-ggplot(df_no_NA, aes(x = MRT_Percent_Correct, y = Economy)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Mental rotation test (%correct)", y = "Economy") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

f4c<-ggplot(df_no_NA, aes(x = Fitts_movement_time_avg_ms, y = Economy)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Fitts test average time (ms)", y = "Economy") +
  theme_minimal(base_size = 14)

patchwork <- f4a + f4b + f4c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/SFig.4 economy cog-motor.png", width = 18,
                height = 5, bg = "white", dpi = 600)



## fig 5 Relationship of average knapping Quality to individual a) grip strength, b) mental rotation, and c) motor accuracy. Panel a) also includes a Loess fit line (50% of points, Epanechnikov kernel) to illustrate the changing relationship at approximately 30 kg.
# f5a<-ggplot(ind_level, aes(x = Grip_Strength_kg, y = Quality_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Grip strength (kg) (n=46)", y = "Quality") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)
# 
# f5b<-ggplot(ind_level, aes(x = MRT_Percent_Correct, y = Quality_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Mental rotation test (%correct) (n=49)",y = NULL) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)
# 
# f5c<-ggplot(ind_level, aes(x = Fitts_movement_time_avg_ms, y = Quality_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   geom_smooth(method = "loess", aes(group = 1),se = FALSE, colour="black", linetype = "dotdash", linewidth = 0.5)+
#   labs(x = "Fitts test average time (ms) (n=46)", y = NULL) +
#   theme_minimal(base_size = 14) + labs(color='Age group') 
# +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)
f5a<-ggplot(df_no_NA, aes(x = BFI_O, y = Economy)) +
  geom_point(aes(color = Condition)) +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  labs(x = "Openness", y = "Economy") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f5b<-ggplot(df_no_NA, aes(x = BFI_A, y = Economy)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Agreeableness", y = "Economy") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

f5c<-ggplot(df_no_NA, aes(x = BFI_N, y = Economy)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Neuroticism", y = "Economy") +
  theme_minimal(base_size = 14)
patchwork <- f5a + f5b + f5c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/SFig.5 ECONOMY BIG FIVE.png", width = 18,
                height = 5, bg = "white", dpi = 600)

## fig 6 Relationship of average knapping Economy to individual a) grip strength, b) mental rotation, and c) motor accuracy. Panel a) also includes a Loess fit line (50% of points, Epanechnikov kernel) to illustrate the changing relationship at approximately 30 kg.
# f6a<-ggplot(ind_level, aes(x = Grip_Strength_kg, y = Economy_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Grip strength (kg) (n=46)", y = "Economy") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)
# 
# f6b<-ggplot(ind_level, aes(x = MRT_Percent_Correct, y = Economy_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Mental rotation test (%correct) (n=49)",y = NULL) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)
# 
# f6c<-ggplot(ind_level, aes(x = Fitts_movement_time_avg_ms, y = Economy_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="black", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Fitts test average time (ms) (n=48)", y = NULL) +
#   theme_minimal(base_size = 14) + labs(color='Age group') 
# # +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)
f6a<-ggplot(df_no_NA, aes(x = BFI_O, y = Quality)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Openness", y = "Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f6b<-ggplot(df_no_NA, aes(x = BFI_A, y = Quality)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Agreeableness", y = "Quality") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

f6c<-ggplot(df_no_NA, aes(x = BFI_N, y = Quality)) +
  geom_point(aes(color = Condition)) +
  # geom_smooth(method = "lm", aes(group = 1),se = FALSE, colour="darkred", linewidth = 0.5)+
  labs(x = "Neuroticism", y = "Quality") +
  theme_minimal(base_size = 14)
patchwork <- f6a + f6b + f6c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/SFig.6 quality big five.png", width = 18,
                height = 5, bg = "white", dpi = 600)


## fig 7 Relationship of average knapping Quantity to a) Quality and b) Economy, and of c) Economy to change in Quantity over training. Note that correlations are only present in the adult sample. 
# f7a<-ggplot(ind_level, aes(x = Quality_Average, y = Quantity_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(data=subset(ind_level,Age.Group=="Adult"),method = "lm", se = FALSE, colour="darkred", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Quality", y = "Quantity") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)
# 
# f7b<-ggplot(ind_level, aes(x = Economy_Average, y = Quantity_Average, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(data=subset(ind_level,Age.Group=="Adult"),method = "lm", se = FALSE, colour="darkred", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Economy",y = NULL) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# # +  ggpubr::stat_cor(method = "pearson", label.x = 85, label.y = 1.5)
# 
# f7c<-ggplot(ind_level, aes(x = Economy_Average, y = Quantity.Change.over.Training, color = Age.Group)) +
#   geom_point() +
#   geom_smooth(data=subset(ind_level,Age.Group=="Adult"),method = "lm", se = FALSE, colour="darkred", linewidth = 0.5)+
#   scale_color_manual(values = c("darkred", "deepskyblue")) +
#   labs(x = "Economy", y = "Quantity increase over training") +
#   theme_minimal(base_size = 14) + labs(color='Age group') 
# # +  ggpubr::stat_cor(method = "pearson", label.x = 2200, label.y = 1.5)
f7a<-ggplot(df_no_NA, aes(x = BFI_O, y = Quantity)) +
  geom_point(aes(color = Condition)) +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE, linewidth = 0.5)+
  labs(x = "Openness", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# +   ggpubr::stat_cor(method = "pearson", label.x = 45, label.y = 1.2)

f7b<-ggplot(df_no_NA, aes(x = BFI_A, y = Quantity)) +
  geom_point(aes(color = Condition)) +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE,  linewidth = 0.5)+
  labs(x = "Agreeableness", y = "Quantity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

f7c<-ggplot(df_no_NA, aes(x = BFI_N, y = Quantity)) +
  geom_point(aes(color = Condition)) +
  geom_smooth(method = "lm", aes(group = 1),se = FALSE,  linewidth = 0.5)+
  labs(x = "Neuroticism", y = "Quantity") +
  theme_minimal(base_size = 14)
patchwork <- f7a + f7b + f7c
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("figure/SFig.7 Quantity big five.png", width = 18,
                height = 5, bg = "white", dpi = 600)
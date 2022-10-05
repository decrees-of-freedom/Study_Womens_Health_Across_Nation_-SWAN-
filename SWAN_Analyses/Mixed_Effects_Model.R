# import required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(reshape2)
library(lme4)
library(compiler)
library(parallel)
library(boot)
library(lattice)
library(tidyr)

#install.packages("optimx")
#install.packages("dfoptim")
#install.packages("ResourceSelection")

library(optimx)
library(dfoptim)
library(ResourceSelection)
library(caret)

# load data
df <- read.csv("Downloads/full_data_long (1).csv")

# create target variable
df$TRBLSLE <- factor(df$TRBLSLE, levels = c("No, Not In The Past 2 Wks", 
                                            "No, not in the past 2 wks", 
                                            "No, not in the past 2 weeks", 
                                            "1: No, not in the past 2 weeks",
                                            "1: No, Not In The Past 2 Wks", 
                                            "1: No, Not In The Past 2 Weeks",
                                            "Yes, < Once A wk", 
                                            "Yes, < once a Wk", 
                                            "Yes, < once a week", 
                                            "Yes, less than once a week", 
                                            "2: Yes, < Once A Wk", 
                                            "2: Yes, < Once A Week", 
                                            "2: Yes, less than once a week",
                                            "Yes, 1-2 Times/wk", 
                                            "Yes, 1-2 times/Wk", 
                                            "Yes, 1-2 times/week",
                                            "Yes, 1-2 times per week", 
                                            "3: Yes, 1-2 times/week", 
                                            "3: Yes, 1-2 Times/Wk", 
                                            "3: Yes, 1-2 Times/Week", 
                                            "Yes, 3-4 times/wk", 
                                            "Yes, 3-4 Times/Wk", 
                                            "Yes, 3-4 times/week", 
                                            "Yes, 3-4 times per week", 
                                            "4: Yes, 3-4 Times/Wk", 
                                            "4: Yes, 3-4 Times/Week", 
                                            "4: Yes, 3-4 times/week",
                                            "Yes, 5 or more times/wk", 
                                            "Yes, 5 or More Times/Wk", 
                                            "5: Yes, 5 or More Times/Wk",
                                            "5: Yes, 5 or more times/week", 
                                            "5: Yes, 5 or More Times/Week", 
                                            "Yes, 5 or more times per week",
                                            "Yes, 5 or more times/week"),
                     labels = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

df$WAKEUP <- factor(df$WAKEUP, levels = c("No, Not In The Past 2 Wks", 
                                          "No, not in the past 2 wks", 
                                          "No, not in the past 2 weeks", 
                                          "1: No, not in the past 2 weeks",
                                          "1: No, Not In The Past 2 Wks", 
                                          "1: No, Not In The Past 2 Weeks",
                                          "Yes, < Once A Wk", 
                                          "Yes, < once a wk", 
                                          "Yes, < once a week",
                                          "Yes, less than once a week",
                                          "2: Yes, < Once A Week", 
                                          "2: Yes, < Once A Wk", 
                                          "2: Yes, less than once a week",
                                          "Yes, 1-2 Times/Wk", 
                                          "Yes, 1-2 times/wk", 
                                          "Yes, 1-2 times/week", 
                                          "Yes, 1-2 times per week",
                                          "3: Yes, 1-2 Times/Wk", 
                                          "3: Yes, 1-2 Times/Week", 
                                          "3: Yes, 1-2 times/week", 
                                          "Yes, 3-4 times/wk",
                                          "Yes, 3-4 Times/Wk", 
                                          "Yes, 3-4 times/week", 
                                          "Yes, 3-4 times per week",
                                          "4: Yes, 3-4 Times/Wk", 
                                          "4: Yes, 3-4 Times/Week", 
                                          "4: Yes, 3-4 times/week", 
                                          "Yes, 5 or more times/wk", 
                                          "Yes, 5 or More Times/Wk",
                                          "5: Yes, 5 or More Times/Wk",
                                          "5: Yes, 5 or more times/week", 
                                          "5: Yes, 5 or More Times/Week",
                                          "Yes, 5 or more times per week",
                                          "Yes, 5 or more times/week"),
                    labels = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

df$WAKEARL <- factor(df$WAKEARL, levels = c("No, Not In The Past 2 Wks", 
                                            "No, not in the past 2 wks", 
                                            "No, not in the past 2 weeks", 
                                            "1: No, not in the past 2 weeks",
                                            "1: No, Not In The Past 2 Wks", 
                                            "1: No, Not In The Past 2 Weeks",
                                            "Yes, < Once A Wk", 
                                            "Yes, < once a wk", 
                                            "Yes, < once a week", 
                                            "Yes, less than once a week",
                                            "2: Yes, < Once A Wk", 
                                            "2: Yes, < Once A Week", 
                                            "2: Yes, less than once a week", 
                                            "Yes, 1-2 Times/Wk", 
                                            "Yes, 1-2 times/wk", 
                                            "Yes, 1-2 times/week", 
                                            "Yes, 1-2 times per week",
                                            "3: Yes, 1-2 Times/Wk", 
                                            "3: Yes, 1-2 Times/Week", 
                                            "3: Yes, 1-2 times/week", 
                                            "Yes, 3-4 times/wk", 
                                            "Yes, 3-4 Times/Wk", 
                                            "Yes, 3-4 times/week", 
                                            "Yes, 3-4 times per week", 
                                            "4: Yes, 3-4 Times/Wk", 
                                            "4: Yes, 3-4 Times/Week", 
                                            "4: Yes, 3-4 times/week", 
                                            "Yes, 5 or more times/wk", 
                                            "Yes, 5 or More Times/Wk", 
                                            "5: Yes, 5 or More Times/Wk",
                                            "5: Yes, 5 or more times/week", 
                                            "5: Yes, 5 or More Times/Week",
                                            "Yes, 5 or more times per week",
                                            "Yes, 5 or more times/week"),
                     labels = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

df$STATUS <- factor(df$STATUS, levels = c("1: Post by BSO", 
                                          "2: Natural Post",
                                          "3: Late Peri",
                                          "4: Early Peri",
                                          "7: Unknown due to HT use",
                                          "Early peri",
                                          "Early perimenopause",
                                          "Late peri",
                                          "Late perimenopause",
                                          "Natural Post",
                                          "Post by BSO",
                                          "Pre-menopausa",
                                          "Unknown due to hormone therapy use",
                                          "Unknown due to hysterectomy",
                                          "2: Natural post",
                                          "3: Late peri",
                                          "4: Early peri",
                                          "5: Pre-menopausal",
                                          "8: Unknown due to hysterectomy",
                                          "Early Peri",
                                          "Hysterectomy/both ovaries removed",
                                          "Late Peri",
                                          "Natural post",
                                          "Post by Bilateral Salpingo Oophorectomy",
                                          "Post-menopausal",
                                          "Pregnant/breastfeeding",
                                          "Unknown due to HT use",
                                          "use"),
                    labels = c("Post-menopausal",
                               "Post-menopausal",
                               "Late perimenopausal",
                               "Early perimenopausal",
                               "Unknown", 
                               "Early perimenopausal",
                               "Early perimenopausal",
                               "Late perimenopausal",
                               "Late perimenopausal",
                               "Post-menopausal",
                               "Post-menopausal",
                               "Pre-menopausal",
                               "Unknown",
                               "Unknown",
                               "Post-menopausal",
                               "Late perimenopausal",
                               "Early perimenopausal",
                               "Pre-menopausal",
                               "Unknown",
                               "Early perimenopausal",
                               "Unknown",
                               "Late perimenopausal",
                               "Post-menopausal",
                               "Post-menopausal",
                               "Post-menopausal",
                               "Pregnant/breastfeeding",
                               "Unknown",
                               "Unknown"))

# convert from factor to numeric
df$TRBLSLE <- ifelse(df$TRBLSLE == "0", 0, 1)
df$WAKEUP <- ifelse(df$WAKEUP == "0", 0, 1)
df$WAKEARL <- ifelse(df$WAKEARL == "0", 0, 1)

# select vars. for modeling
all_df <- df %>%
  select("SWANID", 
         "VISIT", 
         "STATUS",
         "ALLARE",
         "ALLVITD",
         "ALLVITE",
         "ALLVITC",
         "ALLB12",
         "ALLB1",
         "ALLB6", 
         "ALLFOL",
         "ALLIRON",
         "ALLZINC",
         "ALLCALC",
         "E2AVE",
         "TRBLSLE", 
         "WAKEUP", 
         "WAKEARL") %>%
  drop_na()

# re-add target vars once NANs are dropped
all_df <- all_df %>% 
  rowwise() %>% 
  mutate(AVERAGE = mean(c(TRBLSLE, WAKEUP, WAKEARL), na.rm = T)) %>%
  mutate(TARGET = ceiling(AVERAGE))

# scale data
all_df_scaled <- all_df %>%
  select(c(4:15)) %>%
  scale() %>%
  as.data.frame()

# re-add SWANID, VISIT, STATUS, and TARGET
all_df_scaled$SWANID <- as.factor(all_df$SWANID)
all_df_scaled$VISIT <- as.factor(all_df$VISIT)
all_df_scaled$STATUS <- as.factor(all_df$STATUS) 
all_df_scaled$TARGET <- as.factor(all_df$TARGET) 

# set seed
set.seed(123456)

# split training and test data
train_test_split <- sample(c(TRUE, FALSE), nrow(all_df_scaled), replace = TRUE, prob = c(0.8, 0.2))
train  <- all_df_scaled[train_test_split, ]
test <- all_df_scaled[!train_test_split, ]

# fit the model
me_model <- glmer(TARGET ~ ALLARE + 
                     ALLVITC + 
                     ALLVITD +
                     ALLVITE + 
                     ALLB1 + 
                     ALLB6 + 
                     ALLB12 +
                     ALLCALC +
                     ALLFOL + 
                     ALLIRON + 
                     ALLZINC +
                     E2AVE +
                     STATUS +
                    (1 | SWANID/VISIT),
                   data = train,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa")) 

# display a summary of model's performance
summary(me_model)

# make predictions on test set
obs <- predict(me_model, newdata = test, type = "response", allow.new.levels = TRUE)
obs <- as.factor(ifelse(obs > 0.5, "1", "0"))

# view summary of model performance on test set
caret::confusionMatrix(as.factor(obs), test$TARGET, mode = "everything", positive = "1")





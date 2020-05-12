library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)

glimpse(Final_merged_set)

set.seed(4150) 

index = sample(1:nrow(Final_merged_set), 0.8*nrow(Final_merged_set)) 

train = Final_merged_set[index,] # Create the training data 
test = Final_merged_set[-index,] # Create the test data

lmMod <- lm( Total  ~ Inventories_Total_Millions+Values_in_Million + Total_Wages_in_Millions + ExIm_Value_Millions, data=train)
summary(lmMod)
distPred <- predict(lmMod, test)  # predict distance
summary (lmMod) 
AIC (lmMod)

actuals_preds <- data.frame(cbind(actuals=test$Total, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

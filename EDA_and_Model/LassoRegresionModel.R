library(tidyverse)
library(caret)
library(glmnet)

# Split the data into training and test set
set.seed(5000)
training.samples <- Final_merged_set$Year %>%
  createDataPartition(p = 0.6, list = FALSE)
train.data  <- Final_merged_set[training.samples, ]
test.data <- Final_merged_set[-training.samples, ]
# Predictor variables
x <- model.matrix(Total~., train.data)[,-1]
# Outcome variable
y <- train.data$Total
glmnet(x, y, alpha = 1, lambda = NULL)

# Find the best lambda using cross-validation
set.seed(100) 
cv <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
coef(model)


# Make predictions on the test data
x.test <- model.matrix(Total ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$Total),
  Rsquare = R2(predictions, test.data$Total)
)

lambda <- 10^seq(-3, 3, length = 100)

# Build the model
set.seed(123)
lasso <- train(
  Total ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 8),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)
# Make predictions
predictions <- lasso %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$Total),
  Rsquare = R2(predictions, test.data$Total)
)


actuals_preds <- data.frame(cbind(actuals=test$Total, predicteds=predictions))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

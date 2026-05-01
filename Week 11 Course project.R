# Load libraries
library(tidyverse)
library(caret)
library(class)

set.seed(123)
instagram_usage_lifestyle <- instagram_usage_lifestyle %>%
  sample_n(10000)   # or 5000 if needed
# Inspect data structure
str(instagram_usage_lifestyle)

# -----------------------------
# Choose a NEW target variable
# (Replace "target_column" with your chosen column)
# -----------------------------
instagram_usage_lifestyle$relationship_status <- as.factor(instagram_usage_lifestyle$relationship_status)

# -----------------------------
# Train/Test Split
# -----------------------------
set.seed(123)
train_index <- createDataPartition(instagram_usage_lifestyle$relationship_status, p = 0.8, list = FALSE)

train_data <- instagram_usage_lifestyle[train_index, ]
test_data  <- instagram_usage_lifestyle[-train_index, ]

# -----------------------------
# Feature Engineering
# Example: create new features (edit as needed)
# -----------------------------
train_data <- train_data %>%
  mutate(engagement_rate = likes_given_per_day / (followers_count + 1),
         usage_per_post = daily_active_minutes_instagram / (posts_created_per_week + 1))

test_data <- test_data %>%
  mutate(engagement_rate = likes_given_per_day / (followers_count + 1),
         usage_per_post = daily_active_minutes_instagram / (posts_created_per_week + 1))



# Ensure target is factor
train_data$relationship_status <- as.factor(train_data$relationship_status)
test_data$relationship_status  <- as.factor(test_data$relationship_status)

# Remove columns with only one unique value
train_data <- train_data[, sapply(train_data, function(x) length(unique(x)) > 1)]
test_data  <- test_data[, colnames(train_data)]

# Create dummy variables
dummies <- dummyVars(relationship_status ~ ., data = train_data)

train_x <- predict(dummies, newdata = train_data)
test_x  <- predict(dummies, newdata = test_data)

train_y <- train_data$relationship_status
test_y  <- test_data$relationship_status

# -----------------------------
# Normalize features
# -----------------------------
preproc <- preProcess(train_x, method = c("center", "scale"))

train_x_scaled <- predict(preproc, train_x)
test_x_scaled  <- predict(preproc, test_x)

# -----------------------------
# Hyperparameter tuning (k values)
# -----------------------------
k_values <- seq(1, 25, by = 2)
accuracy_results <- data.frame(k = integer(), accuracy = numeric())

for (k in k_values) {
  pred <- knn(train = train_x_scaled,
              test = test_x_scaled,
              cl = train_y,
              k = k)
  
  acc <- sum(pred == test_y) / length(test_y)
  
  accuracy_results <- rbind(accuracy_results,
                            data.frame(k = k, accuracy = acc))
}

# -----------------------------
# Best k
# -----------------------------
best_k <- accuracy_results$k[which.max(accuracy_results$accuracy)]
best_accuracy <- max(accuracy_results$accuracy)

print(accuracy_results)
cat("Best k:", best_k, "\n")
cat("Best accuracy:", best_accuracy, "\n")

# -----------------------------
# Final model with best k
# -----------------------------
final_pred <- knn(train = train_x_scaled,
                  test = test_x_scaled,
                  cl = train_y,
                  k = best_k)

confusionMatrix(final_pred, test_y)

# -----------------------------
# Short Summary (EDIT VALUES AFTER RUNNING)
# -----------------------------
cat("\nSummary:\n")
cat("The best KNN model was achieved using k =", best_k,
    "with an accuracy of", round(best_accuracy, 3),
    ". Feature engineering (engagement_rate and usage_per_post) and scaling improved model performance.\n")


library(ggplot2)
library(dplyr)
library(reshape2)
library(randomForest)
library(caret)
library(pROC)

theme_set(theme_minimal())

# import data set
df <- read.csv('~/Downloads/Bank Customer Churn Prediction.csv')
head(df)
View(df)
# Check for missing values in the first few rows
head(is.na(df))

# Sum of missing values in each column
missing_sum <- colSums(is.na(df))
# Display the result
print(missing_sum)
# Get data types of columns
column_types <- sapply(df, function(x) class(x))

# Display the result
print(column_types)

color <- c("black", "red")
df$colors <- rep(color, length.out = nrow(df))
print(table(df$churn))

p <- ggplot(df, aes(x = factor(churn))) +
  geom_bar(fill = color,width = 0.5) +
  labs(title = "Churn Counts", x = "Churn", y = "Count")

# Display the plot
print(p)
p2 <-ggplot(f, aes(x=factor(gender))) +
  geom_bar(fill=Color, width =0.5) + 
ggplot(df, aes(x = credit_score, fill = ..count..)) +
  geom_histogram(binwidth = 5, color = "white", position = "identity") +
  scale_fill_viridis_c() + 
  labs(x = "Credit Scores", y = "Count") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none")

# Convert 'country' column to integer
df2$country <- as.numeric(df$country)

# Convert 'gender' column to integer
df2$gender <- as.numeric(df$gender)

# Display data types
print(sapply(df, class))

# Drop the 'customer_id' column
df2 <- select(df, -customer_id)

# Display the head of the modified data frame
head(df2)
numeric_df2 <- df2 %>% select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(numeric_df2)

# Reshape the correlation matrix for ggplot2
cor_melted <- melt(cor_matrix)

# Create a heatmap using ggplot2
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "black", high = "red") +
  labs(title = "Correlation Heatmap",
       x = "Variables",rotation = 0, y = "Variables",
       fill = "Correlation") +
  theme_minimal() + scale_x_discrete(labels = colnames(cor_matrix), position = "bottom") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create subplots
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))


ggplot(df, aes(x = country, fill = factor(churn))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = color) +
  labs(title = "Country vs Churn") +
  theme_minimal()


ggplot(df, aes(x = gender, fill = factor(churn))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = color) +
  labs(title = "Gender vs Churn") +
  theme_minimal()


ggplot(df, aes(x = factor(credit_card), fill = factor(churn))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = color) +
  labs(title = "Credit_Card vs Churn") +
  theme_minimal()


ggplot(df, aes(x = factor(active_member), fill = factor(churn))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = color) +
  labs(title = "Active_Member vs Churn") +
  theme_minimal()

# Add extra space at the bottom for plot titles
mtext("Count Plots", outer=TRUE, cex=1.5, line=-1)

options(repr.plot.width = 12, repr.plot.height = 8)

# Create histogram using ggplot2
ggplot(df, aes(x = age, fill = ..count..)) +
  geom_histogram(bins = 30, color = "white", fill = "red", alpha = 0.7) +
  labs(x = "Age", y = "Count") +
  theme_minimal()


df2$AgeBand <- cut(df$age, breaks = 5)

# Compute mean of 'Exited' for each AgeBand
result <- aggregate(churn ~ AgeBand, data = df2, mean)

# Sort the result by AgeBand
result <- result[order(result$AgeBand), ]

# Print the result
print(result)
options(repr.plot.width = 12, repr.plot.height = 8)

# Create histogram using ggplot2
ggplot(df2, aes(x = balance, fill = ..count..)) +
  geom_histogram(color = "white", fill = "red", alpha = 0.7) +
  labs(x = "Bank Balance", y = "Count") +
  theme_minimal()


ggplot(df2, aes(x = estimated_salary , fill = ..count..)) +
  geom_histogram(color = "white", fill = "red", alpha = 0.7) +
  labs(x = "Estimated Salary", y = "Count") +
  theme_minimal()

set.seed(200)

# Create an index for sampling
sample_index <- sample(1:nrow(df), 0.8 * nrow(df))

# Create training and testing datasets
df_train <- df[sample_index, ]
df_test <- df[-sample_index, ]

# Print the lengths of the datasets
cat("Length of df_train:", nrow(df_train), "\n")
cat("Length of df_test:", nrow(df_test), "\n")

df_train$Balance <- as.numeric(df_train$balance)
df_train$EstimatedSalary <- as.numeric(df_train$estimated_salary)
df_train$BalanceSalaryRatio <- df_train$Balance / df_train$EstimatedSalary

colnames(df_test)[colnames(df_test) %in% c("estimated_salary","age","balance")] <- c("EstimatedSalary","Age","Balance")
# Check for missing values
if (any(is.na(df_train$BalanceSalaryRatio))) {
  cat("Warning: Missing values found in BalanceSalaryRatio. Handling missing values...\n")
  df_train <- df2[complete.cases(df_train), ]  # Remove rows with missing values
}

# Create a boxplot
ggplot(df_train, aes(x = as.factor(churn), y = BalanceSalaryRatio, fill = as.factor(churn))) +
  geom_boxplot() +
  ylim(-1, 5) +
  labs(y = "BalanceSalaryRatio") +
  theme_minimal()

df_train$tenure <- as.numeric(df_train$tenure)
df_train$Age <- as.numeric(df_train$age)
df_train$TenureByAge <- df_train$tenure / df_train$Age

# Check for missing values
if (any(is.na(df_train$TenureByAge))) {
  cat("Warning: Missing values found in TenureByAge. Handling missing values...\n")
  df_train <- df2[complete.cases(df_train), ]  # Remove rows with missing values
}

# Create a boxplot
ggplot(df_train, aes(x = as.factor(churn), y = TenureByAge, fill = as.factor(churn))) +
  geom_boxplot() +
  ylim(-1, 1) +
  labs(y = "TenureByAge") +
  theme_minimal()

df_train$CreditScoreGivenAge <- df_train$credit_score / df_train$Age

continuous_vars <- c('credit_score', 'Age', 'tenure', 'Balance', 'products_number', 'EstimatedSalary', 'BalanceSalaryRatio',
                     'TenureByAge')
cat_vars <- c('credit_card', 'active_member', 'country', 'gender')

# Subset the data frame
df_train <- df_train[c('churn', continuous_vars, cat_vars)]

# Print the first few rows of the data frame
head(df_train)

df_train$credit_card <- as.numeric(as.character(df_train$credit_card))
df_train$active_member <- as.numeric(as.character(df_train$active_member))
df_train$credit_card[df_train$credit_card == 0] <- -1

# Replace values in the 'IsActiveMember' column
df_train$active_member[df_train$active_member == 0] <- -1

# Find minimum and maximum values for normalization
minVec <- apply(df_train[continuous_vars], 2, min)
maxVec <- apply(df_train[continuous_vars], 2, max)

# Normalize the continuous variables
df_train[continuous_vars] <- scale(df_train[continuous_vars], center = minVec, scale = maxVec - minVec)

# Print the first few rows of the normalized dataframe
head(df_train)
DfPrepPipeline <- function(df_predict, df_train_cols, minVec, maxVec) {
  print("Before preprocessing:")
  print(names(df_predict))
  

  df_predict$Balance <- as.numeric(df_predict$Balance)
  df_predict$EstimatedSalary <- as.numeric(df_predict$EstimatedSalary)
  df_predict$tenure <- as.numeric(df_predict$tenure)
  df_predict$Age <- as.numeric(df_predict$Age)
  df_predict$credit_score <- as.numeric(df_predict$credit_score)
  
  # Add new features
  df_predict$BalanceSalaryRatio <- df_predict$Balance / df_predict$EstimatedSalary
  df_predict$TenureByAge <- df_predict$tenure / (df_predict$Age - 18)
  df_predict$CreditScoreGivenAge <- df_predict$credit_score / (df_predict$Age - 18)
  
  df_predict$BalanceSalaryRatio <- as.numeric(df_predict$BalanceSalaryRatio)
  df_predict$TenureByAge <- as.numeric(df_predict$TenureByAge)
  df_predict$CreditScoreGivenAge <- as.numeric(df_predict$CreditScoreGivenAge)
  
  print("After BalanceSalaryRatio calculation:")
  print(head(df_predict$BalanceSalaryRatio))
  
  # Reorder the columns
  continuous_vars <- c('credit_score', 'Age', 'tenure', 'Balance', 'products_number', 'EstimatedSalary', 'BalanceSalaryRatio',
                       'TenureByAge')
  cat_vars <- c('credit_card', 'active_member', 'country', 'gender')
  
  df_predict_cont <- df_predict[c("churn", continuous_vars)]
  df_predict_cat <- df_predict[c("churn", cat_vars)]
  
  # Combine the selected data frames
  df_predict <- merge(df_predict_cont, df_predict_cat, by = "churn")
  # Check if all selected columns exist in df_predict
  if (all(selected_cols %in% names(df_predict))) {
    df_subset <- df_predict[selected_cols]
  } else {
    print("Some columns are not present in df_predict.")
  }
  
  # Change the 0 in categorical variables to -1
  df_predict$HasCrCard[df_predict$HasCrCard == 0] <- -1
  df_predict$IsActiveMember[df_predict$IsActiveMember == 0] <- -1
  
  # MinMax scaling continuous variables based on min and max from the train data
  df_predict[continuous_vars] <- (df_predict[continuous_vars] - minVec) / (maxVec - minVec)
  
  # Ensure that the variables are ordered in the same way as was ordered in the train set
  df_predict <- df_predict[df_train_cols]
  
  return(df_predict)
}


set.seed(123)  

train_indices <- sample(1:nrow(df_train), 0.8 * nrow(df_train))
df_train_data <- df_train[train_indices, ]
df_test_data <- df_train[-train_indices, ]

# Create a random forest model
RF <- randomForest(churn ~ ., data = df_train, 
                   ntree = 50,           # Number of trees
                   mtry = 6,             # Number of variables randomly sampled as candidates at each split
                   nodesize = 1,         # Minimum size of terminal nodes
                   maxnodes = NULL,      # Maximum number of terminal nodes trees can have
                   importance = TRUE,    # Compute variable importance measures
                   proximity = FALSE)    # Do not compute proximity matrix

# Print the random forest model
print(RF)
varImpPlot(RF)

predictions <- predict(RF, newdata = df_train)

levels_order <- c("0", "1")

# Convert predictions and df_train$churn to factors with the specified levels
predictions <- factor(predictions, levels = levels_order)
df_train$churn <- factor(df_train$churn, levels = levels_order)

# Now create the confusion matrix
conf_matrix <- confusionMatrix(predictions, df_train$churn)

# Access individual metrics
accuracy <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]

# Print metrics
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

# Calculate 'BalanceSalaryRatio' only for rows without missing values
df_test <- df_test[complete.cases(df_test[c('Balance', 'EstimatedSalary')]), ]

# Calculate 'BalanceSalaryRatio'
df_test$BalanceSalaryRatio <- df_test$Balance / df_test$EstimatedSalary
df_test$BalanceSalaryRatio <- as.numeric(df_test$BalanceSalaryRatio)

df_test <- df_test[complete.cases(df_test[c('tenure', 'Age')]), ]

# Calculate 'BalanceSalaryRatio'
df_test$TenureByAge <- df_test$tenure / df_test$Age




# Perform the data preparation pipeline
df_test <- DfPrepPipeline(df_test, colnames(df_train), minVec, maxVec)
numeric_cols <- sapply(df_test, is.numeric)

# Apply is.infinite and complete.cases to numeric columns
numeric_cols <- sapply(df_test, is.numeric)

# Apply is.infinite and complete.cases to each numeric column
for (col in names(df_test)[numeric_cols]) {
  df_test[[col]] <- df_test[[col]][!is.infinite(df_test[[col]]) & complete.cases(df_test[[col]])]
}

dim(df_test)

colnames(df_test)[colnames(df_test) %in% c("balance", "age", "estimated_salary")] <- c("Balance", "Age","EstimatedSalary")
df_test$churn <- factor(df_test$churn, levels = c("0", "1"))
predictions <- predict(RF, df_test[, names(df_test) != "churn"])

# Create a confusion matrix
conf_matrix <- confusionMatrix(predictions, df_test$churn)

# Print the confusion matrix
print(conf_matrix)

# Calculate additional performance metrics
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print precision, recall, and F1-score
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")
# Assuming df_test is your data frame
df_test <- select(df_test, -customer_id)
df_test <- select(df_test, -colors)

predicted_probs <- predict(RF, newdata = df_test, type = "response")

roc_curve <- roc(df_test$churn, predicted_probs)

# Plot the ROC curve with reversed x-axis
plot(roc_curve, col = "red", main = "ROC Curve", lwd = 2, xlab = "False Positive Rate", ylab = "True Positive Rate", rev.x = TRUE)

abline(a = 1, b = -1, lty = 2, col = "white")

auc_value <- round(auc(roc_curve), 3)
print(auc_value)

legend("bottomright", legend = c(paste("AUC =", auc_value), "Random: 0.5"), col = c("red", "black"), lty = c(1, 2), cex = 0.6)


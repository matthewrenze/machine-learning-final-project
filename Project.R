# Load packages
library(caret)

# Set random number seed
set.seed(42)

# Set working directory
setwd("C:/Work/School/Machine Learning/Project")

# Read training data file
training.all <- read.csv("pml-training.csv")

# Read test data file
testing <- read.csv("pml-testing.csv")

# Remove first seven columns
training.all <- training.all[, -(1:7)]
testing <- testing[, -(1:7)]

# Remove variables with nearly zero variance
near.zero.var <- nearZeroVar(training.all)
training.all <- training.all[, -near.zero.var]
testing <- testing[, -near.zero.var]

# Remove variables with 95% NA values
mostly.na <- sapply(training.all, function(x) mean(is.na(x))) > 0.95
training.all <- training.all[, mostly.na == FALSE]
testing <- testing[, mostly.na == FALSE]

# Create training set
index <- createDataPartition(
  y = training.all$classe,
  p = 0.75)[[1]]

# Create training set
training <- training.all[index, ]

# Create test set
validation <- training.all[-index, ]

# NOTE: REMOVE FOR FINAL MODEL
# training <- training[sample(1:nrow(training), 1000), ]
# validation <- validation[sample(1:nrow(validation), 1000), ]

# Create tree model
tree.model <- train(
  form = classe ~ .,
  data = training,
  method = "rpart")

# Predict with tree model
tree.predictions <- predict(
  object = tree.model,
  newdata = validation)

# Create tree confusion matrix
tree.matrix <- confusionMatrix(
  data = tree.predictions,
  reference = validation$classe)

# Inspect tree accuracy
accuracy <- tree.matrix$overall[1]

# Print accuracy
print(accuracy)

# Create random forest model
forest.model <- train(
  form = classe ~ .,
  data = training,
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 5))

# Predict with random forest model
forest.predictions <- predict(
  object = forest.model,
  newdata = validation)

# Create random forest confusion matrix
forest.matrix <- confusionMatrix(
  data = forest.predictions,
  reference = validation$classe)

# Inspect random forest accuracy
accuracy <- forest.matrix$overall[1]

# Print random forest accuracy
print(accuracy)

# Perform final predictions on test set
final.predictions <- predict(
  object = forest.model,
  newdata = testing)

print(final.predictions)

plot(
  x = forest.model$finalModel,
  main = "Random Forest Error by Number of Trees")

plot(varImp(forest.model))

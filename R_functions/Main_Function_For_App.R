# Library all necessary packages
library(randomForest)
library(ggplot2)
library(ROCit)
library(reshape2)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

# Function 1 -> Decision tree function
# Description: fit data to a single decision tree model
# Usage: decision_tree(x, m, index, indicator, argument)
# Arguments:x-the input data frame
# Arguments:m-proportion of rows that are used as train set, range from 0 to 1
# Arguments:index-the column number in x where the response data column locates
# Arguments:indicator-can equal to 0 or 1, when indicator equals to 0 x will be fitted with regression tree, 
# when indicator equals to 1 x will be fitted with classification tree
# Arguments: positive_class-tell the numeric value representing the row index of anyone single observation whose #response should be regarded as positive. 
# maxdepth is the maximal depth allowed for this decison tree, in default, it's 10.
decision_tree <- function(data, m, response_name = NULL, indicator, positive_class_name = NULL, maxdepth = 10){
  n <- nrow(data)
  if (m <= 0 | m >= 1){
    stop("Sorry, your training set is invalid.")
  }
  if (indicator!="classification" & indicator!="regression"){
    stop("Your problem can only be a classification problem or a regression problem!")
  }
  # Get response_index from response_name
  response_name <- as.character(response_name)
  data_col_names <- as.character(colnames(data))
  index <- which(data_col_names == response_name)[1]
  # Get the positive_class_index from positive_class_name for classification problem
  if (indicator == "classification"){
    positive_class_name <- as.character(positive_class_name)
    tmp <- as.character(data[ ,index])
    positive_class <- which(tmp == positive_class_name)[1]
  }
  if (indicator == "classification" & length(unique(as.factor(data[ ,index]))) > 2){
    stop("Sorry, we only support binary classification problem at this stage!")
  }
  train_num <- floor(m*n)
  train_data <- data[1:train_num, ]
  test_data <- data[(train_num+1):n, ]
  train_x <- train_data[ ,-index]
  train_y <- train_data[ ,index]
  test_x <- test_data[ ,-index]
  test_y <- test_data[ ,index]
  # Get the name of the response
  response_var <- colnames(data)[index]
  # Get the names of the predictors
  predictor_var <- colnames(data)[-index]
  # Get the model formula
  formula <- as.formula(paste(response_var, "~", "." ,sep = " "))
  
  if (indicator == "regression"){
    # Make sure the response variable is in the numeric type
    train_y <- as.numeric(train_y)
    test_y <- as.numeric(test_y)
    # Get the new train data
    train_data <- as.data.frame(cbind(train_x, train_y))
    colnames(train_data) <- c(predictor_var, response_var)
    # Regression tree
    model <- rpart(formula, method = "anova", train_data, maxdepth = maxdepth) # training
    predict <- predict(model, newdata = test_x, type = "vector") # testing
    predict <- as.numeric(predict)
    # RMSE value
    reg.rmse <- sqrt(sum((predict - test_y)^2) / length(test_y))
    # MSE plot missing
    return(list("PlotObject" = model, "RMSE" = reg.rmse))
  } else{
    if (length(positive_class) > 1){
      stop("You only need to specify one observation whose response should be regarded as positive!")
    }
    # Get the positive class
    pos <- as.character(data[ ,index])[positive_class]
    # Transfer the response variable into character type first
    train_y <- as.character(train_y)
    test_y <- as.character(test_y)
    # Transfer the reponse variable into 0/1 type, 0 stands for negative, 1 stands for positive
    train_y[train_y == pos] <- 1
    train_y[train_y != 1] <- 0
    test_y[test_y == pos] <- 1
    test_y[test_y != 1] <- 0
    train_y <- as.factor(train_y)
    test_y <- as.factor(test_y)
    # Get the new train data
    train_data <- as.data.frame(cbind(train_x, train_y))
    colnames(train_data) <- c(predictor_var, response_var)
    # Classification tree
    model <- rpart(formula, method = "class", train_data, maxdepth = maxdepth) # training
    predict <- predict(model, newdata = test_x, type = "prob") # testing
    # Generate the ROC object, which can be used for generating ROC curve and K-S curve later
    roc_object <- rocit(score = predict[ ,"1"], class = test_y, method = "empirical")
    # Generate the ROC Curve Plot with the roc_plot function, which is defined below
    rocgraph <- roc_plot(roc_object)
    # Generate the K-S Curve Plot with the ks_plot function, which is also defined below
    ksgraph <- ks_plot(roc_object)
    # Generate the confusion matrix object, which then will be returned and used for generation Confusion Matrix Visualization
    predict.compare <- c()
    for (i in 1:nrow(test_data)){
      predict.compare[i] <- as.numeric(predict[i, "0"] < predict[i, "1"])
    }
    confusion_mat <- confusionMatrix(data = as.factor(predict.compare), reference = test_y, positive = "1")
    # Get the accuracy measure of this classification on the test dataset
    accuracy <- confusion_mat$overall[1]
    return(list("PlotObject" = model, "ROCPlot" = rocgraph, "KSPlot" = ksgraph,
                "ConfusionMatrix" = confusion_mat, "Accuracy" = accuracy))
  }
}

# Function 2 -> Build a random forest model according to the user's input arguments
# Arguments: data is the whole dataframe.
# m is the proportion of observations that user wants to use as the train dataset, m should be less than 1.
# index is a numeric value representing the column index of the response in this dataset.
# indicator="classification" for classification problem and indicator="regression" for regression problem.
# num_trees is the number of trees we want to include in our forest model, in default, it's 200.
# num_variables_split is the number of variables sampled as candidates variables at each split, in default, it's sqrt(total number of predictors) for classification problem and (total number of predictors)/3 for regression problem.
# randomforest will perform bootstrap sampling when fitting each tree and use the sampled observations to fit the tree, this argument represents whether the sampling process should be done with or without replacement, in default, it's TRUE.
# node_size is the minimum number of observations to keep in the terminal node. Larger this number, smaller the individual tree will be. In default, it's set to be 1 for classification problem and 5 for regression problem.
# importance_meansure is the method used to measure the importance of each variable in our random forest model, we support two types of measurements. "accuracy" is mean decrease in accuracy, "impurity" is mean decrease in node impurity. For classification problem, impurity is measured by Gini Index, for regression problem, impurity is measured by residual sum of squares.
# This package only supports binary classification problems at this stage, therefore, positve_class is a numeric value representing
# the row index of anyone single observation whose response should be regarded as positive in this problem, in default it's 1.
randomforest_build <- function(data, m, response_name = NULL, indicator = c("classification", "regression"), num_trees = 200,
                               num_variables_split = NULL, replace = TRUE, node_size = NULL, 
                               importance_measure = c("accuracy", "impurity"), positive_class_name = NULL){
  # Check if the input arguments are valid
  if (m >= 1 | m <= 0){
    stop("The propotion of observations used as train set should lie in the interval (0,1)!")
  }
  if (num_trees%%1 != 0){
    stop("Your input number of trees must be an integer!")
  }
  if (indicator!="classification" & indicator!="regression"){
    stop("Your problem can only be a classification problem or a regression problem!")
  }
  if (!is.null(num_variables_split)){
    if (num_variables_split < 1){
      stop("At least you need to consider one variable as the candidate at split!")
    }
    if (num_variables_split%%1 != 0){
      stop("The number of variables sampled as candidates variables at each split should be an integer!")
    }
  }
  if (!is.null(node_size)){
    if (node_size < 1){
      stop("At least there should be one observation in the terminal node!")
    }
    if (node_size%%1 != 0){
      stop("The minimum number of observations to keep in the terminal node should be an integer!")
    }
  }
  if (importance_measure!="accuracy" & importance_measure!="impurity"){
    stop("We only support two types of importance measurements, accuracy and impurity, you should use one of them")
  }
  # Get response_index from response_name
  response_name <- as.character(response_name)
  data_col_names <- as.character(colnames(data))
  index <- which(data_col_names == response_name)[1]
  # Get the positive_class_index from positive_class_name for classification problem
  if (indicator == "classification"){
    positive_class_name <- as.character(positive_class_name)
    tmp <- as.character(data[ ,index])
    positive_class <- which(tmp == positive_class_name)[1]
  }
  if (indicator == "classification" & length(unique(as.factor(data[ ,index]))) > 2){
    stop("Sorry, we only support binary classification problem at this stage!")
  }
  train_id <- seq(1, floor(m*nrow(data)), 1) # Pick up observations in train dataset
  train <- data[train_id, ] # Train set
  test <- data[-train_id, ] # Test set
  # Divide the train set into predictors and response
  train_x <- train[ ,-index]
  train_y <- train[ ,index]
  # Divide the test set into predictors and response
  test_x <- test[ ,-index]
  test_y <- test[ ,index]
  
  # Deal with the classification problem
  if (indicator == "classification"){
    # Get the positive class
    pos <- as.character(data[ ,index])[positive_class]
    # Transfer the response variable into character type first
    train_y <- as.character(train_y)
    test_y <- as.character(test_y)
    # Transfer the reponse variable into 0/1 type, 0 stands for negative, 1 stands for positive
    train_y[train_y == pos] <- 1
    train_y[train_y != 1] <- 0
    test_y[test_y == pos] <- 1
    test_y[test_y != 1] <- 0
    train_y <- as.factor(train_y)
    test_y <- as.factor(test_y)
    # Check if the user input the num_variables_split
    if (is.null(num_variables_split)){
      num_variables_split <- floor(sqrt(ncol(train_x))) # Use the default value for classification problem
    }
    # Determine the random sample size needed to be drawn when fitting each tree
    if (replace){
      sample_size <- nrow(train_x) # With replacement, just the same as the number of observations in the train set
    } else{
      sample_size <- ceiling(0.632*nrow(train_x)) # Without replacement, another rule of thumb value is used
    }
    # Check if the user input the node_size
    if (is.null(node_size)){
      node_size <- 1 # Use the default value for classification problem
    }
    # Fit the random forest model using train set
    mod <- randomForest(x = train_x, y = train_y, ntree = num_trees, mtry = num_variables_split, replace = replace,
                        sampsize = sample_size, nodesize = node_size, importance = TRUE, proximity = FALSE,
                        norm.votes = TRUE, keep.forest = TRUE)
    # Use the fitted model to predict our test dataset
    predict_y <- predict(mod, newdata = test_x, type = "prob", predict.all = FALSE)
    # Generate the variable importance plot
    if (importance_measure == "accuracy"){
      mert <- 1
      axis_name <- "Mean decrease in accuracy"
    } else{
      mert <- 2
      axis_name <- "Mean decrease in Gini Index"
    } # Determine the type of measurement user want
    varimpgraph <- var_imp_plot(model = mod, measure_type = mert, name = axis_name)
    # Generate the ROC object, which can be used for generating ROC curve and K-S curve later
    roc_object <- rocit(score = predict_y[ ,"1"], class = test_y, method = "empirical")
    # Generate the ROC Curve Plot with the roc_plot function, which is defined below
    rocgraph <- roc_plot(roc_object)
    # Generate the K-S Curve Plot with the ks_plot function, which is also defined below
    ksgraph <- ks_plot(roc_object)
    # Generate the confusion matrix object, which then will be returned and used for generation Confusion Matrix Visualization
    predict_y_factor <- predict(mod, newdata = test_x, type = "response", predict.all = FALSE)
    confusion_mat <- confusionMatrix(data = predict_y_factor, reference = test_y, positive = "1")
    # Get the accuracy measure of this classification on the test dataset
    accuracy <- confusion_mat$overall[1]
    # Return all the results we have for classification problem
    return(list("VarImportancePlot" = varimpgraph, "ROCPlot" = rocgraph, "KSPlot" = ksgraph,
                "ConfusionMatrix" = confusion_mat, "Accuracy" = accuracy))
  } else{
    # Deal with the regression problem
    # Make sure the response variable in is the numeric type
    train_y <- as.numeric(train_y)
    test_y <- as.numeric(test_y)
    # Check if the user input the num_variables_split
    if (is.null(num_variables_split)){
      num_variables_split <- max(floor(ncol(train_x)/3), 1) # Use the default value for regression problem
    }
    # Determine the random sample size needed to be drawn when fitting each tree
    if (replace){
      sample_size <- nrow(train_x) # With replacement, just the same as the number of observations in the train set
    } else{
      sample_size <- ceiling(0.632*nrow(train_x)) # Without replacement, another rule of thumb value is used
    }
    # Check if the user input the node_size
    if (is.null(node_size)){
      node_size <- 5 # Use the default value for regression problem
    }
    # Fit the random forest model using train set
    mod <- randomForest(x = train_x, y = train_y, ntree = num_trees, mtry = num_variables_split, replace = replace,
                        sampsize = sample_size, nodesize = node_size, importance = TRUE, proximity = FALSE,
                        norm.votes = TRUE, keep.forest = TRUE)
    # Use the fitted model to predict our test dataset
    # For regression problem, we only need the fitted value, type="response" here
    predict_y <- predict(mod, newdata = test_x, type = "response", predict.all = FALSE)
    # Generate the variable importance plot
    if (importance_measure == "accuracy"){
      mert <- 1
      axis_name <- "Mean decrease in accuracy"
    } else{
      mert <- 2
      axis_name <- "Mean decrease in residual sum of squares" 
    } # Determine the type of measurement user want
    varimpgraph <- var_imp_plot(model = mod, measure_type = mert, name = axis_name)
    # Calculate the RMSE of this model on the testing dataset
    rmse <- sqrt(sum((predict_y - test_y)^2) / length(test_y))
    # Return all the results we have for regression problem
    return(list("VarImportancePlot" = varimpgraph, "RMSE" = rmse))
  }
}
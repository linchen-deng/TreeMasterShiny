# Library all necessary packages
library(randomForest)
library(ggplot2)
library(ROCit)
library(reshape2)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

# Function 3 -> Generate the ROC Curve plot and calculate the AUC score
# Arguments: roc here is the roc object returned by previous randomforest_build function, this object contains
# all information needed for ROC Curve plot and AUC score calculation.
roc_plot <- function(roc){
  # Get the AUC score and round it to four digits
  auc <- round(roc$AUC, 4)
  # Get the TPR and FPR given different cutoff values
  tpr <- roc$TPR
  fpr <- roc$FPR
  # Combine them into a dataframe, we need another fpr column so that we can draw the Chance line
  roc_draw_data <- as.data.frame(cbind(tpr, fpr, fpr))
  colnames(roc_draw_data) <- c("TPR", "FPR", "FPR2")
  # Melt the dataframe so that it can be used for ggplot 
  roc_draw_data <- melt(roc_draw_data, id.vars = "FPR")
  # Generate the ROC Curve
  roc_curve <- ggplot(roc_draw_data, aes(x = FPR, y = value, color = variable)) + geom_line() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(x = "1-Specificity (FPR)", y = "Sensitivity (TPR)", col = "Line Types") +
    ggtitle(paste("ROC Curve\n", "AUC Score = ", as.character(auc), sep = "")) +
    scale_color_manual(labels = c("Empirical ROC curve", "Random line"),
                       values = c("#f8766d", "#00b0f6")) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1)) +
    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1))
  return(roc_curve)
}

# Function 4 -> Generate the K-S Curve plot and calculate the K-S test statistic
# Arguments: roc here is the roc object returned by previous randomforest_build function, this object contains
# all information needed for K-S Curve plot and K-S test statistic calculation.
ks_plot <- function(roc){
  # We first get the contents of a KS plot with the built-in function ksplot in package ROCit
  # We will make the plot invisible and only get the contents of it because we will generate this graph
  # again using ggplot2
  ff <- tempfile()
  png(filename = ff)
  plot_content <- ksplot(roc, values = TRUE)
  invisible(dev.off())
  unlink(ff)
  # Prepare the plotting data
  ecdf_pos <- plot_content$`G(c)` # CDF of positive case
  ecdf_neg <- plot_content$`F(c)` # CDF of negative case
  ks_stat <- plot_content$`KS stat` # K-S statistic
  ks_cutoff <- plot_content$`KS Cutoff` # Cut off value corresponding to the K-S statistic
  cutoff <- plot_content$Cutoff
  # Get the ecdf_pos and ecdf_neg value corresponding to the K-S statistic
  ks_pos <- sort(ecdf_pos)[which(sort(cutoff) == ks_cutoff)[1]]
  ks_neg <- sort(ecdf_neg)[which(sort(cutoff) == ks_cutoff)[1]]
  # Combine them into a dataframe
  ks_draw_data <- as.data.frame(cbind(sort(cutoff), sort(ecdf_pos), sort(ecdf_neg)))
  colnames(ks_draw_data) <- c("CUT", "POS", "NEG")
  # Melt the dataframe so that it can be used for ggplot
  ks_draw_data <- melt(ks_draw_data, id.vars = "CUT")
  # Generate the K-S Curve
  ks_curve <- ggplot(ks_draw_data, aes(x = CUT, y = value, color = variable)) + geom_line() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(x = "Cutoff(c)", y = "Empirical cumulative density function", col = "Line Types") +
    ggtitle(paste("K-S Curve\n", "K-S Statistic = ", as.character(round(ks_stat, 4)), sep = "")) +
    scale_color_manual(labels = c("CDF of Positive Cases", "CDF of Negative Cases"), 
                       values = c("#f8766d", "#00b0f6")) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1)) +
    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1)) +
    geom_segment(aes(x = ks_cutoff, y = ks_pos, xend = ks_cutoff, yend = ks_neg),
                 linetype = "dashed", color = "green") +
    geom_point(aes(x = ks_cutoff, y= ks_pos), color="green", size = 4) +
    geom_point(aes(x = ks_cutoff, y= ks_neg), color="green", size = 4) +
    annotate("label", x = ks_cutoff+0.1, y = (ks_pos+ks_neg)/2, fill = "yellow",
             label= paste("K-S Statistic at Cutoff = ", as.character(round(ks_cutoff, 2)), sep = ""))
  return(ks_curve)
}

# Function 5 -> Generate the visualized confusion matrix
# Arguments: cm_matrix is the confusion matrix object returned by previous randomforest_build function for binary classification problem.
# positive is a character string for the name of the positive class.
# negative is a character string for the name of the negative class.
confusion_matrix_plot <- function(cm_matrix, positive = NULL, negative = NULL){
  # Check if the user input positive class and negative class names correctly
  if (is.null(positive)){
    stop("Please enter a character name for your positive class!")
  } else{
    positive <- as.character(positive)
  }
  if(is.null(negative)){
    stop("Please enter a character name for your negative class!")
  } else{
    negative <- as.character(negative)
  }
  # Initialize the visualization matrix
  layout(matrix(c(1, 1, 2)))
  par(mar=c(2, 2, 2, 2))
  plot(c(100, 345), c(300, 450), type = "n", xlab = "", ylab= "", xaxt = 'n', yaxt = 'n')
  title('CONFUSION MATRIX', cex.main=2)
  # Add labels and colors to the marix
  rect(150, 430, 240, 370, col = '#f8766d')
  text(195, 435, positive, cex = 1.2)
  rect(250, 430, 340, 370, col = '#00b0f6')
  text(295, 435, negative, cex=1.2)
  text(125, 370, 'Predicted Class', cex = 1.3, srt = 90, font = 2)
  text(245, 450, 'Actual Class', cex = 1.3, font=2)
  rect(150, 305, 240, 365, col = '#00b0f6')
  rect(250, 305, 340, 365, col = '#f8766d')
  text(140, 400, positive, cex = 1.2, srt = 90)
  text(140, 335, negative, cex = 1.2, srt = 90)
  # Add count values to the matrix
  val <- as.numeric(cm_matrix$table)
  text(195, 400, val[4], cex=1.6, font=2, col='white')
  text(195, 335, val[3], cex=1.6, font=2, col='white')
  text(295, 400, val[2], cex=1.6, font=2, col='white')
  text(295, 335, val[1], cex=1.6, font=2, col='white')
  # Add some evaluation statistics to the matrix
  plot(c(100, 0), c(100, 0), type = "n", xlab = "", ylab = "", main = "DETAILS", xaxt = 'n', yaxt = 'n')
  text(10, 85, names(cm_matrix$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm_matrix$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm_matrix$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm_matrix$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm_matrix$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm_matrix$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm_matrix$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm_matrix$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm_matrix$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm_matrix$byClass[7]), 3), cex=1.2)
  # Add two overall evaluation metrics to the matrix
  text(30, 35, names(cm_matrix$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm_matrix$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm_matrix$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm_matrix$overall[2]), 3), cex=1.4)
}

# Function 6 -> Generate the variable importance plot
# Arguments: model is the model object fitted using the train dataset.
# measure_type can either be 1 or 2, which are two types of importance measurements. 1 represents mean 
# decrease in accuracy, 2 represents mean decrease in node impurity. For classification problem, 
# impurity is measured by Gini Index, for regression problem, impurity is measured by residual 
# sum of squares.
# name is a character string, which represents the detailed name of the importance measurement used, this 
# arguement is used in the title of the variable importance plot.
var_imp_plot <- function(model, measure_type = c(1, 2), name){
  # Check if the input measurement type is valid
  if (measure_type!=1 & measure_type!=2){
    stop("We only support two types of importance measurements now, please either input 1 or 2!")
  }
  # Get the importance measure for each variable
  var_importance <- importance(model, type = measure_type, scale = TRUE)
  # Transfer the variable importance vector into a dataframe
  var_importance <- cbind(rownames(var_importance), as.data.frame(var_importance, row.names = FALSE))
  colnames(var_importance) <- c("variable", "importance")
  # Axis name for the importance measure
  axis_name_full <- paste("Variable Importance (", name, ")", sep = "")
  # Generate the variable importance plot
  varimpgraph <- ggplot(var_importance, aes(x = reorder(variable, importance), y = importance)) +
    geom_bar(stat = "identity", aes(fill = factor(variable))) + coord_flip() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.text.y = element_text(face = "italic")) + 
    labs(x = "Variable Name", y = axis_name_full) +
    ggtitle(paste("Variable Importance Plot based on", name))
  return(varimpgraph)
}

# Function 7 -> Help the user to tune one important parameters in the random forest model, number of trees(num_trees)
# Arguments: max_trees is the maximal number of trees we are going to try, in default, it's 150
# min_trees is the minimal number of trees we are going to try, in default it's 100 
# We will use every number within the interval [min_trees, max_trees] as the num_trees to fit a 
# random forest model with all other parameters keeping the same among these fitted trees, then accuracy
# for classification problem and RMSE for regression problem of each tree will be returned, a plot of 
# different num_trees tried and different accuracy/RMSE values will then be generated to help the user choose
# the best parameter num_trees with respect to accuracy or RMSE
# All the remainning arguments are the same as what have been mentioned in the previous function
# Note that the total number of values tried = max_trees - min_trees can't be larger than 100, otherwise the
# running time of this function will be too long.
num_trees_tune <- function(data, m, response_name = NULL, indicator = c("classification", "regression"), max_trees = 150,
                           min_trees = 100, num_variables_split = NULL, replace = TRUE, node_size = NULL, 
                           importance_measure = c("accuracy", "impurity"), positive_class_name = NULL){
  if (max_trees < min_trees){
    stop("Be careful, your input maximal number of trees cannot be less than the minimal number of trees!")
  }
  if ((max_trees - min_trees) > 100){
    stop("Sorry, too many tries attempted, the running time will be too long!")
  }
  if (max_trees%%1!=0 | min_trees%%1!=0){
    stop("Your input number of trees must be an integer!")
  }
  if (indicator!="classification" & indicator!="regression"){
    stop("Your problem can only be a classification problem or a regression problem!")
  }
  # Create an empty vector to store the returning accuracy/RMSE value
  try_time <- max_trees - min_trees + 1
  res_vector <- rep(NA, try_time)
  # Deal with the classification problem
  if (indicator == "classification"){
    for (k in seq(min_trees,max_trees,1)){
      tmp <- randomforest_build(data = data, m = m, response_name = response_name, indicator = indicator, num_trees = k,
                                num_variables_split = num_variables_split, replace = replace,
                                node_size = node_size, importance_measure = importance_measure,
                                positive_class_name = positive_class_name)
      res_vector[k - min_trees + 1] <- tmp$Accuracy
    }
    tries <- seq(min_trees,max_trees,1)
    # Combine two columns into dataframe so that can be used for generating the plot
    tune_draw_data <- as.data.frame(cbind(tries, res_vector))
    colnames(tune_draw_data) <- c("ntree", "accuracy")
    acc_plot <- ggplot(tune_draw_data, aes(x = ntree, y = accuracy)) + geom_line(color = "#f8766d") +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      labs(x = "Number of Trees", y = "Model Accuracy on Test set") +
      ggtitle("Model Accuracy V.S. Number of Trees") +
      scale_x_continuous(breaks = seq(min_trees,max_trees,2), limits = c(min_trees, max_trees))
    return(acc_plot)
  } else{
    # Deal with the regression problem
    for (k in seq(min_trees,max_trees,1)){
      tmp <- randomforest_build(data = data, m = m, response_name = response_name, indicator = indicator, num_trees = k,
                                num_variables_split = num_variables_split, replace = replace,
                                node_size = node_size, importance_measure = importance_measure)
      res_vector[k - min_trees + 1] <- tmp$RMSE
    }
    tries <- seq(min_trees,max_trees,1)
    # Combine two columns into dataframe so that can be used for generating the plot
    tune_draw_data <- as.data.frame(cbind(tries, res_vector))
    colnames(tune_draw_data) <- c("ntree", "rmse")
    rmse_plot <- ggplot(tune_draw_data, aes(x = ntree, y = rmse)) + geom_line(color = "#f8766d") +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      labs(x = "Number of Trees", y = "Model RMSE on Test set") +
      ggtitle("Model RMSE V.S. Number of Trees") +
      scale_x_continuous(breaks = seq(min_trees,max_trees,2), limits = c(min_trees, max_trees))
    return(rmse_plot)
  }
}

# Function 8 -> Plot the decision tree
# Arguments: model is the decision tree object returned by previous decision_tree function
Tree_plot <- function(model){
  rpart.plot(model, roundint=FALSE)
}
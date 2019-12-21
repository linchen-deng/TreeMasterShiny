library(randomForest)
library(ggplot2)
library(ROCit)
library(reshape2)
library(caret)
library(e1071)
library(mlbench)
library(rpart)
library(rpart.plot)

################################Random Forest########################################################
# Regression problem
data("BostonHousing")
data <- BostonHousing
###
result1 <- randomforest_build(data, m = .75, response_name =  "medv", indicator = "regression", num_trees = 100,
                              importance_measure = "impurity")
result1$VarImportancePlot
result1$RMSE
###
result2 <- randomforest_build(data, m = .75, response_name =  "medv", indicator = "regression", num_trees = 100,
                              importance_measure = "accuracy")
result2$VarImportancePlot
result2$RMSE
###
result3 <- num_trees_tune(data, m = .75, response_name =  "medv", indicator = "regression", max_trees = 75, min_trees = 20,
                          importance_measure = "accuracy")
result3





# Classification problem
data("PimaIndiansDiabetes")
data <- PimaIndiansDiabetes
###
result4 <- randomforest_build(data, m = .75, response_name =  "diabetes", indicator = "classification", num_trees = 100,
                              importance_measure = "impurity", positive_class_name = "pos")
result4$VarImportancePlot
result4$ROCPlot
result4$KSPlot
confusion_matrix_plot(cm_matrix = result4$ConfusionMatrix, positive = "Disease", negative = "Health")
result4$Accuracy
###
result5 <- randomforest_build(data, m = .75, response_name =  "diabetes", indicator = "classification", num_trees = 100,
                              importance_measure = "accuracy", positive_class_name = "pos")
result5$VarImportancePlot
result5$ROCPlot
result5$KSPlot
confusion_matrix_plot(cm_matrix = result5$ConfusionMatrix, positive = "Disease", negative = "Health")
result5$Accuracy
###
result6 <- num_trees_tune(data, m = .75, response_name =  "diabetes", indicator = "classification", max_trees = 75,
                          min_trees = 20, importance_measure = "accuracy", positive_class_name = "pos")
result6






########################################Decision Tree###############################################
# Regression problem
data("BostonHousing")
data <- BostonHousing
###
result7 <- decision_tree(data, m = .75, response_name =  "medv", indicator = "regression")
result7$RMSE
Tree_plot(result7$PlotObject)


# Classification problem
data("PimaIndiansDiabetes")
data <- PimaIndiansDiabetes
###
result8 <- decision_tree(data, m = .75, response_name =  "diabetes", indicator = "classification", 
                         positive_class_name = "pos", maxdepth = 4)
result8$ROCPlot
result8$KSPlot
confusion_matrix_plot(cm_matrix = result8$ConfusionMatrix, positive = "Disease", negative = "Health")
result8$Accuracy
Tree_plot(result8$PlotObject)

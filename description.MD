# Tree Master
Authors: Zeyu Chen, Linchen Deng, Tingyi Li, Yanning Wu, Yue Yu

# Overview
Our shinyapp provides user with algorithms based on decision tree machine learning methods to deal with data. There are in all four pages, the Read Data page, the Regression Page, the Classification page and the Help page.
Users can choose to use the two default datasets (One for regression and one for classification) or upload their own data. By selecting data type (regression or classification) and method (random forest or decision tree), users can generate useful plots and values. With interactive sliderInput, selectInput, radioButton, actionButton, conditional panels, we are able to provide users with smooth transition from one model to another, interactively testing different combinations of the input values. The Help page contains the vignette of the R package, and tells users the instructions for all functions involved. 

# Why this is important
Tree-based algorithm is a very popular machine learning technique, which can easily be used to deal with both classification problem and regression problem. Decision tree and random forest are the two most common tree-based algorithms. However, for those users without enough programming and statistics knowledge, it may be difficult to correctly use these two models. Therefore, we decided to build a Tree Master shiny app, which can not only help users to fit a desired decision tree and random forest model easily by simply choose some parameters in our panel, but also provide users with some model diagonositc plots so that they can clearly know the performance of their models. Because of the time constraints, this app only supports Binary Classification problem and regression problem at this stage, but we believe that it can still handle most problems in real life. In the future, we also consider adding more features to our app so that it will become more powerful. From now on, please enjoy exploring our Tree Master app!


# Components
- ## Data Input
- The data input part of the app is named “read data”. And it consists of a side panel for data selection and also, a main panel for displaying data. In the “read data” page, users can either load their own data for analysis or use the default data. For loading their own data, users can browse and select the data in “csv” format on their computer. For using the defaulting data, users can choose “Boston Housing” data for regression modeling or “Pima Indians Diabetes” data for classification modeling. After selecting or loading the data, a first few lines of the data can be displayed in the main panel. Then users can choose regression or classification algorithm based on the data.

- ## Regression
- The regression part of the app consists of a side panel for the parameters choosing and also, a main panel for plots and root mean square error (RMSE). Additionally, a default data ‘BostonHousing’ is built in this model for users to test and see the results.
The side panel provides users with parameter choices for displaying the variance importance plot on the main panel, as well as the calculated RMSE. These choices include two different supervised methods of Machine Learning: Random Forest and Decision Tree. 
By choosing Random Forest, users could further set different training ratio (0-1) and choose different number of trees (1-500) along with the variable of interest from the dataset. Also, users are able to specify the importance measure of interest such as either impurity or accuracy.
The other method could be chosen is decision tree. By using this method, tree plot with each variable of interest could be visualized along with RMSE via setting the training ratio required.


- ## Binary Classification
- ### Random Forest
- This widget allows users to manually set parameters of the `randomfroest_build()` function in our package, including proportion of training set, selecting binary response variable, selecting the positive level of the response variable, importance measure criteria, and number of trees in the random forest. Plots shown in the main panel includes variable importance plot, Receiver Operating Characteristic(ROC) Curve, Kolmogorov-Smirnov(KS) Plot, and Confusion Matrix. Tree Number Tuning provides a interface for users to select a range of trees in the random forest to observe the optimal tree numbers that give high testing accuracy, realized by the `num_trees_tune()` function from our package.

- ### Decision Tree
- This widget allows users to manually set parameters of the `decision_tree()` function in our package, including proportion of training set, selecting binary response variable, selecting the positive level of the response variable, and the maximal tree depth. Plots includes Receiver Operating Characteristic(ROC) Curve, Kolmogorov-Smirnov(KS) Plot, and Confusion Matrix. Tree visualization provides users an visualization of the decision tree using the `tree_plot()` function from the package.

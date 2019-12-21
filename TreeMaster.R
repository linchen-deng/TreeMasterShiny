
# install.packages("V8")
library(V8)
source("R_functions/Sub_Functions_For_App.R")
source("R_functions/Main_Function_For_App.R")
library(mlbench)
library(shinyjs)
library(shiny)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

ui <- navbarPage(title="TreeMaster",
                 id = "Navbar",
                 
                 
                 tabPanel(title = 'Read Data',
                          value = 'page1',
                          sidebarLayout(
                            sidebarPanel(
                              fileInput(inputId = "file1", label="Select CSV File", 
                                        accept =c("text/csv","text/comma-separated-values,text/plain",".csv")),
                              checkboxInput(inputId = "header", label= "Header", value=TRUE),
                              #to determine whether the file contains the names of the variables as its first line
                              
                              useShinyjs(),                      # Include shinyjs in the UI
                              extendShinyjs(text = jsResetCode), # Add the js code to the page
                              actionButton(inputId = "reset", label="Reset Data",
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                              
                              tags$hr(),#a horizontal rule separate the file input and other options
                              
                              selectInput(inputId = 'default', label="Load Default Data",width="100%",
                                          choices =  c("( Please select the default data )"="def",
                                                       "Regression: Boston Housing"="reg",
                                                       "Classification: Pima Indians Diabetes"="cla"),selected="def"),
                              #user choose to load the default regression data or the default classification data
                              
                              tags$hr(),
                              
                              
                              actionButton(inputId = "regression", label="Regression",
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                              actionButton(inputId = "classification", label="Classification",
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            ),
                            mainPanel(tableOutput("contents"))
                          )
                 ),  # end of page 1
                 
                 ####################################################
                 #2nd page:regression
                 tabPanel(title="Regression",
                          value = 'page2',
                          
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("model", label="Regression Models:",
                                           choices = c("Random Forest","Decision Tree"),selected = "Random Forest"),
                              
                              conditionalPanel("input.model == 'Random Forest'",
                                               sliderInput("train_RF",label="Training set ratio: ",
                                                           min= 0.0,max = 1.0, step=0.05,value = 0.75),
                                               htmlOutput("variable_rf"),
                                               sliderInput("num_trees", label="Number of trees: ",min = 1,max = 500,value = 100),
                                               radioButtons("importance", label="Importance measure:",
                                                            choices = c("impurity","accuracy"),selected = "impurity")
                              ),
                              
                              conditionalPanel("input.model == 'Decision Tree'",
                                               sliderInput(inputId="train_DT",label="Training set ratio: ",min = 0.0,max = 1.0,step=0.05,value = 0.75),
                                               htmlOutput("variable_dt"))
                            ),
                            
                            #3 plots could be chosen for Regression
                            mainPanel(  
                              conditionalPanel("input.model == 'Random Forest'", 
                                               tabsetPanel(
                                                 tabPanel("Variable Importance",plotOutput("VarImportancePlot"),textOutput("RMSE_RF")),
                                                 tabPanel("Tree Number Tuning",
                                                          sliderInput("RegParamTuneCenter","Specify the center of number of tree",
                                                                      min=1,max=100,value=50,step=1),
                                                          sliderInput("RegParamTuneRange","Specify how many neighbors to test",
                                                                      min=2,max=100,value=50,step=2),
                                                          plotOutput("num_trees_tune"))
                                               )),
                              conditionalPanel("input.model == 'Decision Tree'", 
                                               tabsetPanel(
                                                 tabPanel("Tree Visualization",plotOutput("Treeplot"),textOutput("RMSE_DT"))
                                               ))
                            ))
                 ),
                 
                 
                 ############# Classification ##################
                 tabPanel(title = 'Classification',
                          value = 'page3',
                          sidebarLayout(
                            ################ User input ###################
                            sidebarPanel(
                              #  Warning the user only this model can only handle binary classification

                              h4("Error will appear for data without any binary factor variable."),
                              #  Selecting models
                              radioButtons("ClfModel","Classification Model:",
                                           choices=list("Random Forest","Decision Tree")),
                              #  Selecting Training set proportion
                              sliderInput("TrainProp","Proportion of training set:",
                                          min=0.05,max=0.95,value=0.75, step=0.05),
                              
                              #  Selecting binary response name 
                              htmlOutput("BinaryResp"),
                              
                              #  Select which level should be considered as positive class in 
                              #  classification, will affect the confusion matrix output
                              
                              htmlOutput("BinaryLevel"),
                              
                              conditionalPanel(
                                condition = "input.ClfModel == 'Decision Tree'",
                                #  Decision Tree Only: max-depth parameter
                                sliderInput("MaxDepth","Max Depth of tree (Decision Tree only)",
                                            min=1,max=20,value=4, step=1)
                              ),
                              
                              conditionalPanel(
                                condition = "input.ClfModel == 'Random Forest'",
                                #  Select split criteria
                                radioButtons("SplitCriteria","Importance Measure Criteria:",
                                             choices=list("impurity",
                                                          "accuracy")),
                                #  Select number of trees for random forest classifier
                                sliderInput("NumTree","Number of trees in random forest:",
                                            min=1,max=500,value=100, step=1)
                              )
                            ), # end of sidebar panel
                            
                            
                            ############### UI Output ##########################
                            mainPanel(
                              ############ Random Forest ###################
                              conditionalPanel(
                                condition = "input.ClfModel == 'Random Forest'",
                                
                                tabsetPanel(
                                  # Variable importance
                                  tabPanel(
                                    title = "Variable Importance",
                                    plotOutput("RfImpPlot"),
                                    textOutput("RfAcc")
                                  ),
                                  
                                  # ROC plot
                                  tabPanel(
                                    title = "Receiver Operating Characteristic(ROC) Curve",
                                    plotOutput("RfRoc"),
                                  ),
                                  
                                  # KS plot
                                  tabPanel(
                                    title = "Kolmogorov-Smirnov(KS) Plot",
                                    plotOutput("RfKs"),
                                  ),
                                  # Confusion matrix
                                  tabPanel(
                                    title = "Confusion Matrix",
                                    htmlOutput("RfPosText"),
                                    htmlOutput("RfNegText"),
                                    plotOutput("RfConfMat")
                                  ),
                                  
                                  # Parameter Tuning 
                                  tabPanel(
                                    title = "Tree Number Tuning",
                                    sliderInput("ParamTuneCenter",
                                                "Specify the center of number of tree",
                                                min=1,max=500,value=100,step=1),                   
                                    sliderInput("ParamTuneRange","Specify how many neighbors to test",
                                                min=2,max=100,value=50,step=2),                  
                                    plotOutput("RfParamTune")
                                    
                                  )
                                )# end of tabset panel
                              ), # end condition panel
                              
                              ############### Decision Tree #################
                              conditionalPanel(
                                condition = "input.ClfModel == 'Decision Tree'",
                                
                                tabsetPanel(
                                  # Tree Plot
                                  tabPanel(
                                    title = "Tree Visualization",
                                    plotOutput("DtTreePlot"),
                                    textOutput("DtAcc")
                                  ),
                                  
                                  # ROC plot
                                  tabPanel(
                                    title = "Receiver Operating Characteristic(ROC) Curve",
                                    plotOutput("DtRoc"),
                                  ),
                                  
                                  # KS plot
                                  tabPanel(
                                    title = "Kolmogorov-Smirnov(KS) Plot",
                                    plotOutput("DtKs"),
                                  ),
                                  # Confusion matrix
                                  tabPanel(
                                    title = "Confusion Matrix",
                                    htmlOutput("DtPosText"),
                                    htmlOutput("DtNegText"),
                                    plotOutput("DtConfMat")
                                    
                                  )
                                )# end of tabset panel
                              ) # end condition panel
                              
                              
                            ) # end main panel
                          )# end of sidebar layout
                 ),# end of tab panel3/page3
                 
                 
                 
                 #############################################
                 #4th page:help page
                 tabPanel(title="Help",
                          value='page4',
                          htmlOutput("inc")
                 )
)# end of navbarPage





server <- function(input, output,session) {
  
  ##1st page server
  #read data reactive function
  loaddt <- reactive({
    if(input$default=="def"){
      inFile<-input$file1
      if(is.null(inFile)) return(NULL)#not to get warning messages before reading data
      data <- read.csv(file = inFile$datapath, header = input$header)
    }
    if(input$default=="reg") {
      data("BostonHousing")
      data <- BostonHousing
    }
    if(input$default=="cla") {
      data("PimaIndiansDiabetes")
      data <- PimaIndiansDiabetes
    }
    return(data)
  })

  ########## observeEvent functions #############
  observeEvent(input$regression, {
    updateNavbarPage(session, "Navbar",selected = "page2")
  })
  
  observeEvent(input$classification, {
    updateNavbarPage(session, "Navbar",selected = "page3")
  })
  
  observeEvent(input$file1, {
    output$contents <- renderTable({
      data <- loaddt()
      head(data)
    })
  })
  
  observeEvent(input$default, {
    output$contents <- renderTable({
      data <- loaddt()
      head(data)
    })
  })
  
  # reset page for file uploading
  observeEvent(input$reset, {
    js$reset()
  })
  
  ######################  Regression ################################
  output$variable_rf <- renderUI({
    
    if (identical(data(), '') || identical(data(),data.frame())) return(NULL)
    
    # Independent Variable selection:    
    selectInput("variable_rf", "variable",
                names(loaddt()), multiple =FALSE)
  })
  
  output$variable_dt <- renderUI({
    
    if (identical(data(), '') || identical(data(),data.frame())) return(NULL)
    
    # Independent Variable selection:    
    selectInput("variable_dt", "variable",
                names(loaddt()), multiple =FALSE)
  })
  
  ###random forest model
  model.rf<-reactive({
    data<-loaddt()
    fit.rf <- randomforest_build(data, m = input$train_RF, response_name =input$variable_rf, 
                                 indicator = "regression", num_trees = input$num_trees,
                                 importance_measure = input$importance)
    return(fit.rf)})
  
  ###plot  
  output$VarImportancePlot = renderPlot({model.rf()$VarImportancePlot})
  output$RMSE_RF <- renderText({paste("RMSE: ", model.rf()$RMSE)})
  
  
  ###parameter tuning function
  model.tune<-reactive({
    data<-loaddt()
    min_tree<-max(1,input$RegParamTuneCenter-(input$RegParamTuneRange/2))
    max_tree<-input$RegParamTuneCenter+(input$RegParamTuneRange/2)
    fit.tune<-num_trees_tune(data, m = input$train_RF, response_name =input$variable_rf, 
                             indicator = "regression", 
                             max_trees = max_tree, 
                             min_trees=min_tree,
                             importance_measure = "accuracy")
    return(fit.tune)})
  
  
  ###tuning plot
  output$num_trees_tune = renderPlot({model.tune()}) 
  
  ###decision tree model  
  model.dt<-reactive({
    data<-loaddt()
    fit.dt<-decision_tree(data, m = input$train_DT, response_name =input$variable_dt, 
                          indicator = "regression")
    return(fit.dt)}
  )
  
  ###plot
  output$Treeplot = renderPlot({Tree_plot(model.dt()$PlotObject)})
  output$RMSE_DT <- renderText({paste("RMSE: ", model.dt()$RMSE)})
  
  
  ######################  Classification ################################
  
  ################# Random Forest classifier ###########################
  # helper functions to get binray variable names
  binary_response_name <- function(data){
    factor_var_names <- names(Filter(is.factor, data))
    binary_var_name <- c()
    for (var_name in factor_var_names){
      if (length(unique(data[,var_name])) == 2){
        binary_var_name <- c(binary_var_name, var_name)
      }
    }
    return(binary_var_name)
  }
  
  binary_response_level <- function(data, resp_name){
    as.character(unique(data[,resp_name]))
  }
  
  # updated binary response name 
  output$BinaryResp <- renderUI({
    
    if (identical(loaddt(), '') || identical(loaddt(),data.frame())) return(NULL)
    
    # Binary Variable selection:   
    binary_vars <- binary_response_name(loaddt())
    selectInput("ResponseName","Select a binary variable as the response variable:",
                binary_vars, multiple =FALSE)
  })
  
  # updated binary reponse levels
  output$BinaryLevel <- renderUI({
    if (identical(loaddt(), '') || identical(loaddt(),data.frame())) return(NULL)
    
    # Binary level selection
    binary_levels <- binary_response_level(loaddt(),input$ResponseName)
    selectInput("PosLevel","Select the positive level",
                binary_levels, selected=binary_levels[1], multiple =FALSE)
  })
  
  # The user input for positive labels of confusion matrix
  output$RfPosText <- renderUI({
    textInput("RfPosText","Enter positive level label",value=input$PosLevel)
  })
  
  # The user input for negative labels of confusion matrix
  output$RfNegText <- renderUI({
    binary_levels <- binary_response_level(loaddt(),input$ResponseName)
    neg_label <- binary_levels[binary_levels!=input$PosLevel]
    textInput("RfNegText", "Enter negative level label", value=neg_label)
  })
  
  # output list from random forest
  RfClfResult <- reactive({
    result <- randomforest_build(loaddt(), 
                                 m = input$TrainProp, 
                                 response_name = input$ResponseName, 
                                 indicator = "classification", 
                                 num_trees = input$NumTree,
                                 importance_measure = input$SplitCriteria,
                                 positive_class_name = input$PosLevel)
    return(result)
  })
  # Plot showing accuracies over a range of number of trees in random forest
  output$RfParamTune <- renderPlot({
    
    min_tree <- max(1, input$ParamTuneCenter - input$ParamTuneRange/2)
    max_tree <- input$ParamTuneCenter + input$ParamTuneRange/2
    num_trees_tune(loaddt(), m = input$TrainProp, response_name = input$ResponseName, 
                   indicator = "classification", max_trees = max_tree,
                   min_trees = min_tree, importance_measure = "accuracy", 
                   positive_class_name = "pos")
  })
  
  # Random forest classifier variable importance plot 
  output$RfImpPlot <- renderPlot({
    RfClfResult()$VarImportancePlot
  })  
  
  # Random forest classifier ROC curve
  output$RfRoc <- renderPlot({
    RfClfResult()$ROCPlot
  })  
  
  # Random forest classifier KS plot
  output$RfKs <- renderPlot({
    RfClfResult()$KSPlot
  })  
  
  # Random forest classifier confusino matrix 
  output$RfConfMat <- renderPlot({
    confusion_matrix_plot(cm_matrix = RfClfResult()$ConfusionMatrix, 
                          positive = input$RfPosText, 
                          negative = input$RfNegText)
  })  
  
  # Random forest classifier accuracy
  output$RfAcc <- renderText({
    paste("Current Accuracy:",RfClfResult()$Accuracy)
  })  
  
  ################## Decision Tree Classifier #######################
  DtClfResult <- reactive({
    
    result <- decision_tree(loaddt(), m = input$TrainProp, 
                            response_name = input$ResponseName, 
                            indicator = "classification", 
                            positive_class_name = input$PosLevel, 
                            maxdepth = input$MaxDepth)
    return(result)
  })
  
  # The user input for positive labels of confusion matrix
  output$DtPosText <- renderUI({
    textInput("DtPosText","Enter positive level label",value=input$PosLevel)
  })
  
  # The user input for negative labels of confusion matrix
  output$DtNegText <- renderUI({
    binary_levels <- binary_response_level(loaddt(),input$ResponseName)
    neg_label <- binary_levels[binary_levels!=input$PosLevel]
    textInput("DtNegText", "Enter negative level label", value=neg_label)
  })
  
  # Decision Tree classifier ROC curve
  output$DtRoc <- renderPlot({
    DtClfResult()$ROCPlot
  })  
  
  # Decision Tree classifier KS plot
  output$DtKs <- renderPlot({
    DtClfResult()$KSPlot
  })  
  
  # Decision Tree classifier confusino matrix 
  output$DtConfMat <- renderPlot({
    confusion_matrix_plot(cm_matrix = DtClfResult()$ConfusionMatrix, 
                          positive = input$DtPosText, 
                          negative = input$DtNegText)
  })  
  
  # Decision Tree classifier accuracy
  output$DtAcc <- renderText({
    paste("Current Accuracy:",DtClfResult()$Accuracy)
  })  
  
  # Decision Tree classifier tree plot
  output$DtTreePlot <- renderPlot({
    Tree_plot(DtClfResult()$PlotObject)
  })
  
  
  ############################ Help Page #######################
  getPage<-function() {return(includeHTML("User-Guidance.html"))}
  output$inc<-renderUI({getPage()})
}




shinyApp(ui = ui, server = server)
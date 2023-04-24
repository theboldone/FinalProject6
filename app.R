library(shiny)
library(caret)
library(ggplot2)
library(shinythemes)
library(plotly)
library(DT)
library (shinyglide)
install.packages('shinydashboardPlus')
library(plsdepot)
library(dplyr)
library(visreg)
library(openintro)


original_df <<- read.csv("Salary_Data.csv")
learningModelLinear=NULL
trainSalData=NULL
testSalData=NULL
vec1=NULL
vec2=NULL
df=NULL

ui <- fluidPage(
  headerPanel("Predicting Salary (In Dollars) Given Age (In Years) or Work Experience (In Years)"),
  h5("Please Note All Salary Values in Graphs, Tables, Text, Etc... Are in Dollars, Experience Values in Years, and Age Values in Years"),
  HTML("<br><br><br>"),
  width = 10.3,
  tabsetPanel(
    tabPanel("Data, Topic, Analysis Overview, and Results", textOutput("text1.1"),HTML("<br><br><br>"), textOutput("text1.2"),HTML("<br><br><br>"),textOutput("text1.3")),
    tabPanel("Linear Regression Algorithm Chosen and Explanation", textOutput("text2.1"),HTML("<br><br><br>"), textOutput("text2.2"),HTML("<br><br><br>"), textOutput("text2.3")),
    tabPanel("Methodology and Data Science Life Cycle", textOutput("text3.1"), HTML("<br><br><br>"), textOutput("text3.2"), HTML("<br><br><br>"), textOutput("text3.3"), HTML("<br><br><br>"), textOutput("text3.4"), HTML("<br><br><br>"), textOutput("text3.5")),
    tabPanel("References", textOutput("text4.1"))
  ),
  HTML("<br><br><br>"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "y", 
                  label = "Please select the y-value or dependent variable:", 
                  choices = c("Age", "YearsExperience", "Salary"), 
                  selected = "Salary"),
      selectInput(inputId = "x", 
                  label = "Please select the x-value or independent variable:",
                  choices = c("Age", "YearsExperience", "Salary"), 
                  selected = "Age"),
      sliderInput("integer", "Training Set Ratio to Test Set (0.8 is 80% for Training and 20% for Testing):",
                  min = 0.3, max = 0.85,
                  value = 0.8),
    ),
    
    mainPanel(
      titlePanel("Explore the Changed Regression Values By Moving the Slider and Specifying X and Y Values (Independent and Dependent Variables):"),
      plotOutput(outputId = "scp"),
      tableOutput("values"),
      verbatimTextOutput("theNextSetOfValues"),
      plotOutput(outputId ="learningModelLinearPlot"),
      plotOutput("predictionPlot"),
      tableOutput("AfterRegressionMetrics"),
      plotOutput("cIPlot"),
    )
  )
)

server <- function(input, output) {
  output$text1.1 <- renderText({
    "Topic and Analysis: As someone who is interviewing for job opportunities across all disciplines, I am asked to either provide my years of experience or undergraduate graduation date so employers can calculate my age. I am not asked both age and years of experience. With such large importance given to years of work experience or age in deciding the salary range for a role, I wanted to explore the relationship between age and salary as well as the relationship between years of experience and salary. I want to know what salary I will be offered in the industry so I am not undervalued and can ask for a salary that is appropriate. Thus through linear regression, I was able to create a model to help predict Salary when given Age or YearsExperience. My analysis used tools and methods such as the  lm to save my linear model, R^2 to estimate how well the model fit the data, and RMSEP as the average difference between the observed value and predicted value for y.
"  })
  output$text1.2 <- renderText({
    "Data and Github: The dataset I found is: https://www.kaggle.com/datasets/codebreaker619/salary-data-with-age-and-experience?select=Salary_Data.csv that covers salary, age, and years of experience. The Github Link to my code is here: https://github.com/theboldone/FinalProject6
"
  })
  output$text1.3 <- renderText({
    "Insights and Results: When we train the model with 80% data in the training set and 20% in the test set, we are able to calculate 2 linear regression models for Age and Salary and also YearsExperience and Salary. Looking at Figure 3 where we have applied the model found by training, we can understand the trend between Age or YearsExperience with Salary. We were able to use the model to predict the y-values for Salary(when y-value is Salary that has been selected) with the test set. The R-squared values for Age predicting Salary and for YearsExperience predicting Salary are very high at (0.940) and (0.949) respectively, close to 1. This means that the model is very accurate and can help us more accurately determine the variance of y and its variability.  My hypothesis is correct that there is a strong relationship between Age predicting Salary and for YearsExperience predicting Salary and Age or YearsExperience individually are strong factors to predict Salary. The Root Mean Squared Error is around $6100 which is great for six figure salary numbers. That means the average distance between the predicted salary value and the actual value is $6000, based on the experiment I recently ran. Given this information, I can generally provide my Age or YearsExperience to estimate what I should expect to be paid and see the trend if that overtime. This insight lets me more accurately estimate my salary, advocate for a fair number for myself and other jobseekers, and educates employers and recruiters to pay underpaid workers a fair price. 
"
  })
  output$text2.1 <- renderText({
    "Model Chosen: The model I chose was linear regression and I wanted to compare the input value of Age to see how it affects the output value Salary. I also wanted to see how the input value of YearsExperience (Years of Experience) affects the output value Salary. With the way I have designed the application, you can explore these selected x,y or input, output values and check the relationship of any 2 combinations of these 3 variables. Regression also shows the trend of these 2 combinations of input and output values, such as if they move in opposite directions where the value of one variable increases and the other decreases. For my purposes, I want to predict the y value I am interested in (Salary) based on either Age or YearsExperience and understand the general trend between either of Age or YearsExperience x values and y values such as Salary which I am most interested in. By originally plotting the scatterplot, I saw that the relation between Age and Salary was linear and the relation between YearsExperience and Salary was linear. The correlation value was also close to 1 so I knew this linear regression model has a strong positive relationship. 
"
  })
  output$text2.2 <- renderText({
    "The Algorithm Explained: Through linear regression with one input variable for x and one output variable for y, we want to see the trend between these two variables (for example: both increase positively or in opposite directions when one value for a variable grows larger). We also want to create a model that can help us predict the relationship between x and y and understand the strength at which x can be used to predict y when using the model and the general variance or deviation of values from predicted vs what they are listed and observed as in reality. By collecting x and y pairs of data showing what x value is there for the y value and generating a robust dataset, I strongly support plotting the scatterplot of values to see if there is a linear relationship (where a line can pass through most of the points or a similar number of points are on or above and below the line). I also look at the correlation value and see if it is close to 1 to see an early indicator of strong relationship between the x and y values. In order to train our model for prediction, we need to divide it into training dataset and testing dataset. With 80% of our dataset for training, we can look to establish a model through y=ax+b+e where y’s the dependent variable, a’s the slope, b’s the intercept, x’s the independent variable, and e is the error or residual (which is the absolute value of the difference of the predicted vs listed/observed value in reality). 
"
  })
  output$text2.3 <- renderText({
    "The lm() function in r helps estimate a and b and stores the model in that variable. That helps to get the predicted y value and thus use the least squares method. The least squares method version of linear regression minimizes the predicted residual sum of squares, called PRESS.  PRESS = sum((yValueObserved - yValuePredicted)^2). PRESS helps us calculate RMSEP which is the root mean squared error of prediction. RMSEP finds the average difference between the observed value and predicted value for y. We want to keep that value low for a smaller deviation. RMSEP = sqrt(PRESS/ numberOfEntriesInTestingData). The SST is the total sum of squares is the sum((yValuePredicted - mean(yValueObserved))^2), which is the difference between the data points from the mean value. It is used to calculate R^2 which is the coefficient of determination. R^2 = 1 - (PRESS/SST).  R^2 helps us determine the variance of y and its variability and with values close to 1, that means there is a greater fit of the model with the data. Essentially R^2 helps determine how well the model has fitted the data and you want to get that number close to 1 to show the strength of the relationship between x and y variable and how strongly the independent variable can predict the dependent variable. RMSEP tells us the average difference between the observed value and predicted value for y and we want to get that lower for great model accuracy. 
"
  })
  output$text3.1 <- renderText({
    "Discovery: I did research with employers and recruiters to find articles such as the one from Salary.com, US Bureau of Labor Statistics, and sites (mentioned in references) to see if Age or YearsExpereince impact salary. Within this business domain, recruiters and employers see both Age and YearsOfExperience as predictors to whether they can trust the employee to successfully and thoroughly complete the tasks assigned. Thus these are the right factors to focus on (Age and YearsOfExperience). Hypothesis: Age or YearsOfExperience working can accurately predict Salary. Pain Points: As an employee and job searcher, I want to know how to evaluate a salary against industry standard so I am not undervalued and paid less. Goal: Understand the trend and model between Age and Salary or YearsExperience and Salary to predict Salary based on Age or YearsExperience.Risks: The salary range may fluctuate based on location and industry so the predictions need to be scaled based on other factors like cost of living, value addition, and age of industry. Stakeholders: jobseekers to determine a fair salary and recruiter and employers to determine a fair salary to pay to underpaid workers and attract talent to join their company for fair pay and salary.  
"
  })
  output$text3.2 <- renderText({
    "Data Prep: The data scripting and transformation will happen in RStudio and the data is from kaggle as mentioned, The data is visualized through Shiny through help of libraries such as ggplot. I have enough data, although more datapoints to work with that have been verified would be great. "
  })
  output$text3.3 <- renderText({
    "Model Planning: Linear regression modeling with Age or YearsExperience as the independent variable and Salary as the dependent variable. These x and y values were confirmed to be the most important variables for the region in which salaries were collected across multiple disciplines. If you want to choose other x and y values for the sliders, explore and feel free to see the relationship between other combinations of variables. I choose 80% of data to train to get the model and see the trend , and 20% of the data to test for prediction and how close the model was to predicting the value and trend. 
"
  })
  output$text3.4 <- renderText({
    "Communicate Results:I have communicated findings, business value, and original hypothesis in the Insights and Results tab.
"
  })
  output$text3.5 <- renderText({
    "Operationalize: We can run a pilot to collect this data based on the same factors of Age, YearsExperience, and Salary and risk factors I mentioned such as location and job type as well as others which can be found through experts who have experience in this business domain like job analysts, recruiters, and talent acquisition. The benefit we have is that we can predict salary based on age or experience. This can help jobseekers and employees everywhere to be paid fairly and for companies to attract more talent by paying workers wages with industry recommendations. In order to refine the model, we can look at other methods such as using nfold for sampling or even regression with both Age and Years of Experience as input variables to get Salary which is the output variable and predicted. 
"
  })
  output$text4.1 <- renderText({
    "https://shiny.rstudio.com/, https://www.bls.gov/careeroutlook/2015/article/wage-differences.htm, https://work.chron.com/factors-affect-starting-salary-8712.html, https://www.salary.com/articles/eight-factors-that-can-affect-your-pay/, https://www.kaggle.com/datasets/codebreaker619/salary-data-with-age-and-experience?select=Salary_Data.csv, https://stackoverflow.com/questions/68241763/r-shiny-error-trying-to-output-reactive-model-summary, https://stackoverflow.com/questions/24701806/r-shiny-output-summary-statistics, https://stackoverflow.com/questions/33266157/how-to-add-more-whitespace-to-the-main-panel-in-shiny-dashboard, https://www.digitalocean.com/community/tutorials/r-read-csv-file-into-data-frame"
  })
  output$scp <- renderPlot({
    ggplot(data = original_df, aes_string(x = input$x, y = input$y,color = input$z)) +geom_point()+ggtitle("Figure 1: Graph for All Data in Dataset Given Selected Dependent and Independent Variable (Pre-Data Split)")
  })
  output$learningModelLinearPlot <- renderPlot({
    trainingDataDF <- read.csv("Training_Data.csv")
    testingDataDF <- read.csv("Testing_Data.csv")
    
    ggplot(data = trainingDataDF, aes_string(x = input$x, y = input$y, color = input$z)) +ggtitle("Figure 2: Training Data") + geom_point()+geom_smooth(method = "lm",formula = y ~ x) 
  }) 
  output$cIPlot <- renderPlot({
    confidenceIntervalD=read.csv("ConfidenceIntervalSection.csv")
    colnames(confidenceIntervalD)=c("Predicted", "Observed", "LowerBound", "UpperBound")
     
    ggplot(data=confidenceIntervalD, aes(x=Predicted, y=Observed))+geom_point(color="blue")+ggtitle("Figure 4: Confidence Interval for Selected Y Value [2 LowerBound, 3 UpperBound]")+geom_errorbar(aes(ymin=LowerBound, ymax=UpperBound)) 
  })
  output$predictionPlot <- renderPlot({
    trainingDataDF <- read.csv("Training_Data.csv")
    testingDataDF <- read.csv("Testing_Data.csv")
    
    if ((input$x == "Age") && (input$y == "Age")){
      learningModelLinear<<- lm(Age ~ Age, data=trainingDataDF)
    }
    if ((input$x == "Age") && (input$y == "Salary")){
      learningModelLinear<<- lm(Salary ~ Age, data=trainingDataDF)
    }
    if ((input$x == "Age") && (input$y == "YearsExperience")){
      learningModelLinear<<- lm(YearsExperience ~ Age, data=trainingDataDF)
    }
    if ((input$x == "YearsExperience") && (input$y == "Age")){
      learningModelLinear<<- lm(Age ~ YearsExperience, data=trainingDataDF)
    }
    if ((input$x == "YearsExperience") && (input$y == "Salary")){
      learningModelLinear<<- lm(Salary ~ YearsExperience, data=trainingDataDF)
    }
    if ((input$x == "YearsExperience") && (input$y == "YearsExperience")){
      learningModelLinear<<- lm(YearsExperience ~ YearsExperience, data=trainingDataDF)
    }
    if ((input$x == "Salary") && (input$y == "YearsExperience")){
      learningModelLinear<<- lm(YearsExperience ~ Salary, data=trainingDataDF)
    }
    if ((input$x == "Salary") && (input$y == "Age")){
      learningModelLinear<<- lm(Age ~ Salary, data=trainingDataDF)
    }
    if ((input$x == "Salary") && (input$y == "Salary")){
      learningModelLinear<<- lm(Salary ~ Salary, data=trainingDataDF)
    }

    predictionDF = data.frame(predict(learningModelLinear, newdata=testingDataDF))
    names(predictionDF)[1] = 'Predicted'
    write.csv(predictionDF, "TheXValues.csv", row.names=FALSE)
    if(input$y=='Age' ){
      observedValues = testingDataDF[,c('Age')]
      names(observedValues)[2] = 'Value_Observed'
      write.csv(observedValues, "TheYValues.csv", row.names=FALSE)
    }
    if(input$y=='YearsExperience' ){
      observedValues = testingDataDF[,c('YearsExperience')]
      names(observedValues)[2] = 'Value_Observed'
      write.csv(observedValues, "TheYValues.csv", row.names=FALSE)
    }
    if(input$y=='Salary' ){
      observedValues = testingDataDF[,c('Salary')]
      names(observedValues)[2] = 'Value_Observed'
      write.csv(observedValues, "TheYValues.csv", row.names=FALSE)
    }
    
    xval <- read.csv("TheXValues.csv")
    yval <-read.csv(("TheYValues.csv"))
    predictionDfPlotted=data.frame(xval, yval)
    write.csv(predictionDfPlotted, "predictionDfPlotted.csv", row.names=FALSE)
    PRESS = sum((predictionDfPlotted$Predicted - predictionDfPlotted$x)^2)
    write.csv(PRESS, "PRESS.csv", row.names=FALSE)
    RMSEP = sqrt(PRESS/ nrow(predictionDfPlotted))
    write.csv(RMSEP, "RMSEP.csv", row.names=FALSE)
    SST = sum((predictionDfPlotted$x - mean(predictionDfPlotted$x))^2)
    write.csv(SST, "SST.csv", row.names=FALSE)
    R2 = 1 - (PRESS/SST)
    write.csv(R2, "R2.csv", row.names=FALSE)
    
    predictionDfPlotted$lower = predict(learningModelLinear, newdata=testingDataDF, interval = "confidence")[,2]
    predictionDfPlotted$upper = predict(learningModelLinear, newdata=testingDataDF, interval = "confidence")[,3]
    write.csv(predictionDfPlotted, "ConfidenceIntervalSection.csv", row.names=FALSE)
    ggplot(data = predictionDfPlotted, aes_string(predictionDfPlotted$Predicted, y = predictionDfPlotted$x,color = input$z)) +ggtitle("Figure 3: Prediction of Selected Y Value With Test Set") +geom_point()+geom_smooth(method = "lm",formula = y ~ x) + xlab("Predicted Value") + ylab("Listed/Observed Value") 
  })
  
  sliderValues <- reactive({
    if ((input$x == "Age") && (input$y == "Age")){
      corr= (cor(original_df$Age, original_df$Age))
    }
    if ((input$x == "Age") && (input$y == "Salary")){
      corr= (cor(original_df$Age, original_df$Salary))
    }
    if ((input$x == "Age") && (input$y == "YearsExperience")){
      corr= (cor(original_df$Age, original_df$YearsExperience))
    }
    if ((input$x == "YearsExperience") && (input$y == "Age")){
      corr= (cor(original_df$Age, original_df$YearsExperience))
    }
    if ((input$x == "YearsExperience") && (input$y == "Salary")){
      corr= (cor(original_df$Salary, original_df$YearsExperience))
    }
    if ((input$x == "YearsExperience") && (input$y == "YearsExperience")){
      corr= (cor(original_df$YearsExperience, original_df$YearsExperience))
    }
    if ((input$x == "Salary") && (input$y == "YearsExperience")){
      corr= (cor(original_df$Salary, original_df$YearsExperience))
    }
    if ((input$x == "Salary") && (input$y == "Age")){
      corr= (cor(original_df$Salary, original_df$Age))
    }
    if ((input$x == "Salary") && (input$y == "Salary")){
      corr= (cor(original_df$Salary, original_df$Salary))
    }
    else
      print(cor(original_df$Age, original_df$Salary))
    
    data.frame(
      Data_Represented = c("Value Selected for Training Set Ratio",
                           "Correlation Coefficient Given All Dataset Data"),
      Value = as.character(c(input$integer,
                             round(corr, digits = 3)))
    )
  })
  
  regressionValuesContinued  <- reactive({
    data.frame(
      Data_Represented = c("Predicted residual sum of squares (PRESS) ",
                           "Root mean sqaured error of prediction (RMSEP)", 
                           "Total sum of squares (SST)", "Calculating R^2 (Coefficient of Determination)"),
      Value = as.character(c(read.csv("PRESS.csv"), read.csv("RMSEP.csv"), read.csv("SST.csv"),read.csv("R2.csv")
      ))
    )
  })
  theSecondPartWhereThereIsText <- reactive({
    if(TRUE){
      set.seed(2020)
      splitData = caret::createDataPartition(original_df[,1], p = input$integer, list=F, times=1)
      trainSalData <<- original_df[splitData,]
      testSalData = original_df[!row.names(original_df) %in% row.names(trainSalData),]
      testSalData<<-original_df[-splitData,]
      if ((input$x == "Age") && (input$y == "Age")){
        learningModelLinear<<- lm(Age ~ Age, data=trainSalData)
      }
      if ((input$x == "Age") && (input$y == "Salary")){
        learningModelLinear<<- lm(Salary ~ Age, data=trainSalData)
      }
      if ((input$x == "Age") && (input$y == "YearsExperience")){
        learningModelLinear<<- lm(YearsExperience ~ Age, data=trainSalData)
      }
      if ((input$x == "YearsExperience") && (input$y == "Age")){
        learningModelLinear<<- lm(Age ~ YearsExperience, data=trainSalData)
      }
      if ((input$x == "YearsExperience") && (input$y == "Salary")){
        learningModelLinear<<- lm(Salary ~ YearsExperience, data=trainSalData)
      }
      if ((input$x == "YearsExperience") && (input$y == "YearsExperience")){
        learningModelLinear<<- lm(YearsExperience ~ YearsExperience, data=trainSalData)
      }
      if ((input$x == "Salary") && (input$y == "YearsExperience")){
        learningModelLinear<<- lm(YearsExperience ~ Salary, data=trainSalData)
      }
      if ((input$x == "Salary") && (input$y == "Age")){
        learningModelLinear<<- lm(Age ~ Salary, data=trainSalData)
      }
      if ((input$x == "Salary") && (input$y == "Salary")){
        learningModelLinear<<- lm(Salary ~ Salary, data=trainSalData)
      }
      write.csv(trainSalData, "Training_Data.csv", row.names=FALSE)
      write.csv(testSalData, "Testing_Data.csv", row.names=FALSE)
      summary(learningModelLinear)
    }
  })
  
  output$values <- renderTable({
    sliderValues()
  })
  output$theNextSetOfValues <- renderPrint({
    theSecondPartWhereThereIsText()
  })
  output$AfterRegressionMetrics <- renderTable({
    regressionValuesContinued()
  })
}

shinyApp(ui = ui, server = server)







# install.packages("shinydashboard")
# install.packages("rsconnect")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("caTools")
# install.packages("randomForest")
#install.packages("rlang")
# Install readr version 2.0.1
# install.packages("readr", version = "2.0.1")


library(caTools)
library(shinydashboard)
library(rsconnect)
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(readxl)
library(caret)
library(dplyr)
library(randomForest)

# Importing required packages and dataset 

d = read.table("processed_cleveland.csv",sep=",",header=FALSE,
               col.names = c('age','sex','cp','trestbps','chol','fbs','restecg',
                             'thalach','exang','oldpeak','slope','ca','thal','target'))
d$ca <- as.integer(d$ca)
d$thal <- as.integer(d$thal)
head(d,n=3)

# Data Pre-processing
d[d == '?'] <- NA
d <- transform(
  d,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  chol=as.integer(chol),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thal=as.factor(thal),
  target=as.factor(target)
)


#Dropping rows with nulls
d <- d[!(d$ca %in% c(NA)),]
d <- d[!(d$thalach %in% c(NA)),]
head(d)

table(d['target'])
d[,'target'] <- ifelse(d[,'target']==0,0,1)
d[,'target'] = as.factor(d[,'target'])
levels(d$target) <- make.names(levels(factor(d$target)))
levels(d$target)
d$target <- relevel(d$target,"X1")
head(d)
str(d)

# Renaming target variable
names(d)[names(d) == "target"] <- "y"

#Removing null values
d <- na.omit(d)


#Prediction model
sample = sample.split(d$y, SplitRatio = .75)
train = subset(d, sample == TRUE)
test  = subset(d, sample == FALSE)
set.seed(123)
myModel1 <- glm(
  y ~ ., family = 'binomial',
  data=train
)

# Building dataset for EDA
df = read.csv(file = "processed_cleveland.csv")
df <- na.omit(df)
head(df)

names(df) <- c('age','sex','cp','trestbps','chol','fbs','restecg',
               'thalach','exang','oldpeak','slope','ca','thal','target')
head(df)

for (row in 1:nrow(df)) {
  if (df[row, "target"]>0){
    df[row, "target"] <- 1
  }
  
}

head(df)

# mutate() function from the dplyr package is used to manipulate dataframe.

# The target column is modified using the ifelse() function to create a new column 
# named target that contains either a 1 or the original value of target, 
# depending on whether the original value is greater than 0.

df <- df %>% mutate(target = ifelse(target > 0, 1, target))


# A new column heart_disease is created and the values are set to "Yes" 
# if the target column is greater than 0, and "No" otherwise.

df <- df %>% mutate(heart_disease = ifelse(target > 0, "Yes", "No"))


# A new column fbs_range is created and if "fbs" is greater than 120, the value in "fbs_range" 
# is set to "> 120". Otherwise, the value is set to "< 120".

df <- df %>% mutate(fbs_range = ifelse(fbs > 120, "> 120", "< 120"))


# A new column Gender is created and the values are set to "Male" 
# if the sex column is equal to 1, and "Female" otherwise.

df <- df %>% mutate(Gender = ifelse(sex == 1, "Male", "Female"))


# A new column exercise_angina is created and the values are set the value to 
# "Yes" if the exang column is greater than 0, and "No" otherwise.

df <- df %>% mutate(exercise_angina = ifelse(exang > 0, "Yes", "No"))


# A new column slope_segment is created and the value is set to "Upslope" 
# if the slope column is equal to 1, "Flat" if the slope column is equal to 2, 
#"Downslope" if the slope column is equal to 3, and NA otherwise.

df <- df %>% mutate(slope_segment = case_when(slope == 1 ~ "Upslope",
                                              slope == 2 ~ "Flat",
                                              slope == 3 ~ "Downslope",
                                              TRUE ~ NA_character_ ))

# A new column restecg_report is  created and the value is set to "Normal" if the restecg
# column is equal to 0, "ST-T wave abnormality" if the restecg column is equal to 1, 
# "Probable or definite left ventricular hypertrophy" if the restecg column is equal to 2, 
# and NA otherwise.

df <- df %>%mutate(restecg_report = case_when(restecg == 0 ~ "Normal",
                                              restecg == 1 ~ "ST-T wave abnormality",
                                              restecg == 2 ~ "Probable or definite left ventricular hypertrophy",
                                              TRUE ~ NA_character_ ))
# if none of the above conditions are met then True ~ NA_character_ 
# sets the default value to NA which basically represents missing or undefined value


# A new column Chest_Pain_type is created and the value is set to "typical angina" 
# if the cp column is equal to 1, "atypical angina" if the cp column is equal to 2,
# "non-anginal pain" if the cp column is equal to 3, "asymptomatic" otherwise.

df <- df %>% mutate(Chest_Pain_type = case_when(cp == 1 ~ "typical angina",
                                                cp == 2 ~ "atypical angina",
                                                cp == 3 ~ "non-anginal pain",
                                                TRUE ~ "asymptomatic"))


# A new column thal_report is created and the value is set to "Normal" if thal column is equal 
# to "3.0", "Fixed Defect"  if the thal column is equal to "6.0", "Reversible Defect" otherwise.

df <- df %>% mutate(thal_report = case_when(thal == "3.0" ~ "Normal",
                                            thal == "6.0" ~ "Fixed Defect",
                                            TRUE ~ "Reversible Defect" ))

# A new column ca_number is created the value is set to "0" if ca column is equal 
# to "0", "1"  if the ca column is equal to "1", "2" if the ca column is equal to "2"
# and "3" otherwise. 

df <- df %>% mutate(ca_number = case_when(
  ca == "0.0" ~ 0,
  ca == "1.0" ~ 1,
  ca == "2.0" ~ 2,
  TRUE ~ 3
))


# fluidRow() is a layout function in shiny, that is used to create a horizontal row, 
# here it creates a row with two boxes
Row1 <- fluidRow(
  box(
    
    plotOutput("Age", height = "300px")
    ,numericInput("Age", "Enter the age:",  50, min=20, max=80)
  ),
  box(
    plotOutput("Gender", height = "300px")
    ,checkboxGroupInput("Gender", "Gender",
                        c("Male" = "Male",
                          "Female" = "Female")
                        
    )
  ))

Row2 <- fluidRow(
  box(
    
    plotOutput("trestbps", height = "300px")
    ,sliderInput("trestbps", "Select range for Trestbps(resting bood pressure):", min(df['trestbps']),max(df['trestbps']), 130)
  ),
  box(
    plotOutput("ChestPaintype", height = "300px")
    ,checkboxGroupInput("ChestPaintype", "Type of Chest pain",
                        c("typical angina" = "typical angina",
                          "atypical angina" = "atypical angina",
                          "non-anginal pain" = "non-anginal pain",
                          "asymptomatic" = "asymptomatic")
                        
    )
  ))

Row3 <- fluidRow(
  box(
    
    plotOutput("Chol", height = "300px")
    ,sliderInput("Chol", "Select range for Cholesterol:", min(df['chol']),max(df['chol']), 210)
  ),
  box(
    
    plotOutput("Thalach", height = "300px")
    ,numericInput("Thalach", "Enter value for thalach(Maximum heart rate achieved):",  120)
  ))

Row4 <- fluidRow(
  box(
        plotOutput("fbs_range", height = "300px")
    ,checkboxGroupInput("fbs_range", "Fasting blood sugar",
                        c("< 120" = "< 120",
                          "> 120" = "> 120"))
  ),
  box(
    plotOutput("restecg_report", height = "300px")
    ,checkboxGroupInput("restecg_report", "Resting electrocardiographic results",
                        c("Probable or definite left ventricular hypertrophy" = "Probable or definite left ventricular hypertrophy",
                          "Normal" = "Normal",
                          "ST-T wave abnormality"="ST-T wave abnormality"))
  ))

Row5 <- fluidRow(
  box(
    plotOutput("exercise_angina", height = "300px")
    ,checkboxGroupInput("exercise_angina", "Excercise induced angina",
                        c("Yes" = "Yes",
                          "No" = "No"))
  ),
  box(
    plotOutput("slope_segment", height = "300px")
    ,checkboxGroupInput("slope_segment", "slope of the ST segment",
                        c("Downslope" = "Downslope",
                          "Upslope" = "Upslope",
                          "Flat"="Flat"))
  ))

Row6 <- fluidRow(
  box(
    
    plotOutput("oldpeak", height = "300px")
    ,sliderInput("oldpeak", "Select range for oldpeak( difference between the ST segment of the ECG at rest and after exercise):",
                 min(df['oldpeak']),max(df['oldpeak']), 2.5)
  )
  )


Row7 <- fluidRow(
  numericInput(inputId = "age", 
               label = "Enter your age",
               value = 55, min=20, max=80
  ),
  radioButtons(inputId = "sex", 
               label = "Select your gender",choices = list("Male" = 1, "Female" = 0
               ),selected = 1),
  radioButtons(inputId = "cp", 
               label = "Select the type of your chest pain",choices = list("Typical angina" = 1,"Atypical angina" = 2, 
                                                                           "Non-anginal pain" = 3, "Asymptomatic" = 4),
               selected = 1
  ),
  numericInput(inputId = "trestbps", 
               label = "Enter resting blood pressure (in mm Hg on admission to the hospital)",
               value = 130, min=80, max=200
  ),
  numericInput(inputId = "chol", 
               label = "Enter serum cholestoral in mg/dl",
               value = 250, min=125, max=565
  ),
  radioButtons(inputId = "fbs", 
               label = "Select if fasting blood sugar > 120mg/dL",
               choices = list("No" = 0,"Yes" = 1),
               selected = 1
  ),
  radioButtons(inputId = "restecg", 
               label = "Enter your resting electrocardiographic results",
               choices = list("Normal" = 0,"Having ST-T wave abnormality" = 1, 
                              "Showing probable or definite left ventricular hypertrophy" = 2),
               selected = 1
  ),
  numericInput(inputId = "thalach", 
               label = "What was your maximum heart rate achieved during excercise(in beats per minute)?",
               value = 150, min =71 ,max= 202
  ),
  radioButtons(inputId = "exang", 
               label = "Do you have exercise induced angina? ",
               choices = list("No" = 0,"Yes" = 1),
               selected = 1
  ),
  numericInput(inputId = "oldpeak", 
               label = "Oldpeak value(difference between the ST segment of the ECG at rest and after exercise)",
               value=1, min=0, max=6.2
  ),
  radioButtons(inputId = "slope", 
               label = "What is the slope of your peak exercise ST segment?",
               choices = list("Upsloping" = 1,"Flat" = 2, 
                              "Downsloping" = 3),
               selected = 1
  ),  
  radioButtons(inputId = "ca", 
               label = "What is the number of major vessels (0-3) colored by flouroscopy?",
               choices = list("0" = 0,"1" = 1, "2" = 2, "3" = 3),
               selected = 1
  ),
  radioButtons(inputId = "thal", 
               label = "What is your thalassemic status(Nuclear stress test results)?",
               choices = list("Normal" = 3,"Fixed defect" = 6,
                              "Reversable defect" = 7),
               selected = 3
  ),
  
  actionButton(inputId = "go", 
               label = "Get Prediction"),
  htmlOutput ("pred"),
  htmlOutput ("feedback"))



# HTML tags are used to define the layout and structure of the dashboard page, and
# CSS code is used to customize the appearance and style of the dashboard page.
# fluidPage creates a web page with a responsive, fluid layout that automatically
# adjusts to the size of the user's screen.
# The dashboardPage() function creates the overall structure of the dashboard page, which consists 
# of a header, a sidebar, and a body and the skin argument specifies the color theme of the dashboard.



header <- dashboardHeader(title = "Predictive Analytics for Heart Failure", titleWidth = "100%")  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("EDA-1", tabName = "EDA-1", icon = icon("chart-bar")),
    menuItem("EDA-2", tabName = "EDA-2", icon = icon("chart-line")),
    menuItem("EDA-3", tabName = "EDA-3", icon = icon("columns")),
    menuItem("Prediction", tabName = "Prediction", icon = icon("bullseye"))
  )
)


body <- dashboardBody(tabItems(tabItem(tabName = "EDA-1",Row1,Row2),
                               tabItem(tabName = "EDA-2",Row3,Row4),
                               tabItem(tabName = "EDA-3",Row5,Row6),
                               tabItem(tabName = "Prediction",Row7)))


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        .sidebar-toggle{
        display: none;
        }
        .skin-blue .main-sidebar{
        background-color: #367fa9;
        }
        .skin-blue .sidebar a {
        color: black;
        font-weight: 700; 
        }
        .skin-blue .sidebar-menu>li.active>a{
        border-left-color: black; 
        color: #fff; // text color of the side bar when it is in active state
        }
        .skin-blue .sidebar-menu>li>a:hover{
         border-left-color: black; // slight border in the left when its in hover state
        }
        #shiny-tab-Prediction{
         margin:0 20px;
        }
        .skin-blue .main-header .logo{
        font-weight:bold;
        color: black;
        }
        .container-fluid{
        padding:0;
        }
        "
      )
    )
  ),
  dashboardPage(header, sidebar, body, skin='blue')
)


server = function(input, output) {
  data1 <- reactive({
    subset(df, df$age < input$Age)})
  output$Age <- renderPlot({
    ggplot(as.data.frame(data1()), aes(x = age, fill = heart_disease)) +
      geom_density() +
      labs(x = "Age", y= "Density of Patients")
    
  })
  
  data2 <- reactive({
    subset(df, df$Gender %in% input$Gender)})
  output$Gender <- renderPlot({
    ggplot(as.data.frame(data2()), aes(x = Gender, fill = heart_disease)) +
      geom_bar() +
      labs(x = "Gender", y= "Number of Patients")
  })
  data3 <- reactive({
    subset(df, df$trestbps < input$trestbps)})
  output$trestbps <- renderPlot({
    ggplot(as.data.frame(data3()), aes(x = trestbps, fill = heart_disease)) +
      geom_histogram() +
      labs(x = "Resting blood pressure (in mm Hg)", y= "Number of Patients")
  })
  data4 <- reactive({
    subset(df, df$Chest_Pain_type %in% input$ChestPaintype)})
  output$ChestPaintype <- renderPlot({
    ggplot(as.data.frame(data4()), aes(x = Chest_Pain_type, fill = heart_disease)) +
      geom_bar() +
      labs(x = "Chest pain type", y= "Number of Patients")
  })
  data5 <- reactive({
    subset(df, df$chol < input$Chol)})
  output$Chol <- renderPlot({
    ggplot(as.data.frame(data5()), aes(x = chol, fill = heart_disease)) +
      geom_density() +
      labs(x = "Cholestarol(in mg/dl)", y= "Density of Patients")
  })
  data6 <- reactive({
    subset(df, df$thalach < input$Thalach)})
  output$Thalach <- renderPlot({
    ggplot(as.data.frame(data6()), aes(x = thalach, fill = heart_disease)) +
      geom_histogram() +
      labs(x = "Maximum heart rate achieved during exercise", y= "Density of Patients")
  })
  
  data7 <- reactive({
    subset(df, df$fbs_range %in% input$fbs_range)})
  output$fbs_range <- renderPlot({
    ggplot(as.data.frame(data7()), aes(x = fbs_range, fill = heart_disease)) +
      geom_bar() +
      labs(x = "Fasting blood sugar(in mg/dl)", y= "Number of Patients")
  })
  
  data8 <- reactive({
    subset(df, df$restecg_report %in% input$restecg_report)})
  output$restecg_report <- renderPlot({
    ggplot(as.data.frame(data8()), aes(x = heart_disease, fill = restecg_report)) +
      geom_bar() +
      labs(x = "Heart Disease", y= "Number of Patients")
  })
  
  data9 <- reactive({
    subset(df, df$exercise_angina %in% input$exercise_angina)})
  output$exercise_angina <- renderPlot({
    ggplot(as.data.frame(data9()), aes(x = exercise_angina, fill = heart_disease)) +
      geom_bar() +
      labs(x = "Exercise induced angina", y= "Number of Patients")
  })
  
  data10 <- reactive({
    subset(df, df$slope_segment %in% input$slope_segment)})
  output$slope_segment <- renderPlot({
    ggplot(as.data.frame(data10()), aes(x = slope_segment, fill = heart_disease)) +
      geom_bar() +
      labs(x = "slope of the ST segment", y= "Number of Patients")
  })
  
  data11 <- reactive({
    subset(df, df$oldpeak < input$oldpeak)})
  output$oldpeak <- renderPlot({
    ggplot(as.data.frame(data11()), aes(x = oldpeak, fill = heart_disease)) +
      geom_density() +
      labs(x = "oldpeak", y= "Density of Patients")
  })
  
 
# eventReactive() function "value", reacts to an event 'go' and 
# returns a predicted value based on the input variables.
  value <- eventReactive(input$go, {
    sample.obs <- cbind(input$age,input$sex,input$cp,input$trestbps,input$chol,
                        input$fbs,input$restecg,input$thalach,input$exang,
                        input$oldpeak,input$slope,input$ca,input$thal)
    colnames(sample.obs) <- c('age','sex','cp','trestbps','chol','fbs','restecg',
                              'thalach','exang','oldpeak','slope','ca','thal')
    cat.cols = c('sex','cp','fbs','restecg','exang','slope','ca','thal')
    sample.obs <- data.frame(sample.obs)
    sample.obs <- transform(
      sample.obs,
      age=as.integer(age),
      sex=factor(sex, levels = levels(d$sex)),
      cp=factor(cp, levels = levels(d$cp)),
      trestbps=as.integer(trestbps),
      chol=as.integer(chol),
      fbs=factor(fbs, levels = levels(d$fbs)),
      restecg=factor(restecg,levels = levels(d$restecg)),
      thalach=as.integer(thalach),
      exang=factor(exang, levels = levels(d$exang)),
      oldpeak=as.numeric(oldpeak),
      slope=factor(slope, levels = levels(d$slope)),
      ca=factor(ca, levels = levels(d$ca)),
      thal=factor(thal, levels = levels(d$thal))
    )
    round(1-predict(myModel1, newdata=sample.obs,type='response'),digits = 2)
  })
  
# observeEvent() executes the code in the curly braces when the button with id "go" is clicked  
  
  observeEvent(input$go,output$pred <- renderUI({
        str1 <- paste("<br>","<b>","Based on your reports, you have a ", value()*100,"% probability of having a heart disease.
                      (However, note that the prediction provided by the model may always not be correct or accurate.)",sep="","<b>")
    HTML(paste(str1))
  }))
  
  output$feedback <- renderUI({
    if (value()>0.5){
      "We recommend you to consult doctor immediately, and moreover make necessary lifestyle changes such as avoiding fatty and junk foods and exercising everyday to manage the condition" } else{
      "Yay!! Seems like you are fit and healthy. We recommend you continue to follow a healthy diet and engage in some form of physical activity on a regular basis." }
  })
  
}

shinyApp(ui, server)

library(shiny)
library(tidyverse)
library(readr)
library("party")

b64 <- base64enc::dataURI(file="../titanic_ad.jpg", mime="image/jpg")
data <- read.csv("../Data/interim/titanic.csv",colClasses = c('integer','factor','factor','character',
                                                              'factor','integer','integer','integer',
                                                              'character','double','character','factor'))
#test_data <- read.csv("../Data/interim/titanic_test.csv",colClasses = c('character','integer','factor','factor','factor',
 #                                                             'double','integer','integer'))
# model to generate tree plot:
tree_image <- base64enc::dataURI(file="../Data/mdl_plot.png")
log_Reg_image <- base64enc::dataURI(file="../Data/mdl_logRegplot.png")
age_pl <- base64enc::dataURI(file="../Data/Age_plot.png")
class_pl <- base64enc::dataURI(file="../Data/Class_plot.png")
legend <- base64enc::dataURI(file="../Data/Legend.png")
gender <- base64enc::dataURI(file="../Data/gender_plot.png")
harbour <- base64enc::dataURI(file="../Data/harbour_plot.png")
CM_rpart <- base64enc::dataURI(file="../Data/ConM_rpart.png")
CM_logreg <- base64enc::dataURI(file="../Data/LogReg_Matrix.png")
CM_all <- base64enc::dataURI(file="../Data/CM_all.png")

ui <- navbarPage("Titanic",
  tabPanel("Who survived?",
  fluidRow(
    column(3,
           img(src = b64,height="100%", width="100%", align="left")),
    column(9,
           h1("TITANIC: Would you have survived?"),
           br(),
           actionButton("do", "Click Me"),
           br(),
           verbatimTextOutput("result")
    )
  ),
    fluidRow(
          column(6,
              wellPanel(
                  textInput("name", label = h6("Please enter your name"), 
                            value = "name"),
                  textInput("age", label = h6("Please enter your age"), 
                            value = "15"),
                  selectInput("gender", label = h6("Your gender"), 
                              choices = list("female" = "female", "male" = "male"), selected = "female"),
                  selectInput("port", label = h6("Select your Departure harbour"), 
                              choices = list("Southhampton, UK" = "S", "Cherbourg, France" = "C",
                                             "Cobh, Ireland" = "Q"), selected = "S"))),
          column(6,
                 wellPanel(        
                  selectInput("class", label = h6("Which class would you like to travel in:"), 
                              choices = list("1st" = 1, "2nd" = 2,
                                             "3rd" = 3), selected = 1),
                  sliderInput("fare", label = h6("How much would you be willing to spend on a ticket?"), min = 0, max = 130, 
                              value = 15),
                  textInput("sibs", label = h6("Any siblings/spouse travelling with you? How many:"), 
                            value = "0"),
                  textInput("parch", label = h6("Any children/parents travelling with you? How many:"), 
                            value = "0")
                         )
                  ))),
  tabPanel("Data Exploration",
           fluidRow(column(6,
                           h5("Does age have something to do with death or survival? And is there a difference between genders?"),
                            img(src = age_pl,height="100%", width="100%", align="left")),
                            #plotOutput("agePlot")),
                    column(6,
                           h5("Travelling first class, it would be easier to get to deck. 
                              Could this have had influenced your chances of survival?"),
                              img(src = class_pl,height="90%", width="95%", align="left")),
             column(width = 12, align = "center",
               img(src = legend,height="20%", width="20%", align="center"))),
                            #plotOutput("classPlot"))),
           fluidRow(column(6, 
                           h5("As mentioned above, do we see any survival differences between men and women?"),
                          img(src = gender,height="80%", width="80%", align="left")),
                            #plotOutput("sexPlot")),
                    column(6,
                           h5("And what about the harbour from which passengers embarked?"),
                            img(src = harbour,height="65%", width="90%", align="left"))
           )
                            #plotOutput("embPlot")))    
  ),
  tabPanel("Model Specification",
    fluidRow(column(6,
      h5("Decision tree estimated Model"),
      img(src = tree_image,height="80%", width="80%", align="left"),
      column(width = 12, 
        htmlOutput("decision_tree"))),
      column(width = 6,
        h5("Logistic Regression Model"),
        img(src = log_Reg_image,height="95%", width="95%", align="left"),
        column(width = 12,
        htmlOutput("Log_reg")))
    )
  ),
  tabPanel("Model Evaluation",
    fluidRow(column(2, h5("Type of model")),
      column(width = 4,
        h5("Decision Tree Model")),
      column(width = 4,
        h5("Logistic Regression Model"))),
    hr(),
    fluidRow(column(2, h5("Variables used")),
      column(width = 4,
        htmlOutput("dec_tree_variables")),
      column(width = 4,
        htmlOutput("Log_reg_variables"))),
    hr(),
    fluidRow(column(2, h5("Confusion Matrix")),
      column(width = 8,
        img(src = CM_all,height="95%", width="95%", align="left"))),
    hr(),
    fluidRow(column(12, h5("Sensitivity - Percentage of people who are correctly identified as having survived"))),
    fluidRow(column(4),
      column(width = 3, h6(0.92)),
      column(width = 3, h6(0.98))),
    hr(),
    fluidRow(column(12, h5("Specificity - Percentage of people who are correctly identified as not surviving"))),
    fluidRow(column(4),
      column(width = 3, h6(0.97)),
      column(width = 3, h6(0.84))),
    hr()
  )
)


server <- function(input, output) {
  
  runPrediction <- eventReactive(input$do, {
    df = data.frame("Name" = as.character(input$name),"Age" = as.integer(input$age), 
      "Sex" = factor(input$gender, levels = c("female", "male")), "Embarked" = factor(input$port, levels = c("C", "Q", "S")), 
      "Pclass" = factor(input$class, levels = c("1", "2", "3")), "Fare" = as.double(input$fare), 
      "SibSp" = as.integer(input$sibs), "Parch" = as.integer(input$parch))
    model<-ctree(Survived ~ Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=data)
    pred<-predict(model,df)
    pred
  })
  
  
  output$result <- renderPrint({
    if(runPrediction() == 1){
      cat(paste0("Congratulations ", input$name, "!\nYou would have survived the disaster."))
    }else{
      cat(paste0("Sorry ", input$name, "!\nYou would have not survived."))
    }
  })
  
  output$decision_tree = renderUI({
    HTML("Each node shows:</b>
      <br>
      <li>the predicted class (died or survived),
      <li>the predicted probability of survival,
      <li>the percentage of observations in the node.
      <br>")
  })
  
  output$Log_reg = renderUI({
    HTML("<br>
      <li>Women had higher probability of surviving than men,
      <li>1st and 2nd class women had high probability of surviving,
      <li>Men, even in 1st class, had less than 50% probability of surviving.
      <br>")
  })
  
  output$Log_reg_variables = renderUI({
    HTML("<br> Variables used for the Logistic Regression model are:
      <li>Gender
      <li>Fare
      <li>Class
      <br>")
  })
  
  output$dec_tree_variables = renderUI({
    HTML("<br> Variables used for the Decision Tree model are:
      <li>Gender
      <li>Age
      <li>Number of siblings/spouse travelling
      <li>Number of children
      <li>Class
      <br>")
  })
  
  ######### PLOTS #############
  
  #output$agePlot = renderPlot({
  #  ggplot(data)+
  #    geom_histogram(aes(Age,fill=Survived),color = "white",position="dodge", bins = 20)+
  #    facet_wrap(~Sex)
  #  })

  # output$classPlot = renderPlot({  
  #   ggplot(data)+
  #     geom_histogram(aes(Pclass,fill=Survived),color = "white",stat="count",position="dodge")+
  #     xlab("Class")
  # })
  # 
  # output$sexPlot = renderPlot({  
  #   ggplot(data)+
  #     geom_histogram(aes(Sex,fill=Survived),color = "white",stat="count",position="dodge")+
  #     xlab("Gender")
  # })
  # 
  # output$embPlot = renderPlot({ 
  #   ggplot(data)+
  #     geom_histogram(aes(Embarked,fill=Survived),color = "white",stat="count",position="dodge")+
  #     xlab("Harbour")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #     scale_x_discrete(breaks=c("Q","S","C"),
  #       labels=c("Queenstown", "Southhampton", "Cherbourg"))
  # })
  
}


shinyApp(ui = ui, server = server)
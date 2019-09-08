library(shiny)
library(tidyverse)
library(readr)
library("party")

b64 <- base64enc::dataURI(file="../titanic_ad.jpg", mime="image/jpg")
data <- read.csv("../Data/interim/titanic.csv",colClasses = c('integer','factor','factor','character',
                                                              'factor','integer','integer','integer',
                                                              'character','double','character','factor'))
test_data <- read.csv("../Data/interim/titanic_test.csv",colClasses = c('character','integer','factor','factor','factor',
                                                              'double','integer','integer'))

ui <- fluidPage(
  h1("TITANIC: Would you have survived?"),br(),
    fluidRow(
          column(4,
              wellPanel(
                  textInput("name", label = h4("Please enter your name"), 
                            value = "name"),
                  textInput("age", label = h4("Please enter your age"), 
                            value = "15"),
                  selectInput("gender", label = h4("Your gender"), 
                              choices = list("female" = "female", "male" = "male"), selected = "female"),
                  selectInput("port", label = h4("Select your Departure harbour"), 
                              choices = list("Southhampton, UK" = "S", "Cherbourg, France" = "C",
                                             "Cobh, Ireland" = "Q"), selected = "S"),
                  selectInput("class", label = h4("Which class would you like to travel in:"), 
                              choices = list("1st" = 1, "2nd" = 2,
                                             "3rd" = 3), selected = 1),
                  sliderInput("fare", label = h4("How much would you be willing to spend on a ticket?"), min = 0, max = 130, 
                              value = 15),
                  textInput("sibs", label = h4("Any siblings/spouse travelling with you? How many:"), 
                            value = "0"),
                  textInput("parch", label = h4("Any children/parents travelling with you? How many:"), 
                            value = "0")
                         )
                  ),
          column(6,
                 actionButton("do", "Click Me"),
                 br(),
                 img(src = b64,height="50%", width="50%", align="right"), 
                 br(),
                 verbatimTextOutput("result")
                         )
    )
)


server <- function(input, output) {
  test = reactive({
    df = data.frame("Name" = as.character(input$name),"Age" = as.integer(input$age), "Sex" = as.factor(input$gender), "Embarked" = as.factor(input$port), 
                    "Pclass" = as.factor(input$class),
                    "Fare" = as.double(input$fare), "SibSp" = as.integer(input$sibs), "Parch" = as.integer(input$parch))
    df = c(test_data,df)
  df
  })
  
  runPrediction <- eventReactive(input$do, {
    model<-ctree(Survived ~ Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=data)
    pred<-predict(model,test())
    pred
  })
  
  output$result <- renderPrint({
    if(as.numeric(tail(runPrediction(),1)) == 1){
      cat("Congratulations!\nYou would have survived the disaster.")
    }else{
      cat("Sorry!\nYou would have not survived.")
    }
  })
  
  
  
}


shinyApp(ui = ui, server = server)
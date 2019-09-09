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
           fluidRow(column(8,
                            plotOutput("agePlot")),
                    column(4,
                            plotOutput("classPlot"))),
           fluidRow(column(4,
                            plotOutput("sexPlot")),
                    column(4,
                            plotOutput("embPlot")))    
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
  
  ######### PLOTS #############
  
  output$agePlot = renderPlot({
    ggplot(titanic_train)+
      geom_histogram(aes(Age,fill=Survived),color = "white",position="dodge")+facet_wrap(~Sex)
    })

output$classPlot = renderPlot({  
  ggplot(titanic_train)+
    geom_histogram(aes(Pclass,fill=Survived),color = "white",stat="count",position="dodge")
})

output$sexPlot = renderPlot({  
  ggplot(titanic_train)+
    geom_histogram(aes(Sex,fill=Survived),color = "white",stat="count",position="dodge")
})

output$embPlot = renderPlot({ 
  ggplot(titanic_train)+
    geom_histogram(aes(Embarked,fill=Survived),color = "white",stat="count",position="dodge")
})
  
}


shinyApp(ui = ui, server = server)
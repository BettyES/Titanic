library(shiny)

b64 <- base64enc::dataURI(file="../titanic_ad.jpg", mime="image/jpg")

ui <- fluidPage(
  h1("TITANIC: Would you have survived?"),br(),
    fluidRow(
          column(4,
              wellPanel(
                  textInput("name", label = h4("Please enter your name"), 
                            value = ""),
                  textInput("age", label = h4("Please enter your age"), 
                            value = ""),
                  selectInput("gender", label = h4("Your gender"), 
                              choices = list("female" = 1, "male" = 2), selected = 1),
                  selectInput("port", label = h4("Select your Departure harbour"), 
                              choices = list("Southhampton, UK" = 1, "Cherbourg, France" = 2,
                                             "Cobh, Ireland" = 3), selected = 1),
                  selectInput("class", label = h4("Which class would you like to travel in:"), 
                              choices = list("1st" = 1, "2nd" = 2,
                                             "3rd" = 3), selected = 1),
                  sliderInput("fare", label = h4("How much would you be willing to spend on a ticket?"), min = 0, max = 130, 
                              value = 15),
                  textInput("sibs", label = h4("Any siblings/spouse travelling with you? How many:"), 
                            value = ""),
                  textInput("parch", label = h4("Any children/parents travelling with you? How many:"), 
                            value = "")
                         )
                  ),
          column(6,
                 img(src = b64,height="50%", width="50%", align="right")
                         )
    )
)


server <- function(input, output) {}


shinyApp(ui = ui, server = server)
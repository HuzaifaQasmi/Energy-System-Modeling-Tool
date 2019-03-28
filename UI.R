library(shiny)
library(plotly)

shinyUI(fluidPage(
  
  titlePanel(title=h2("Hourly Power Demand Data Analysis", align="center")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("demfile", "1. Upload the Demand CSV file"),
      helpText("Default maximum file size is 5MB."),
      
      selectInput("param", "2. Select the required Parameter",
                  choices = list("Demand", "Predicted Demand")),
      
      # textInput("year", "Enter the Year:",""),
      numericInput("predem","3. Enter the Predicted Demand:",""),
      
      selectInput("type", "4. Select the Data Type for Plot",
                  choices = list("MegaWatt","Normalized")),
      
      radioButtons("sep", "5. File Separator Options",
                   choices = c(Comma = ',',Semicolon = ';',Tab = '\t',Space = ''),
                   selected = ','
                   )
      
      
    ),
    
    mainPanel(
       uiOutput("out")

                  )
  
    )
  )
  
)
library(shiny)
library(plotly)


shinyServer(function(input,output){
  
#Extracting data from uploaded CSV file in dataframe.
  data <- reactive({
    req(input$demfile)
    read.table(file=input$demfile$datapath, sep = input$sep, header = TRUE)
  })
  
#Normalization Function for Demand parameter  
  norm_data <- reactive({
    req(data())
    # Demand_norm <- read.table(file=input$demfile$datapath, sep = input$sep, header = TRUE)
    data()$Demand/max(data()$Demand)
    #cbind(data(),Demand_norm)
  })

#multiplier function for future demand model on predicted peak demands
  pred_data <- reactive({
    req(norm_data())
    norm_data()*input$predem
  })  
     
#Combining Processed Parameters to original extracted CSV dataframe   
  dem_data <- reactive({
    req(data())
    Demand_norm <- norm_data()
    Predicted_Demand <- pred_data()
    cbind(data(),Demand_norm,Predicted_Demand)
  })
  

  
  
  
  
#this output gives the file information      
  output$filedf <- renderTable({
    req(input$demfile)
    input$demfile
  })
  
#this output display data of the ploted graph  
  output$table <- renderTable({
    req(dem_data())
    dem_data()
  })
  
#this output gives the plot of Load Demand Curve (LDC)   
  output$plot <- renderPlotly({
    req(dem_data())

    switch (input$param,
            "Demand" =  if(input$type=='MegaWatt'){
                  plot_ly(dem_data(), x = ~Hours, y = ~Demand, type = "scatter", mode = "lines")
                  }
                  else if(input$type=='Normalized'){
                  plot_ly(dem_data(), x = ~Hours, y = ~Demand_norm, type = "scatter", mode = "lines")
                  },
                  
            "Predicted Demand" = plot_ly(dem_data(), x = ~Hours, y = ~Predicted_Demand, type = "scatter", mode = "lines")
    ) 
    
  })
  

  
#this output dynamically generate the tabsets when  the file is loaded.
  output$out <- renderUI({
    tabsetPanel(
      tabPanel("Plot",plotlyOutput("plot")
               ),
      tabPanel("Data",tableOutput("table")),
      tabPanel("About File", tableOutput("filedf"))
    )
  })
  
  
})
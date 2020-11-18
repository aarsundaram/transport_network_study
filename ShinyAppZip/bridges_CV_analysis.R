
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shiny)

#runGitHub( "transport_network_study", "aarsundaram")

filepath=getwd()
setwd(filepath)

###Load your data: 

bridges <- read.csv('CandV_Bridges.csv', header=TRUE, sep=',')    

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bridges Visualization"),
  
  sidebarPanel(
    htmlOutput("bridge_road_selector")# from objects created in server
      ),
  
  
  mainPanel(
    
    #map showing road-segment data point depending on which road you choose
    leafletOutput("mymap"),
    print("Top 10 most critical bridges"),
    tableOutput("mytable"),
    print("Top 10 most vulnerable bridges"),
    tableOutput("mytable2"),
    p()
  )
  
  
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  
  output$bridge_road_selector = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "bridge_road", #name of input
                label = "Bridge Road:", #label displayed in ui
                choices = as.character(unique(bridges$road)),
                # calls unique values from the State column in the previously created table
                selected = "N1") #default choice (not required)
    
  })
  
  
    mydata <- eventReactive(input$bridge_road, {bridges[bridges$road == input$bridge_road,]})
    ## table output for criticality 
    output$mytable <- renderTable({
      sortedtable <- top_n(mydata(),10,mydata()$CriticalityScore)

      to_return <- sortedtable %>% select('road','name','condition','zone')
      return(to_return)
    })
    
    ## table output for vulnerability
    output$mytable2 <- renderTable({
      sortedtable2 <- top_n(mydata(),10,mydata()$VulnerabilityScore)
      
      to_return2 <- sortedtable2 %>% select('road','name','condition','zone')
      return(to_return2)
    })
    
    #map to show road segments:
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        
        addMarkers(mydata(),lng= mydata()$lon,lat = mydata()$lat, popup = paste("Condition: ",mydata()$condition, "<br/>","Zone: ", mydata()$zone) )
        
    })
    
    
    
})   
  
  


# Run the application 
shinyApp(ui = ui, server = server)

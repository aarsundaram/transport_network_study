
library(shiny)
library(leaflet)
library(sf)
#setwd("~/Documents/GitHub/advanced-des/RShiny App")

#sidebar panel, where you can choose:
# roads : from dropdown 
# view map of road segment points 
#chose criticality or vulnerability 
#each point is gradient based on C/V value

# if you click on road-segment point
#information on density 

# bridges: based on roads and category 
#choose road
# get bridge points 
#click on bridge point 
#get C/V information 

###Load your data: 

bridges <- read.csv('BMMS_overview.csv', header=TRUE, sep=',')

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
    p(),
    fluidRow(verbatimTextOutput("map_marker_click"))
    #plotOutput("distPlot")
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
  
  #deciding which points to display based on whether bridges or roads is selected: 
  #req_road_points <- roads[roads$road == input$road,]
  #req_bridge_road <- roads[bridges$road == input$road,]
  
  points <- eventReactive(input$bridge_road,{
    cbind(bridges[bridges$road == input$bridge_road,]$lon, bridges[bridges$road == input$bridge_road,]$lat)
  }, ignoreNULL = FALSE)  
  
  #map to show road segments:
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data=points())
  })

  
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click  
    print(p)
    })
  
})
  
  
  


# Run the application 
shinyApp(ui = ui, server = server)

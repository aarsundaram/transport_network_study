
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)

filepath=getwd()
setwd(filepath)

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

roads <- read.csv('processed data/merged_final_roads.csv', header=TRUE, sep=',')
#bridges <- read.csv('BMMS_overview.csv', header=TRUE, sep=',')

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Road Segment Visualization"),

  sidebarPanel(
    
    htmlOutput("road_selector"),#add selectinput boxs
    
    #checkboxInput('trucks_density', 'Trucks', FALSE),
    #checkboxInput('car_density', 'Cars', FALSE),
    #checkboxInput('bus_density', 'Bus', FALSE),
    #checkboxInput('nonmotorized_density', 'Non Motororized', FALSE)
    
  ),
  
  
  mainPanel(
    leafletOutput("mymap"),
    p(),
    print("Top 10 most critical road-segments"),
    tableOutput("mytable"),
    print("Top 10 most vulnerable road-segments"),
    tableOutput("mytable2"),
    #map showing road-segment data point depending on which road you choose
    plotOutput("distPlot1",width = "100%", height = "1500px"),
    plotOutput("distPlot2",width = "100%", height = "1500px"),
    plotOutput("distPlot3",width = "100%", height = "1500px"),
    plotOutput("distPlot4",width = "100%", height = "1500px"),
    p()
  )
  
  
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$road_selector = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "road2", #name of input
                label = "Road:", #label displayed in ui
                choices = as.character(unique(roads$road)),
                # calls unique values from the State column in the previously created table
                selected = "N1") #default choice (not required)

  })
  
  mydata <- eventReactive(input$road2, {roads[roads$road == input$road2,]})
  output$mytable <- renderTable({
    sortedtable <- top_n(mydata(),10,mydata()$criticality)
    to_return <- sortedtable %>% select('road_segment','road_name')
    return(to_return)
  })
  
  output$mytable2 <- renderTable({
    sortedtable2 <- top_n(mydata(),10,mydata()$vul_roadseg)
    #df1 %>% select(A, B, E)
    to_return2 <- sortedtable2 %>% select('road_segment','road_name')
    return(to_return2)
  })
  #plot to show traffic density per mode of transport 
  output$distPlot1 <- renderPlot( {
    pl1 <- barplot(mydata()$trucks_density,
                  main = "Truck Density per Road Segment",
                  xlab = "Trucks per km per day",
                  #ylab = "Road Segment",
                  names.arg = mydata()$road_segment,
                  col = "darkred",
                  horiz = TRUE,
                  las=2,
                  )
    return(pl1)

  })
  ## for cars:
  output$distPlot2 <- renderPlot( {
    pl2 <- barplot(mydata()$car_density,
                   main = "Car Density per Road Segment",
                   xlab = "Cars per km per day",
                   #ylab = "Road Segment",
                   names.arg = mydata()$road_segment,
                   col = "blue",
                   horiz = TRUE,
                   las=2,
    )
    return(pl2)
  })
  ## for bus:
  output$distPlot3 <- renderPlot( {
    pl3 <- barplot(mydata()$bus_density,
                   main = "Bus Density per Road Segment",
                   xlab = "Bus per km per day",
                   #ylab = "Road Segment",
                   names.arg = mydata()$road_segment,
                   col = "green",
                   horiz = TRUE,
                   las=2,
    )
    return(pl3)
  })
  
  ## for nonmotorized:
  output$distPlot4 <- renderPlot( {
    pl4 <- barplot(mydata()$nonmotorized_density,
                   main = "NonMotorized Density per Road Segment",
                   xlab = "NonMotor vehicles per km per day",
                   #ylab = "Road Segment",
                   names.arg = mydata()$road_segment,
                   col = "pink",
                   horiz = TRUE,
                   las=2,
    )
    return(pl4)
  })
  
  
  #map to show road segments:
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      

      addMarkers(mydata(),lng= mydata()$lon,lat = mydata()$lat, popup = paste("Road_Segment :",mydata()$road_segment,"<br/>", "Criticality: ",mydata()$criticality, "<br/>","Vulnerability: ", "<br/>", mydata()$vul_roadseg) )
     
  })
  
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click
  })
    
}) 

  
# Run the application 
shinyApp(ui = ui, server = server)

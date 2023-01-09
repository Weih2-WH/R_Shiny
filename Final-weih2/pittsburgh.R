library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(maps) 
library(maptools)
library(rgeos)
library(leaflet)
library(geojsonsf)

# data source
# https://data.wprdc.org/dataset/city-pittsburgh-operating-budget/ -- 2 plotly, 3 input, datatable, get raw data
# https://data.wprdc.org/dataset/pittsburgh-public-school-locations/ --map
# https://mropengate.blogspot.com/2016/06/rleafletshiny.html
# https://data.wprdc.org/dataset/pittsburgh-watersheds/ --  API?

# Load data 
budget <- read.csv('Pittsburgh_budget.csv')
school <- read.csv('publish_school.csv')
bridge <- geojson_sf("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/ArcGIS/rest/services/PGHBridges/FeatureServer/0/query?where=FID+%3C+%27143%27&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=*&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=")
print(bridge)

# Header
header <- dashboardHeader(title = "Pittsburgh Operating Information")

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Public School Map",icon = icon("map-location-dot"),tabName = "mapM"),
    menuItem("Budget Chart",icon = icon("bar-chart"), tabName = "chartM"),    
    menuItem("Budget Table", icon = icon("table"), tabName = "tableM"),
    menuItem("API Table", icon = icon("bridge"), tabName = "apiM"),
    
    # Select variables to plot 
    # Municipal
    selectInput(inputId = "depSelect",
                "Department Option:",
                choices = sort(unique(budget$Department)),
                multiple = TRUE,
                selectize = TRUE),
    
    #Checkbox
    checkboxInput(inputId = "typeCheck",
                  label = "Hide Revenue",
                  value = TRUE),
    #text
    textInput(inputId = "titleText", 
              label = "Input your own plot title - Each department budget:", 
              placeholder = "Enter the plot title"),
    
    # slider
    sliderInput(inputId = "amountSlider",
                "Range of the amount:",
                min = min(budget$Amount, na.rm = T),
                max = max(budget$Amount, na.rm = T),
                value = c(min(budget$Amount, na.rm = T), max(budget$Amount, na.rm = T)),
                step = 1)
  )
)

# body 
body <- dashboardBody(tabItems(
  
  # Map
  tabItem("mapM",
          fluidRow(title = "Map", 
          leafletOutput("schoolMap", height = "100vh")),
          absolutePanel(top = 20, right = 42,
                        titlePanel(h2("Pittsburgh public school")))),
  
  # Plot
  tabItem("chartM",
          fluidRow(
            tabBox(title = "Chart", 
                   width = 12,
                   tabPanel("Each department budget", plotlyOutput("plot_department")),
                   tabPanel("Amount description", plotlyOutput("plot_amount")))
          )),
  
  # table
  tabItem("tableM",
          fluidPage(
            box(title = "Table", 
                dataTableOutput("table"), width = 12,
                downloadButton('downloadData', 'Download')))),
  
  # table
  tabItem("apiM",
          fluidPage(
            box(title = "Table", 
                dataTableOutput("apitable"), width = 12)))
))

ui <- dashboardPage(header, sidebar, body)


# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Input with municipality, facility, and race all selected ----------
  filterData <- reactive({
    # Time Filter ----------------------------------------------
    budget <- filter(budget, budget$Amount >= input$amountSlider[1] & budget$Amount <= input$amountSlider[2])
    
    # budget type Filter ----------------------------------------------
    if (input$typeCheck) {
      budget <- subset(budget, Type %in% ("Expenditure")) #%in%
    }
    
    # department Filter ----------------------------------------------
    if (length(input$depSelect) > 0 ) {
      budget <- subset(budget, Department %in% input$depSelect) #%in%
    }
    
    # Return dataframe ----------------------------------------------
    return(budget)
  })
  #  titleText toTitleCase ----------------------------------
  pretty_plot_title <- reactive({ toTitleCase(input$titleText) })
  
  #plot_department
  output$plot_department <- renderPlotly({
    ggplot(data = filterData(), aes(x=Department, y=Amount)) + 
      geom_jitter(width = .5, size=1) +
      ylab('Thousand Dollar') +
      labs(title = pretty_plot_title()) +
      theme_classic()
  })
  #plot_amount
  output$plot_amount <- renderPlotly({
    ggplot(data = filterData(), aes(x=Description, y=Amount)) + 
      geom_bar(stat = "identity")  +
      ylab('Thousand Dollar') +
      theme_classic()
  }) 
  # table
  output$table <- DT::renderDataTable({
    subset(filterData(), select = c(Year,Type,Department,Description,Amount))
  })
  # downloadHandler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('tmp', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(filterData(), file)
    }
  )
  # api table
  output$apitable <- DT::renderDataTable({
    subset(bridge, select = c(Latitude,Longitude,FID,geometry))
  })
  # map
  output$schoolMap <- renderLeaflet({
    leaflet(school) %>% addTiles() %>%
      addMarkers(lng = school$longitude,lat = school$latitude,popup = ~paste("<h5>",name,"</h5>",
                                                                             address,"<br>"))%>%
      addPolylines(lng = school$longitude,lat = school$latitude)
  })
  
  observe({
    proxy <- leafletProxy("map", data = school)
    proxy %>% clearControls()
    leafletProxy("map", data = school) %>%
      clearShapes() %>% clearMarkerClusters() %>%
      addMarkers(lng = school$longitude,lat = school$latitude,popup = ~paste("<h5>",name,"</h5>",
                                address,"<br>"),
                 clusterOptions = markerClusterOptions())
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(data.table)

#load data
COVID<-fread('covid.csv', verbose = F)
COVID$month <- as.Date(COVID$month, format = "%Y-%m-%d")
print(COVID)

pdf(NULL)

header <- dashboardHeader(title = "COVID Death and Case")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Cases", icon = icon("chart-simple"), tabName = "casesM"),
    menuItem("Death", icon = icon("chart-line"), tabName = "deathM"),
    menuItem("Mask", icon = icon("chart-column"), tabName = "maskM"),
    menuItem("Table", icon = icon("table"), tabName = "tableM"),
    # states Selection ----------------------------------------------
    selectInput("stateS",
                "State:",
                choices = sort(unique(COVID$states)),
                multiple = TRUE,
                selectize = TRUE,
                selected = sort(unique(COVID$states))),
    
    # case number Selection ----------------------------------------------
    sliderInput("numberS",
                "Cases of Number:",
                min = min(COVID$cases, na.rm = T),
                max = max(COVID$cases, na.rm = T),
                value = c(min(COVID$cases, na.rm = T), max(COVID$cases, na.rm = T)),
                step = 1),
    # death number Selection ----------------------------------------------
    sliderInput("DnumberS",
                "Death of Number:",
                min = min(COVID$death, na.rm = T),
                max = max(COVID$death, na.rm = T),
                value = c(min(COVID$death, na.rm = T), max(COVID$death, na.rm = T)),
                step = 1),
    # time Selection ----------------------------------------------
    sliderInput("timeS",
                "Month Select:",
                min = min(COVID$month, na.rm = T),
                max = max(COVID$month, na.rm = T),
                value = c(min(COVID$month, na.rm = T), max(COVID$month, na.rm = T)),
                timeFormat = "%Y-%m-%d")
    
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Cases page ----------------------------------------------
  tabItem("casesM",
          fluidPage(
            tabBox(title = "Number of Cases",
                   width = 12,
                   tabPanel("Monthly data: ", plotlyOutput("plot_cases_date")),
                   tabPanel("Geography data: ", plotlyOutput("plot_cases_geo"))
            )
          )
  ),
  # Death page ----------------------------------------------
  tabItem("deathM",
          fluidPage(
            tabBox(title = "Number of Death",
                   width = 12,
                   tabPanel("Monthly data: ", plotlyOutput("plot_death_date")),
                   tabPanel("Geography data: ", plotlyOutput("plot_death_geo"))
            )
          )
  ),
  # Mask page ----------------------------------------------
  tabItem("maskM",
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("casesC"),
            valueBoxOutput("deathC")
          ),
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Percentage of mask wearing",
                   width = 12,
                   tabPanel("Monthly data: ", plotlyOutput("plot_mask_date"))
                   )
          ),
          fluidRow(
            tabBox(title = "Percentage of mask wearing",
                   width = 12,
                   tabPanel("Geography data: ", plotlyOutput("plot_mask_geo"))
            )
          )
  ),
  # Data Table Page ----------------------------------------------
  tabItem("tableM",
          fluidPage(
            box(title = "COVID detail", DT::dataTableOutput("table"), width = 12))
     )
    )
  )

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
      
    # Time Filter ----------------------------------------------
    COVID <- filter(COVID, COVID$month >= input$timeS[1] & COVID$month <= input$timeS[2])
    
    # Cases Filter ----------------------------------------------
    COVID <- filter(COVID,cases >= input$numberS[1] & cases <= input$numberS[2])
    # Death Filter ----------------------------------------------
    COVID <- filter(COVID,death >= input$DnumberS[1] & death <= input$DnumberS[2])
    
    # states Filter ----------------------------------------------
    if (length(input$stateS) > 0 ) {
      COVID <- subset(COVID, states %in% input$stateS) #%in%
    }
    
    # Return dataframe ----------------------------------------------
    return(COVID)
  })
  
  # Cases -----------------------------------
  # over time
  output$plot_cases_date <- renderPlotly({
    ggplot(data = swInput(), aes(x=month, y=cases)) + 
      geom_jitter(width = .5, size=1) +
      ylab('Number of people') +
      theme_classic()
  })

  #  -----------------------------------
  output$plot_cases_geo <- renderPlotly({
    ggplot(data = swInput(), aes(x=states, y=cases)) + 
    geom_bar(stat = "identity")+  
      ylab('Number of people') +
      theme_classic()
  })  
  
  # Death -----------------------------------
  # over time
  output$plot_death_date <- renderPlotly({
    ggplot(data = swInput(), aes(x=month, y=death, color=death)) + 
      geom_jitter(width = .5, size=1) +
      ylab('Number of people') +
      theme_classic()
  })
  
  #  -----------------------------------
  output$plot_death_geo <- renderPlotly({
    ggplot(data = swInput(), aes(x=states, y=death)) + 
      geom_bar(stat = "identity") +
      ylab('Number of people') +
      theme_classic() 
  }) 
  
  # Mask -----------------------------------
  #  -----------------------------------
  output$plot_mask_date <- renderPlotly({
    ggplot(data = swInput(), aes(x=month, y=mask)) + 
      geom_bar(stat = "identity")  +
      ylab('Persentage of people wearing mask') +
      theme_classic()
  }) 
  #  -----------------------------------
  output$plot_mask_geo <- renderPlotly({
    ggplot(data = swInput(), aes(x=death, y=mask, color=states, size = cases)) + 
      geom_point(alpha = 0.5) +
      #facet_wrap(~ SerialNoFactor) +
      ylab('Persentage of people wearing mask') +
      xlab('Number of people death') +
      theme_classic()
  })
  
  # total number
  output$casesC <- renderValueBox({
    dat <- swInput()
    num <- sum(dat$cases, na.rm = T)
    valueBox(subtitle = "Total of cases", value = num, color = "purple")
  })
  
  output$deathC <- renderValueBox({
    dat <- swInput()
    num <- sum(dat$death, na.rm = T)
    valueBox(subtitle = "Total of death", value = num, color = "red")
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(month, states, mask, cases, death))
  })
  
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(data.table)

# Load data 
budget <- read.csv('Pittsburgh_budget.csv')
school <- read.csv('publish_school.csv')
print(budget)

# Handle null
pdf(NULL)

header <- dashboardHeader(title = "Pittsburgh Operating Information")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Map", icon = icon("chart-simple"), tabName = "mapM"),
    menuItem("Chart", icon = icon("chart-line"), tabName = "chartM"),
    menuItem("Table", icon = icon("table"), tabName = "tableM"),
    menuItem("API", icon = icon("chart-column"), tabName = "apiM"),
    # department Selection ----------------------------------------------
    selectInput("depSelect",
                "Department Option:",
                choices = sort(unique(budget$Department)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("CITY COUNCIL", "CITY CLERK")),
    
    # slider
    sliderInput(inputId = "amountSlider",
                "Range of the amount:",
                min = min(budget$Amount, na.rm = T),
                max = max(budget$Amount, na.rm = T),
                value = c(min(budget$Amount, na.rm = T), max(budget$Amount, na.rm = T)),
                step = 1)
    
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
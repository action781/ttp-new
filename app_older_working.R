# app.R - Main Shiny application file

# Load configuration
source("config.R")
initialize_app()

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(dplyr)
library(lpSolve)

# Load module files
source("R/projections_module.R")
source("R/optimizer_module.R")
source("R/db_module.R")

# Initialize database
db_pool <- init_db()

# UI Definition
ui <- dashboardPage(
  # Header
  dashboardHeader(
    title = "Top Tier Projections",
    tags$li(
      class = "dropdown",
      style = "padding: 8px 20px; font-size: 16px;",
      uiOutput("user_menu")
    )
  ),
  
  # Sidebar with menu
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Free Resources", tabName = "free_resources", icon = icon("file-alt")),
      menuItem("Account Management", tabName = "account", icon = icon("user-cog")),
      # These will be conditionally shown based on subscription
      menuItemOutput("menu_all_projections"),
      menuItemOutput("menu_my_collection"),
      menuItemOutput("menu_additional")
    )
  ),
  
  # Body content
  dashboardBody(
    # Use shinyjs for dynamic UI elements
    useShinyjs(),
    
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    ),
    
    # Tab content
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              fluidRow(
                column(12, 
                  box(
                    width = NULL,
                    status = "primary",
                    solidHeader = FALSE,
                    h2("Welcome to Top Tier Projections"),
                    p("Your source for fantasy basketball projections for Sorare NBA."),
                    p("Top Tier Projections provides advanced statistical projections to help you win in Sorare NBA."),
                    hr(),
                    h3("Features:"),
                    tags$ul(
                      tags$li("Daily updated projections for all NBA players"),
                      tags$li("Advanced lineup optimizer"),
                      tags$li("Personalized lineup recommendations based on your collection")
                    ),
                    hr(),
                    uiOutput("auth_ui")
                  )
                )
              )
      ),
      
      # Free Resources tab
      tabItem(tabName = "free_resources",
              h2("Free Resources"),
              p("Public content available to all users will appear here.")
      ),
      
      # Account Management tab
      tabItem(tabName = "account",
              h2("Account Management"),
              uiOutput("account_management_ui")
      ),
      
      # All Projections tab (premium)
      tabItem(tabName = "all_projections",
              h2("All Projections"),
              fluidRow(
                column(12, 
                  tabBox(
                    width = NULL,
                    tabPanel("Projections Data", projectionsUI("all_projections")),
                    tabPanel("Lineup Optimizer", optimizerUI("lineup_optimizer"))
                  )
                )
              )
      ),
      
      # My Collection tab (premium)
      tabItem(tabName = "my_collection",
              h2("My Collection"),
              uiOutput("my_collection_content")
      ),
      
      # Additional Resources tab (premium)
      tabItem(tabName = "additional",
              h2("Additional Resources"),
              uiOutput("additional_content")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # User authentication UI (simplified for now)
  output$auth_ui <- renderUI({
    actionButton("login_btn", "Log In with Sorare NBA", 
               icon = icon("sign-in-alt"), 
               class = "btn-primary btn-lg")
  })
  
  # Placeholder for login button action
  observeEvent(input$login_btn, {
    showModal(modalDialog(
      title = "Login Feature",
      "OAuth integration with Sorare NBA will be implemented here.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # User menu in header
  output$user_menu <- renderUI({
    div(style = "color: white;", "Welcome, Guest")
  })
  
  # Dynamic sidebar menu items
  output$menu_all_projections <- renderMenu({
    menuItem("All Projections", tabName = "all_projections", icon = icon("table"))
  })
  
  output$menu_my_collection <- renderMenu({
    menuItem("My Collection", tabName = "my_collection", icon = icon("chart-line"))
  })
  
  output$menu_additional <- renderMenu({
    menuItem("Additional Resources", tabName = "additional", icon = icon("folder-open"))
  })
  
  # Account Management UI placeholder
  output$account_management_ui <- renderUI({
    fluidRow(
      column(12,
        box(
          width = NULL,
          h3("Account Management"),
          p("This section will allow users to manage their subscriptions and account settings.")
        )
      )
    )
  })
  
  # My Collection content placeholder
  output$my_collection_content <- renderUI({
    fluidRow(
      column(12,
        box(
          width = NULL,
          h3("My Collection"),
          p("This feature will display personalized projections based on your Sorare NBA card collection.")
        )
      )
    )
  })
  
  # Additional content placeholder
  output$additional_content <- renderUI({
    fluidRow(
      column(12,
        box(
          width = NULL,
          h3("Additional Resources"),
          p("Premium tools and resources will appear here.")
        )
      )
    )
  })
  
  # Initialize projections module
  projections_data <- callModule(projections, "all_projections")
  
  # Initialize optimizer module with projections data
  callModule(optimizer, "lineup_optimizer", projections_data = projections_data$data)
}

# Run the application
shinyApp(ui = ui, server = server)

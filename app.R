# app.R - Main Shiny application file

# Load configuration
library(dotenv)
dotenv::load_dot_env()

source("config.R")
initialize_app()

library(memoise)
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
source("R/free_resources_module.R")
source("R/premium_resources_module.R")
source("R/admin_module.R")
source("R/mock_auth_module.R")       # Added mock auth module for testing
source("R/my_collection_module.R")   # Added my collection module

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
      # These will be conditionally shown based on subscription
      menuItemOutput("menu_all_projections"),
      menuItemOutput("menu_my_collection"),
      menuItemOutput("menu_premium"),
      menuItem("Account Management", tabName = "account", icon = icon("user-cog")),
      menuItemOutput("menu_admin")
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
                    # Replace auth_ui with mock auth UI for testing
                    mockAuthUI("auth")
                  )
                )
              )
      ),
      
      # Free Resources tab
      tabItem(tabName = "free_resources",
              h2("Free Resources"),
              freeResourcesUI("free_resources")
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
      
      # My Collection tab (premium) - Only including the projections data tab for now
      tabItem(tabName = "my_collection",
              h2("My Collection"),
              uiOutput("my_collection_content")
      ),
      
      # Premium Resources tab
      tabItem(tabName = "premium",
              h2("Premium Resources"),
              premiumResourcesUI("premium_resources")
      ),

      # Admin tab
      tabItem(tabName = "admin",
              h2("Database Administration"),
              adminUI("admin")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Initialize mock auth module
  auth_values <- callModule(mockAuth, "auth")
  
  # User menu in header
  output$user_menu <- renderUI({
    if (auth_values$is_authenticated) {
      div(
        style = "color: white;",
        paste0("Welcome, ", auth_values$sorare_username),
        span(
          style = "margin-left: 10px; font-size: 12px; padding: 2px 6px; background-color: #5cb85c; border-radius: 4px;",
          auth_values$subscription_status
        )
      )
    } else {
      div(style = "color: white;", "Welcome, Guest")
    }
  })
  
  # Dynamic sidebar menu items
  output$menu_all_projections <- renderMenu({
    menuItem("All Projections", tabName = "all_projections", icon = icon("table"))
  })
  
  output$menu_my_collection <- renderMenu({
    if (auth_values$is_authenticated) {
      menuItem("My Collection", tabName = "my_collection", icon = icon("chart-line"))
    }
  })
  
  output$menu_premium <- renderMenu({
    if (has_premium_access(auth_values)) {
      menuItem("Premium Resources", tabName = "premium", icon = icon("folder-open"))
    }
  })
  
  output$menu_admin <- renderMenu({
    # For now, always show admin menu
    menuItem("Admin", tabName = "admin", icon = icon("cogs"))
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
  
  # My Collection content
  output$my_collection_content <- renderUI({
    if (!auth_values$is_authenticated) {
      fluidRow(
        column(12,
          box(
            width = NULL,
            h3("Authentication Required"),
            p("Please log in to view your collection."),
            mockAuthUI("auth_collection")
          )
        )
      )
    } else if (!has_premium_access(auth_values)) {
      fluidRow(
        column(12,
          box(
            width = NULL,
            h3("Premium Subscription Required"),
            p("Access to your collection with projections requires a premium subscription."),
            actionButton("upgrade_btn", "Upgrade Now", class = "btn-success")
          )
        )
      )
    } else {
      # Show the My Collection module UI (just the projections data tab for now)
      myCollectionUI("user_collection")
    }
  })
  
  # Initialize projections module
  projections_data <- callModule(projections, "all_projections")
  
  # Initialize optimizer module with projections data
  callModule(optimizer, "lineup_optimizer", projections_data = projections_data$data)
  
  # Initialize free resources module
  callModule(freeResources, "free_resources")
  
  # Initialize premium resources module
  callModule(premiumResources, "premium_resources")

  # Initialize admin module with database pool
  callModule(admin, "admin", db_pool = db_pool)
  
  # Initialize auth module for collection page
  auth_collection_values <- callModule(mockAuth, "auth_collection")
  
  # Initialize my collection module - only if authenticated
  observe({
    req(auth_values$is_authenticated)
    callModule(myCollection, "user_collection", auth_values, projections_data$data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

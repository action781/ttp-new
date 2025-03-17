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
source("R/free_resources_module.R")
source("R/premium_resources_module.R")
source("R/admin_module.R")

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
  
  # Sidebar with menu - reordered and renamed
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
                    uiOutput("auth_ui")
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
      
      # My Collection tab (premium)
      tabItem(tabName = "my_collection",
              h2("My Collection"),
              uiOutput("my_collection_content")
      ),
      
      # Premium Resources tab (renamed from Additional Resources)
      tabItem(tabName = "premium",
              h2("Premium Resources"),
              premiumResourcesUI("premium_resources")
      ),

      # Admin tab (new)
      tabItem(tabName = "admin",
              h2("Database Administration"),
              adminUI("admin")
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
  
  # Renamed from menu_additional to menu_premium
  output$menu_premium <- renderMenu({
    menuItem("Premium Resources", tabName = "premium", icon = icon("folder-open"))
  })
  
  # Admin menu item - conditional based on admin status
  output$menu_admin <- renderMenu({
    # For now, always show it. Later you may want to check if the user is an admin
    # Example: if(is_admin_user()) { ... }
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
  
  # Initialize projections module
  projections_data <- callModule(projections, "all_projections")
  
  # Initialize optimizer module with projections data
  callModule(optimizer, "lineup_optimizer", projections_data = projections_data$data)
  
  # Initialize free resources module
  callModule(freeResources, "free_resources")
  
  # Initialize premium resources module (renamed from additional resources)
  callModule(premiumResources, "premium_resources")

  # Initialize admin module with database pool
  callModule(admin, "admin", db_pool = db_pool)
}

# Run the application
shinyApp(ui = ui, server = server)

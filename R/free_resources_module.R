# free_resources_module.R - Module for free resources section

library(shiny)
library(shinydashboard)
library(DT)

# UI component for the free resources module
freeResourcesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      width = NULL,
      id = ns("resources_tabs"),
      tabPanel(
        "Next Gameweek Caps",
        h3("Next Gameweek Caps"),
        p("Projected caps for players in the upcoming gameweek"),
        DT::dataTableOutput(ns("nextgw_caps_table"))
      )
      # Additional tabs can be added here in the future
    )
  )
}

# Server component for the free resources module
freeResources <- function(input, output, session) {
  ns <- session$ns
  
  # Load Next Gameweek Caps data
  nextgw_caps_data <- reactive({
    # Define path to your data file
    file_path <- "/home/action781/vm-ttp-data/nextGWcaps.rds"
    
    # Check if file exists
    if (!file.exists(file_path)) {
      warning(paste("Next Gameweek Caps file not found at", file_path))
      
      # Return dummy data if file not found
      return(data.frame(
        name = c("Player 1", "Player 2", "Player 3"),
        team = c("Team A", "Team B", "Team C"),
        cap = c(15, 20, 25)
      ))
    }
    
    # Load the data
    tryCatch({
      data <- readRDS(file_path)
      return(data)
    }, error = function(e) {
      warning("Error loading Next Gameweek Caps data: ", e$message)
      
      # Return dummy data if there's an error
      return(data.frame(
        name = c("Player 1", "Player 2", "Player 3"),
        team = c("Team A", "Team B", "Team C"),
        cap = c(15, 20, 25)
      ))
    })
  })
  
  # Render the Next Gameweek Caps table
  output$nextgw_caps_table <- DT::renderDataTable({
    data <- nextgw_caps_data()
    
    # Create the datatable
    DT::datatable(
      data,
      options = list(
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        dom = 'lBfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
}

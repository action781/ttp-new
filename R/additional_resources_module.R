# additional_resources_module.R - Module for additional (premium) resources section

library(shiny)
library(shinydashboard)
library(DT)

# UI component for the additional resources module
additionalResourcesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      width = NULL,
      id = ns("resources_tabs"),
      tabPanel(
        "Value Caps",
        h3("Value Caps"),
        p("Player value relative to cap value"),
        DT::dataTableOutput(ns("value_caps_table"))
      )
      # Additional tabs can be added here in the future
    )
  )
}

# Server component for the additional resources module
additionalResources <- function(input, output, session) {
  ns <- session$ns
  
  # Load Value Caps data
  value_caps_data <- reactive({
    # Define path to your data file
    file_path <- "/home/action781/vm-ttp-data/valuecaps.rds"
    
    # Check if file exists
    if (!file.exists(file_path)) {
      warning(paste("Value Caps file not found at", file_path))
      
      # Return dummy data if file not found
      return(data.frame(
        name = c("Player 1", "Player 2", "Player 3"),
        team = c("Team A", "Team B", "Team C"),
        cap = c(15, 20, 25),
        value = c(2.3, 2.1, 1.5)
      ))
    }
    
    # Load the data
    tryCatch({
      data <- readRDS(file_path)
      return(data)
    }, error = function(e) {
      warning("Error loading Value Caps data: ", e$message)
      
      # Return dummy data if there's an error
      return(data.frame(
        name = c("Player 1", "Player 2", "Player 3"),
        team = c("Team A", "Team B", "Team C"),
        cap = c(15, 20, 25),
        value = c(2.3, 2.1, 1.5)
      ))
    })
  })
  
  # Render the Value Caps table
  output$value_caps_table <- DT::renderDataTable({
    data <- value_caps_data()
    
    # Create the datatable
    dt <- DT::datatable(
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
    
    # Format numeric columns if present
    if("value" %in% names(data)) {
      dt <- DT::formatRound(dt, columns = 'value', digits = 2)
    }
    
    return(dt)
  })
}

# admin_module.R - Module for admin database interface

library(shiny)
library(shinydashboard)
library(DT)

# UI component for the admin module
adminUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12, 
        box(
          title = "Database Tables",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          
          # Table selector
          selectInput(ns("table_select"), "Select Table:", 
                     choices = c("users", "subscriptions", "user_collections")),
          
          # Table display
          DT::dataTableOutput(ns("table_data"))
        )
      )
    )
  )
}

# Server component for the admin module
admin <- function(input, output, session, db_pool) {
  ns <- session$ns
  
  # Reactive expression for the selected table data
  table_data <- reactive({
    req(input$table_select)
    
    tryCatch({
      query <- paste0("SELECT * FROM ", input$table_select)
      data <- dbGetQuery(db_pool, query)
      return(data)
    }, error = function(e) {
      # Return empty data frame with message if error
      return(data.frame(Error = paste("Error loading table:", e$message)))
    })
  })
  
  # Render the table data
  output$table_data <- DT::renderDataTable({
    data <- table_data()
    
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

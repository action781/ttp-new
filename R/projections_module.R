# projections_module.R - Module for handling projections data

# UI component for the projections module
projectionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        box(
          title = "Projections Data",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          DT::dataTableOutput(ns("projections_table"))
        )
      )
    )
  )
}

# Server component for the projections module
projections <- function(input, output, session) {
  ns <- session$ns
  
  # Function to load projections data
  load_projections <- function() {
    # Define path to your projections RDS file
    file_path <- "/home/action781/vm-ttp-data/final_projections_output.rds"
    
    # Debug information
    message("Attempting to load file from: ", file_path)
    message("File exists: ", file.exists(file_path))
    
    # Check if file exists
    if (!file.exists(file_path)) {
      warning(paste("Projections file not found at", file_path))
      
      # Return dummy data if file not found
      return(data.frame(
        name = c("Player 1", "Player 2", "Player 3"),
        team = c("Team A", "Team B", "Team C"),
        g = c(3, 4, 5),
        b2b = c(1, 0, 1),
        cap = c(15, 20, 25),
        projection = c(35.5, 42.3, 38.7),
        value = c(2.3, 2.1, 1.5),
        status = c("Active", "Active", "Injured")
      ))
    }
    
    # Load the data
    tryCatch({
      data <- readRDS(file_path)
      message("Successfully loaded projections data")
      message("Column names: ", paste(names(data), collapse = ", "))
      return(data)
    }, error = function(e) {
      warning("Error loading projections data: ", e$message)
      
      # Return dummy data if there's an error
      return(data.frame(
        name = c("Player 1", "Player 2", "Player 3"),
        team = c("Team A", "Team B", "Team C"),
        g = c(3, 4, 5),
        b2b = c(1, 0, 1),
        cap = c(15, 20, 25),
        projection = c(35.5, 42.3, 38.7),
        value = c(2.3, 2.1, 1.5),
        status = c("Active", "Active", "Injured")
      ))
    })
  }
  
  # Reactive data source that reloads every 5 minutes
  projections_data <- reactivePoll(
    intervalMillis = 5 * 60 * 1000,  # 5 minutes in milliseconds
    session = session,
    checkFunc = function() {
      # Check if file has been modified
      file_path <- "/home/action781/vm-ttp-data/final_projections_output.rds"
      if (file.exists(file_path)) {
        file.info(file_path)$mtime
      } else {
        Sys.time()  # Just return current time if file not found
      }
    },
    valueFunc = function() {
      load_projections()
    }
  )
  
  # Output the projections table
  output$projections_table <- DT::renderDataTable({
    data <- projections_data()
    
    # Debug information
    message("Projections data loaded, rows: ", nrow(data))
    message("Column names: ", paste(names(data), collapse = ", "))

    # Filter to only show the desired columns
    display_cols <- c("name", "team", "g", "b2b", "cap", "projection", "value", "status")
    data <- data[, display_cols, drop = FALSE]
    
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
          list(className = 'dt-center', targets = '_all')  # Center-align all columns
        )
      ),
      rownames = FALSE,
      filter = 'top',
      selection = 'multiple'
    )
    
    # Format numeric columns
    dt <- DT::formatRound(dt, columns = c('projection', 'value'), digits = 1)
    
    return(dt)
  })
  
  # Return the data in a reactive expression for other modules to use
  return(list(
    data = projections_data
  ))
}

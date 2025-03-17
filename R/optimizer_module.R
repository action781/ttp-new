# optimizer_module.R - Module for the lineup optimizer

library(lpSolve)

# UI component for the optimizer module
optimizerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        box(
          title = "Lineup Optimizer",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          
          fluidRow(
            column(4,
              selectInput(ns("contest_type"), "Contest Type:",
                           choices = c("Contender", "Champion", "Underdog"),
                           selected = "Contender")
            ),
            column(4,
              selectInput(ns("scarcity"), "Scarcity:",
                           choices = c("limited", "rare", "superrare", "unique"),
                           selected = "limited")
            ),
            column(4,
              actionButton(ns("optimize_btn"), "Optimize Lineup", 
                           class = "btn-success", 
                           style = "margin-top: 25px;")
            )
          ),
          
          # Locked and excluded players
          hr(),
          fluidRow(
            column(6,
              h4("Locked Players"),
              selectizeInput(ns("locked_players"), NULL, choices = NULL, multiple = TRUE),
              helpText("Select players to lock into your lineup")
            ),
            column(6,
              h4("Excluded Players"),
              selectizeInput(ns("excluded_players"), NULL, choices = NULL, multiple = TRUE),
              helpText("Select players to exclude from your lineup")
            )
          ),
          
          # Optimizer results
          hr(),
          h4("Optimal Lineup"),
          DT::dataTableOutput(ns("optimal_lineup")),
          
          # Total projection
          hr(),
          fluidRow(
            column(6,
              h4("Total Projection:"),
              textOutput(ns("total_projection"))
            ),
            column(6,
              h4("Total Cap:"),
              textOutput(ns("total_cap"))
            )
          )
        )
      )
    )
  )
}

# Server component for the optimizer module
optimizer <- function(input, output, session, projections_data) {
  ns <- session$ns
  
  # Make sure projections data is available
  observe({
    data <- projections_data()
    
    # Update the player selection dropdowns
    updateSelectizeInput(session, "locked_players",
                         choices = setNames(data$name, data$name),
                         server = TRUE)
    
    updateSelectizeInput(session, "excluded_players",
                         choices = setNames(data$name, data$name),
                         server = TRUE)
  })
  
  # Function to run the optimization when button is clicked
  optimal_lineup <- eventReactive(input$optimize_btn, {
    # Get current projections data
    data <- projections_data()
    
    # Make sure we have data
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Filter out excluded players
    if (!is.null(input$excluded_players) && length(input$excluded_players) > 0) {
      data <- data[!data$name %in% input$excluded_players, ]
    }
    
    # Get the locked players
    locked_players <- input$locked_players
    
    # Run the optimizer
    tryCatch({
      result <- optimize_lineup(
        contest_type = input$contest_type,
        scarcity = input$scarcity,
        projections_data = data,
        locked_players = locked_players
      )
      
      return(result)
    }, error = function(e) {
      # If there's an error, show a modal with the error message
      showModal(modalDialog(
        title = "Optimization Error",
        tags$p("An error occurred during optimization:"),
        tags$pre(e$message),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return(NULL)
    })
  })
  
  # Render the optimal lineup table
  output$optimal_lineup <- DT::renderDataTable({
    lineup <- optimal_lineup()
    
    if (is.null(lineup)) {
      return(data.frame())
    }
    
    # Determine if we need to mark the MVP in Champion contests
    if (input$contest_type == "Champion") {
      # Identify the MVP (highest projection)
      mvp_index <- which.max(lineup$Projection)
      lineup$Role <- "Regular"
      lineup$Role[mvp_index] <- "MVP"
      
      # Reorder columns to show Role first
      lineup <- lineup[, c("Role", setdiff(names(lineup), "Role"))]
    }
    
    # Display the table
    DT::datatable(
      lineup,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
    DT::formatRound(columns = c('Projection'), digits = 1)
  })
  
  # Display total projection
  output$total_projection <- renderText({
    lineup <- optimal_lineup()
    
    if (is.null(lineup)) {
      return("N/A")
    }
    
    sprintf("%.1f", sum(lineup$Projection))
  })
  
  # Display total cap
  output$total_cap <- renderText({
    lineup <- optimal_lineup()
    
    if (is.null(lineup)) {
      return("N/A")
    }
    
    # In Champion contests, the MVP doesn't count against the cap
    if (input$contest_type == "Champion") {
      mvp_index <- which.max(lineup$Projection)
      cap_sum <- sum(lineup$Cap[-mvp_index])
    } else {
      cap_sum <- sum(lineup$Cap)
    }
    
    sprintf("%.1f / %s", cap_sum, ifelse(input$contest_type == "Underdog", "75", "120"))
  })
}

# Implementation of the optimization function
optimize_lineup <- function(contest_type, scarcity, projections_data, locked_players = NULL) {
  # Convert input contest_type to the format expected by the optimizer
  contest_type <- paste("Classic", contest_type)
  
  # Prepare the data for optimization
  # Make sure column names match what the optimizer expects
  projections_data <- projections_data[, c("name", "cap", "projection")]
  colnames(projections_data) <- c("Name", "Cap", "Projection")
  
  # Step 1: Initialize remaining cap
  total_cap <- if(contest_type == "Classic Underdog") 75 else 120
  
  # Initialize an empty data frame for locked players
  locked_players_data <- data.frame()
  
  # Step 2: If players are locked, adjust the cap and remaining player pool
  if (!is.null(locked_players) && length(locked_players) > 0) {
    # Filter out the locked players from the pool
    locked_players_data <- projections_data[projections_data$Name %in% locked_players, ]
    
    if (nrow(locked_players_data) == 0) {
      stop("Error: Locked players not found in the projections data.")
    }
    
    # Deduct the total cap of locked players
    remaining_cap <- total_cap - sum(locked_players_data$Cap)
    
    # Check if the remaining cap is valid
    if (remaining_cap < 0) {
      stop("Error: Total cap exceeded with locked players.")
    }
    
    # Remove locked players from the available pool
    projections_data <- projections_data[!projections_data$Name %in% locked_players, ]
  } else {
    remaining_cap <- total_cap  # If no players are locked, remaining cap is 120
  }
  
  # Step 3: Create a unique player constraint matrix
  unique_player_names <- unique(projections_data$Name)
  unique_player_constraint <- sapply(unique_player_names, function(player) {
    as.numeric(projections_data$Name == player)
  })
  
  # Ensure the constraint matrix has the correct dimensions
  unique_player_constraint <- t(unique_player_constraint)
  
  # Step 4: Adjust optimization based on the contest type
  if (contest_type == "Classic Contender" || contest_type == "Classic Underdog") {
    # Contender/Underdog logic: Require 5 players within the cap
    f.con <- rbind(rep(1, nrow(projections_data)))  # Total number of players
    f.dir <- c("==")
    f.rhs <- c(5 - length(locked_players))  # Number of remaining players to choose
    f.con <- rbind(f.con, projections_data$Cap)  # Add the cap constraint
    f.dir <- c(f.dir, "<=")
    f.rhs <- c(f.rhs, remaining_cap)  # Add the remaining cap constraint
    
    # Add constraint to ensure each player can be selected only once
    f.con <- rbind(f.con, unique_player_constraint)
    f.dir <- c(f.dir, rep("<=", nrow(unique_player_constraint)))
    f.rhs <- c(f.rhs, rep(1, nrow(unique_player_constraint)))
    
    # Solve the optimization problem
    result <- lp("max", projections_data$Projection, f.con, f.dir, f.rhs, all.bin = TRUE)
    
    if (result$status != 0) {
      stop("Error: No valid solution found in the optimization process.")
    }
    
    # Get selected players
    selected_players <- projections_data[result$solution == 1, ]
    
    # Combine locked players with selected players
    final_lineup <- rbind(locked_players_data, selected_players)
    
  } else if (contest_type == "Classic Champion") {
    # Classic Champion logic: Select the MVP separately, and find the optimal 4 other players
    # 1. Select the MVP (highest projection)
    mvp_player <- projections_data[which.max(projections_data$Projection), ]
    
    # 2. Remove the MVP from the pool of remaining players
    remaining_players <- projections_data[!(projections_data$Name %in% mvp_player$Name), ]
    
    # 3. Set up constraints for the remaining 4 players
    f.con <- rbind(rep(1, nrow(remaining_players)))  # Total number of non-MVP players
    f.dir <- c("==")
    f.rhs <- c(4 - length(locked_players))  # Number of non-MVP players minus locked players
    f.con <- rbind(f.con, remaining_players$Cap)  # Add the cap constraint
    f.dir <- c(f.dir, "<=")
    f.rhs <- c(f.rhs, remaining_cap)  # Add the remaining cap constraint
    
    # Add constraint to ensure each player can be selected only once
    remaining_unique_player_names <- unique(remaining_players$Name)
    remaining_unique_player_constraint <- sapply(remaining_unique_player_names, function(player) {
      as.numeric(remaining_players$Name == player)
    })
    remaining_unique_player_constraint <- t(remaining_unique_player_constraint)
    
    f.con <- rbind(f.con, remaining_unique_player_constraint)
    f.dir <- c(f.dir, rep("<=", nrow(remaining_unique_player_constraint)))
    f.rhs <- c(f.rhs, rep(1, nrow(remaining_unique_player_constraint)))
    
    # Solve the optimization problem
    result <- lp("max", remaining_players$Projection, f.con, f.dir, f.rhs, all.bin = TRUE)
    
    if (result$status != 0) {
      stop("Error: No valid solution found in the optimization process.")
    }
    
    # Get selected players
    selected_players <- remaining_players[result$solution == 1, ]
    
    # Combine MVP and selected players
    final_lineup <- rbind(locked_players_data, selected_players, mvp_player)
  }
  
  return(final_lineup)
}

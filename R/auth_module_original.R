# auth_module.R - User authentication module

library(shiny)
library(httr2)
library(jsonlite)
library(DBI)
library(RSQLite)

# UI component for the authentication module
authUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("auth_ui"))
  )
}

# Server component for the authentication module
auth <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive values to store authentication state
  values <- reactiveValues(
    is_authenticated = FALSE,
    user_data = NULL,
    sorare_username = NULL,
    access_token = NULL,
    subscription_status = "free"  # Default to free
  )
  
  # Render the appropriate UI based on authentication status
  output$auth_ui <- renderUI({
    if (!values$is_authenticated) {
      # Not authenticated - show login button
      actionButton(ns("login_btn"), "Log In with Sorare NBA", 
                 icon = icon("sign-in-alt"), 
                 class = "btn-primary btn-lg")
    } else {
      # Authenticated - show user info and logout
      div(
        h4(paste("Welcome,", values$sorare_username)),
        p(paste("Subscription Status:", values$subscription_status)),
        actionButton(ns("logout_btn"), "Log Out", 
                   icon = icon("sign-out-alt"), 
                   class = "btn-outline-secondary")
      )
    }
  })
  
  # Handle login button click
  observeEvent(input$login_btn, {
    # In a real implementation, this would redirect to Sorare OAuth
    # For now, we'll create a simulated login dialog
    showModal(modalDialog(
      title = "Sorare NBA Login",
      textInput(ns("username_input"), "Enter Sorare NBA Username:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("submit_login"), "Login", class = "btn-primary")
      )
    ))
  })
  
  # Handle the login submission
  observeEvent(input$submit_login, {
    req(input$username_input)
    
    # Make sure username is not empty
    if (nchar(input$username_input) < 1) {
      showNotification("Please enter a valid username", type = "error")
      return()
    }
    
    # Simulate successful authentication with Sorare
    # In a real implementation, this would verify with Sorare's OAuth
    username <- input$username_input
    
    # Check if user exists in database, create if not
    con <- dbConnect(RSQLite::SQLite(), "/srv/shiny-server/ttp/data/toptierprojections.db")
    
    existing_user <- dbGetQuery(con, 
                             paste0("SELECT * FROM users WHERE sorare_username = '", 
                                   username, "'"))
    
    if (nrow(existing_user) == 0) {
      # New user, create record
      dbExecute(con, paste0("INSERT INTO users (email, sorare_username, last_login) VALUES ('", 
                          username, "@example.com', '", 
                          username, "', datetime('now'))"))
      
      new_user_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
      
      # Create free subscription record
      dbExecute(con, paste0("INSERT INTO subscriptions (user_id, status, plan_type, start_date, end_date) VALUES (", 
                          new_user_id, ", 'active', 'free', datetime('now'), datetime('now', '+100 years'))"))
      
      user_data <- list(
        id = new_user_id,
        email = paste0(username, "@example.com"),
        sorare_username = username,
        subscription_status = "free"
      )
    } else {
      # Existing user, update last login
      user_id <- existing_user$id
      dbExecute(con, paste0("UPDATE users SET last_login = datetime('now') WHERE id = ", user_id))
      
      # Get subscription status
      subscription <- dbGetQuery(con, paste0("SELECT * FROM subscriptions WHERE user_id = ", 
                                          user_id, " AND status = 'active'"))
      
      subscription_status <- if (nrow(subscription) > 0) subscription$plan_type[1] else "free"
      
      user_data <- list(
        id = user_id,
        email = existing_user$email,
        sorare_username = username,
        subscription_status = subscription_status
      )
    }
    
    dbDisconnect(con)
    
    # Update reactive values with user data
    values$is_authenticated <- TRUE
    values$user_data <- user_data
    values$sorare_username <- username
    values$subscription_status <- user_data$subscription_status
    
    # Close the modal
    removeModal()
    
    # Show success notification
    showNotification(paste0("Welcome, ", username, "!"), type = "message")
  })
  
  # Handle logout button click
  observeEvent(input$logout_btn, {
    # Reset authentication state
    values$is_authenticated <- FALSE
    values$user_data <- NULL
    values$sorare_username <- NULL
    values$access_token <- NULL
    
    # Show notification
    showNotification("You have been logged out", type = "message")
  })
  
  # Return reactive values to be used by other modules
  return(values)
}

# Helper function to check if user has premium access
has_premium_access <- function(auth_values) {
  return(auth_values$is_authenticated && 
         auth_values$subscription_status %in% c("premium", "monthly", "annual"))
}

# Function to implement the real OAuth flow with Sorare
# This is just a skeleton - we'll implement the actual OAuth flow later
implement_sorare_oauth <- function(client_id, client_secret, redirect_uri) {
  # Create the authorization URL
  auth_url <- paste0(
    "https://api.sorare.com/oauth/authorize?",
    "client_id=", client_id,
    "&redirect_uri=", URLencode(redirect_uri, reserved = TRUE),
    "&response_type=code",
    "&scope=", URLencode("read_users read_cards", reserved = TRUE)
  )
  
  # This function would be used in production to redirect to Sorare
  return(auth_url)
}

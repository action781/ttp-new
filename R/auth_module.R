# auth_module.R - User authentication module

library(shiny)
library(httr)  # Changed from httr2 to ensure compatibility with OAuth functions
library(jsonlite)
library(DBI)
library(RSQLite)

# Sorare OAuth configuration - update these when you receive your credentials
SORARE_CLIENT_ID <- "your_client_id"  # Replace with your actual client ID
SORARE_CLIENT_SECRET <- "your_client_secret"  # Replace with your actual client secret
REDIRECT_URI <- "http://your-domain.com/oauth/callback"  # Update with your domain

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
    # Check if we have OAuth credentials
    if (SORARE_CLIENT_ID == "your_client_id") {
      # OAuth credentials not set up yet, use simulated login
      showModal(modalDialog(
        title = "Sorare NBA Login (Simulation Mode)",
        p("OAuth credentials not yet configured. Using simulated login for development."),
        textInput(ns("username_input"), "Enter Sorare NBA Username:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_login"), "Login", class = "btn-primary")
        )
      ))
    } else {
      # OAuth credentials available, use real OAuth
      auth_url <- implement_sorare_oauth(SORARE_CLIENT_ID, SORARE_CLIENT_SECRET, REDIRECT_URI)
      
      # In a production environment, you would redirect directly with:
      # session$sendCustomMessage("redirect", auth_url)
      
      # For development, show the URL
      showModal(modalDialog(
        title = "Redirect to Sorare OAuth",
        tags$div(
          p("In production, clicking the button would redirect to:"),
          p(tags$code(auth_url)),
          p("Once OAuth credentials are configured, this will be a seamless redirect.")
        ),
        footer = modalButton("Close")
      ))
    }
  })
  
  # Handle the simulated login submission (for development purposes)
  observeEvent(input$submit_login, {
    req(input$username_input)
    
    # Make sure username is not empty
    if (nchar(input$username_input) < 1) {
      showNotification("Please enter a valid username", type = "error")
      return()
    }
    
    # Simulate successful authentication with Sorare
    username <- input$username_input
    
    # Process the user login - same logic for both simulated and OAuth
    process_user_login(username, paste0(username, "@example.com"))
    
    # Close the modal
    removeModal()
  })
  
  # Function to process user login (used by both simulation and OAuth)
  process_user_login <- function(username, email) {
    # Check if user exists in database, create if not
    con <- dbConnect(RSQLite::SQLite(), "data/toptierprojections.db")
    
    existing_user <- dbGetQuery(con, 
                             "SELECT * FROM users WHERE sorare_username = ?",
                             params = list(username))
    
    if (nrow(existing_user) == 0) {
      # New user, create record
      dbExecute(con, 
              "INSERT INTO users (email, sorare_username, created_at, last_login) 
               VALUES (?, ?, datetime('now'), datetime('now'))",
              params = list(email, username))
      
      new_user_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
      
      # Create free subscription record
      dbExecute(con, 
              "INSERT INTO subscriptions (user_id, status, plan_type, start_date, end_date) 
               VALUES (?, 'active', 'free', datetime('now'), datetime('now', '+100 years'))",
              params = list(new_user_id))
      
      user_data <- list(
        id = new_user_id,
        email = email,
        sorare_username = username,
        subscription_status = "free"
      )
    } else {
      # Existing user, update last login
      user_id <- existing_user$id
      dbExecute(con, 
              "UPDATE users SET last_login = datetime('now') WHERE id = ?",
              params = list(user_id))
      
      # Get subscription status
      subscription <- dbGetQuery(con, 
                              "SELECT * FROM subscriptions WHERE user_id = ? AND status = 'active'",
                              params = list(user_id))
      
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
    
    # Show success notification
    showNotification(paste0("Welcome, ", username, "!"), type = "message")
  }
  
  # Handle OAuth callback (to be called from the main app)
  process_oauth_callback <- function(code) {
    if (is.null(code) || code == "") {
      return(FALSE)
    }
    
    tryCatch({
      # Exchange authorization code for access token
      token_response <- POST(
        "https://api.sorare.com/oauth/token",
        body = list(
          client_id = SORARE_CLIENT_ID,
          client_secret = SORARE_CLIENT_SECRET,
          code = code,
          grant_type = "authorization_code",
          redirect_uri = REDIRECT_URI
        ),
        encode = "form"
      )
      
      # Parse the token response
      token_data <- content(token_response, "parsed")
      access_token <- token_data$access_token
      
      if (!is.null(access_token)) {
        # Store the access token for later use
        values$access_token <- access_token
        
        # Use the access token to get user information
        user_response <- POST(
          "https://api.sorare.com/graphql",
          body = toJSON(list(
            query = "query { currentUser { email slug } }"
          )),
          add_headers(
            "Authorization" = paste("Bearer", access_token),
            "Content-Type" = "application/json"
          )
        )
        
        user_info <- content(user_response, "parsed")
        email <- user_info$data$currentUser$email
        sorare_username <- user_info$data$currentUser$slug
        
        # Process the user login with the retrieved information
        process_user_login(sorare_username, email)
        
        return(TRUE)
      }
    }, error = function(e) {
      showNotification(paste("OAuth error:", e$message), type = "error")
      return(FALSE)
    })
    
    return(FALSE)
  }
  
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
  
  # Add process_oauth_callback to the returned values
  values$process_oauth_callback <- process_oauth_callback
  
  # Return reactive values to be used by other modules
  return(values)
}

# Helper function to check if user has premium access
has_premium_access <- function(auth_values) {
  return(auth_values$is_authenticated && 
         auth_values$subscription_status %in% c("premium", "monthly", "annual"))
}

# Function to implement the real OAuth flow with Sorare
implement_sorare_oauth <- function(client_id, client_secret, redirect_uri) {
  # Create the authorization URL
  auth_url <- paste0(
    "https://sorare.com/oauth/authorize?",
    "client_id=", client_id,
    "&redirect_uri=", URLencode(redirect_uri, reserved = TRUE),
    "&response_type=code",
    "&scope="  # Empty scope is sufficient for basic auth
  )
  
  return(auth_url)
}

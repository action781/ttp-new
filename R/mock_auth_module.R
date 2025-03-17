# mock_auth_module.R - Simulated authentication module for development

library(shiny)

# UI component for the mock auth module
mockAuthUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("login_btn"), "Simulate Login", icon = icon("sign-in-alt"), class = "btn-primary btn-lg"),
    uiOutput(ns("auth_status"))
  )
}

# Server component for the mock auth module
mockAuth <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive values to store authentication state
  values <- reactiveValues(
    is_authenticated = FALSE,
    user_data = NULL,
    sorare_username = NULL,
    subscription_status = "premium"  # Default to premium for testing
  )
  
  # Handle login button click
  observeEvent(input$login_btn, {
    if (!values$is_authenticated) {
      # Simulate a successful login
      values$is_authenticated <- TRUE
      values$sorare_username <- "ohthatsteve"  # Example Sorare username
      values$user_data <- list(
        id = 123,
        email = "example@example.com",
        sorare_username = "ohthatsteve",
        subscription_status = "premium"
      )
      
      # Show success notification
      showNotification("Successfully logged in as ohthatsteve", type = "message")
    } else {
      # If already logged in, log out
      values$is_authenticated <- FALSE
      values$sorare_username <- NULL
      values$user_data <- NULL
      
      # Show notification
      showNotification("Logged out", type = "message")
    }
  })
  
  # Display auth status
  output$auth_status <- renderUI({
    if (values$is_authenticated) {
      div(
        style = "margin-top: 10px;",
        h4(paste0("Logged in as: ", values$sorare_username)),
        p(paste0("Subscription: ", values$subscription_status))
      )
    } else {
      div(
        style = "margin-top: 10px;",
        p("Not logged in")
      )
    }
  })
  
  # Return reactive values
  return(values)
}

# Helper function to check if user has premium access
has_premium_access <- function(auth_values) {
  return(auth_values$is_authenticated && 
         auth_values$subscription_status %in% c("premium", "monthly", "annual"))
}

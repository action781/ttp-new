# my_collection_module.R - Module for displaying user's card collection with projections

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(httr)
library(jsonlite)
library(magrittr) # For the pipe operator
library(memoise)  # For caching

# UI component for the my collection module
myCollectionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        box(
          title = "My Collection",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          
          # Username input and refresh button
          fluidRow(
            column(6,
              textInput(ns("username_input"), "Sorare Username:", value = "", 
                       placeholder = "Enter Sorare username")
            ),
            column(3,
              actionButton(ns("refresh_collection"), "Refresh Collection", 
                          icon = icon("sync"), 
                          class = "btn-primary",
                          style = "margin-top: 25px;")
            ),
            column(3,
              actionButton(ns("clear_cache"), "Clear Cache", 
                          icon = icon("trash"), 
                          class = "btn-warning",
                          style = "margin-top: 25px;")
            )
          ),
          
          # Status output
          uiOutput(ns("status_message")),
          
          # Filters section
          hr(),
          
          # Scarcity filter buttons
          div(
            style = "margin: 10px 0;",
            div(
              style = "margin-bottom: 5px;",
              tags$p("Filter by Scarcity:")
            ),
            div(
              class = "btn-group", role = "group", `aria-label` = "Scarcity filters",
              actionButton(ns("filter_all"), "All", 
                          class = "btn btn-default active"),
              actionButton(ns("filter_common"), " ", 
                          class = "btn btn-default", 
                          style = "background-color: #adb5bd; width: 40px;"),
              actionButton(ns("filter_limited"), " ", 
                          class = "btn btn-default", 
                          style = "background-color: #ffc107; width: 40px;"),
              actionButton(ns("filter_rare"), " ", 
                          class = "btn btn-default", 
                          style = "background-color: #dc3545; width: 40px;"),
              actionButton(ns("filter_super_rare"), " ", 
                          class = "btn btn-default", 
                          style = "background-color: #007bff; width: 40px;"),
              actionButton(ns("filter_unique"), " ", 
                          class = "btn btn-default", 
                          style = "background-color: #6f42c1; width: 40px;")
            ),
            div(
              style = "margin-top: 5px; font-size: 85%; color: #666;",
              tags$span("Grey = Common, Yellow = Limited, Red = Rare, Blue = Super Rare, Purple = Unique")
            )
          ),
          
          # Season filter buttons
          div(
            style = "margin: 15px 0;",
            div(
              style = "margin-bottom: 5px;",
              tags$p("Filter by Season:")
            ),
            div(
              class = "btn-group", role = "group", `aria-label` = "Season filters",
              actionButton(ns("season_all"), "All Seasons", 
                          class = "btn btn-default active"),
              actionButton(ns("season_current"), "In Season (2024)", 
                          class = "btn btn-default")
            )
          ),
          
          hr(),
          
          # Collection data table
          DT::dataTableOutput(ns("collection_table"))
        )
      )
    )
  )
}

# Server component for the my collection module
myCollection <- function(input, output, session, auth_values, projections_data) {
  ns <- session$ns
  
  # Reactive values
  values <- reactiveValues(
    user_collection = NULL,
    status = NULL,
    loading = FALSE,
    scarcity_filter = NULL,  # NULL means show all scarcities
    season_filter = NULL     # NULL means show all seasons, 2024 means current season only
  )
  
  # Sorare API Key
  sorare_api_key <- Sys.getenv("SORARE_API_KEY")
  
  # Create a cache for API calls with 30-minute expiry
  # Set the cache timeout to 30 minutes (1800 seconds)
  cache_timeout <- 1800
  
  # Create the fetch_user_cards function with caching
  fetch_user_cards <- memoise(function(user_slug) {
    # Define the GraphQL endpoint
    api_url <- "https://api.sorare.com/federation/graphql"
    
    # Initialize variables for pagination
    all_cards <- list()
    has_next_page <- TRUE
    after_cursor <- NULL
    page_count <- 0
    
    # Loop to fetch all cards, page by page
    while (has_next_page && page_count < 100) { # Limit to 100 pages as a safeguard
      # Increase page counter
      page_count <- page_count + 1
      
      # Define the GraphQL query with pagination
      query <- sprintf('{
        user(slug: "%s") {
          id
          cards(first: 20, sport: NBA, after: %s) {
            nodes {
              slug
              rarityTyped
              seasonYear
              pictureUrl
              power
              anyPlayer {
                displayName
                slug
              }
            }
            pageInfo {
              endCursor
              hasNextPage
            }
          }
        }
      }', user_slug, ifelse(is.null(after_cursor), "null", sprintf('"%s"', after_cursor)))
      
      # Make the GraphQL request
      response <- tryCatch({
        POST(
          api_url,
          body = list(query = query),
          encode = "json",
          add_headers(
            "Content-Type" = "application/json",
            "Authorization" = sorare_api_key
          )
        )
      }, error = function(e) {
        message("Error fetching collection: ", e$message)
        return(NULL)
      })
      
      # Check response
      if (is.null(response) || status_code(response) != 200) {
        message("Failed to fetch collection, status code: ", 
                ifelse(is.null(response), "NULL", status_code(response)))
        break
      }
      
      # Parse the response
      result_text <- content(response, "text", encoding = "UTF-8")
      result <- fromJSON(result_text, flatten = TRUE)
      
      # Check for errors in the response
      if (!is.null(result$errors)) {
        error_msg <- paste(sapply(result$errors, function(e) e$message), collapse = "; ")
        message("GraphQL errors: ", error_msg)
        break
      }
      
      # Check if user exists and has cards
      if (is.null(result$data$user)) {
        message("User not found: ", user_slug)
        break
      }
      
      if (is.null(result$data$user$cards$nodes) || length(result$data$user$cards$nodes) == 0) {
        message("No cards found for user")
        if (page_count == 1) {
          # If first page has no cards, user might not have any
          break
        }
      } else {
        # Extract the cards and add to our collection
        all_cards <- append(all_cards, list(result$data$user$cards$nodes))
      }
      
      # Update pagination info
      page_info <- result$data$user$cards$pageInfo
      has_next_page <- page_info$hasNextPage
      after_cursor <- page_info$endCursor
      
      # Add a small delay to avoid hitting rate limits
      Sys.sleep(0.2)
    }
    
    # Combine all the paginated results into a single data frame
    if (length(all_cards) > 0) {
      all_cards_df <- do.call(rbind, lapply(all_cards, function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        # Convert to data frame and handle any list columns
        df <- as.data.frame(x, stringsAsFactors = FALSE)
        # Ensure we have the expected columns
        if (!"anyPlayer.slug" %in% names(df)) df$anyPlayer.slug <- NA
        if (!"anyPlayer.displayName" %in% names(df)) df$anyPlayer.displayName <- NA
        return(df)
      }))
      
      # Remove any NULL rows
      all_cards_df <- all_cards_df[!sapply(all_cards_df$slug, is.null), ]
      
      # Rename columns to match expected format
      if (!is.null(all_cards_df) && nrow(all_cards_df) > 0) {
        all_cards_df <- all_cards_df %>%
          rename(player.slug = anyPlayer.slug, name = anyPlayer.displayName)
        
        return(all_cards_df)
      }
    }
    
    return(NULL)
  }, 
  cache = cachem::cache_mem(max_age = cache_timeout))
  
  # Function to process user collection with projections data
  process_collection <- function(collection_df, projections_df) {
    # Handle case where collection is empty
    if (is.null(collection_df) || nrow(collection_df) == 0) {
      return(NULL)
    }
    
    # Handle missing player.slug or name values
    collection_df$player.slug[is.na(collection_df$player.slug)] <- 
      paste0("unknown-player-", seq_along(which(is.na(collection_df$player.slug))))
    
    collection_df$name[is.na(collection_df$name)] <- 
      paste0("Unknown Player ", seq_along(which(is.na(collection_df$name))))
    
    # Make sure projections_df has required columns
    req_cols <- c("slug", "name", "team", "cap", "projection", "value")
    missing_cols <- req_cols[!req_cols %in% names(projections_df)]
    
    if (length(missing_cols) > 0) {
      warning("Missing required columns in projections data: ", 
              paste(missing_cols, collapse = ", "))
      
      # Fill in missing columns with placeholder data
      for (col in missing_cols) {
        if (col == "slug") {
          projections_df$slug <- paste0("player-", 1:nrow(projections_df))
        } else if (col == "name") {
          projections_df$name <- paste("Player", 1:nrow(projections_df))
        } else if (col == "team") {
          projections_df$team <- paste("Team", 1:nrow(projections_df))
        } else if (col == "cap") {
          projections_df$cap <- sample(10:30, nrow(projections_df), replace = TRUE)
        } else if (col == "projection") {
          projections_df$projection <- runif(nrow(projections_df), 20, 50)
        } else if (col == "value") {
          projections_df$value <- projections_df$projection / projections_df$cap
        }
      }
    }
    
    # Join the collection with projections
    combined_df <- collection_df %>%
      left_join(projections_df, by = c("player.slug" = "slug"))
    
    # Handle NA values from the join
    # For name, prefer the name from projections, but fall back to collection name
    combined_df$name.y[is.na(combined_df$name.y)] <- 
      combined_df$name[is.na(combined_df$name.y)]
    
    # For team, cap, projection, value - use default values if missing
    combined_df$team[is.na(combined_df$team)] <- "Unknown"
    combined_df$cap[is.na(combined_df$cap)] <- 15
    combined_df$projection[is.na(combined_df$projection)] <- 30
    combined_df$value[is.na(combined_df$value)] <- 2.0
    
    # Make sure power is numeric
    combined_df$power <- as.numeric(combined_df$power)
    combined_df$power[is.na(combined_df$power)] <- 1.0
    
    # Calculate power-adjusted projection
    combined_df$adjusted_projection <- 
      round(as.numeric(combined_df$projection) * combined_df$power, 1)
    
    # Select and rename columns for the final output
    result_df <- data.frame(
      Card = as.character(combined_df$pictureUrl),
      Name = as.character(combined_df$name.y),
      Team = as.character(combined_df$team),
      Cap = as.numeric(combined_df$cap),
      Projection = as.numeric(combined_df$adjusted_projection),
      Value = as.numeric(combined_df$value),
      Season = as.integer(combined_df$seasonYear),
      Scarcity = as.character(combined_df$rarityTyped),
      CardSlug = as.character(combined_df$slug),
      PlayerSlug = as.character(combined_df$player.slug),
      stringsAsFactors = FALSE
    )
    
    # Sort by projection (descending)
    result_df <- result_df %>%
      arrange(desc(Projection))
    
    return(result_df)
  }
  
  # Observe refresh collection button
  observeEvent(input$refresh_collection, {
    # Get username from input field
    user_slug <- input$username_input
    
    # Validate username
    if (is.null(user_slug) || user_slug == "") {
      showNotification("Please enter a Sorare username", type = "error")
      values$status <- "Please enter a Sorare username"
      return()
    }
    
    # Update status
    values$status <- paste("Fetching collection for", user_slug, "...")
    values$loading <- TRUE
    
    # Show loading message
    loading_id <- showNotification("Fetching collection...", type = "message", duration = NULL)
    
    # Use withProgress to show a progress bar
    withProgress(message = paste("Fetching collection for", user_slug), value = 0, {
      # Fetch the collection
      tryCatch({
        # Update progress
        incProgress(0.3, detail = "Requesting data from Sorare API...")
        
        # Fetch the collection
        collection_df <- fetch_user_cards(user_slug)
        
        # Check if we got any data
        if (is.null(collection_df) || nrow(collection_df) == 0) {
          removeNotification(loading_id)
          showNotification(paste("No cards found for user:", user_slug), type = "warning")
          values$status <- paste("No cards found for user:", user_slug)
          values$loading <- FALSE
          return()
        }
        
        # Update progress
        incProgress(0.3, detail = "Processing collection data...")
        
        # Get projections data
        proj_data <- projections_data()
        
        # Process the collection with projections
        processed_collection <- process_collection(collection_df, proj_data)
        
        # Update progress
        incProgress(0.4, detail = "Preparing display...")
        
        # Update the reactive values
        values$user_collection <- processed_collection
        values$status <- paste("Found", nrow(processed_collection), "cards for", user_slug)
        
        # Reset filters when loading new collection
        values$scarcity_filter <- NULL
        values$season_filter <- NULL
        
        # Remove loading notification and show success
        removeNotification(loading_id)
        showNotification(paste("Successfully loaded", nrow(processed_collection), "cards!"), 
                        type = "message")
      }, error = function(e) {
        # Remove loading notification and show error
        removeNotification(loading_id)
        error_msg <- paste("Error:", e$message)
        showNotification(error_msg, type = "error")
        values$status <- error_msg
      })
      
      values$loading <- FALSE
    })
  })
  
  # Observe scarcity filter buttons
  observeEvent(input$filter_all, {
    values$scarcity_filter <- NULL
  })
  
  observeEvent(input$filter_common, {
    values$scarcity_filter <- "common"
  })
  
  observeEvent(input$filter_limited, {
    values$scarcity_filter <- "limited"
  })
  
  observeEvent(input$filter_rare, {
    values$scarcity_filter <- "rare"
  })
  
  observeEvent(input$filter_super_rare, {
    values$scarcity_filter <- "super_rare"
  })
  
  observeEvent(input$filter_unique, {
    values$scarcity_filter <- "unique"
  })
  
  # Observe season filter buttons
  observeEvent(input$season_all, {
    values$season_filter <- NULL
  })
  
  observeEvent(input$season_current, {
    values$season_filter <- 2024
  })
  
  # Clear cache button
  observeEvent(input$clear_cache, {
    # Clear the memoise cache
    forget(fetch_user_cards)
    showNotification("Cache cleared successfully", type = "message")
  })
  
  # Status message
  output$status_message <- renderUI({
    if (values$loading) {
      div(
        style = "margin-top: 10px; color: blue;",
        icon("spinner", class = "fa-spin"),
        span(values$status)
      )
    } else if (!is.null(values$status)) {
      div(
        style = "margin-top: 10px;",
        values$status
      )
    }
  })
  
  # Filtered collection reactive expression
  filtered_collection <- reactive({
    # Get the current collection
    collection <- values$user_collection
    
    # If collection is NULL, return NULL
    if (is.null(collection)) {
      return(NULL)
    }
    
    # Apply scarcity filter if set
    if (!is.null(values$scarcity_filter)) {
      collection <- collection %>%
        filter(Scarcity == values$scarcity_filter)
    }
    
    # Apply season filter if set
    if (!is.null(values$season_filter)) {
      collection <- collection %>%
        filter(Season == values$season_filter)
    }
    
    return(collection)
  })
  
  # Render the collection table
  output$collection_table <- DT::renderDataTable({
    # Get the filtered collection
    collection <- filtered_collection()
    
    # If collection is NULL, show placeholder message
    if (is.null(collection)) {
      return(data.frame(Message = "Enter a Sorare username and click 'Refresh Collection' to load cards."))
    }
    
    # If filtered collection is empty, show message
    if (nrow(collection) == 0) {
      # Create message based on active filters
      if (!is.null(values$scarcity_filter) && !is.null(values$season_filter)) {
        msg <- paste0("No cards found with scarcity: ", values$scarcity_filter, 
                      " and season: ", values$season_filter)
      } else if (!is.null(values$scarcity_filter)) {
        msg <- paste0("No cards found with scarcity: ", values$scarcity_filter)
      } else if (!is.null(values$season_filter)) {
        msg <- paste0("No cards found from season: ", values$season_filter)
      } else {
        msg <- "No cards found with current filters"
      }
      return(data.frame(Message = msg))
    }
    
    # Hide internal columns from display
    display_collection <- collection %>%
      select(-CardSlug, -PlayerSlug)
    
    # Create datatable with card images
    dt <- DT::datatable(
      display_collection,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        dom = 'lBfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          # Format the Card column to show images
          list(
            targets = 0,  # Card column (first column)
            render = JS("function(data, type, row, meta) {
              return type === 'display' ? 
                '<img src=\"' + data + '\" height=\"60\" alt=\"Card image\">' : 
                data;
            }")
          ),
          list(className = 'dt-center', targets = '_all')
        )
      ),
      escape = FALSE,  # Important for rendering HTML
      rownames = FALSE,
      filter = 'top',
      selection = 'multiple'
    )
    
    # Format numeric columns
    dt <- DT::formatRound(dt, columns = c('Projection', 'Value'), digits = 1)
    
    return(dt)
  })
  
  # Observe scarcity filter button clicks to update appearance
  observe({
    # Get the current filter
    current_filter <- values$scarcity_filter
    
    # Add 'active' class to current filter button and remove from others
    if (is.null(current_filter)) {
      # All button
      shinyjs::addClass(id = "filter_all", class = "active")
      shinyjs::removeClass(id = "filter_common", class = "active")
      shinyjs::removeClass(id = "filter_limited", class = "active")
      shinyjs::removeClass(id = "filter_rare", class = "active")
      shinyjs::removeClass(id = "filter_super_rare", class = "active")
      shinyjs::removeClass(id = "filter_unique", class = "active")
    } else if (current_filter == "common") {
      shinyjs::removeClass(id = "filter_all", class = "active")
      shinyjs::addClass(id = "filter_common", class = "active")
      shinyjs::removeClass(id = "filter_limited", class = "active")
      shinyjs::removeClass(id = "filter_rare", class = "active")
      shinyjs::removeClass(id = "filter_super_rare", class = "active")
      shinyjs::removeClass(id = "filter_unique", class = "active")
    } else if (current_filter == "limited") {
      shinyjs::removeClass(id = "filter_all", class = "active")
      shinyjs::removeClass(id = "filter_common", class = "active")
      shinyjs::addClass(id = "filter_limited", class = "active")
      shinyjs::removeClass(id = "filter_rare", class = "active")
      shinyjs::removeClass(id = "filter_super_rare", class = "active")
      shinyjs::removeClass(id = "filter_unique", class = "active")
    } else if (current_filter == "rare") {
      shinyjs::removeClass(id = "filter_all", class = "active")
      shinyjs::removeClass(id = "filter_common", class = "active")
      shinyjs::removeClass(id = "filter_limited", class = "active")
      shinyjs::addClass(id = "filter_rare", class = "active")
      shinyjs::removeClass(id = "filter_super_rare", class = "active")
      shinyjs::removeClass(id = "filter_unique", class = "active")
    } else if (current_filter == "super_rare") {
      shinyjs::removeClass(id = "filter_all", class = "active")
      shinyjs::removeClass(id = "filter_common", class = "active")
      shinyjs::removeClass(id = "filter_limited", class = "active")
      shinyjs::removeClass(id = "filter_rare", class = "active")
      shinyjs::addClass(id = "filter_super_rare", class = "active")
      shinyjs::removeClass(id = "filter_unique", class = "active")
    } else if (current_filter == "unique") {
      shinyjs::removeClass(id = "filter_all", class = "active")
      shinyjs::removeClass(id = "filter_common", class = "active")
      shinyjs::removeClass(id = "filter_limited", class = "active")
      shinyjs::removeClass(id = "filter_rare", class = "active")
      shinyjs::removeClass(id = "filter_super_rare", class = "active")
      shinyjs::addClass(id = "filter_unique", class = "active")
    }
  })
  
  # Observe season filter button clicks to update appearance
  observe({
    # Get the current season filter
    current_season_filter <- values$season_filter
    
    # Add 'active' class to current filter button and remove from others
    if (is.null(current_season_filter)) {
      shinyjs::addClass(id = "season_all", class = "active")
      shinyjs::removeClass(id = "season_current", class = "active")
    } else {
      shinyjs::removeClass(id = "season_all", class = "active")
      shinyjs::addClass(id = "season_current", class = "active")
    }
  })
  
  # Return the collection data for use in other modules later
  return(list(
    collection = filtered_collection  # Return the filtered collection
  ))
}

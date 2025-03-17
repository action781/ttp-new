# db_module.R - Database connectivity and functions

library(DBI)
library(RSQLite)
library(pool)

# Initialize the database connection pool
db_pool <- NULL

# Initialize database
init_db <- function(db_path = "/srv/shiny-server/ttp/data/toptierprojections.db") {
  # Create a connection pool
  db_pool <<- dbPool(
    RSQLite::SQLite(),
    dbname = db_path
  )
  
  # Check if tables exist, create if they don't
  if (!dbExistsTable(db_pool, "users")) {
    dbExecute(db_pool, "
      CREATE TABLE users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        email TEXT UNIQUE NOT NULL,
        sorare_username TEXT UNIQUE NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        last_login TIMESTAMP
      )
    ")
    
    message("Created users table")
  }
  
  if (!dbExistsTable(db_pool, "subscriptions")) {
    dbExecute(db_pool, "
      CREATE TABLE subscriptions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL,
        stripe_customer_id TEXT,
        stripe_subscription_id TEXT,
        status TEXT NOT NULL,
        plan_type TEXT NOT NULL,
        start_date TIMESTAMP,
        end_date TIMESTAMP,
        FOREIGN KEY (user_id) REFERENCES users(id)
      )
    ")
    
    message("Created subscriptions table")
  }
  
  if (!dbExistsTable(db_pool, "user_collections")) {
    dbExecute(db_pool, "
      CREATE TABLE user_collections (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL,
        player_slug TEXT NOT NULL,
        player_name TEXT NOT NULL,
        scarcity TEXT NOT NULL,
        season INTEGER NOT NULL,
        date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (user_id) REFERENCES users(id)
      )
    ")
    
    message("Created user_collections table")
  }
  
  return(db_pool)
}

# Get user by Sorare username
get_user_by_username <- function(sorare_username) {
  query <- sprintf("SELECT * FROM users WHERE sorare_username = '%s'", sorare_username)
  result <- dbGetQuery(db_pool, query)
  
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  return(result[1,])
}

# Create new user
create_user <- function(email, sorare_username) {
  query <- sprintf(
    "INSERT INTO users (email, sorare_username, last_login) VALUES ('%s', '%s', datetime('now'))",
    email, sorare_username
  )
  
  result <- dbExecute(db_pool, query)
  
  if (result == 1) {
    # Get the user ID
    query <- "SELECT last_insert_rowid() as id"
    user_id <- dbGetQuery(db_pool, query)$id
    
    # Create default free subscription
    query <- sprintf(
      "INSERT INTO subscriptions (user_id, status, plan_type, start_date, end_date) 
       VALUES (%d, 'active', 'free', datetime('now'), datetime('now', '+100 years'))",
      user_id
    )
    dbExecute(db_pool, query)
    
    return(user_id)
  }
  
  return(NULL)
}

# Get user subscription status
get_subscription_status <- function(user_id) {
  query <- sprintf(
    "SELECT * FROM subscriptions WHERE user_id = %d AND status = 'active'",
    user_id
  )
  
  result <- dbGetQuery(db_pool, query)
  
  if (nrow(result) == 0) {
    return("free")
  }
  
  return(result$plan_type[1])
}

# Update user's last login time
update_last_login <- function(user_id) {
  query <- sprintf(
    "UPDATE users SET last_login = datetime('now') WHERE id = %d",
    user_id
  )
  
  dbExecute(db_pool, query)
}

# Save user collection
save_user_collection <- function(user_id, collection_df) {
  # First, delete existing collection for this user
  query <- sprintf("DELETE FROM user_collections WHERE user_id = %d", user_id)
  dbExecute(db_pool, query)
  
  # Now insert the new collection
  # We're assuming collection_df has columns: player_slug, player_name, scarcity, season
  for (i in 1:nrow(collection_df)) {
    query <- sprintf(
      "INSERT INTO user_collections (user_id, player_slug, player_name, scarcity, season) 
       VALUES (%d, '%s', '%s', '%s', %d)",
      user_id, 
      collection_df$player_slug[i], 
      collection_df$player_name[i],
      collection_df$scarcity[i],
      collection_df$season[i]
    )
    
    dbExecute(db_pool, query)
  }
  
  return(TRUE)
}

# Get user collection
get_user_collection <- function(user_id) {
  query <- sprintf("SELECT * FROM user_collections WHERE user_id = %d", user_id)
  collection <- dbGetQuery(db_pool, query)
  
  return(collection)
}

# Clean up database pool on application exit
onStop(function() {
  if (!is.null(db_pool)) {
    poolClose(db_pool)
  }
})

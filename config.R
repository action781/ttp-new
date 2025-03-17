# config.R - Configuration settings for Top Tier Projections

# App configuration
APP_NAME <- "Top Tier Projections"
APP_VERSION <- "1.0.0"
DEBUG_MODE <- TRUE

# Database configuration
DB_TYPE <- "sqlite"  # Change to "postgres" later if needed
DB_PATH <- "/srv/shiny-server/ttp/data/toptierprojections.db"

# API configuration
# These would be stored as environment variables in production
SORARE_API_URL <- "https://api.sorare.com/graphql"
SORARE_CLIENT_ID <- "YOUR_CLIENT_ID"  # Replace with actual values later
SORARE_CLIENT_SECRET <- "YOUR_CLIENT_SECRET"  # Replace with actual values later
SORARE_REDIRECT_URI <- "http://34.30.34.27:3838/ttp/oauth/callback"  # Update with your actual domain

# Stripe configuration
STRIPE_PUBLIC_KEY <- "YOUR_STRIPE_PUBLIC_KEY"  # Replace with actual values later
STRIPE_SECRET_KEY <- "YOUR_STRIPE_SECRET_KEY"  # Replace with actual values later

# Subscription tiers
SUBSCRIPTION_PRICES <- list(
  monthly = 9.99,
  annual = 99.99
)

# Function to initialize app environment
initialize_app <- function() {
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data", recursive = TRUE)
  }
  
  # Load any other required resources
  message("App environment initialized.")
}

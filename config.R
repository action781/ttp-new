# config.R - Configuration settings for Top Tier Projections

# App configuration
APP_NAME <- "Top Tier Projections"
APP_VERSION <- "1.0.0"
DEBUG_MODE <- TRUE

# Database configuration
DB_TYPE <- "sqlite"  # Change to "postgres" later if needed
DB_PATH <- "/srv/shiny-server/ttp/data/toptierprojections.db"

# API configuration
SORARE_API_URL <- "https://api.sorare.com/graphql"
SORARE_API_KEY <- Sys.getenv("SORARE_API_KEY")
SORARE_CLIENT_ID <- Sys.getenv("SORARE_CLIENT_ID")
SORARE_CLIENT_SECRET <- Sys.getenv("SORARE_CLIENT_SECRET")
SORARE_REDIRECT_URI <- Sys.getenv("SORARE_REDIRECT_URI")

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

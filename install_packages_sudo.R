# Required packages
required_packages <- c(
  'shiny',
  'shinydashboard',
  'shinyjs',
  'DT',
  'httr2',
  'jsonlite',
  'dplyr',
  'tidyr',
  'ggplot2',
  'lubridate',
  'pool',
  'DBI',
  'RSQLite',
  'memoise',
  'future',
  'promises'
)

# Get currently installed packages
installed <- rownames(installed.packages())

# Determine which packages need to be installed
to_install <- required_packages[!required_packages %in% installed]

# Print packages that need to be installed
if (length(to_install) > 0) {
  cat("Packages to install: ", paste(to_install, collapse=", "), "\n")
} else {
  cat("All required packages are already installed.\n")
}

# Write the list to a file for the sudo command to use
write(to_install, file="packages_to_install.txt")

# NFL Offensive Player Data Scraper
# Scrapes player data from Pro-Football-Reference.com for years 2000-2024
# Includes all offensive positions: QB, RB, FB, WR, TE

# Load required libraries
library(dplyr)
library(rvest)
library(tidyr)

# Function to scrape player data for a specific year and position
scrape_players_by_position <- function(year, position) {
  
  # Construct URL based on position
  # Pro-Football-Reference URLs for different positions:
  # QB: /years/YEAR/passing.htm
  # RB/FB: /years/YEAR/rushing.htm  
  # WR/TE: /years/YEAR/receiving.htm
  
  position_urls <- list(
    "QB" = "passing.htm",
    "RB" = "rushing.htm",
    "FB" = "rushing.htm",
    "WR" = "receiving.htm",
    "TE" = "receiving.htm"
  )
  
  url <- paste0('https://www.pro-football-reference.com/years/', year, '/', position_urls[[position]])
  
  cat("Scraping", position, "data for", year, "from:", url, "\n")
  
  # Add a small delay to be respectful to the server
  Sys.sleep(2)
  
  tryCatch({
    # Read the HTML page
    page <- read_html(url)
    
    # Pro-Football-Reference often has tables that need to be parsed
    # Try to get the main statistics table
    if (position == "QB") {
      table_id <- "#passing"
    } else if (position %in% c("RB", "FB")) {
      table_id <- "#rushing"
    } else {
      table_id <- "#receiving"
    }
    
    # Extract the table
    player_data <- page %>%
      html_node(table_id) %>%
      html_table(fill = TRUE)
    
    # Add year and position columns
    player_data$Year <- year
    player_data$Position <- position
    
    # Clean up the data
    # Remove header rows that repeat in the middle of tables
    player_data <- player_data %>%
      filter(Rk != "Rk")
    
    return(player_data)
    
  }, error = function(e) {
    cat("Error scraping", position, "for year", year, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Function to scrape all offensive positions for a given year
scrape_all_positions <- function(year) {
  
  cat("\n=== Scraping all positions for", year, "===\n")
  
  positions <- c("QB", "RB", "FB", "WR", "TE")
  all_data <- list()
  
  for (pos in positions) {
    data <- scrape_players_by_position(year, pos)
    if (!is.null(data)) {
      all_data[[pos]] <- data
    }
  }
  
  # Combine all position data
  if (length(all_data) > 0) {
    combined_data <- bind_rows(all_data)
    return(combined_data)
  } else {
    return(NULL)
  }
}

# Main execution: Scrape data for years 2000-2024
main <- function() {
  
  cat("Starting NFL player data scraping...\n")
  cat("Years: 2000-2024\n")
  cat("Positions: QB, RB, FB, WR, TE\n\n")
  
  years <- 2000:2024
  all_years_data <- list()
  
  for (year in years) {
    year_data <- scrape_all_positions(year)
    if (!is.null(year_data)) {
      all_years_data[[as.character(year)]] <- year_data
    }
    
    # Progress update
    cat("\nCompleted", year, "-", 
        length(all_years_data), "of", length(years), "years processed\n")
  }
  
  # Combine all years
  if (length(all_years_data) > 0) {
    final_data <- bind_rows(all_years_data)
    
    cat("\n=== Scraping Complete ===\n")
    cat("Total rows collected:", nrow(final_data), "\n")
    cat("Years covered:", paste(range(final_data$Year), collapse = "-"), "\n")
    cat("Positions:", paste(unique(final_data$Position), collapse = ", "), "\n")
    
    # Save to CSV
    output_file <- "nfl_offensive_players_2000_2024.csv"
    write.csv(final_data, output_file, row.names = FALSE)
    cat("\nData saved to:", output_file, "\n")
    
    return(final_data)
  } else {
    cat("No data was collected.\n")
    return(NULL)
  }
}

# Run the scraper
nfl_data <- main()

# Display summary statistics
if (!is.null(nfl_data)) {
  cat("\n=== Data Summary ===\n")
  print(summary(nfl_data))
  
  cat("\n=== First few rows ===\n")
  print(head(nfl_data))
  
  cat("\n=== Player count by position and year ===\n")
  player_counts <- nfl_data %>%
    group_by(Year, Position) %>%
    summarize(Count = n(), .groups = "drop")
  print(player_counts)
}

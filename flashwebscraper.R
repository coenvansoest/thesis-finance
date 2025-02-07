library(chromote)
library(rvest)
library(dplyr)
library(lubridate)
# Start a Chrome Session
b <- ChromoteSession$new()
url <- "https://www.flashscore.com/team/galatasaray/riaqqurF/transfers/"
b$Page$navigate(url)
Sys.sleep(5)  # Wait for the initial page load

# Function to Click "Show More" Until It Disappears
click_show_more_until_end <- function(session, max_clicks = 25) {
  for (i in 1:max_clicks) {
    Sys.sleep(2)  # Short wait before checking
    
    # Check if the "Show More" button exists
    button_exists <- session$Runtime$evaluate('document.querySelector(".transferTab__moreLink") !== null')$result$value
    
    if (button_exists) {
      # Click the button
      session$Runtime$evaluate('document.querySelector(".transferTab__moreLink").click();')
      Sys.sleep(3)  # Allow time for content to load
    } else {
      break  # Stop if button is gone
    }
  }
}

# Keep clicking until all data loads
click_show_more_until_end(b, max_clicks = 25)
Sys.sleep(5)  # Final wait to ensure all content is present

# Get updated page source
page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
html_page <- read_html(page_source)

# Extract Transfer Data
transfer_dates <- html_page %>% html_nodes(".transferTab__date") %>% html_text()
player_names <- html_page %>% html_nodes(".transferTab__player") %>% html_text()

# Extract Transfer Direction (Incoming or Outgoing)
transfer_direction <- html_page %>%
  html_nodes(".transferTab__team--to") %>%
  html_nodes(xpath = "./preceding-sibling::svg[contains(@class, 'arrow')]") %>% 
  html_attr("class")

# Convert arrow class to Incoming/Outgoing


# Combine into a DataFrame
transfer_data <- data.frame(
  Date = transfer_dates,
  Player = player_names,
  stringsAsFactors = FALSE
)

transfer<-transfer_data[-1,]

# Function to swap first and last names
# Define common last name prefixes
prefixes <- c("van", "de", "von", "da", "der", "la", "di", "le")

# Function to correctly swap first and last name
swap_name <- function(name) {
  parts <- unlist(strsplit(name, " "))  # Split into words
  n <- length(parts)  # Get number of words
  
  if (n > 1) {
    # Check if the first word is a prefix
    if (parts[1] %in% prefixes & n > 2) {
      # Format as: "First Name Last Name Prefix"
      return(paste(parts[n], paste(parts[1:(n-1)], collapse = " ")))
    } else {
      # Standard swap (e.g., "Morata Alvaro" → "Alvaro Morata")
      return(paste(parts[n], paste(parts[-n], collapse = " ")))
    }
  } else {
    return(name)  # If only one word, return as is
  }
}



prefixes <- c("van", "de", "von", "da", "der", "la", "di", "le")
# Apply the function to the "Player" column
transfer$Player <- sapply(transfer$Player, swap_name)

transfers <- transfer %>%
  mutate(date = as.Date(Date, format = "%d.%m.%Y")) %>%
  mutate(year = year(date),
         month = month(date),
         # Adjust year for December to belong to next year's first period
         adjusted_year = ifelse(month == 12, year + 1, year),
         # Calculate period_id with December adjustment
         period_id = (adjusted_year - min(adjusted_year)) * 2 + ifelse(month <= 4 | month == 12, 1, 2)) %>%
  select(-adjusted_year, -month, -year, -Date) # Remove temporary column

transfers<-transfers%>%
  rename(player_name = Player)


# Close the Chrome session
b$close()





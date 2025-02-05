# Load libraries
library(rvest)
library(tidyverse)
library(lubridate)

# Define the URL of the Ajax transfers page
url <- "https://www.voetbalkrant.com/nederland/ajax/transfers"

# Read the webpage
webpage <- read_html(url)

# Extract season titles (e.g., "Zomertransfers Ajax 2024")
season_titles <- webpage %>%
  html_nodes("div.title-underline h2") %>%
  html_text(trim = TRUE)

# Extract all tables
tables <- webpage %>% html_nodes("div.table-responsive table")

dates<-webpage%>%
  html_nodes('td:nth-child(1)')%>%
  html_text(trim=T)

players<-webpage%>%
  html_nodes('td:nth-child(3)')%>%
  html_text(trim=T)

clubs<-webpage%>%
  html_nodes('td:nth-child(4)')%>%
  html_text(trim=T)

tochange<-data.frame(dates = dates, id = c(1:127))
tochange<-tochange%>%
  mutate(cutoff = ifelse(grepl('=', x=dates),1,0))

tochange<-tochange%>%
  mutate(group=cumsum(cutoff))%>%
  mutate(season = season_titles[group+1])

tochange<-tochange%>%
  filter(!grepl('=', dates))
years<-str_extract(tochange$season,"\\d{4}$")%>%as.numeric()
tochange<-tochange%>%
  mutate(season = "Ajax")
tochange<-tochange%>%
  select(-id, -cutoff,-group)

ajaxtransfers<-tochange%>%
  mutate(years=years, counter_team_name=clubs, player_name = players)

ajaxtransfers<-ajaxtransfers%>%
  mutate(player_name=gsub('\\*', '', player_name))%>%
  mutate(date = paste0(dates, "/", years))%>%
  select(-dates, -years)

ajaxtransfers <- ajaxtransfers %>%
  mutate(date = as.Date(date, format="%d/%m/%Y")) %>%  # Ensure correct Date format
  mutate(date = case_when(
    month(date) == 12 ~ date - years(1),  # Subtract 1 year if month is December
    TRUE ~ date  # Keep the same date otherwise
  ))

# Convert date column to Date format
ajaxtransfers <- ajaxtransfers %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  mutate(year = year(date),
         month = month(date),
         # Adjust year for December to belong to next year's first period
         adjusted_year = ifelse(month == 12, year - 1, year),
         # Calculate period_id with December adjustment
         period_id = (adjusted_year - min(adjusted_year)) * 2 + ifelse(month <= 4 | month == 12, 1, 2)) %>%
  select(-adjusted_year) # Remove temporary column

# View result
print(ajaxtransfers)


write.csv(ajaxtransfers, 'datesajax.csv')

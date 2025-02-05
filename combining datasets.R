ajaxtotal <- ajaxtotal %>% mutate(player_name = trimws(player_name))
ajaxtotal<-ajaxtotal%>%
  filter(!grepl('Ajax', counter_team_name), !grepl('End', transfer_fee))
ajaxtransfers <- ajaxtransfers %>% mutate(player_name = trimws(player_name))
ajaxtransfers<-ajaxtransfers%>%
  filter(!grepl('06-30', as.character(date)))%>%
  select(-year, -month, -counter_team_name)

ajaxtotal<-ajaxtotal%>%filter(period_id<9)
fullajax<-ajaxtotal%>%
  left_join(ajaxtransfers, by = c('period_id','player_name'))


unmatched <- anti_join(ajaxtotal, ajaxtransfers, by = c("period_id", "player_name"))
print(unmatched)

announcement_dates <- data.frame(
  player_name = c("Ahmetcan Kaplan", "Francisco Conceição", "Jorge Sánchez", "Lucas Ocampos", "Carlos Forbs", 
                  "Gastón Ávila", "Sivert Mannsverk", "Jakov Medic", "Julian Rijkhoff", "Bertrand Traoré", 
                  "Davy Klaassen", "Naci Ünüvar", "Kik Pierie", "Edson Álvarez", "Mohamed Daramy", 
                  "Francisco Conceição", "Jorge Sánchez", "Maarten Stekelenburg", "Lucas Rosa", 
                  "Oliver Edvardsen"),
  date = as.Date(c("2022-08-19", "2022-07-21", "2022-08-10", "2022-08-31", "2023-08-03",
                                "2023-08-22", "2023-09-01", "2023-08-06", "2024-02-01", "2024-07-15",
                                "2024-09-17", "2025-01-16", "2023-07-11", "2023-08-10", "2023-08-11",
                                "2024-06-05", "2024-07-04", "2023-05-18", "2025-02-04", "2025-01-31"))
)

rest<-unmatched%>%
  inner_join(announcement_dates, by='player_name')
rest<-rest%>%
  select(player_name, period_id, date)

fullajax<-fullajax%>%
  left_join(rest, by=c('player_name', 'period_id'))

fullajax <- fullajax %>%
  mutate(date = coalesce(date.x, date.y)) %>%
  select(-date.x, -date.y) 
fullajax$season<-'Ajax'
fullajax <- fullajax %>%
  mutate(player_nat = sapply(player_nat, function(x) paste(x, collapse = ", ")))

write.csv(fullajax, "ajaxcomplete.csv")

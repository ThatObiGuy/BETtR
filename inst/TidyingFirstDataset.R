
#library(dplyr)
#epl_weekend_data <- read_csv("~/epl_weekend_data.csv")
#epl_weekend_data

#eplArranged <- epl_weekend_data |> group_by(event_id) |> arrange(logged_time
   #,.by_group = TRUE) |> arrange(event_id, logged_time)
#eplArranged
#unique(eplArranged[,1])
#eplArranged %>% select(event_id) %>% group_by(event_id) %>%  mutate(matchID = 1:10)
#view(eplArranged)
#vec <- c(1:10)
#eplArranged %>%
 # group_by(event_id)
#unique(eplArranged$event_id)
#view(eplArranged)

#mapping <- data.frame(
 # old = c(1619454336, 1619454337, 1619454338, 1619454339, 1619454340, 1619454450,
 #         1619454451 ,1619454452, 1619456908, 1619456909),
 # new = c(1:length(unique(eplArranged$event_id)))
#)

#eplArranged$event_id <- mapping$new[match(eplArranged$event_id, mapping$old)]
#view(eplArranged)

#saveRDS(eplArranged, file = "OddsData1.rds")
#readRDS(file = "OddsData1.rds")
#save("OddsData1.rds")
# Replace USERNAME with your actual username
#epl_weekend <- readRDS("/Users/sorinbivol/Desktop/OddsData1.rds")
#save(epl_weekend, file = "OddsRatio1.RData")
#load("~/bettr/data/OddsRatio1.RData")

load("~/bettr/data/OddsRatio1.RData")
library(tsibble)
colnames(epl_weekend)
epl_weekend_bettr <- subset(epl_weekend, select = c(event_id, logged_time,
                                                    home_odds, away_odds,
                                                    draw_odds, home_team,
                                                    away_team, max_money_line
                                                    ,league_name))
#epl_weekend_bettr
epl_weekend_tsibble <- epl_weekend_bettr |>
  as_tsibble(key = event_id, index = logged_time)
epl_weekend_tsibble <- epl_weekend_tsibble |> ungroup()
class(epl_weekend_tsibble) <- c("bettr_data", class(epl_weekend_tsibble))


class(epl_weekend_tsibble)

save(epl_weekend_tsibble, file = "epl_weekend.RData")
epl_weekend_tsibble <- readRDS("~/bettr/data/epl_weekend_tsibble.rds")



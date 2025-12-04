
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



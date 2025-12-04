library(dplyr)
library(readr)

epl_weekend_data <- read_csv("bettr/inst/epl_weekend_data.csv",
                             col_types = cols(event_id = col_character(),
                                              logged_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                              sport_id = col_skip(), league_id = col_skip(), league_id = col_character(),
                                              starts = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

epl_weekend_data |> group_by(event_id) |> arrange(logged_time,.by_group = TRUE) |> arrange(event_id, logged_time) -> epl_arranged

epl_arranged %>% select(event_id) %>% group_by(event_id) %>%  mutate(matchID = 1:10)

mapping <- data.frame(
  old = c(1619454336, 1619454337, 1619454338, 1619454339, 1619454340, 1619454450,
          1619454451 ,1619454452, 1619456908, 1619456909),
  new = c(1:length(unique(epl_arranged$event_id)))
)

epl_arranged$event_id <- mapping$new[match(epl_arranged$event_id, mapping$old)]

epl_weekend <- epl_arranged

class(epl_weekend) <- c("bettr_data", "data.frame")
  
saveRDS(epl_arranged, file = "epl_weekend.rds")
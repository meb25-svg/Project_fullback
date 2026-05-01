library(nflreadr)
library(tidyverse)
nflreadr::.clear_cache()
# Load data
pbp_data <- load_pbp(seasons = 2020:2024)
participation <- load_participation(seasons = 2020:2024)

# Merge datasets
pbp_with_personnel <- pbp_data %>%
  left_join(participation, by = c("play_id", "old_game_id"))

# Create analysis dataset with situations
analysis_data <- pbp_with_personnel %>%
  filter(
    play_type %in% c("run", "pass"),
    !is.na(down),
    !is.na(ydstogo)
  ) %>%
  mutate(
    situation_type = case_when(
      yardline_100 <= 5 ~ "goal_line",
      (down %in% c(3, 4) & ydstogo <= 2) ~ "short_yardage",
      (down == 1 & ydstogo == 10) | 
        (down == 2 & ydstogo >= 7 & ydstogo <= 10) ~ "base_down",
      TRUE ~ "other"
    )
  )

# Check for personnel column
colnames(analysis_data)

# View situation breakdown
analysis_data %>%
  count(situation_type)

# Save dataset
write_csv(analysis_data, "fullback_analysis_data.csv")

#define success try yards gained 

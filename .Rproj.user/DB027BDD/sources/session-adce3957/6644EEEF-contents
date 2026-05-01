#install.packages("glmnet")  
install.packages("gt")
library(nflreadr)
library(tidyverse)

library(gt)
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


Fullback_data<-read.csv("fullback_analysis_data.csv")
head(Fullback_data)
library(glmnet)
library(tidyverse)
Fullback_data <- Fullback_data %>%
  mutate(
    has_fullback = case_when(
      str_detect(offense_personnel, "^2") ~ 1,  # With FB
      TRUE ~ 0  # No FB
    )
  )

# 3 situations
situation_data <- Fullback_data %>%
  filter(situation_type %in% c("goal_line", "short_yardage", "base_down"))
# basic plus/minus 
basic_plus_minus <- situation_data %>%
  group_by(situation_type, has_fullback) %>%
  summarise(
    plays = n(),
    avg_epa = mean(epa, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    avg_yards = mean(yards_gained, na.rm = TRUE),
    .groups = "drop"
  )

#Calculate the difference 
plus_minus_results <- basic_plus_minus %>%
  pivot_wider(
    names_from = has_fullback,
    values_from = c(plays, avg_epa, success_rate, avg_yards)
  ) %>%
  mutate(
    epa_plus_minus = avg_epa_1 - avg_epa_0,
    success_plus_minus = success_rate_1 - success_rate_0,
    yards_plus_minus = avg_yards_1 - avg_yards_0
  )

print(plus_minus_results)
plus_minus_results %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  gt()

#Regression based plus minus###################################################
model_short_yardage <- lm(
  epa ~ has_fullback + down + ydstogo + yardline_100 + 
    score_differential + defenders_in_box + offense_personnel + defense_personnel,
  data = filter(situation_data, situation_type == "short_yardage")
)

model_goal_line <- lm(
  epa ~ has_fullback + down + ydstogo + yardline_100 + 
    score_differential + defenders_in_box,
  data = filter(situation_data, situation_type == "goal_line")
)

model_base_down <- lm(
  epa ~ has_fullback + down + ydstogo + yardline_100 + 
    score_differential + defenders_in_box,
  data = filter(situation_data, situation_type == "base_down")
)


print(summary(model_short_yardage)$coefficients["has_fullback", ])


print(summary(model_goal_line)$coefficients["has_fullback", ])


print(summary(model_base_down)$coefficients["has_fullback", ])



model_short_yardage <- lm(
  ydstogo ~ has_fullback + down + yardline_100 + 
    score_differential + defenders_in_box + goal_to_go+ play_type + epa,
  data = filter(situation_data, situation_type == "short_yardage")
)
print(model_short_yardage)

######## Updated mdoel##########################################################
fb_data <- Fullback_data %>%
  mutate(
    has_fullback <- case_when(
      str_detect(offense_personnel, "1 FB") ~ 1,  # Explicit 1 FB in the personnel string
      TRUE ~ 0
    ),
    has_tightend = case_when(
      str_detect(offense_personnel, "1 TE|2 TE|3 TE") ~ 1,  # Any TE on field
      TRUE ~ 0
    ),
    short_yardage_success = if_else(yards_gained >= 1, 1, 0),
    goal_line_success = if_else(touchdown == 1, 1, 0),
    situation_type = case_when(
      yardline_100 <= 5 ~ "goal_line",
      (down %in% c(3, 4) & ydstogo <= 2) ~ "short_yardage",
      (down == 1 & ydstogo == 10) | 
        (down == 2 & ydstogo >= 5 & ydstogo <= 10) ~ "base_down",
      TRUE ~ "other"
    )
  )


situation_data <- fb_data %>%
  filter(situation_type %in% c("goal_line", "short_yardage", "base_down"))

# SHORT YARDAGE - SUCCESS RATE (1+ yard)
model_short_all <- glm(
  short_yardage_success ~ has_fullback + down + ydstogo + yardline_100 + 
    defenders_in_box + first_down_rush + first_down + wpa + drive_first_downs + drive_play_count + xpass ,
  data = filter(situation_data, 
                situation_type == "short_yardage",
                !is.na(defenders_in_box)),
  family = binomial(link = "logit")
)
print(summary(model_short_all)$coefficients["has_fullback", ])

model_short_rush <- glm(
  short_yardage_success ~ has_fullback + down + ydstogo + yardline_100 + 
    defenders_in_box + first_down_rush + first_down + wpa + drive_first_downs + drive_play_count + xpass  ,
  data = filter(situation_data, 
                situation_type == "short_yardage", 
                rush == 1,
                !is.na(defenders_in_box)),
  family = binomial(link = "logit")
)
print(summary(model_short_rush)$coefficients["has_fullback", ])



model_short_pass <- glm(
  short_yardage_success ~ has_fullback + down + ydstogo + yardline_100 + 
    defenders_in_box + first_down + wpa + drive_first_downs + drive_play_count + xpass ,
  data = filter(situation_data, 
                situation_type == "short_yardage", 
                pass == 1,
                !is.na(defenders_in_box)),
  family = binomial(link = "logit")
)
print(summary(model_short_pass)$coefficients["has_fullback", ])
 
# GOAL LINE - EPA
model_goal_all <- glm(
   goal_line_success ~ has_fullback + wpa + epa + yardline_100 + ydstogo +  first_down + first_down_rush + drive_first_downs + drive_play_count + drive_inside20 + success + series_success + no_score_prob ,
   data = filter(situation_data, 
                 situation_type == "goal_line",
                 !is.na(defenders_in_box))
)
print(summary(model_goal_all)$coefficients["has_fullback", ])

model_goal_rush <- glm(
  goal_line_success ~ has_fullback + wpa + epa + yardline_100 + ydstogo +  first_down + first_down_rush + drive_first_downs + drive_play_count + drive_inside20 + success + series_success + no_score_prob ,
  data = filter(situation_data, 
                situation_type == "goal_line",
                rush == 1,
                !is.na(defenders_in_box))
)
print(summary(model_goal_rush)$coefficients["has_fullback", ])


model_goal_pass <- glm(
  goal_line_success ~ has_fullback + wpa + epa + yardline_100 + ydstogo +  first_down + first_down_rush + drive_first_downs + drive_play_count + drive_inside20 + success + series_success + no_score_prob ,
  data = filter(situation_data, 
                situation_type == "goal_line", 
                pass == 1,
                !is.na(defenders_in_box))
)
print(summary(model_goal_pass)$coefficients["has_fullback", ])


# BASE DOWN - EPA
model_base_all <- lm(
  epa ~ has_fullback + wpa + first_down + first_down_rush + drive_first_downs + drive_play_count + ydstogo + xpass + down + defenders_in_box + shotgun + rush_attempt + ep + air_epa + yac_epa + wp +cp + cpoe + pass_oe + defense_man_zone_type + defense_coverage_type ,
  data = filter(situation_data, 
                situation_type == "base_down",
                !is.na(defenders_in_box))
)
print(summary(model_base_down)$coefficients["has_fullback", ])

model_base_rush <- lm(
  epa ~ has_fullback + wpa + first_down + first_down_rush + drive_first_downs + drive_play_count + ydstogo + down + defenders_in_box + shotgun+ rush_attempt + wp + run_location ,
  data = filter(situation_data, 
                situation_type == "base_down", 
                rush == 1,
                !is.na(defenders_in_box))
)
print(summary(model_base_rush)$coefficients["has_fullback", ])

model_base_pass <- lm(
  epa ~ has_fullback + wpa + first_down + drive_first_downs + drive_play_count + ydstogo + xpass + down + defenders_in_box + shotgun + ep + air_epa + yac_epa + wp +cp + cpoe + pass_oe,
  data = filter(situation_data, 
                situation_type == "base_down", 
                pass == 1,
                !is.na(defenders_in_box))
)
print(summary(model_base_pass)$coefficients["has_fullback", ])

get_fixed_gt_base <- function(model, model_name) {
  summary(model)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    gt() %>%
    tab_header(title = model_name) %>%
    tab_style(
      style = cell_fill(color = "#f2f2f2"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    cols_align(align = "left",   columns = "Predictor") %>%
    cols_align(align = "center", columns = -"Predictor")
}

# SHORT YARDAGE
get_fixed_gt_base(model_short_all,  "Short Yardage - All Plays")
get_fixed_gt_base(model_short_rush, "Short Yardage - Rush Plays")
get_fixed_gt_base(model_short_pass, "Short Yardage - Pass Plays")

# GOAL LINE
get_fixed_gt_base(model_goal_all,  "Goal Line - All Plays")
get_fixed_gt_base(model_goal_rush, "Goal Line - Rush Plays")
get_fixed_gt_base(model_goal_pass, "Goal Line - Pass Plays")

# BASE DOWN
get_fixed_gt_base(model_base_all,  "Base Down - All Plays")
get_fixed_gt_base(model_base_rush, "Base Down - Rush Plays")
get_fixed_gt_base(model_base_pass, "Base Down - Pass Plays")



library(tidyverse)

#Pearson Corr to select features short yardage
correlation_data <- fb_data %>%
  filter(situation_type == "short_yardage",
         rush == 1,
         !is.na(defenders_in_box)) %>%
  select(
    short_yardage_success,
    has_fullback,
    down,
    first_down,
    first_down_rush,
    ydstogo,
    yardline_100,
    score_differential,
    defenders_in_box,
    shotgun,
    no_huddle,
    qtr,
    wp,
    wpa,
    ep,
    td_prob,
    fg_prob,
    xpass,
    posteam_score,
    defteam_score,
    drive_play_count,
    drive_first_downs
  ) %>%
  na.omit()

# Calculate correlation
cor_matrix <- cor(correlation_data)

correlations <- cor_matrix[, "short_yardage_success"]
correlations <- correlations[names(correlations) != "short_yardage_success"]

# Sort and print
correlations_df <- data.frame(
  Feature = names(correlations),
  Correlation = correlations
) %>%
  arrange(desc(abs(Correlation)))
print(correlations_df)
#P corr for goal line
correlation_goal_line <- fb_data %>%
  filter(situation_type == "goal_line",
         !is.na(defenders_in_box)) %>%
  select(
    goal_line_success,
    epa,
    has_fullback,
    down,
    first_down,
    first_down_rush,
    ydstogo,
    yardline_100,
    score_differential,
    defenders_in_box,
    shotgun,
    no_huddle,
    qtr,
    wp,
    wpa,
    ep,
    td_prob,
    fg_prob,
    xpass,
    posteam_score,
    defteam_score,
    drive_play_count,
    drive_first_downs
  ) %>%
  na.omit()

cor_matrix_goal <- cor(correlation_goal_line)
correlations_goal <- cor_matrix_goal[, "goal_line_success"]
correlations_goal <- correlations_goal[names(correlations_goal) != "goal_line_success"]

print("GOAL LINE EPA CORRELATIONS (All Plays):")
print(sort(abs(correlations_goal), decreasing = TRUE))


# Base down pearson 
# BASE DOWN - ALL PLAYS (rush and pass)
correlation_base_down <- fb_data %>%
  filter(situation_type == "base_down",
         !is.na(defenders_in_box)) %>%
  select(
    epa,
    epa,
    has_fullback,
    down,
    first_down,
    first_down_rush,
    ydstogo,
    yardline_100,
    score_differential,
    defenders_in_box,
    shotgun,
    no_huddle,
    qtr,
    wp,
    wpa,
    ep,
    td_prob,
    fg_prob,
    xpass,
    posteam_score,
    defteam_score,
    drive_play_count,
    drive_first_downs
  ) %>%
  na.omit()

cor_matrix_base <- cor(correlation_base_down)
correlations_base <- cor_matrix_base[, "epa"]
correlations_base <- correlations_base[names(correlations_base) != "epa"]

print("BASE DOWN EPA CORRELATIONS (All Plays):")
print(sort(abs(correlations_base), decreasing = TRUE))

grep("id", names(fb_data), value = TRUE, ignore.case = TRUE)



#######################Random effects Model #######################################
library(nflreadr)
library(tidyverse)
library(dplyr)
library(stringr)
library(purrr)
library(lme4)


pbp <- load_pbp(seasons = 2023:2024)
participation <- load_participation(seasons = 2023:2024) %>%
  mutate(season = as.integer(str_sub(nflverse_game_id, 1, 4)))


Fullback_data_re <- pbp %>%
  left_join(
    participation %>% select(-any_of("season")),  # drop season from participation before joining
    by = c("old_game_id", "play_id")
  )

Fullback_data_re <- Fullback_data_re %>%
  mutate(
    offense_player_list   = str_split(offense_players, ";"),
    offense_position_list = str_split(offense_positions, ";"),
    fullback_id = map2_chr(
      offense_player_list,
      offense_position_list,
      ~ {
        if (length(.y) == 0) return(NA_character_)
        fb_index <- which(.y == "FB")
        if (length(fb_index) == 0) return(NA_character_)
        .x[fb_index]
      }
    )
  )


player_lookup <- participation %>%
  select(offense_players, offense_names) %>%
  mutate(
    id_list   = str_split(offense_players, ";"),
    name_list = str_split(offense_names, ";")
  ) %>%
  select(id_list, name_list) %>%
  unnest(c(id_list, name_list)) %>%
  distinct(id_list, .keep_all = TRUE) %>%
  rename(player_id = id_list, player_name = name_list)

Fullback_data_named <- Fullback_data_re %>%
  left_join(player_lookup, by = c("fullback_id" = "player_id"))

# primary fbs
primary_fbs <- Fullback_data_named %>%
  filter(!is.na(fullback_id)) %>%
  count(season, posteam, fullback_id) %>%
  group_by(season, posteam) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

# verify
primary_fbs %>% count(season)


fb_data_re <- Fullback_data_re %>%
  mutate(
    has_fullback = case_when(
      str_detect(offense_personnel, "1 FB") ~ 1,
      TRUE ~ 0
    ),
    has_tightend = case_when(
      str_detect(offense_personnel, "1 TE|2 TE|3 TE") ~ 1,
      TRUE ~ 0
    ),
    short_yardage_success = if_else(yards_gained >= 1, 1, 0),
    goal_line_success     = if_else(touchdown == 1, 1, 0),
    situation_type = case_when(
      yardline_100 <= 5                               ~ "goal_line",
      (down %in% c(3, 4) & ydstogo <= 2)             ~ "short_yardage",
      (down == 1 & ydstogo == 10) |
        (down == 2 & ydstogo >= 5 & ydstogo <= 10)   ~ "base_down",
      TRUE                                            ~ "other"
    )
  )

situation_data_re <- fb_data_re %>%
  select(-fullback_id) %>%  # drop it so primary_fbs can bring it in cleanly
  filter(situation_type %in% c("goal_line", "short_yardage", "base_down")) %>%
  left_join(primary_fbs %>% select(season, posteam, fullback_id),
            by = c("season", "posteam"),
            relationship = "many-to-many") %>%
  filter(!is.na(fullback_id))

# SHORT YARDAGE
re_model_short_all <- glmer(
  short_yardage_success ~ has_fullback + down + ydstogo + yardline_100 +
    defenders_in_box + scale(wpa) + scale(drive_first_downs) + 
    scale(drive_play_count) +
    (1 +has_fullback  | fullback_id),
  data   = filter(situation_data_re, situation_type == "short_yardage", !is.na(defenders_in_box)),
  family = binomial
)
print(summary(re_model_short_all))
ranef(re_model_short_all)$fullback_id

re_model_short_rush <- glmer(
  short_yardage_success ~ has_fullback + down + ydstogo + yardline_100 +
    defenders_in_box + scale(wpa) + scale(drive_first_downs) + 
    scale(drive_play_count) +
    (1 + has_fullback | fullback_id),
  data   = filter(situation_data_re, situation_type == "short_yardage", rush == 1, !is.na(defenders_in_box)),
  family = binomial
)
summary(re_model_short_rush)
ranef(re_model_short_rush)$fullback_id

re_model_short_pass <- glmer(
  short_yardage_success ~ has_fullback + down + ydstogo + yardline_100 +
    defenders_in_box + scale(wpa) + scale(drive_first_downs) + 
    scale(drive_play_count) +
    (1 + has_fullback | fullback_id),
  data   = filter(situation_data_re, situation_type == "short_yardage", pass == 1, !is.na(defenders_in_box)),
  family = binomial
)
summary(re_model_short_pass)
ranef(re_model_short_pass)$fullback_id

# GOAL LINE
re_model_goal_all <- glmer(
  goal_line_success ~ has_fullback + scale(wpa) + scale(epa) + yardline_100 + ydstogo +
     first_down_rush + scale(drive_first_downs) + scale(drive_play_count)  + success  + no_score_prob +
    (1 + has_fullback| fullback_id),
  data   = filter(situation_data_re, situation_type == "goal_line", !is.na(defenders_in_box)),
  family = binomial
)
summary(re_model_goal_all)
ranef(re_model_goal_all)$fullback_id

re_model_goal_rush <- glmer(
  goal_line_success ~ has_fullback + scale(wpa) + scale(epa) + yardline_100 + ydstogo +
    first_down_rush + scale(drive_first_downs) + scale(drive_play_count) +
     success +  no_score_prob +
    (1 + has_fullback | fullback_id),
  data   = filter(situation_data_re, situation_type == "goal_line", rush == 1, !is.na(defenders_in_box)),
  family = binomial
)
summary(re_model_goal_rush)
ranef(re_model_goal_rush)$fullback_id

re_model_goal_pass <- glmer(
  goal_line_success ~ has_fullback + scale(wpa) + scale(epa) + yardline_100 + ydstogo +
     first_down_rush + scale(drive_first_downs) + scale(drive_play_count) +
    success + no_score_prob +
    (1 + has_fullback | fullback_id),
  data   = filter(situation_data_re, situation_type == "goal_line", pass == 1, !is.na(defenders_in_box)),
  family = binomial
)
summary(re_model_goal_pass)
ranef(re_model_goal_pass)$fullback_id

# BASE DOWN
re_model_base_all <- lmer(
  epa ~ has_fullback + scale(wpa) + first_down + first_down_rush + 
    scale(drive_first_downs) + scale(drive_play_count) + ydstogo + 
    xpass + down + defenders_in_box + shotgun + rush_attempt + 
    ep + wp + cp + cpoe + pass_oe +
    defense_man_zone_type + defense_coverage_type +
    (1 + has_fullback | fullback_id),
  data = filter(situation_data_re, situation_type == "base_down", !is.na(defenders_in_box))
)
summary(re_model_base_all)
ranef(re_model_base_all)$fullback_id

re_model_base_rush <- lmer(
  epa ~ has_fullback + scale(wpa) + first_down + first_down_rush + 
    scale(drive_first_downs) + scale(drive_play_count) + ydstogo + 
    down + defenders_in_box + shotgun + rush_attempt + wp + run_location +
    (1 + has_fullback | fullback_id),
  data = filter(situation_data_re, situation_type == "base_down", rush == 1, !is.na(defenders_in_box))
)
summary(re_model_base_rush)
ranef(re_model_base_rush)$fullback_id

re_model_base_pass <- lmer(
  epa ~ has_fullback + scale(wpa) + first_down + scale(drive_first_downs) + 
    scale(drive_play_count) + ydstogo + xpass + down + defenders_in_box + 
    shotgun + ep + wp + cp + cpoe + pass_oe +
    (1 + has_fullback | fullback_id),
  data = filter(situation_data_re, situation_type == "base_down", pass == 1, !is.na(defenders_in_box))
)
summary(re_model_base_pass)
ranef(re_model_base_pass)$fullback_id


# build id to name lookup
fb_name_lookup <- Fullback_data_named %>%
  filter(!is.na(fullback_id), !is.na(player_name)) %>%
  distinct(fullback_id, player_name)

fb_name_lookup


get_fixed_gt <- function(model, model_name) {
  summary(model)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    gt() %>%
    tab_header(title = model_name) %>%
    tab_style(
      style = cell_fill(color = "#f2f2f2"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    cols_align(align = "left", columns = "Predictor") %>%
    cols_align(align = "center", columns = -"Predictor")
}

get_ranef_gt <- function(model, model_name) {
  ranef(model)$fullback_id %>%
    tibble::rownames_to_column("fullback_id") %>%
    left_join(fb_name_lookup, by = "fullback_id") %>%
    select(Player = player_name, everything(), -fullback_id) %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    gt() %>%
    tab_header(title = model_name) %>%
    tab_style(
      style = cell_fill(color = "#f2f2f2"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    cols_align(align = "left", columns = "Player") %>%
    cols_align(align = "center", columns = -"Player")
}

#  SHORT YARDAGE
get_fixed_gt(re_model_short_all,  "Short Yardage - All Plays")
get_fixed_gt(re_model_short_rush, "Short Yardage - Rush Plays")
get_fixed_gt(re_model_short_pass, "Short Yardage - Pass Plays")

get_ranef_gt(re_model_short_all,  "Short Yardage - All Plays")
get_ranef_gt(re_model_short_rush, "Short Yardage - Rush Plays")
get_ranef_gt(re_model_short_pass, "Short Yardage - Pass Plays")

#GOAL LINE 
get_fixed_gt(re_model_goal_all,  "Goal Line - All Plays")
get_fixed_gt(re_model_goal_rush, "Goal Line - Rush Plays")
get_fixed_gt(re_model_goal_pass, "Goal Line - Pass Plays")

get_ranef_gt(re_model_goal_all,  "Goal Line - All Plays")
get_ranef_gt(re_model_goal_rush, "Goal Line - Rush Plays")
get_ranef_gt(re_model_goal_pass, "Goal Line - Pass Plays")

#BASE DOWN 
get_fixed_gt(re_model_base_all,  "Base Down - All Plays")
get_fixed_gt(re_model_base_rush, "Base Down - Rush Plays")
get_fixed_gt(re_model_base_pass, "Base Down - Pass Plays")

get_ranef_gt(re_model_base_all,  "Base Down - All Plays")
get_ranef_gt(re_model_base_rush, "Base Down - Rush Plays")
get_ranef_gt(re_model_base_pass, "Base Down - Pass Plays")

###### Coef Model###############################################################
library(tidyverse)
library(ggplot2)

# extract fixed effects from all models
extract_fixed <- function(model, model_name) {
  summary(model)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>%
    mutate(model = model_name)
}

coef_data <- bind_rows(
  extract_fixed(re_model_short_all,  "Short Yardage - All"),
  extract_fixed(re_model_short_rush, "Short Yardage - Rush"),
  extract_fixed(re_model_short_pass, "Short Yardage - Pass"),
  extract_fixed(re_model_goal_all,   "Goal Line - All"),
  extract_fixed(re_model_goal_rush,  "Goal Line - Rush"),
  extract_fixed(re_model_goal_pass,  "Goal Line - Pass"),
  extract_fixed(re_model_base_all,   "Base Down - All"),
  extract_fixed(re_model_base_rush,  "Base Down - Rush"),
  extract_fixed(re_model_base_pass,  "Base Down - Pass")
) %>%
  filter(Predictor == "has_fullback") %>%
  rename(estimate = Estimate, se = `Std. Error`) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant"),
    model = factor(model, levels = c(
      "Short Yardage - All", "Short Yardage - Rush", "Short Yardage - Pass",
      "Goal Line - All", "Goal Line - Rush", "Goal Line - Pass",
      "Base Down - All", "Base Down - Rush", "Base Down - Pass"
    ))
  )

ggplot(coef_data, aes(x = estimate, y = model, color = significant)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Significant" = "darkblue", "Not Significant" = "grey")) +
  labs(
    title = "Effect of Fullback Presence Across Situations",
    x = "Coefficient Estimate",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


###### player model ###########################################################
# extract random intercepts with names
ranef_data <- ranef(re_model_goal_all)$fullback_id %>%
  tibble::rownames_to_column("fullback_id") %>%
  left_join(fb_name_lookup, by = "fullback_id") %>%
  rename(intercept = `(Intercept)`) %>%
  mutate(
    player_name = reorder(player_name, intercept),
    direction = ifelse(intercept > 0, "Above Average", "Below Average")
  )

ggplot(ranef_data, aes(x = intercept, y = player_name, color = direction)) +
  geom_point(size = 4) +
  geom_segment(aes(x = 0, xend = intercept, y = player_name, yend = player_name),
               linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Above Average" = "darkblue", "Below Average" = "grey")) +
  labs(
    title = "Fullback Random Effects - Goal Line (All Plays)",
    subtitle = "Deviation from average goal line success rate",
    x = "Random Intercept",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



####### success rate model #####################################################
# calculate success rates by situation and fullback presence
success_data <- situation_data %>%
  mutate(
    fullback_present = ifelse(has_fullback == 1, "Fullback Present", "No Fullback"),
    success = case_when(
      situation_type == "short_yardage" ~ short_yardage_success,
      situation_type == "goal_line"     ~ goal_line_success,
      situation_type == "base_down"     ~ as.numeric(epa > 0),
      TRUE ~ NA_real_
    ),
    situation_label = case_when(
      situation_type == "short_yardage" ~ "Short Yardage",
      situation_type == "goal_line"     ~ "Goal Line",
      situation_type == "base_down"     ~ "Base Down",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(success)) %>%
  group_by(situation_label, fullback_present) %>%
  summarise(
    success_rate = mean(success, na.rm = TRUE),
    n = n(),
    se = sqrt(success_rate * (1 - success_rate) / n),
    lower = success_rate - 1.96 * se,
    upper = success_rate + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    situation_label = factor(situation_label, 
                             levels = c("Short Yardage", "Goal Line", "Base Down"))
  )

ggplot(success_data, aes(x = situation_label, y = success_rate, fill = fullback_present)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(success_rate * 100, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Fullback Present" = "darkblue", "No Fullback" = "grey")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Success Rate With vs Without Fullback by Situation",
    x = NULL,
    y = "Success Rate",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )



###### glm and lm models #######################################################
# extract has_fullback from regular models
extract_coef_glm <- function(model, model_name, situation, play_type) {
  coefs <- summary(model)$coefficients
  if (!"has_fullback" %in% rownames(coefs)) return(NULL)
  data.frame(
    model     = model_name,
    situation = situation,
    play_type = play_type,
    estimate  = coefs["has_fullback", "Estimate"],
    se        = coefs["has_fullback", "Std. Error"]
  )
}

forest_data_base <- bind_rows(
  extract_coef_glm(model_short_all,  "Short Yardage - All",  "Short Yardage", "All Plays"),
  extract_coef_glm(model_short_rush, "Short Yardage - Rush", "Short Yardage", "Rush Plays"),
  extract_coef_glm(model_short_pass, "Short Yardage - Pass", "Short Yardage", "Pass Plays"),
  extract_coef_glm(model_goal_all,   "Goal Line - All",      "Goal Line",     "All Plays"),
  extract_coef_glm(model_goal_rush,  "Goal Line - Rush",     "Goal Line",     "Rush Plays"),
  extract_coef_glm(model_goal_pass,  "Goal Line - Pass",     "Goal Line",     "Pass Plays"),
  extract_coef_glm(model_base_all,   "Base Down - All",      "Base Down",     "All Plays"),
  extract_coef_glm(model_base_rush,  "Base Down - Rush",     "Base Down",     "Rush Plays"),
  extract_coef_glm(model_base_pass,  "Base Down - Pass",     "Base Down",     "Pass Plays")
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant"),
    model = factor(model, levels = rev(c(
      "Short Yardage - All",  "Short Yardage - Rush", "Short Yardage - Pass",
      "Goal Line - All",      "Goal Line - Rush",     "Goal Line - Pass",
      "Base Down - All",      "Base Down - Rush",     "Base Down - Pass"
    )))
  )

ggplot(forest_data_base, aes(x = estimate, y = model, color = significant)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_grid(situation ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("Significant" = "darkblue", "Not Significant" = "grey")) +
  labs(
    title    = "Effect of Fullback Presence Across Situations",
    subtitle = "Coefficient estimates all NFL teams",
    x        = "Coefficient Estimate ",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal() 




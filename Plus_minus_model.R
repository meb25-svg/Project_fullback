#install.packages("glmnet")                                                                       
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


#Regression based plus minus
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

######## Updated mdoel
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

#colnames(fb_data)

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



#fb_data %>%
#  count(offense_personnel, sort = TRUE) %>%
#  head(400)

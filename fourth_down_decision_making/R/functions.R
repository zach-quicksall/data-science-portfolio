clean_pbp <- function(df) {

  df %>%
    filter(
    down == 4,
    play_type %in% c("run", "pass", "field_goal", "punt"),
    !is.na(posteam),
    !is.na(yardline_100),
    !is.na(ydstogo),
    !is.na(game_seconds_remaining),
    !is.na(score_differential)
  ) %>%
  mutate(
    is_go = play_type %in% c("run", "pass"),
    is_conversion = case_when(
      is_go & !is.na(first_down) ~ first_down == 1,
      is_go & !is.na(yards_gained) ~ yards_gained >= ydstogo,
      TRUE ~ NA
    ),
    fg_attempt = play_type == "field_goal",
    fg_made = field_goal_result == "made",
    punt_attempt = play_type == "punt"
  )

}

fit_conversion_mod <- function(df) {

  conv_model <- glm(
    is_conversion ~ ydstogo + yardline_100 + score_differential + game_seconds_remaining,
    data = df,
    family = binomial()
  )

  return(conv_model)

}

fit_fieldgoal_mod <- function(df) {

  fg_model <- glm(
    fg_made ~ kick_distance,
    data = df,
    family = binomial()
  )

  return(fg_model)

}


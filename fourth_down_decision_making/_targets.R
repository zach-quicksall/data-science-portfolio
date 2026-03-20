source("R/packages.R")
source("R/functions.R")
set.seed(2026)

tar_plan(

  # Load raw play-by-play data
  pbp_raw = load_pbp(2005:2025),

  # Subset and clean for 4th-down prediction
  pbp_clean = clean_pbp(pbp_raw),

  # Compute a quick decision summary
  observed_decisions = pbp_clean %>%
    mutate(decision = case_when(
      is_go ~ "Go for it",
      fg_attempt ~ "Field goal",
      punt_attempt ~ "Punt",
      TRUE ~ "Other"
    )) %>%
    count(decision) %>%
    mutate(pct = n / sum(n)),

  # Subset to true "go-for-it" plays
  go_plays = pbp_clean %>%
    filter(is_go, !is.na(is_conversion)) %>%
    mutate(
      is_conversion = as.integer(is_conversion),
      late_game = game_seconds_remaining < 600
    ),

  # Fit a baseline model to predict successful conversion
  baseline_conv_mod = fit_conversion_mod(go_plays),

  # Update with probabilities
  go_plays_annot = go_plays %>%
    mutate(
      p_convert = predict(baseline_conv_mod, newdata = ., type = "response")
    ),

  # Subset to "field goal" plays
  fg_plays = pbp_clean %>%
    filter(
      play_type == "field_goal",
      !is.na(kick_distance),
      !is.na(field_goal_result)
    ) %>%
    mutate(
      fg_made = as.integer(field_goal_result == "made")
    ),

  # Fit a baseline model to predict field goal success
  baseline_fg_mod = fit_fieldgoal_mod(fg_plays),

  

)
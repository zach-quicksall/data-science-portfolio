clean_pbp <- function(pbp_raw) {
  
  pbp_raw %>%
    mutate(sequence_number = as.numeric(sequence_number)) %>%
    filter(!is.na(period)) %>%
    mutate(
      home_score = as.numeric(home_score),
      away_score = as.numeric(away_score),
      clock_minutes = suppressWarnings(as.numeric(clock_minutes)),
      clock_seconds = suppressWarnings(as.numeric(clock_seconds)),
      period_seconds_remaining = clock_minutes * 60 + clock_seconds
    ) %>%
    arrange(game_id, sequence_number) %>%
    mutate(
      made_shot = str_detect(type_text, "Shot|Three Pointer|Three Point") & !str_detect(type_text, "Miss"),
      free_throw = str_detect(type_text, "Free Throw|MadeFreeThrow"),
      defensive_rebound = type_text == "Defensive Rebound",
      turnover = str_detect(type_text, "Turnover|Offensive Charge"),
      offensive_rebound = type_text == "Offensive Rebound",
      possession_end = made_shot | defensive_rebound | turnover
    ) %>%
    group_by(game_id) %>%
    mutate(
      prev_home_score = lag(home_score),
      prev_away_score = lag(away_score),
      home_points = pmax(coalesce(home_score - prev_home_score, 0), 0),
      away_points = pmax(coalesce(away_score - prev_away_score, 0), 0),
      possession_id = cumsum(lag(possession_end, default = FALSE))
    ) %>%
    ungroup() %>%
    mutate(
      points = case_when(
        team_id == home_team_id ~ home_points,
        team_id == away_team_id ~ away_points,
        TRUE ~ 0
      )
    )
}

summarize_possessions <- function(pbp_possessions) {
  
  infer_offense_team <- function(team_id, points, turnover, offensive_rebound, made_shot, free_throw) {
    
    team_id_nonmissing <- team_id[!is.na(team_id)]
    
    # Team that scored on the possession
    scoring_team <- team_id[points > 0 & !is.na(team_id)]
    if (length(scoring_team) > 0) {
      return(dplyr::first(scoring_team))
    }
    
    # Team charged with turnover
    turnover_team <- team_id[turnover & !is.na(team_id)]
    if (length(turnover_team) > 0) {
      return(dplyr::first(turnover_team))
    }
    
    # Team credited with offensive rebound
    orb_team <- team_id[offensive_rebound & !is.na(team_id)]
    if (length(orb_team) > 0) {
      return(dplyr::first(orb_team))
    }
    
    # Team taking shots / free throws
    shot_team <- team_id[(made_shot | free_throw) & !is.na(team_id)]
    if (length(shot_team) > 0) {
      return(dplyr::first(shot_team))
    }
    
    # Fallback: first tagged team in possession
    if (length(team_id_nonmissing) > 0) {
      return(dplyr::first(team_id_nonmissing))
    }
    
    NA_real_

  }

  possession_summary <- pbp_possessions %>%
    arrange(game_id, game_play_number) %>%
    group_by(game_id, possession_id) %>%
    summarise(
      season = first(season),
      season_type = first(season_type),
      game_date = first(game_date),
      period = first(period),
      
      home_team_id = first(home_team_id),
      away_team_id = first(away_team_id),
      home_team_name = first(home_team_name),
      away_team_name = first(away_team_name),
      
      offense_team_id = infer_offense_team(
        team_id = team_id,
        points = points,
        turnover = turnover,
        offensive_rebound = offensive_rebound,
        made_shot = made_shot,
        free_throw = free_throw
      ),
      
      possession_points = sum(points, na.rm = TRUE),
      scoring_possession = possession_points > 0,
      turnover_on_possession = any(turnover, na.rm = TRUE),
      offensive_rebound_on_possession = any(offensive_rebound, na.rm = TRUE),
      defensive_rebound_on_possession = any(defensive_rebound, na.rm = TRUE),
      made_shot_on_possession = any(made_shot, na.rm = TRUE),
      free_throw_on_possession = any(free_throw, na.rm = TRUE),
      
      events_in_possession = n(),
      start_game_play_number = min(game_play_number, na.rm = TRUE),
      end_game_play_number = max(game_play_number, na.rm = TRUE),
      start_period_seconds_remaining = first(period_seconds_remaining),
      end_period_seconds_remaining = last(period_seconds_remaining),
      .groups = "drop"
    ) %>%
    mutate(
      offense_team_name = case_when(
        offense_team_id == home_team_id ~ home_team_name,
        offense_team_id == away_team_id ~ away_team_name,
        TRUE ~ NA_character_
      ),
      defense_team_id = case_when(
        offense_team_id == home_team_id ~ away_team_id,
        offense_team_id == away_team_id ~ home_team_id,
        TRUE ~ NA_real_
      ),
      defense_team_name = case_when(
        offense_team_id == home_team_id ~ away_team_name,
        offense_team_id == away_team_id ~ home_team_name,
        TRUE ~ NA_character_
      ),
      possession_length_seconds =
        start_period_seconds_remaining - end_period_seconds_remaining
    )
  
  # Fallback using previous possession's defense if offense still missing
  possession_summary %>%
    arrange(game_id, possession_id) %>%
    group_by(game_id) %>%
    mutate(
      offense_team_id = dplyr::coalesce(offense_team_id, lag(defense_team_id)),
      offense_team_name = case_when(
        offense_team_id == home_team_id ~ home_team_name,
        offense_team_id == away_team_id ~ away_team_name,
        TRUE ~ offense_team_name
      ),
      defense_team_id = case_when(
        offense_team_id == home_team_id ~ away_team_id,
        offense_team_id == away_team_id ~ home_team_id,
        TRUE ~ defense_team_id
      ),
      defense_team_name = case_when(
        offense_team_id == home_team_id ~ away_team_name,
        offense_team_id == away_team_id ~ home_team_name,
        TRUE ~ defense_team_name
      )
    ) %>%
    ungroup()
}

make_team_game_stats <- function(possessions) {
  
  offense_stats <- possessions %>%
    group_by(
      game_id,
      season,
      season_type,
      game_date,
      offense_team_id,
      offense_team_name
    ) %>%
    summarise(
      possessions = n(),
      points_scored = sum(possession_points, na.rm = TRUE),
      turnovers = sum(turnover_on_possession, na.rm = TRUE),
      offensive_rebounds = sum(offensive_rebound_on_possession, na.rm = TRUE),
      scoring_possessions = sum(scoring_possession, na.rm = TRUE),
      avg_possession_length = mean(possession_length_seconds, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      points_per_possession = points_scored / possessions,
      turnover_pct = turnovers / possessions,
      offensive_rebound_pct = offensive_rebounds / possessions,
      scoring_possession_pct = scoring_possessions / possessions
    ) %>%
    rename(
      team_id = offense_team_id,
      team_name = offense_team_name
    )
  
  defense_stats <- possessions %>%
    group_by(
      game_id,
      defense_team_id,
      defense_team_name
    ) %>%
    summarise(
      possessions_defended = n(),
      points_allowed = sum(possession_points, na.rm = TRUE),
      turnovers_forced = sum(turnover_on_possession, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      team_id = defense_team_id,
      team_name = defense_team_name
    )
  
  team_game_stats <- offense_stats %>%
    left_join(defense_stats, by = c("game_id", "team_id", "team_name")) %>%
    mutate(
      offensive_rating = 100 * points_scored / possessions,
      defensive_rating = 100 * points_allowed / possessions_defended,
      net_rating = offensive_rating - defensive_rating
    ) %>%
    arrange(game_date, game_id, desc(net_rating))
  
  team_game_stats
}

make_team_season_stats <- function(team_game_stats) {
  
  team_game_stats %>%
    group_by(season, season_type, team_id, team_name) %>%
    summarise(
      games = n(),
      
      total_possessions = sum(possessions, na.rm = TRUE),
      total_possessions_defended = sum(possessions_defended, na.rm = TRUE),
      total_points_scored = sum(points_scored, na.rm = TRUE),
      total_points_allowed = sum(points_allowed, na.rm = TRUE),
      total_turnovers = sum(turnovers, na.rm = TRUE),
      total_turnovers_forced = sum(turnovers_forced, na.rm = TRUE),
      total_offensive_rebounds = sum(offensive_rebounds, na.rm = TRUE),
      total_scoring_possessions = sum(scoring_possessions, na.rm = TRUE),
      
      avg_possession_length = sum(
        avg_possession_length * possessions,
        na.rm = TRUE
      ) / sum(possessions, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      offensive_rating = 100 * total_points_scored / total_possessions,
      defensive_rating = 100 * total_points_allowed / total_possessions_defended,
      net_rating = offensive_rating - defensive_rating,
      
      turnover_pct = total_turnovers / total_possessions,
      offensive_rebound_pct = total_offensive_rebounds / total_possessions,
      scoring_possession_pct = total_scoring_possessions / total_possessions,
      
      pace_proxy = total_possessions / games
    ) %>%
    arrange(season, season_type, desc(net_rating))
}

make_four_factors <- function(possessions) {
  
  possessions %>%
    group_by(
      season,
      season_type,
      game_id,
      offense_team_id,
      offense_team_name
    ) %>%
    summarise(
      possessions = n(),
      points = sum(possession_points, na.rm = TRUE),
      turnovers = sum(turnover_on_possession, na.rm = TRUE),
      offensive_rebounds = sum(offensive_rebound_on_possession, na.rm = TRUE),
      free_throw_possessions = sum(free_throw_on_possession, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      efg_proxy = points / possessions,
      tov_pct = turnovers / possessions,
      orb_rate = offensive_rebounds / possessions,
      ft_rate = free_throw_possessions / possessions
    ) %>%
    rename(
      team_id = offense_team_id,
      team_name = offense_team_name
    )
}
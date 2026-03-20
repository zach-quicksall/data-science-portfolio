clean_pbp <- function(pbp_raw) {
  
  pbp_clean <- pbp_raw %>%
    filter(!is.na(period)) %>%

    # Create columns annotating certain actions
    mutate(
        made_shot = str_detect(type_text, "Shot|Three Pointer|Three Point") & !str_detect(type_text, "Miss"),
        free_throw = str_detect(type_text, "Free Throw|MadeFreeThrow"),
        defensive_rebound = type_text == "Defensive Rebound",
        turnover = str_detect(type_text, "Turnover|Offensive Charge"),
        offensive_rebound = type_text == "Offensive Rebound"
    ) %>%
    
    # Determine endpoint of current possession
    mutate(
        possession_end = made_shot | defensive_rebound | turnover
    ) %>%
    arrange(game_id, period, clock_display_value) %>%
    group_by(game_id) %>%

    # Assign unique possession IDs within each game
    mutate(
        possession_id = cumsum(lag(possession_end, default = FALSE))
    ) %>%
    ungroup() %>%

    # Create rolling home/away point counts
    arrange(game_id, period, desc(clock_display_value)) %>% 
    group_by(game_id) %>%
    mutate(
        home_points = home_score - lag(home_score, default = first(home_score)),
        away_points = away_score - lag(away_score, default = first(away_score))
    ) %>%
    ungroup() %>%
    
    # Assign points to each team
    mutate(
        points = case_when(
            team_id == home_team_id ~ home_points,
            team_id == away_team_id ~ away_points,
            TRUE ~ 0
        )
    )
  
}
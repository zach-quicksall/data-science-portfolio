source("R/packages.R")
source("R/functions.R")
set.seed(2026)

con <- DBI::dbConnect(RSQLite::SQLite(), "data/hoopr_mbb.sqlite")

tar_plan(

  # Collect available data
  pbp_raw = tbl(con, "mbb_pbp") %>% collect(),

  # Restrict to valid periods and clean PBP data
  pbp_clean = clean_pbp(pbp_raw), #%>% filter(game_id == 283192166)

  # Annotate possession changes and add incremental IDs
  pbp_possessions = pbp_clean %>%
    group_by(game_id) %>%
    mutate(
      possession_change = lag(defensive_rebound | turnover | made_shot, default = FALSE)
    ) %>%
    mutate(possession_id = cumsum(possession_change)),

  # Summarize possessions
  possession_summary = summarize_possessions(pbp_possessions),

  # Summarize team metrics (per game)
  team_summary = make_team_game_stats(possession_summary),

  # Summarize season metrics (per team & season)
  season_summary = make_team_season_stats(team_summary),

  # Compute KenPom four-factors (per game?)
  kp_four_factors = make_four_factors(possession_summary),

)


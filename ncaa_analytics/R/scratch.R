source("R/packages.R")
source("R/functions.R")
set.seed(2026)

tar_load(pbp_clean)
tar_load(team_summary)
tar_load(season_summary)
tar_load(kp_four_factors)

tmp <- season_summary %>%
  filter(offensive_rating == 0)

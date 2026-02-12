source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_load(prices_mat)
tar_load(returns_mat)
tar_load(universe)

# Define trading years to simulate
years <- 2020:2025

# Simulate trades
all_trades <- purrr::map_dfr(
  years,
  ~ run_single_year_sim(
      prices_mat,
      returns_mat,
      universe,
      trade_year = .x,
      formation_years = 5,
      top_n = 30
    )
)

# Aggregate results
portfolio_daily <- aggregate_portfolio_daily(all_trades, capital_mode = "active_only")

portfolio_daily_net <- aggregate_portfolio_daily_net(all_trades, capital_mode = "active_only")

pair_summary <- summarise_pair_performance(all_trades)

portfolio_metrics <- portfolio_metrics(portfolio_daily_net)

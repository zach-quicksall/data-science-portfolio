source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_plan(

  # Define initial universe as the "iShares Russell 2000 ETF" (IWM)
  tar_file(holdings_file, "data/raw/IWM_holdings_mod.csv"),
  holdings_raw = read_csv(holdings_file),

  # Clean raw holdings file and select top N = 500 equities
  # based on market capitalization as proxy to select
  # equities with reasonable liquidity for trading.
  holdings_clean = clean_holdings(holdings_raw, n_keep = 500),

  # Pull OHLCV from Yahoo finance
  start_date = "2010-01-01",
  tickers = holdings_clean %>% pull(ticker),
  series_raw = map_dfr(tickers, ~{
    Sys.sleep(0.1)
    pull_yahoo_ohlcv(.x, from = start_date)
  }),

  # Compute liquidity metrics for filtering
  liquidity_summary = compute_liquidity_summary(series_raw) %>%
    left_join(select(holdings_clean, ticker, sector), by = "ticker"),

  # Define starting universe of tickers
  universe = define_starting_universe(liquidity_summary),

  # Subset series to universe
  series = series_raw %>%
    filter(ticker %in% universe$ticker),

  # Wide adjusted close (Date x Ticker)
  prices_wide = series %>%
    select(date, ticker, adj_close) %>%
    tidyr::pivot_wider(names_from = ticker, values_from = adj_close) %>%
    arrange(date),

  # Convert price dataframe to a matrix
  prices_mat = prices_wide %>%
    tibble::column_to_rownames("date") %>%
    as.matrix(),

  # Compute returns
  returns_mat = apply(prices_mat, 2, function(x) c(NA_real_, diff(x))),

  # Subset matrix on return correlations and sectors
  candidate_pairs = generate_candidate_pairs(returns_mat, universe),

  # Compute pair metrics
  #pair_metrics = compute_pair_metrics(candidate_pairs, prices_mat[5:1265, ]),

  # Rank pairs based on metrics
  #pair_ranks = rank_pairs(pair_metrics),

  # Pick top N pairs for trading
  #top_pairs = select_top_n_pairs(pair_ranks, diversify_by = "sector"),

  # Get price series
  #top_pair_prices = join_pair_prices(prices_mat, top_pairs),

  # Compute spreads
  #top_pair_spreads = compute_spread(top_pair_prices, top_pairs),
  
  # Compute z-scores
  #top_pair_zscores = compute_zscore_roll(top_pair_spreads),

  # Generate buy/sell signals from z-scores
  #top_pair_signals = generate_signals(top_pair_zscores),

  # Simulate trades
  #top_pair_trades = simulate_pair_pnl(top_pair_signals),

  # Summarise trades
  #top_pair_daily_summary = aggregate_portfolio_daily(top_pair_trades),

  #top_pair_performance = summarise_pair_performance(top_pair_trades),
  
)
source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_plan(

  # Define initial universe as the "iShares Russell 2000 ETF" (IWM)
  tar_file(holdings_file, "data/raw/IWM_holdings_mod.csv"),
  holdings_raw = read_csv(holdings_file),

  # Clean raw holdings file and select top N equities
  # based on market capitalization as proxy to select
  # equities with reasonable liquidity for trading.
  holdings_clean = clean_holdings(holdings_raw),

  # Pull OHLCV from yahoo finance
  start_date = "2016-01-01",
  tickers = holdings_clean %>% pull(ticker),
  series_raw = map_dfr(tickers, ~{
    Sys.sleep(0.1)
    pull_yahoo_ohlcv(.x, from = start_date)
  }),

  # Compute liquidity metrics for filtering
  liquidity_summary = compute_liquidity_summary(series_raw) %>%
    left_join(select(holdings_clean, ticker, sector), by = "ticker"),

  # Define final universe of tickers
  universe = define_universe(liquidity_summary),
  
  # Subset series to final universe of tickers
  series_filt = series_raw %>%
    inner_join(select(universe, ticker, sector), by = "ticker"),

  # Create matrices for fast filtering and computation
  adj_close_mat = make_price_mat(series_filt),
  log_adj_close_mat = log(adj_close_mat),
  log_return_mat = (diff(log_adj_close_mat))[-1, ],

  # Define train/evaluation sets
  matrix_dates = rownames(log_adj_close_mat),
  split_date = matrix_dates[ceiling(length(matrix_dates) / 2)],
  train_mask = matrix_dates < split_date,
  eval_mask = matrix_dates >= split_date,

)
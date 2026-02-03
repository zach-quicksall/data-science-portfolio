clean_holdings <- function(df, n_keep = 300) {

  df <- df %>%
    janitor::clean_names() %>%
    filter(asset_class == "Equity") %>%
    slice_head(n = n_keep)

  return(df)

}

pull_yahoo_ohlcv <- function(ticker, from = "2016-01-01", to = Sys.Date()) {

  tryCatch({
    x <- getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE,
      warnings = FALSE
    )

    df <- tibble(
      date   = as.Date(index(x)),
      ticker = ticker,
      open = as.numeric(Op(x)),
      high = as.numeric(Hi(x)),
      low = as.numeric(Lo(x)),
      close = as.numeric(Cl(x)),
      adj_close  = as.numeric(Ad(x)),
      volume = as.numeric(Vo(x))
    ) %>%
      filter(!is.na(adj_close), !is.na(volume))

    df

  }, error = function(e) {
    message("Failed: ", ticker, " | ", conditionMessage(e))
    tibble(date = as.Date(character()),
           ticker = character(),
           open = numeric(),
           high = numeric(),
           low = numeric(),
           close = numeric(),
           adj_close = numeric(),
           volume = numeric())
  })

}

compute_liquidity_summary <- function(df) {

  summary_df <- df %>%
    mutate(dollar_volume = adj_close * volume) %>%
    group_by(ticker) %>%
    summarise(
      median_dollar_volume = median(dollar_volume, na.rm = TRUE),
      mean_dollar_volume   = mean(dollar_volume, na.rm = TRUE),
      pct_days_traded      = mean(volume > 0, na.rm = TRUE),
      median_price         = median(adj_close, na.rm = TRUE),
      obs                 = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(median_dollar_volume))

  return(summary_df)

}

define_universe <- function(df) {

  # Apply filters
  # Minimum 5-years of data
  # Median price > $5
  # Median dollar trading volume > 5 million
  # Traded every single day available
  liquidity_sub <- df %>%
    filter(
      obs >= 1250,
      median_price >= 5,
      median_dollar_volume >= 5e6,
      pct_days_traded == 1
    )
  
  # Require a minimum of 10 stocks in a given sector
  sector_sub <- liquidity_sub %>%
    group_by(sector) %>%
    count(sort = TRUE) %>%
    filter(n >= 10)

  # Subset to final universe
  final_universe <- liquidity_sub %>%
    inner_join(select(sector_sub, -n), by = "sector")

  # Return universe
  return(final_universe)

}

make_price_mat <- function(df) {

  df <- df %>%
      select(date, ticker, adj_close) %>%
      pivot_wider(
        id_cols = date,
        names_from = "ticker",
        values_from = "adj_close"
      )

  price_mat <- as.matrix(df %>% select(-date))

  rownames(price_mat) <- df$date

  return(price_mat)

}

analyze_pair <- function(px, py, dates, train_idx) {
  # px, py are log price vectors aligned by date

  # 1. Hedge ratio (train only)
  fit <- lm(px[train_idx] ~ py[train_idx])
  beta <- coef(fit)[2]

  # 2. Spread (full history)
  spread <- px - beta * py

  # 3. Cointegration (train only)
  coint_p <- tryCatch(
    tseries::adf.test(residuals(fit))$p.value,
    error = function(e) NA_real_
  )

  # 4. Half-life
  hl <- estimate_half_life(spread[train_idx])

  tibble(
    beta_hat = beta,
    coint_pvalue = coint_p,
    half_life = hl,
    spread_sd = sd(spread[train_idx], na.rm = TRUE)
  )
}

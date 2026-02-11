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
      ) %>%
    arrange(date)

  price_mat <- as.matrix(df %>% select(-date))

  rownames(price_mat) <- as.Date(df$date)

  return(price_mat)

}

define_pair_set <- function(returns, train_mask, universe, min_cor = 0.6) {

  # Create industry map from universe
  sector_map <- universe %>%
    select(ticker, sector)

  # Subset returns using training mask
  returns_train <- returns[train_mask, , drop = FALSE]

  # Correlate returns
  cor_df <- returns_train %>%
    corrr::correlate(method = "pearson",
                     use = "pairwise.complete.obs") %>%
    corrr::shave(upper = FALSE) %>%
    corrr::stretch()

  # Add industry assignments
 cor_df <- cor_df %>%
   left_join(sector_map, by = c("x" = "ticker")) %>%
   rename(sector_x = sector) %>%
   left_join(sector_map, by = c("y" = "ticker")) %>%
   rename(sector_y = sector)

  # Filter to candidate pairs
  cor_df <- cor_df %>%
    filter(sector_x == sector_y,
           r >= min_cor) %>%
    drop_na()

  # Keep top N pairs
  pairs <- cor_df %>%
    group_by(sector_x) %>%
    slice_max(r, n = 30, with_ties = FALSE) %>%
    ungroup()

  pairs <- cor_df

  return(pairs)

}

estimate_half_life <- function(spread, min_obs = 50) {
  s <- as.numeric(spread)
  s <- s[is.finite(s)]
  if (length(s) < min_obs) return(NA_real_)

  s_lag <- s[-length(s)]
  s_now <- s[-1]

  fit <- lm(s_now ~ s_lag)
  rho <- unname(coef(fit)[2])

  # If rho is not in (0, 1), half-life isn't meaningful for mean-reversion
  if (!is.finite(rho) || rho <= 0 || rho >= 1) return(Inf)

  -log(2) / log(rho)
}

analyze_pair <- function(px, py, train_mask,
                         test_mask = NULL,
                         adf_k = 1,
                         min_train = 252) {

  stopifnot(length(px) == length(py))
  train_mask <- as.logical(train_mask)
  if (!is.null(test_mask)) test_mask <- as.logical(test_mask)

  # Keep only finite values (aligned)
  ok <- is.finite(px) & is.finite(py)
  px <- px[ok]; py <- py[ok]
  train_mask <- train_mask[ok]
  if (!is.null(test_mask)) test_mask <- test_mask[ok]

  n_train <- sum(train_mask)
  if (n_train < min_train) {
    return(tibble::tibble(
      n_train = n_train,
      alpha_hat = NA_real_,
      beta_hat = NA_real_,
      beta_se = NA_real_,
      beta_t = NA_real_,
      r2_train = NA_real_,
      resid_sd_train = NA_real_,
      coint_pvalue = NA_real_,
      adf_stat = NA_real_,
      half_life = NA_real_,
      rho = NA_real_,
      spread_sd = NA_real_,
      spread_iqr = NA_real_,
      return_corr = NA_real_,
      zero_cross = NA_real_,
      pct_outside_2sd = NA_real_,
      coint_pvalue_test = NA_real_,
      half_life_test = NA_real_
    ))
  }

  # Hedge ratio: px ~ alpha + beta * py
  fit <- lm(px[train_mask] ~ py[train_mask])
  sm <- summary(fit)

  alpha <- unname(coef(fit)[1])
  beta  <- unname(coef(fit)[2])

  beta_se <- unname(sm$coefficients["py[train_mask]", "Std. Error"])
  beta_t  <- unname(sm$coefficients["py[train_mask]", "t value"])
  r2_train <- unname(sm$r.squared)
  resid_sd_train <- unname(sd(residuals(fit), na.rm = TRUE))

  # Spread from regression
  spread <- px - (alpha + beta * py)

  s_train <- spread[train_mask]

  # Cointegration: ADF on spread
  adf_out <- tryCatch(
    tseries::adf.test(s_train, k = adf_k),
    error = function(e) NULL
  )
  coint_p <- if (is.null(adf_out)) NA_real_ else unname(adf_out$p.value)
  adf_stat <- if (is.null(adf_out)) NA_real_ else unname(adf_out$statistic)

  # Half-life and rho on train spread (AR1)
  s_lag <- s_train[-length(s_train)]
  s_now <- s_train[-1]
  rho_fit <- lm(s_now ~ s_lag)
  rho <- unname(coef(rho_fit)[2])

  half_life <- if (!is.finite(rho) || rho <= 0 || rho >= 1) Inf else -log(2) / log(rho)

  # Spread stats
  spread_sd <- sd(s_train, na.rm = TRUE)
  spread_iqr <- IQR(s_train, na.rm = TRUE)

  # Return correlation
  rx <- diff(px[train_mask])
  ry <- diff(py[train_mask])
  return_corr <- suppressWarnings(cor(rx, ry, use = "pairwise.complete.obs"))

  # Zero-crossing rate
  z <- s_train - mean(s_train, na.rm = TRUE)
  sgn <- sign(z)
  sgn[sgn == 0] <- NA
  for (i in seq_along(sgn)) if (is.na(sgn[i]) && i > 1) sgn[i] <- sgn[i - 1]
  sgn <- sgn[is.finite(sgn)]
  zero_cross <- if (length(sgn) >= 5) sum(diff(sgn) != 0) / (length(sgn) - 1) else NA_real_

  # Frequency of “tradable excursions”: |z| > 1
  zscore <- (s_train - mean(s_train, na.rm = TRUE)) / sd(s_train, na.rm = TRUE)
  pct_outside_2sd <- mean(abs(zscore) > 2, na.rm = TRUE)

  # Optional out-of-sample checks (using test mask if provided)
  coint_p_test <- NA_real_
  hl_test <- NA_real_
  if (!is.null(test_mask) && sum(test_mask) >= min_train) {
    s_test <- spread[test_mask]

    adf_test <- tryCatch(
      tseries::adf.test(s_test, k = adf_k),
      error = function(e) NULL
    )
    coint_p_test <- if (is.null(adf_test)) NA_real_ else unname(adf_test$p.value)

    s_lag2 <- s_test[-length(s_test)]
    s_now2 <- s_test[-1]
    rho2 <- unname(coef(lm(s_now2 ~ s_lag2))[2])
    hl_test <- if (!is.finite(rho2) || rho2 <= 0 || rho2 >= 1) Inf else -log(2) / log(rho2)
  }

  tibble::tibble(
    n_train = n_train,
    alpha_hat = alpha,
    beta_hat = beta,
    beta_se = beta_se,
    beta_t = beta_t,
    r2_train = r2_train,
    resid_sd_train = resid_sd_train,
    coint_pvalue = coint_p,
    adf_stat = adf_stat,
    half_life = half_life,
    rho = rho,
    spread_sd = spread_sd,
    spread_iqr = spread_iqr,
    return_corr = return_corr,
    zero_cross = zero_cross,
    pct_outside_2sd = pct_outside_2sd,
    coint_pvalue_test = coint_p_test,
    half_life_test = hl_test
  )
}

compute_pair_metrics <- function(candidate_pairs,
                                 log_prices,
                                 train_mask,
                                 test_mask = NULL,
                                 adf_k = 1,
                                 min_train = 252) {

  stopifnot(all(c("x", "y") %in% names(candidate_pairs)))
  stopifnot(length(train_mask) == nrow(log_prices))
  if (!is.null(test_mask)) stopifnot(length(test_mask) == nrow(log_prices))

  # ensure logical masks
  train_mask <- as.logical(train_mask)
  if (!is.null(test_mask)) test_mask <- as.logical(test_mask)

  # keep only candidates with tickers present in the price matrix
  keep <- candidate_pairs$x %in% colnames(log_prices) & candidate_pairs$y %in% colnames(log_prices)
  cand <- candidate_pairs[keep, , drop = FALSE]

  # compute metrics row-by-row
  out_list <- lapply(seq_len(nrow(cand)), function(i) {
    x <- cand$x[i]
    y <- cand$y[i]

    metrics <- analyze_pair(
      px = log_prices[, x],
      py = log_prices[, y],
      train_mask = train_mask,
      test_mask  = test_mask,
      adf_k = adf_k,
      min_train = min_train
    )

    # attach identifiers + any metadata columns from candidate table
    cbind(cand[i, , drop = FALSE], metrics)
  })

  dplyr::bind_rows(out_list)
}

rank_pairs <- function(df,
                       max_coint_p = 0.10,
                       half_life_range = c(2, 120),
                       min_zero_cross = 0.02,
                       min_r2 = 0.20,
                       min_train = 252,
                       half_life_target = 20,
                       weights = list(
                         coint = 0.40,
                         half_life = 0.25,
                         zero_cross = 0.15,
                         excursion = 0.10,
                         r2 = 0.10
                       ),
                       eps = 1e-12) {

  stopifnot(is.data.frame(df))

  # Hard filters
  d <- df %>%
    filter(
      n_train >= min_train,
      is.finite(coint_pvalue),
      is.finite(half_life),
      coint_pvalue <= max_coint_p,
      half_life >= half_life_range[1],
      half_life <= half_life_range[2],
      is.finite(zero_cross),
      zero_cross >= min_zero_cross,
      is.finite(r2_train),
      r2_train >= min_r2
    )

  if (nrow(d) == 0) {
    d$score <- numeric(0)
    return(d)
  }

  # Compute components of final score
  mm <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) return(rep(NA_real_, length(x)))
    (x - r[1]) / (r[2] - r[1])
  }

  # Cointegration strength: -log10(p)
  coint_strength <- -log10(pmax(d$coint_pvalue, eps))

  # Half-life: reward closeness to target (symmetric penalty)
  hl_score <- -abs(log(d$half_life / half_life_target))

  # Tradability: crossings + excursions
  zc_score <- d$zero_cross
  ex_score <- d$pct_outside_2sd

  # Fit quality
  r2_score <- d$r2_train

  # Scale each component 0-1 within the candidate set
  s_coint <- mm(coint_strength)
  s_hl    <- mm(hl_score)
  s_zc    <- mm(zc_score)
  s_ex    <- mm(ex_score)
  s_r2    <- mm(r2_score)

  # Weighted sum (renormalize if any NA components)
  comp <- cbind(s_coint, s_hl, s_zc, s_ex, s_r2)
  w <- c(weights$coint, weights$half_life, weights$zero_cross, weights$excursion, weights$r2)

  score <- apply(comp, 1, function(row) {
    ok <- is.finite(row)
    if (!any(ok)) return(NA_real_)
    sum(row[ok] * w[ok]) / sum(w[ok])
  })

  d$score <- score

  d %>%
    arrange(desc(score))
}

limit_pair_overlap <- function(ranked_df, max_per_ticker = 3, top_n = 50) {
  out <- ranked_df[0, , drop = FALSE]
  counts <- new.env(parent = emptyenv())

  add_count <- function(t) {
    counts[[t]] <- if (is.null(counts[[t]])) 1L else counts[[t]] + 1L
  }
  get_count <- function(t) {
    if (is.null(counts[[t]])) 0L else counts[[t]]
  }

  for (i in seq_len(nrow(ranked_df))) {
    x <- ranked_df$x[i]; y <- ranked_df$y[i]
    if (get_count(x) < max_per_ticker && get_count(y) < max_per_ticker) {
      out <- rbind(out, ranked_df[i, , drop = FALSE])
      add_count(x); add_count(y)
    }
    if (nrow(out) >= top_n) break
  }

  out

}


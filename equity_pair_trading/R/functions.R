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

define_starting_universe <- function(df) {

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

generate_candidate_pairs <- function(returns_mat,
                                     universe,
                                     min_cor = 0.6,
                                     same_sector_only = TRUE,
                                     top_per_sector = NULL) {

  # Create sector map
  sector_map <- universe %>% select(ticker, sector)

  # Correlate returns
  cor_df <- as.data.frame(returns_mat) %>%
    corrr::correlate(method = "pearson", use = "pairwise.complete.obs") %>%
    corrr::shave(upper = FALSE) %>%
    corrr::stretch() %>%
    filter(!is.na(r))

  # Add sector assignments
  cor_df <- cor_df %>%
    left_join(sector_map, by = c("x" = "ticker")) %>%
    rename(sector_x = sector) %>%
    left_join(sector_map, by = c("y" = "ticker")) %>%
    rename(sector_y = sector)

  # Filter on correlation
  cor_df <- cor_df %>%
    filter(r >= min_cor)

  # Require same-sector pairs if flagged
  if (isTRUE(same_sector_only)) {
    cor_df <- cor_df %>% filter(sector_x == sector_y)
  }

  # Drop any pairs with missing information
  cor_df <- cor_df %>% drop_na()

  # Optional: keep top K per sector by correlation (helps runtime)
  if (!is.null(top_per_sector)) {
    cor_df <- cor_df %>%
      group_by(sector_x) %>%
      slice_max(r, n = top_per_sector, with_ties = FALSE) %>%
      ungroup()
  }

  return(cor_df)

}

estimate_half_life <- function(spread, min_obs = 50) {
  s <- as.numeric(spread)
  s <- s[is.finite(s)]
  if (length(s) < min_obs) return(NA_real_)

  s_lag <- s[-length(s)]
  s_now <- s[-1]

  fit <- lm(s_now ~ s_lag)
  rho <- unname(coef(fit)[2])

  if (!is.finite(rho) || rho <= 0 || rho >= 1) return(Inf)
  -log(2) / log(rho)
}

analyze_pair_formation <- function(px, py,
                                  adf_k = 1,
                                  min_train = 252) {

  stopifnot(length(px) == length(py))

  # Align finite values
  ok <- is.finite(px) & is.finite(py)
  px <- px[ok]; py <- py[ok]

  n_train <- length(px)
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
      pct_outside_2sd = NA_real_
    ))
  }

  # Hedge ratio: px ~ alpha + beta * py
  fit <- lm(px ~ py)
  sm <- summary(fit)
  coefs <- coef(sm)

  alpha <- unname(coef(fit)[1])
  beta  <- unname(coef(fit)[2])

  beta_se <- unname(coefs["py", "Std. Error"])
  beta_t  <- unname(coefs["py", "t value"])

  r2_train <- unname(sm$r.squared)
  resid_sd_train <- unname(sd(residuals(fit), na.rm = TRUE))

  # Spread from regression (formation)
  spread <- px - (alpha + beta * py)

  # Cointegration proxy: ADF on spread
  adf_out <- tryCatch(
    tseries::adf.test(spread, k = adf_k),
    error = function(e) NULL
  )
  coint_p <- if (is.null(adf_out)) NA_real_ else unname(adf_out$p.value)
  adf_stat <- if (is.null(adf_out)) NA_real_ else unname(adf_out$statistic)

  # Half-life + rho from AR(1) on spread
  if (length(spread) >= 3) {
    s_lag <- spread[-length(spread)]
    s_now <- spread[-1]
    rho_fit <- lm(s_now ~ s_lag)
    rho <- unname(coef(rho_fit)[2])
    half_life <- if (!is.finite(rho) || rho <= 0 || rho >= 1) Inf else -log(2) / log(rho)
  } else {
    rho <- NA_real_
    half_life <- NA_real_
  }

  # Spread stats
  spread_sd <- sd(spread, na.rm = TRUE)
  spread_iqr <- IQR(spread, na.rm = TRUE)

  # Return correlation (log-return correlation)
  rx <- diff(px)
  ry <- diff(py)
  return_corr <- suppressWarnings(cor(rx, ry, use = "pairwise.complete.obs"))

  # Zero-crossing rate (demeaned spread sign flips)
  z <- spread - mean(spread, na.rm = TRUE)
  sgn <- sign(z)
  sgn[sgn == 0] <- NA
  for (i in seq_along(sgn)) if (is.na(sgn[i]) && i > 1) sgn[i] <- sgn[i - 1]
  sgn <- sgn[is.finite(sgn)]
  zero_cross <- if (length(sgn) >= 5) sum(diff(sgn) != 0) / (length(sgn) - 1) else NA_real_

  # Frequency of excursions: |z| > 2
  zscore <- (spread - mean(spread, na.rm = TRUE)) / sd(spread, na.rm = TRUE)
  pct_outside_2sd <- mean(abs(zscore) > 2, na.rm = TRUE)

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
    pct_outside_2sd = pct_outside_2sd
  )
}

compute_pair_metrics <- function(candidate_pairs,
                                 log_prices_form,
                                 adf_k = 1,
                                 min_train = 252) {

  stopifnot(all(c("x", "y") %in% names(candidate_pairs)))
  stopifnot(is.matrix(log_prices_form) || is.data.frame(log_prices_form))

  log_prices_form <- as.matrix(log_prices_form)

  # keep only candidates with tickers present in the matrix
  keep <- candidate_pairs$x %in% colnames(log_prices_form) &
          candidate_pairs$y %in% colnames(log_prices_form)
  cand <- candidate_pairs[keep, , drop = FALSE]

  if (nrow(cand) == 0) return(dplyr::as_tibble(cand))

  out <- purrr::pmap_dfr(cand, function(...) {
    row <- list(...)
    x <- row[["x"]]
    y <- row[["y"]]

    metrics <- analyze_pair_formation(
      px = log_prices_form[, x],
      py = log_prices_form[, y],
      adf_k = adf_k,
      min_train = min_train
    )

    dplyr::bind_cols(tibble::as_tibble(row), metrics)
  })

  out
}

rank_pairs <- function(df,
                       # Hard filters
                       max_coint_p = 0.10,
                       half_life_range = c(2, 120),
                       min_zero_cross = 0.02,
                       min_r2 = 0.20,
                       min_train = 252,

                       # Scoring preferences
                       half_life_target = 20,

                       # Weights (sum doesn't need to be 1)
                       weights = list(
                         coint = 0.40,
                         half_life = 0.25,
                         zero_cross = 0.15,
                         excursion = 0.10,
                         r2 = 0.10
                       ),

                       # Robustness knobs
                       eps = 1e-12,
                       winsor_p = 0.01, # NULL to disable
                       return_components = FALSE) {

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

  # Helper functions
  winsor <- function(x, p = 0.01) {
    if (is.null(p) || !is.finite(p) || p <= 0) return(x)
    lo <- stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7)
    hi <- stats::quantile(x, probs = 1 - p, na.rm = TRUE, names = FALSE, type = 7)
    pmin(pmax(x, lo), hi)
  }

  mm01 <- function(x) {
    # robust min-max to [0,1]; returns 0.5 if constant
    r <- range(x, na.rm = TRUE)
    if (!is.finite(r[1]) || !is.finite(r[2])) return(rep(NA_real_, length(x)))
    if (r[1] == r[2]) return(rep(0.5, length(x)))
    (x - r[1]) / (r[2] - r[1])
  }

  # Construct strength measures
  
  ## Cointegration strength: -log10(p)
  coint_strength <- -log10(pmax(d$coint_pvalue, eps))
  
  ## Half-life: closeness to target (peak at target)
  hl_score <- -abs(log(d$half_life / half_life_target))
  
  ## Tradability: crossings + excursions
  zc_score <- d$zero_cross
  ex_score <- d$pct_outside_2sd
  
  ## Fit quality
  r2_score <- d$r2_train

  # Optional winsorization to reduce outlier dominance
  coint_strength <- winsor(coint_strength, winsor_p)
  hl_score       <- winsor(hl_score, winsor_p)
  zc_score       <- winsor(zc_score, winsor_p)
  ex_score       <- winsor(ex_score, winsor_p)
  r2_score       <- winsor(r2_score, winsor_p)

  # Scale 0-1
  s_coint <- mm01(coint_strength)
  s_hl    <- mm01(hl_score)
  s_zc    <- mm01(zc_score)
  s_ex    <- mm01(ex_score)
  s_r2    <- mm01(r2_score)

  comp <- cbind(s_coint, s_hl, s_zc, s_ex, s_r2)
  w <- c(weights$coint, weights$half_life, weights$zero_cross, weights$excursion, weights$r2)

  score <- apply(comp, 1, function(row) {
    ok <- is.finite(row)
    if (!any(ok)) return(NA_real_)
    sum(row[ok] * w[ok]) / sum(w[ok])
  })

  d$score <- score

  if (isTRUE(return_components)) {
    d$s_coint <- s_coint
    d$s_hl <- s_hl
    d$s_zc <- s_zc
    d$s_ex <- s_ex
    d$s_r2 <- s_r2
  }

  d %>% arrange(desc(score))
}

select_top_n_pairs <- function(ranked_pairs,
                               N = 30,
                               hedge_method = "ols",
                               pair_id_sep = "-",
                               trade_year = NA_integer_,
                               formation_start = NA,
                               formation_end   = NA,
                               trade_start     = NA,
                               trade_end       = NA) {

  ranked_pairs %>%
    arrange(desc(score)) %>%
    slice_head(n = N) %>%
    mutate(
      x_ticker = str_to_upper(str_trim(x)),
      y_ticker = str_to_upper(str_trim(y)),
      pair_id = paste0(x_ticker, pair_id_sep, y_ticker),
      hedge_method = hedge_method,
      alpha = as.numeric(alpha_hat),
      beta  = as.numeric(beta_hat),
      rank = row_number(),
      trade_year = trade_year,
      formation_start = as.Date(formation_start),
      formation_end   = as.Date(formation_end),
      trade_start     = as.Date(trade_start),
      trade_end       = as.Date(trade_end)
    ) %>%
    select(
      pair_id, x_ticker, y_ticker, hedge_method, alpha, beta,
      rank, trade_year, formation_start, formation_end, trade_start, trade_end,
      everything()
    )
}

select_top_n_pairs <- function(pair_ranks,
                               N = 30,
                               hedge_method = "ols_px_on_py",
                               pair_id_sep = "-",
                               diversify_by = c("none", "sector"),
                               max_per_group = ceiling(0.4 * N),
                               trade_year = NA_integer_,
                               formation_start = NA,
                               formation_end   = NA,
                               trade_start     = NA,
                               trade_end       = NA) {

  diversify_by <- match.arg(diversify_by)

  df <- pair_ranks %>%
    filter(!is.na(score),
           is.finite(alpha_hat),
           is.finite(beta_hat)) %>%
    arrange(desc(score))

  if (diversify_by == "sector") {
    df <- df %>%
      group_by(sector_x) %>%
      slice_head(n = max_per_group) %>%
      ungroup() %>%
      arrange(desc(score)) %>%
      slice_head(n = N)
  } else {
    df <- df %>% slice_head(n = N)
  }

  df %>%
    mutate(
      x_ticker = str_to_upper(str_trim(x)),
      y_ticker = str_to_upper(str_trim(y)),
      pair_id = paste0(x_ticker, pair_id_sep, y_ticker),

      hedge_method = hedge_method,
      alpha = as.numeric(alpha_hat),
      beta  = as.numeric(beta_hat),

      rank = row_number(),

      trade_year = trade_year,
      formation_start = as.Date(formation_start),
      formation_end   = as.Date(formation_end),
      trade_start     = as.Date(trade_start),
      trade_end       = as.Date(trade_end)
    ) %>%
    select(
      pair_id, x_ticker, y_ticker, hedge_method, alpha, beta,
      rank, trade_year, formation_start, formation_end, trade_start, trade_end,
      everything()
    )
}

# R/join_pair_prices.R

join_pair_prices <- function(prices_trade,
                             pairs_snapshot,
                             dates = NULL,
                             origin = "1970-01-01") {

  stopifnot(is.matrix(prices_trade))
  stopifnot(!is.null(colnames(prices_trade)))
  stopifnot(all(c("pair_id", "x_ticker", "y_ticker") %in% names(pairs_snapshot)))

  # --- dates ---
  if (is.null(dates)) {
    rn <- rownames(prices_trade)
    if (is.null(rn)) stop("Provide `dates=` or ensure rownames(prices_trade) are set.")
    rn <- trimws(rn)

    if (all(grepl("^[-+]?[0-9]+(\\.[0-9]+)?$", rn))) {
      dates <- as.Date(as.integer(round(as.numeric(rn))), origin = origin)
    } else {
      dates <- as.Date(rn)
      if (any(is.na(dates))) {
        stop("Could not parse rownames(prices_trade) as Dates. Pass `dates=` explicitly.")
      }
    }
  } else {
    dates <- as.Date(dates)
  }

  # --- standardize tickers ---
  colnames(prices_trade) <- str_to_upper(str_trim(colnames(prices_trade)))

  ps <- pairs_snapshot %>%
    transmute(
      pair_id,
      x_ticker = str_to_upper(str_trim(x_ticker)),
      y_ticker = str_to_upper(str_trim(y_ticker))
    ) %>%
    distinct()

  # validate tickers
  missing <- setdiff(unique(c(ps$x_ticker, ps$y_ticker)), colnames(prices_trade))
  if (length(missing) > 0) {
    stop("Missing tickers in prices_trade colnames: ", paste(missing, collapse = ", "))
  }

  n_dates <- length(dates)
  n_pairs <- nrow(ps)

  # Extract price submatrices (n_dates x n_pairs)
  x_mat <- prices_trade[, ps$x_ticker, drop = FALSE]
  y_mat <- prices_trade[, ps$y_ticker, drop = FALSE]

  # Flatten column-wise: dates run fastest within each pair (matches rep(each= n_dates))
  tibble(
    date = rep(dates, times = n_pairs),
    pair_id = rep(ps$pair_id, each = n_dates),
    x_ticker = rep(ps$x_ticker, each = n_dates),
    y_ticker = rep(ps$y_ticker, each = n_dates),
    x_price = as.numeric(x_mat),
    y_price = as.numeric(y_mat)
  ) %>%
    arrange(pair_id, date)
}


compute_spread <- function(pair_prices, pairs_snapshot) {

  pair_prices %>%
    left_join(pairs_snapshot %>% distinct(pair_id, alpha_hat, beta_hat), by = "pair_id") %>%
    mutate(
      alpha_hat = as.numeric(alpha_hat),
      beta_hat  = as.numeric(beta_hat),
      spread = x_price - (alpha_hat + beta_hat * y_price)
    ) %>%
    arrange(pair_id, date)
}

compute_zscore_roll <- function(spreads,
                                pairs_snapshot = NULL,
                                lookback = 60L,
                                use_half_life = FALSE,
                                min_lookback = 60L,
                                half_life_col = "half_life") {
  stopifnot(all(c("date", "pair_id", "spread") %in% names(spreads)))

  df <- spreads %>% arrange(pair_id, date)

  # Attach one lookback per pair_id
  if (isTRUE(use_half_life)) {
    stopifnot(!is.null(pairs_snapshot))
    stopifnot(all(c("pair_id", half_life_col) %in% names(pairs_snapshot)))

    lb_tbl <- pairs_snapshot %>%
      distinct(pair_id, half_life = .data[[half_life_col]]) %>%
      mutate(
        half_life = as.numeric(half_life),
        lookback = pmax(as.integer(min_lookback), as.integer(round(3 * half_life)))
      ) %>%
      select(pair_id, lookback)

    df <- df %>% left_join(lb_tbl, by = "pair_id")
  } else {
    df <- df %>% mutate(lookback = as.integer(lookback))
  }

  # Rolling stats per pair using scalar lookback, then lag by 1 day
  df %>%
    group_by(pair_id) %>%
    group_modify(~{
      d <- .x
      lb <- unique(d$lookback)

      if (length(lb) != 1L || is.na(lb)) {
        stop("Lookback must be a single non-NA value per pair_id. pair_id=", unique(d$pair_id)[1])
      }
      lb <- as.integer(lb)

      mu_raw <- slide_dbl(
        d$spread,
        ~ mean(.x, na.rm = TRUE),
        .before = lb - 1L,
        .complete = TRUE
      )

      sigma_raw <- slide_dbl(
        d$spread,
        ~ stats::sd(.x, na.rm = TRUE),
        .before = lb - 1L,
        .complete = TRUE
      )

      d$mu <- dplyr::lag(mu_raw, 1L)
      d$sigma <- dplyr::lag(sigma_raw, 1L)
      d$z <- dplyr::if_else(!is.na(d$sigma) & d$sigma > 0, (d$spread - d$mu) / d$sigma, NA_real_)

      d
    }) %>%
    ungroup()
}

# R/generate_signals.R

generate_signals <- function(spreads_z,
                             entry_z = 2,
                             exit_z = 0.5,
                             max_holding_days = 60L) {
  stopifnot(all(c("date", "pair_id", "z") %in% names(spreads_z)))

  spreads_z %>%
    arrange(pair_id, date) %>%
    group_by(pair_id) %>%
    group_modify(~{
      d <- .x
      n <- nrow(d)

      signal   <- integer(n)     # -1 short spread, 0 flat, +1 long spread
      entry    <- logical(n)
      exit     <- logical(n)
      hold_days <- integer(n)

      pos <- 0L
      hd  <- 0L

      for (i in seq_len(n)) {
        z <- d$z[i]

        # Carry position across NA z (no new action), but still count holding days
        if (is.na(z)) {
          signal[i] <- pos
          if (pos != 0L) hd <- hd + 1L else hd <- 0L
          hold_days[i] <- hd
          next
        }

        # Force exit if max holding reached (exit signal today -> flat tomorrow in execution)
        if (pos != 0L && hd >= max_holding_days) {
          pos <- 0L
          exit[i] <- TRUE
          hd <- 0L
          signal[i] <- pos
          hold_days[i] <- hd
          next
        }

        if (pos == 0L) {
          # Enter
          if (z > entry_z) {
            pos <- -1L
            entry[i] <- TRUE
            hd <- 1L
          } else if (z < -entry_z) {
            pos <- 1L
            entry[i] <- TRUE
            hd <- 1L
          } else {
            hd <- 0L
          }

        } else if (pos == 1L) {
          # Exit long spread when z crosses back above -exit_z
          if (z > -exit_z) {
            pos <- 0L
            exit[i] <- TRUE
            hd <- 0L
          } else {
            hd <- hd + 1L
          }

        } else if (pos == -1L) {
          # Exit short spread when z crosses back below +exit_z
          if (z < exit_z) {
            pos <- 0L
            exit[i] <- TRUE
            hd <- 0L
          } else {
            hd <- hd + 1L
          }
        }

        signal[i] <- pos
        hold_days[i] <- hd
      }

      d$signal <- signal
      d$entry  <- entry
      d$exit   <- exit
      d$hold_days <- hold_days
      d
    }) %>%
    ungroup()
}

simulate_pair_pnl <- function(signals_df,
                              notional_per_pair = 10000,
                              cost_bps = 5) {

  stopifnot(all(c("date","pair_id","signal","x_price","y_price","beta_hat") %in% names(signals_df)))

  signals_df %>%
    arrange(pair_id, date) %>%
    group_by(pair_id) %>%
    mutate(
      beta_hat = readr::parse_number(as.character(beta_hat)),

      # Execute using 1-day lag
      pos = dplyr::lag(signal, 1L),

      # Previous prices (for sizing + PnL)
      x_prev = dplyr::lag(x_price, 1L),
      y_prev = dplyr::lag(y_price, 1L),

      dx = x_price - x_prev,
      dy = y_price - y_prev,

      # Trade only when inputs are valid
      ok_trade = is.finite(beta_hat) &
        !is.na(pos) & pos != 0 &
        is.finite(x_prev) & x_prev > 0 &
        is.finite(y_prev) & y_prev > 0 &
        is.finite(dx) & is.finite(dy),

      # Allocate capital by beta
      dollar_x = notional_per_pair / (1 + abs(beta_hat)),
      dollar_y = abs(beta_hat) * dollar_x,

      # Desired quantities held during day t (based on pos at start of day)
      qty_x = dplyr::if_else(ok_trade,  pos * (dollar_x / x_prev), 0),
      qty_y = dplyr::if_else(ok_trade, -pos * (dollar_y / y_prev), 0),

      # PnL (gross)
      pnl_x = qty_x * dx,
      pnl_y = qty_y * dy,
      pnl   = pnl_x + pnl_y,

      gross_exposure = abs(qty_x * x_prev) + abs(qty_y * y_prev),
      net_exposure   = (qty_x * x_prev) + (qty_y * y_prev),

      # ----- Transaction costs -----
      # Turnover = dollars traded when quantities change
      dqty_x = qty_x - dplyr::lag(qty_x, 1L),
      dqty_y = qty_y - dplyr::lag(qty_y, 1L),

      turnover_dollars = abs(dqty_x) * x_prev + abs(dqty_y) * y_prev,
      turnover_dollars = dplyr::coalesce(turnover_dollars, 0),

      cost_rate = cost_bps / 10000,
      cost = cost_rate * turnover_dollars,

      pnl_net = pnl - cost,
      ret     = pnl / notional_per_pair,
      ret_net = pnl_net / notional_per_pair
    ) %>%
    ungroup()
}

aggregate_portfolio_daily <- function(daily_pair_pnl,
                                      notional_per_pair = 10000,
                                      capital_mode = c("per_pair", "active_only")) {
  capital_mode <- match.arg(capital_mode)

  stopifnot(all(c("date","pair_id","pnl","gross_exposure","net_exposure","pos") %in% names(daily_pair_pnl)))

  base <- daily_pair_pnl %>%
    mutate(active = !is.na(pos) & pos != 0) %>%
    group_by(date) %>%
    summarise(
      pnl = sum(pnl, na.rm = TRUE),
      gross_exposure = sum(gross_exposure, na.rm = TRUE),
      net_exposure   = sum(net_exposure, na.rm = TRUE),
      n_pairs_total  = n_distinct(pair_id),
      n_active       = sum(active, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date)

  if (capital_mode == "per_pair") {
    base <- base %>%
      mutate(
        capital = n_pairs_total * notional_per_pair,
        ret = pnl / capital
      )
  } else {
    base <- base %>%
      mutate(
        capital = pmax(n_active, 1) * notional_per_pair,
        ret = pnl / capital
      )
  }

  base %>%
    mutate(
      equity = cumsum(replace_na(pnl, 0)),
      cumret = cumprod(1 + replace_na(ret, 0)) - 1
    )
}

aggregate_portfolio_daily_net <- function(trades,
                                          notional_per_pair = 10000,
                                          capital_mode = c("per_pair", "active_only")) {
  capital_mode <- match.arg(capital_mode)

  stopifnot(all(c("date","pair_id","pnl_net","gross_exposure","net_exposure","pos") %in% names(trades)))

  library(dplyr)

  daily <- trades %>%
    mutate(active = !is.na(pos) & pos != 0,
           pnl_net = if_else(is.finite(pnl_net), pnl_net, 0)) %>%
    group_by(date) %>%
    summarise(
      pnl = sum(pnl_net, na.rm = TRUE),
      gross_exposure = sum(gross_exposure, na.rm = TRUE),
      net_exposure   = sum(net_exposure, na.rm = TRUE),
      n_pairs_total  = n_distinct(pair_id),
      n_active       = sum(active, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date)

  daily <- if (capital_mode == "per_pair") {
    daily %>% mutate(capital = n_pairs_total * notional_per_pair)
  } else {
    daily %>% mutate(capital = pmax(n_active, 1) * notional_per_pair)
  }

  daily %>%
    mutate(
      ret = pnl / capital,
      # stable cumret computation
      ret_capped = if_else(is.finite(ret) & ret <= -1, -0.999999, ret),
      cumlogret = cumsum(if_else(is.finite(ret_capped), log1p(ret_capped), 0)),
      cumret = exp(cumlogret) - 1,
      equity = cumsum(pnl)
    ) %>%
    select(-ret_capped, -cumlogret)
}

summarise_pair_performance <- function(daily_pair_pnl) {
  stopifnot(all(c("pair_id","pnl","pos") %in% names(daily_pair_pnl)))

  daily_pair_pnl %>%
    mutate(active = !is.na(pos) & pos != 0) %>%
    group_by(pair_id) %>%
    summarise(
      pnl_total = sum(pnl, na.rm = TRUE),
      pnl_mean  = mean(pnl, na.rm = TRUE),
      pnl_sd    = sd(pnl, na.rm = TRUE),
      n_days    = n(),
      n_active_days = sum(active, na.rm = TRUE),
      active_share  = mean(active, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(pnl_total))
}

portfolio_metrics <- function(portfolio_daily, trading_days = 252) {
  stopifnot(all(c("ret") %in% names(portfolio_daily)))

  r <- portfolio_daily$ret
  r <- r[is.finite(r)]

  if (length(r) < 2) return(list())

  ann_ret <- (prod(1 + r)^(trading_days / length(r))) - 1
  ann_vol <- sd(r) * sqrt(trading_days)
  sharpe  <- if (ann_vol > 0) ann_ret / ann_vol else NA_real_

  equity <- cumprod(1 + r)
  dd <- equity / cummax(equity) - 1
  max_dd <- min(dd)

  list(
    ann_return = ann_ret,
    ann_vol = ann_vol,
    sharpe = sharpe,
    max_drawdown = max_dd
  )
}

run_single_year_sim <- function(prices_mat,
                                returns_mat,
                                universe,
                                trade_year,
                                formation_years = 3,
                                top_n = 30) {

  # Define windows
  formation_start <- as.Date(sprintf("%d-01-01", trade_year - formation_years))
  formation_end   <- as.Date(sprintf("%d-12-31", trade_year - 1))
  trade_start     <- as.Date(sprintf("%d-01-01", trade_year))
  trade_end       <- as.Date(sprintf("%d-12-31", trade_year))

  dates <- as.Date(rownames(prices_mat))

  formation_idx <- dates >= formation_start & dates <= formation_end
  trade_idx     <- dates >= trade_start & dates <= trade_end

  prices_form  <- prices_mat[formation_idx, , drop = FALSE]
  returns_form <- returns_mat[formation_idx, , drop = FALSE]
  prices_trade <- prices_mat[trade_idx, , drop = FALSE]

  # Formation stage
  candidates <- generate_candidate_pairs(returns_form, universe)
  metrics    <- compute_pair_metrics(candidates, prices_form)
  ranked     <- rank_pairs(metrics)
  top_pairs  <- select_top_n_pairs(ranked, N = top_n,
                                   trade_year = trade_year,
                                   formation_start = formation_start,
                                   formation_end = formation_end,
                                   trade_start = trade_start,
                                   trade_end = trade_end)

  # Trading stage
  pair_prices <- join_pair_prices(prices_trade, top_pairs)
  spreads <- compute_spread(pair_prices, pairs_snapshot = top_pairs)
  spreads_z <- compute_zscore_roll(spreads, top_pairs)
  signals     <- generate_signals(spreads_z)
  trades      <- simulate_pair_pnl(signals)

  return(trades)

}

source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_load(log_adj_close_mat)
tar_load(pair_ranks)

#### Trading Simulation ####
score_pairs_formation <- function(returns,
                                  log_prices,
                                  train_mask,
                                  universe,
                                  min_cor = 0.6,
                                  adf_k = 1,
                                  min_train = 252,
                                  rank_params = list(),
                                  overlap_params = list(max_per_ticker = 3, top_n = 200)) {
  
  stopifnot(is.matrix(returns) || is.data.frame(returns))
  stopifnot(is.matrix(log_prices) || is.data.frame(log_prices))
  stopifnot(length(train_mask) == nrow(log_prices))

  # Candidate set from return correlation + sector constraint
  cand <- define_pair_set(
    returns = returns,
    train_mask = train_mask,
    universe = universe,
    min_cor = min_cor
  )

  # Compute formation metrics (ADF pvalue, half-life, beta_hat, etc.)
  met <- compute_pair_metrics(
    candidate_pairs = cand,
    log_prices = log_prices,
    train_mask = train_mask,
    test_mask = NULL,
    adf_k = adf_k,
    min_train = min_train
  )

  # Score / rank (uses coint_pvalue, half_life, zero_cross, etc.)
  ranked <- do.call(rank_pairs, c(list(df = met), rank_params))

  # Optional: limit overlap so one ticker doesn't dominate the book
  #limited <- do.call(limit_pair_overlap, c(list(ranked_df = ranked), overlap_params))

  return(ranked)

}

select_top_n_pairs <- function(candidate_pairs,
                               N = 30,
                               pval_max = 0.1,
                               hl_range = c(5, 120),
                               n_train_min = 252,
                               diversify_by = c("none", "sector"),
                               max_per_group = ceiling(0.4 * N),
                               trade_year = 2016,
                               formation_start = "2011-01-01",
                               formation_end   = "2015-12-31",
                               trade_start     = "2016-01-01",
                               trade_end       = "2016-12-31"
                              ) {
  
  df <- candidate_pairs %>%
    filter(coint_pvalue <= pval_max,
           (half_life >= hl_range[1]) & (half_life <= hl_range[2]),
           n_train >= n_train_min)
  
  if (diversify_by == "sector") {

    df <- df %>%
      arrange(desc(score)) %>%
      group_by(sector_x) %>%
      slice_head(n = max_per_group) %>%
      ungroup() %>%
      arrange(desc(score)) %>%
      slice_head(n = N)

  } else {

    df <- df %>%
      arrange(desc(score)) %>%
      slice_head(n = N)

  }

  df <- df %>%
    mutate(
      pair_id = paste0(x, "-", y),
      trade_year = trade_year,
      formation_start = as.Date(formation_start),
      formation_end   = as.Date(formation_end),
      trade_start     = as.Date(trade_start),
      trade_end       = as.Date(trade_end)
    )

  return(df)
  
}

join_pair_prices <- function(prices_trade,
                             pairs_snapshot,
                             dates = NULL) {

  get_matrix_dates <- function(prices_trade, dates = NULL, origin = "1970-01-01") {
    if (!is.null(dates)) return(as.Date(dates))

    rn <- rownames(prices_trade)
    if (is.null(rn)) stop("Provide `dates`, or set rownames(prices_trade).")

    # If rownames look numeric (e.g., "16804"), treat as days since origin
    if (all(grepl("^\\d+$", rn))) {
      return(as.Date(as.integer(rn), origin = origin))
    }

    # Otherwise, assume rownames are ISO date strings like "2016-01-04"
    as.Date(rn)
  }
  
  stopifnot(is.matrix(prices_trade))
  stopifnot(!is.null(colnames(prices_trade)))
  stopifnot(all(c("pair_id", "x", "y") %in% names(pairs_snapshot)))

  # Dates
  dates <- get_matrix_dates(prices_trade, dates = dates, origin = origin)

  # Standardize tickers in snapshot
  ps <- pairs_snapshot %>%
    transmute(
      pair_id,
      x = str_to_upper(str_trim(x)),
      y = str_to_upper(str_trim(y))
    )

  tickers <- colnames(prices_trade)

  # Check tickers exist
  missing_x <- setdiff(unique(ps$x), tickers)
  missing_y <- setdiff(unique(ps$x), tickers)
  missing_all <- unique(c(missing_x, missing_y))

  if (length(missing_all) > 0) {
    stop("These tickers are missing from colnames(prices_trade): ",
         paste(missing_all, collapse = ", "))
  }

  # Build pair-by-day panel
  out <- purrr::pmap_dfr(ps, function(pair_id, x, y) {
    tibble(
      date = dates,
      pair_id = pair_id,
      x_ticker = x,
      y_ticker = y,
      x_price = as.numeric(prices_trade[, x]),
      y_price = as.numeric(prices_trade[, y])
    )
  }) %>%
    arrange(pair_id, date)

  out

}

# R/compute_spread.R

compute_spread <- function(pair_prices, pairs_snapshot) {
  stopifnot(all(c("date","pair_id","x_price","y_price") %in% names(pair_prices)))
  stopifnot(all(c("pair_id","beta_hat", "alpha_hat") %in% names(pairs_snapshot)))

  coefs <- pairs_snapshot %>%
    distinct(pair_id, beta_hat, alpha_hat)

  pair_prices %>%
    left_join(coefs, by = "pair_id") %>%
    mutate(
      alpha_hat = as.numeric(alpha_hat),
      beta_hat = as.numeric(beta_hat),
      spread = x_price - (alpha_hat + (beta_hat * x_price))
    ) %>%
    select(date, pair_id, x_ticker, y_ticker, x_price, y_price, alpha_hat, beta_hat, spread) %>%
    arrange(pair_id, date)
}


# R/compute_zscore_roll.R

compute_zscore_roll <- function(spreads,
                                pairs_snapshot = NULL,
                                lookback = 60L,
                                use_half_life = FALSE,
                                min_lookback = 60L,
                                half_life_col = "half_life") {
  stopifnot(all(c("date", "pair_id", "spread") %in% names(spreads)))

  df <- spreads %>% arrange(pair_id, date)

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

  # Compute rolling stats per pair using a SINGLE scalar lookback within group
  df %>%
    group_by(pair_id) %>%
    group_modify(~{
      d <- .x
      lb <- unique(d$lookback)

      if (length(lb) != 1L || is.na(lb)) {
        stop("Lookback must be a single non-NA value per pair_id. pair_id=",
             unique(d$pair_id)[1])
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

walk_forward_prepare <- function(prices_mat,
                                 trade_years,
                                 formation_years = 5,
                                 N = 30,
                                 pval_max = 0.10,
                                 hl_range = c(5, 120),
                                 n_train_min = 252,
                                 diversify_by = c("none", "sector"),
                                 max_per_group = ceiling(0.4 * N),
                                 entry_z = 2,
                                 exit_z = 0.5,
                                 use_half_life_lookback = TRUE,
                                 min_lookback = 60L,
                                 score_fn) {

  diversify_by <- match.arg(diversify_by)
  stopifnot(is.matrix(prices_mat))
  stopifnot(!is.null(colnames(prices_mat)))
  stopifnot(is.function(score_fn))

  # Dates from matrix
  rn <- rownames(prices_mat)
  if (is.null(rn)) stop("prices_mat must have rownames as dates (ISO) or numeric day counts.")
  dates <- if (all(grepl("^\\d+$", rn))) as.Date(as.integer(rn), origin = "1970-01-01") else as.Date(rn)

  # Helper to slice matrix by date range (inclusive)
  slice_mat <- function(mat, dates, start_date, end_date) {
    idx <- which(dates >= start_date & dates <= end_date)
    mat[idx, , drop = FALSE]
  }

  out <- vector("list", length(trade_years))
  names(out) <- as.character(trade_years)

  for (i in seq_along(trade_years)) {
    ty <- trade_years[i]

    formation_start <- as.Date(sprintf("%d-01-01", ty - formation_years))
    formation_end   <- as.Date(sprintf("%d-12-31", ty - 1))
    trade_start     <- as.Date(sprintf("%d-01-01", ty))
    trade_end       <- as.Date(sprintf("%d-12-31", ty))

    prices_form  <- slice_mat(prices_mat, dates, formation_start, formation_end)
    prices_trade <- slice_mat(prices_mat, dates, trade_start, trade_end)

    # Score candidate pairs in formation period
    candidate_pairs <- score_fn(prices_form)

    # Select top N
    pairs_snapshot <- select_top_n_pairs(
      candidate_pairs,
      N = N,
      pval_max = pval_max,
      hl_range = hl_range,
      n_train_min = n_train_min,
      diversify_by = diversify_by,
      max_per_group = max_per_group,
      trade_year = ty,
      formation_start = as.character(formation_start),
      formation_end   = as.character(formation_end),
      trade_start     = as.character(trade_start),
      trade_end       = as.character(trade_end)
    )

    # Build trade-year pair panel
    pair_prices <- join_pair_prices(prices_trade, pairs_snapshot)  # uses rownames(dates) internally
    spreads     <- compute_spread(pair_prices, pairs_snapshot)

    spreads_z <- compute_zscore_roll(
      spreads,
      pairs_snapshot = pairs_snapshot,
      use_half_life = use_half_life_lookback,
      min_lookback = min_lookback,
      lookback = min_lookback
    )

    # Generate signals (next step; if you haven’t added it yet, skip)
    # signals <- generate_signals(spreads_z, entry_z = entry_z, exit_z = exit_z)

    out[[i]] <- list(
      trade_year = ty,
      formation_start = formation_start,
      formation_end = formation_end,
      trade_start = trade_start,
      trade_end = trade_end,
      candidate_pairs = candidate_pairs,
      pairs_snapshot = pairs_snapshot,
      pair_prices = pair_prices,
      spreads = spreads,
      spreads_z = spreads_z
      # ,signals = signals
    )
  }

  out

}

#### Running Simulation ####

# Select top pairs
top_pairs <- select_top_n_pairs(pair_ranks, diversify_by = "sector")
dim(top_pairs)

# Get price data for pairs
top_pair_prices = join_pair_prices(log_adj_close_mat, top_pairs)
dim(top_pair_prices)

top_pair_spreads = compute_spread(top_pair_prices, top_pairs)
dim(top_pair_spreads)

top_pair_zscores = compute_zscore_roll(top_pair_spreads)
dim(top_pair_zscores)

wf <- walk_forward_prepare(
  prices_mat = prices_mat,
  trade_years = 2016:2020,
  formation_years = 5,
  N = 30,
  diversify_by = "sector",
  score_fn = score_pairs  # <- your scoring function
)

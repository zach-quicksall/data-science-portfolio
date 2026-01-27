clean_city_prices <- function(df) {
  
  df <- df %>%
    select(-`...1`) %>%
    janitor::clean_names() %>%
    pivot_longer(-c(region_id, region_name, state_name, size_rank),
                 names_to = "date",
                 values_to = "sale_price") %>%
    mutate(date = str_replace(date, "^x", ""),
           date = ym(date),
           region_id = as.factor(region_id))
  
  return(df)
  
}

augment_city_prices <- function(df) {
  
  # Testing
  #df <- cp_clean
  
  # Arrange for simpler grouping and computation
  df <- df %>%
    arrange(region_id, date)
  
  # Compute YoY and MoM growth and smoothed averages
  df <- df %>%
    group_by(region_id) %>%
    mutate(
      yoy_growth = 100 * (sale_price / lag(sale_price, 12) - 1),
      yoy_3m_avg = rollmean(yoy_growth, 3, fill = NA, align = "right"),
      mom_growth = 100 * (sale_price / lag(sale_price, 1) - 1),
      mom_3m_avg = rollmean(mom_growth, 3, fill = NA, align = "right"),
      mom_6m_avg = rollmean(mom_growth, 6, fill = NA, align = "right")
    ) %>%
    ungroup()
  
  # Compute cumulative growth from raw price series
  df <- df %>%
    group_by(region_id) %>%
    mutate(base_price = first(sale_price[!is.na(sale_price)]),
           cumulative_growth = 100 * (sale_price / base_price - 1)) %>%
    ungroup()
  
  # Compute acceleration
  df <- df %>%
    group_by(region_id) %>%
    mutate(yoy_accel = yoy_growth - lag(yoy_growth, 1),
           mom_accel = mom_growth - lag(mom_growth, 1)) %>%
    ungroup()
  
  return(df)
  
}

compute_corr_n <- function(df,
                           id_col = "date",
                           name_col = "region_id",
                           value_col = "yoy_growth") {
  
  # Testing
  #df <- cp_aug %>% select(region_id, date, yoy_growth)
  
  # Create wide df
  df_wide <- df %>%
    pivot_wider(
      id_cols    = {{ id_col }},
      names_from = {{ name_col }},
      values_from = {{ value_col }}
    ) %>%
    select(-date)
  
  # Compute correlation
  df_cor <- df_wide %>%
    correlate(use = "pairwise.complete.obs") %>%
    stretch()
  
  # Compute N
  n_mat <- crossprod(
    !is.na(
      as.matrix(df_wide)
    )
  )
  
  # Merge for return
  df_cor <- df_cor %>%
    mutate(n_obs = as.vector(n_mat))
  
  return(df_cor)
  
}

make_yoy_growth_spreads <- function(df, cp_aug) {
  
  # Testing
  #df <- yoy_growth_cor_neg
  #tar_load(cp_aug)
  
  if (nrow(df) < 1) { return(NULL) }
  
  df <- df %>%
    mutate(pair_id = paste0(x, " vs ", y)) %>%
    pivot_longer(-c(pair_id, r, n_obs),
                 names_to = "role",
                 values_to = "region_id") %>%
    left_join(cp_aug %>% select(region_id, date, yoy_growth),
              by = "region_id",
              relationship = "many-to-many") %>%
    select(-region_id) %>%
    pivot_wider(
      names_from = role,
      values_from = yoy_growth
    ) %>%
    mutate(spread = x - y) %>%
    group_by(pair_id) %>%
    mutate(spread_mu = mean(spread, na.rm = TRUE),
           spread_sd = sd(spread, na.rm = TRUE),
           z = (spread - spread_mu) / spread_sd) %>%
    ungroup() %>%
    select(pair_id, date, x, y, spread, spread_mu, spread_sd, z, r, n_obs)
  
  return(df)
  
}

perform_hclust <- function(df) {
  
  # Testing
  df <- yoy_growth_cor
  
  # Filter on minimum n_obs
  df <- df %>% filter(n_obs >= 10)
  
  # Compute distance and fill diagonal with NA
  df <- df %>%
    mutate(r = 1 - abs(r)) %>%
    rename(dist = r) %>%
    mutate(dist = case_when(x == y ~ 0,
                            TRUE ~ dist))
  
  # Pivot to wide form
  df_wide <- df %>%
    select(-n_obs) %>%
    pivot_wider(id_cols = x,
                names_from = y,
                values_from = dist)
    
  # Cast to matrix
  df_mat <- as.matrix(df_wide %>% select(-x))
  rownames(df_mat) <- df_wide$x
  
  # Subset to regions to retain (reduce dimensionality)
  keep <- colMeans(!is.na(df_mat)) >= 0.9
  
  # Perform hierarchical clustering
  clust_res <- hclust(df_mat, method = "average")
  
}
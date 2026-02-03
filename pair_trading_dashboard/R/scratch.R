source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_load(log_return_mat)
tar_load(train_mask)
tar_load(series_filt)

# Subset returns using train mask
ret_train <- log_return_mat[train_mask[3:length(train_mask)], , drop = FALSE]

# Drop columns with excessive missingness
keep <- colMeans(is.na(ret_train)) < 0.05
ret_train <- ret_train[, keep, drop = FALSE]

# Correlation matrix
corr_mat <- cor(ret_train, use = "pairwise.complete.obs")

# Distance matrix
dist_mat <- as.dist(sqrt(2 * (1 - corr_mat)))

# Iterate through each sector
sector_map <- series_filt %>%
  select(sector, ticker) %>%
  distinct()

sectors <- unique(sector_map$sector)

cluster_list <- list()
for (sec in sectors) {

  # Print industry
  print(sec)

  # Define candidate tickers
  tickers <- sector_map %>%
    filter(sector == sec) %>%
    pull(ticker)

  # Keep only those also present in corr_mat
  common_tickers <- tickers[(tickers %in% rownames(corr_mat))]

  # Subset correlation matrix to tickers of current sector
  corr_mat_sub <- corr_mat[common_tickers, common_tickers]

  # Compute cosine distance
  dist_mat_sub <- as.dist(sqrt(2 * (1 - corr_mat_sub)))

  # Define minimum points in cluster
  minPts <- max(4, floor(log(length(common_tickers))))
    
  # Apply OPTICS
  opt <- optics(dist_mat_sub, minPts = minPts, eps = Inf)

  # Define clusters based on steep changes in reachability
  cl_xi <- extractXi(opt, xi = 0.05)

  # Cluster membership counts
  labels <- attr(dist_mat_sub, "Labels")

  cluster_tbl <- tibble(
    sector  = sec,
    ticker  = labels,
    cluster = cl_xi$cluster
  )

  cluster_list[[sec]] <- cluster_tbl

}

cluster_df <- bind_rows(cluster_list)
View(cluster_df %>% group_by(sector, cluster) %>% count())





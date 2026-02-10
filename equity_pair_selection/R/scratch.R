source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_load(log_adj_close_mat)
tar_load(pair_subset)
tar_load(train_mask_close)
tar_load(eval_mask_close)

# Subset closing prices to desired pair
plot_mat <- log_adj_close_mat[, c("BKH","POR")]

# Prepare dataframe for plotting
plot_df <- as_tibble(plot_mat, rownames = "date") %>%
  mutate(date = as.numeric(date),
         spread = BKH - (0.6526 + 0.9131 * POR),
         train_set = train_mask_close)

spread_mean <- mean(plot_df %>%
  filter(train_set == TRUE) %>%
  pull(spread)
)

spread_sd <- sd(plot_df %>%
  filter(train_set == TRUE) %>%
  pull(spread)
)

plot_df <- plot_df %>%
  mutate(spread_mu = spread_mean,
         spread_sd = spread_sd,
         zscore = (spread - spread_mean) / spread_sd)

# Paired price points
plot_df %>%
  ggplot(aes(x = BKH, y = POR, color = train_set)) +
  scale_color_manual(values = c("darkorange","dodgerblue")) +
  geom_point(alpha = 0.4) +
  labs(color = "Data Set") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Price series
plot_df %>%
  ggplot(aes(x = date, linetype = train_set)) +
  geom_line(aes(y = BKH), color = "darkorange") +
  geom_line(aes(y = POR), color = "dodgerblue") +
  labs(linetype = "Train Data") +
  labs(y = "Price (USD)", x = "Date") +
  theme_minimal()

# Spread
plot_df %>%
  ggplot(aes(x = date, color = train_set)) +
  geom_line(aes(y = spread)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("darkorange","dodgerblue")) +
  labs(y = "Spread (USD)", x = "Date", color = "Train Data") +
  theme_minimal()

# Zscore
plot_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = zscore, color = train_set)) +
  scale_color_manual(values = c("darkorange","dodgerblue")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "black") +
  labs(y = "Z-Score (stdev)", x = "Date", color = "Train Data") +
  theme_minimal() +
  theme(legend.position = "bottom")


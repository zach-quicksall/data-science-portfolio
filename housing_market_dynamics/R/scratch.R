source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_load(cp_aug)

# Raw sale prices
cp_aug %>%
  ggplot(aes(x = sale_price)) +
  geom_histogram() +
  labs(x = "Sale Price (USD)", y = "Count") +
  theme_minimal()

# Log sale prices
cp_aug %>%
  ggplot(aes(x = log10(sale_price))) +
  geom_histogram() +
  labs(x = "Log10 Sale Price (USD)", y = "Count") +
  theme_minimal()

# Months covered by region_id
cp_aug %>%
  group_by(region_id) %>%
  count(sort = TRUE) %>%
  View()

sum_df <- cp_aug %>%
  group_by(region_id) %>%
  summarise(mean = mean(sale_price, na.rm = TRUE),
            median = median(sale_price, na.rm = TRUE),
            sd = sd(sale_price, na.rm = TRUE))
View(sum_df)

# Distribution of average sale price per region
sum_df %>%
  ggplot(aes(x = mean)) +
  geom_histogram() +
  labs(x = "Mean Sale Price (USD)", y = "Count (N)") +
  theme_minimal()

# Distribution of median sale price per region
sum_df %>%
  ggplot(aes(x = median)) +
  geom_histogram() +
  labs(x = "Median Sale Price (USD)", y = "Count (N)") +
  theme_minimal()

# Distribution of variation in sale price per region
sum_df %>%
  ggplot(aes(x = sd)) +
  geom_histogram() +
  labs(x = "Standard Deviation of Sale Price (USD)", y = "Count (N)") +
  theme_minimal()

# Mean/Median sale price over time
cp_aug %>%
  group_by(date) %>%
  summarise(mean = mean(sale_price, na.rm = TRUE),
            median = median(sale_price, na.rm = TRUE)) %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median), color = "dodgerblue") +
  geom_line(aes(y = mean), color = "darkorange") +
  labs(x = "Date", y = "Sale Price (USD)") +
  theme_minimal()

# Plot regional YoY growth
plot_ids <- cp_aug %>%
  select(region_id) %>%
  distinct() %>%
  dplyr::slice_sample(n = 20)

cp_aug %>%
  inner_join(plot_ids, by = "region_id") %>%
  ggplot(aes(x = date, y = yoy_growth, color = region_id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "YoY Growth (%)") +
  theme(legend.position = "none")

cp_aug %>%
  inner_join(plot_ids, by = "region_id") %>%
  ggplot(aes(x = yoy_growth, fill = region_id)) +
  geom_histogram() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  facet_wrap(~ region_id, scales = "free_y") +
  labs(x = "YoY Growth (%)", y = "Count (N)") +
  theme(legend.position = "none")

# Cumulative growth
cp_aug %>%
  inner_join(plot_ids, by = "region_id") %>%
  ggplot(aes(x = date, y = cumulative_growth, color = region_id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "Cumulative Growth (%)") +
  theme(legend.position = "none")

cp_aug %>%
  inner_join(plot_ids, by = "region_id") %>%
  ggplot(aes(date, yoy_growth)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  facet_wrap(~ region_id) +
  labs(title = "Year-over-Year Home Price Growth (%)")

cp_aug %>%
  inner_join(plot_ids, by = "region_id") %>%
  ggplot(aes(date, yoy_accel)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  facet_wrap(~ region_id) +
  labs(title = "Year-over-Year Growth Acceleration")

# Correlation of YoY growth across regions
tar_load(yoy_growth_cor)

n_mat <- crossprod(!is.na(as.matrix(yoy_growth_cor)))

plot_ids <- sample(setdiff(colnames(yoy_growth_cor), c("term")),
                   size = 10)

plot_df <- yoy_growth_cor %>%
  stretch() %>%
  filter(x %in% plot_ids,
         y %in% plot_ids) %>%
  replace_na(list(r = 0))
  
plot_df %>%
  ggplot(aes(x = x, y = y, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  coord_fixed() +
  labs(x = "Region ID", y = "Region ID", fill = "cor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())
  
# Top correlated YoY regions
tar_load(yoy_pos_growth_spreads)
  
plot_ids <- yoy_pos_growth_spreads %>%
  select(pair_id) %>%
  distinct() %>%
  slice_sample(n = 12)

plot_df <- yoy_pos_growth_spreads %>%
  inner_join(plot_ids, by = "pair_id")

plot_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = x), color = "dodgerblue") +
  geom_line(aes(y = y), color = "darkorange") +
  facet_wrap(~ pair_id, scales = "free") +
  labs(x = "Date", y = "YoY Growth") +
  theme_minimal()

plot_df %>%
  ggplot(aes(x = date, y = z)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dodgerblue") +
  facet_wrap(~ pair_id, scales = "free_x") +
  labs(x = "Date", y = "Spread Z-score (x - y)") +
  theme_minimal() +
  theme(legend.position = "bottom")





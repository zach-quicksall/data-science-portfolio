source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_plan(
  
  # Define specific files for analysis (will not be using all of them)
  tar_file(city_prices_file, "data/raw/Sale_Prices_City.csv"),
  
  # Load raw price data
  cp_raw = read_csv(city_prices_file),
  
  # Clean raw price data
  cp_clean = clean_city_prices(cp_raw),
  
  # Augment raw price data
  cp_aug = augment_city_prices(cp_clean),
  
  # YoY growth correlations
  yoy_growth_cor = compute_corr_n(
    cp_aug %>%
      select(region_id, date, yoy_growth)
    ),
  
  # Subset YoY growth to strongest correlations
  yoy_growth_cor_pos = yoy_growth_cor %>%
    filter(r >= 0.8, n_obs >= 100),
  
  yoy_growth_cor_neg = yoy_growth_cor %>%
    filter(r <= -0.8, n_obs >= 100),
  
  # Compute spreads of highly-correlated markets for visualization
  yoy_pos_growth_spreads = make_yoy_growth_spreads(yoy_growth_cor_pos,
                                                   cp_aug),
  
  yoy_neg_growth_spreads = make_yoy_growth_spreads(yoy_growth_cor_neg,
                                                   cp_aug),
  
  # Perform hierarchical clustering
  yoy_growth_clust = perform_hclust(yoy_growth_cor),
  
)
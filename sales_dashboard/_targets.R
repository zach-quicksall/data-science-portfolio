source("R/packages.R")
source("R/functions.R")
set.seed(2026)

tar_plan(

  # Track raw file
  tar_file(data_file, "data/raw/Global_Superstore2.csv"),
  data_raw = read_csv(data_file),

  # Clean raw data
  data_clean = clean_data(data_raw),

)
source("R/packages.R")
source("R/functions.R")
set.seed(2025)

tar_plan(

  tar_file(cqn_file, "data/raw/"),
  cqn_raw = read_rds(cqn_file),

  # Subset samples and restrict to protein-coding genes
  # to reduce computational burden. Reasonable for illustration
  # in this project.

  # Coerce expression dataframe to matrix format for WGCNA

)
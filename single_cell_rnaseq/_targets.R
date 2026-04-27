source("R/packages.R")
source("R/packages.R")
set.seed(2025)

SeuratData::InstallData("ifnb")

tar_plan(

  # Load Seurat data set from SeuratData (already installed via package_setup.R)
  obj = LoadData("ifnb"),

)
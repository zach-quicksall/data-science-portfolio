source("R/packages.R")
source("R/packages.R")
set.seed(2025)

tar_plan(

  # Load Seurat data set from SeuratData (already installed via package_setup.R)
  obj = LoadData("pbmcsca"),

  # Apply a minimum count number to fragments
  obj_sub = subset(obj, nFeature_RNA > 1000),

  # Run the Azimuth pipeline
  obj_az = RunAzimuth(obj, reference = "pbmcref"),

)
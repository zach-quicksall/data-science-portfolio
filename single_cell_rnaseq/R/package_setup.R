# Initialize renv repository (installs packages
# and dependencies used in packages.R by default)
renv::init()

# Install Bioconductor and associated packages
install.packages("BiocManager")

BiocManager::install(c(
  "BSgenome.Hsapiens.UCSC.hg38",
  "EnsDb.Hsapiens.v86",
  "GenomeInfoDb",
  "GenomicRanges",
  "glmGamPoi",
  "JASPAR2020",
  "TFBSTools"
))

# Install other lab-developed packages that enhance speed of Seurat
setRepositories(ind = 1:3, addURLs = c('https://satijalab.r-universe.dev', 'https://bnprks.r-universe.dev/'))
renv::install(c("BPCells", "presto", "glmGamPoi"))

# Install helpful additions to Seurat
renv::install(c("satijalab/seurat-data","satijalab/azimuth","satijalab/seurat-wrappers"), quiet = TRUE)

# Install dataset for illustration
SeuratData::InstallData("pbmcsca")

# Freeze packages 
renv::snapshot()

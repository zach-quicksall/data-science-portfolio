# Initialize renv repository (installs packages
# and dependencies used in packages.R by default)
renv::init()

# Install other lab-developed packages that enhance speed of Seurat
setRepositories(ind = 1:3, addURLs = c('https://satijalab.r-universe.dev', 'https://bnprks.r-universe.dev/'))
renv::install(c("BPCells", "presto", "glmGamPoi"))

# Install helpful additions to Seurat
#renv::install("remotes")
#renv::install('Signac')
renv::install(c("satijalab/seurat-data","satijalab/azimuth","satijalab/seurat-wrappers"), quiet = TRUE)
#renv::install("satijalab/seurat-data")

# Freeze packages 
renv::snapshot()
source("R/packages.R")
source("R/functions.R")
set.seed(2025)

options(stringsAsFactors = FALSE)
allowWGCNAThreads(nThreads = 8)

tar_plan(

  # Track input files
  tar_file(cqn_file, "data/raw/log_cqn_expr"),
  tar_file(subjects_file, "data/raw/subjects_sub"),
  tar_file(samples_file, "data/raw/samples_sub"),

  # Read input files
  cqn_raw = read_rds(cqn_file),
  subjects_raw = read_rds(subjects_file),
  samples_raw = read_rds(samples_file),

  # Convert to matrix and transpose to (samples x genes)
  cqn_mat = make_cqn_matrix(cqn_raw),

  # Quick clustering of samples to look for extreme outliers
  sample_tree = hclust(dist(cqn_mat), method = "average"),

  # Select soft power threshold. This parameter adjusts the
  # strength of correlations and their contribution in network
  # construction.
  soft_powers = evaluate_softpowers(cqn_mat),
  soft_powers_plot = plot_softpowers(soft_powers),
  soft_powers_plot_pdf = ggsave("figures/soft_powers.pdf", soft_powers_plot, width = 7, height = 3),

  # Construct network in blockwise fashion for computational
  # simplicity and resource limitations. Ideally, will construct
  # in a singluar block (i.e. the full gene set)
  net = blockwiseModules(
    cqn_mat,
    power = 16,
    corType = "bicor",
    networkType = "signed",
    TOMType = "signed",
    minModuleSize = 30,
    mergeCutHeight = 0.25,
    numericLabels = TRUE,
    pamRespectsDendro = FALSE,
    saveTOMs = FALSE,
    verbose = 3
  ),

  # Construct merged covariates for plotting
  covars = samples_raw %>%
    left_join(subjects_raw, by = "SUBJID"),

  # Extract module eigengenes and merge with covars
  module_eigengenes = get_module_eigengenes(net, covars),
  
  # Summarise batch variables for module association
  # Due to large number of levels, will convert batch varaibles
  # to a numeric matrix and reduce with PCA. The top N components
  # will be correlated with modules to assess batch effect correlations.
  batch_pcs = get_batch_pcs(covars),

  # Format trait matrix for correlation with eigengenes
  trait_mat = format_traits_for_cor(covars, batch_pcs),

  # Compute module-trait correlations
  eg_trait_cor = correlate_module_traits(net$MEs, trait_mat),

  # Plot module visuals
  eg_cor_heatmap = correlate_modules(net$MEs),

  module_sizes = plot_module_size(net$colors),
  module_sizes_pdf = ggsave("figures/module_sizes.pdf", module_sizes, width = 6, height = 4),

  eg_boxplot = plot_eg_boxplot(module_eigengenes),
  eg_boxplot_pdf = ggsave("figures/eg_boxplot.pdf", eg_boxplot, width = 8, height = 6),

  eg_trait_cor_heatmap = plot_eg_trait_cor_heatmap(eg_trait_cor),
  eg_trait_cor_heatmap_pdf = ggsave("figures/eg_trait_cor_heatmap.pdf", eg_trait_cor_heatmap, width = 5, height = 4),

)
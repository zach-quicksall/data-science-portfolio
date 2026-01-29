source("R/packages.R")
source("R/functions.R")
set.seed(2025)

n_workers <- max(1, parallel::detectCores() - 1)
future::plan(future::multicore, workers = n_workers)

tar_plan(

  # Track input files
  tar_file(count_file, "data/raw/GTEx_Analysis_2025-08-22_v11_RNASeQCv2.4.3_gene_reads.parquet"),
  tar_file(sample_file, "data/raw/GTEx_Analysis_v11_Annotations_SampleAttributesDS.txt"),
  tar_file(subject_file, "data/raw/GTEx_Analysis_v11_Annotations_SubjectPhenotypesDS.txt"),

  # Load input files
  counts_raw = arrow::read_parquet(count_file),
  samples_raw = read_delim(sample_file, delim = "\t"),
  subjects_raw = read_delim(subject_file, delim = "\t"),

  # Subset available samples to an illustrative group to
  # reduce computational burden (i.e. Brain vs. Blood Vessel). Will also use
  # the data dictionaries to restrict sample metadata and
  # subject covariates to only those needed for this analysis.
  subjects_sub = clean_subjects(subjects_raw),
  samples_sub = clean_samples(samples_raw),
  counts_sub = clean_counts(counts_raw, samples_sub),

  # Get ENSG IDs and gene information
  # GC-content  and length are unrelibly populating.
  # Will compute ourselves.
  gene_info = get_annotations(counts_sub),

  # Add GC content and gene length
  # Note: Gene length was approximated using genomic span rather than
  # exonic length. Exonic length better-reflects effective transcript
  # length, but genomic span is sufficient for demonstrating CQN normalization
  # and bias reduction in this analysis.
  annot = add_gc_content(gene_info),

  # Subset to protein-coding genes, align inputs, and run CQN
  cqn_fit = fit_cqn(counts_sub, annot),

  # Extract log-scale CQN expression for analysis
  log_cqn = as_tibble(cqn_fit$y + cqn_fit$offset, rownames = "ensg_id"),

  # Plot CQN densities and select threshold for expression.
  # This is typically the point of convergence of the individual
  # density tracings.
  log_cqn_plot = plot_cqn(log_cqn),
  log_cqn_plot_pdf = ggsave("figures/cqn_distribution.pdf", log_cqn_plot, width = 5, height = 4),

  # Subset to expressed genes for differential expression analysis.
  # Will define "expressed" as median gene expression >= threshold
  log_cqn_expr = subset_cqn(log_cqn, 1.5),

  # Merge phenotypes and covariates with expression data for DEG
  deg_input = prep_deg(log_cqn_expr, samples_sub, subjects_sub),

  # Run DEG analysis
  # expr ~ SMTS + SMRIN + SMNABTCH + SMGEBTCH + SEX + AGE
  # SMTS:     Sample tissue type (Brain or Vascular)
  # SMRIN:    Sample RIN (i.e. RNA integrity number)
  # SMNABTCH: Sample extraction batch
  # MGEBTCH:  Sample sequencing batch
  # SEX:      Subject sex
  # AGE:      Subject age (approx)
  deg_res = run_deg(deg_input),

  # Annotate results
  deg_res_annot = deg_res %>%
    mutate(q.value = p.adjust(p.value, method = "fdr"),
           signif = ifelse((q.value < 0.05) & (abs(estimate) >= 1.5), TRUE, FALSE)) %>%
    left_join(annot, by = c("ensg_id" = "ensembl_gene_id")),

  # Knit report.qmd
  final_report = NA,

)
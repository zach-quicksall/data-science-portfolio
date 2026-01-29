clean_samples <- function(samples_df) {

  samples_cols <- c("SAMPID","SMRIN","SMNABTCH","SMGEBTCH","SMTS","SMTSD")
  
  samples_df <- samples_df %>%
    select(all_of(samples_cols)) %>%
    filter(SMTS %in% c("Brain", "Blood Vessel")) %>%
    mutate(SUBJID = str_extract(SAMPID, "^GTEX-[^-]+"),
           SMRIN = as.numeric(SMRIN),
           SMNABTCH = as.factor(SMNABTCH),
           SMGEBTCH = as.factor(SMGEBTCH),
           SMTS = as.factor(SMTS))

  return(samples_df)

}

clean_subjects <- function(subjects_df) {

  subjects_cols <- c("SUBJID","SEX","AGE")

  subjects_df <- subjects_df %>%
    select(all_of(subjects_cols)) %>%
    mutate(SEX = as.factor(SEX), # Convert to categorical variable
           AGE = as.numeric(as.numeric(str_extract(AGE, "^\\d+")))) # Convert to numeric representation. Will take first age for simplicity due to censoring on the public data.

  return(subjects_df)

}

clean_counts <- function(counts_df, samples_df) {

  counts_df <- counts_df %>%
    select(Name, Description, any_of(samples_df$SAMPID)) %>%
    mutate(Name = sub("\\..*$", "", Name))

  return(counts_df)

}

get_annotations <- function(counts_df) {

  ## Define mart
  ensembl_mart <- useEnsembl(biomart = "ensembl",
                             dataset = "hsapiens_gene_ensembl",
                             mirror  = "useast")

  ## Query mart for annotations
  annot <- getBM(
    attributes = c("ensembl_gene_id",
                   "chromosome_name",
                   "start_position",
                   "end_position",
                   "percentage_gene_gc_content",
                   "gene_biotype"),
      filters = "ensembl_gene_id",
      values = counts_df$Name,
      mart = ensembl_mart)

  return(annot)

}

add_gc_content <- function(annot) {

  chr_to_keep <- c(as.character(1:22), "X", "Y")

  # Restrict annotations to valid chromosomes and positions before adding GC content
  annot <- annot %>%
    filter(chromosome_name %in% chr_to_keep,
           start_position > 0,
           end_position >= start_position)
  
  # Define ranges of the genome
  gr <- GRanges(seqnames = paste0("chr", annot$chromosome_name),
                ranges = IRanges(start = annot$start_position,
                                 end = annot$end_position)
               )
  
  # Get sequence content
  seqs <- getSeq(BSgenome.Hsapiens.UCSC.hg38, gr)
  
  gc_mat <- letterFrequency(seqs, letters = c("G","C"), as.prob = TRUE)

  # Add GC content
  annot$gc_pct <- rowSums(gc_mat) * 100

  # Add gene length
  annot$gene_length <- width(gr)

  return(annot)

}

fit_cqn <- function(counts_df, annot_df) {

  # Convert counts to matrix form
  counts_mat <- as.matrix(counts_df %>% select(-Name, -Description))
  rownames(counts_mat) <- counts_df$Name

  # Subset annotations to protein-coding genes only
  annot_df <- annot_df %>% filter(gene_biotype == "protein_coding")

  # Arrange annotations in order of counts_mat rownames
  idx <- match(rownames(counts_mat), annot_df$ensembl_gene_id)
  gc_aligned   <- annot_df$gc_pct[idx]
  len_aligned  <- annot_df$gene_length[idx]

  # Subset vectors and matrix to only common genes 
  keep <- !is.na(gc_aligned) & !is.na(len_aligned) & is.finite(gc_aligned) & is.finite(len_aligned) & len_aligned > 0
  counts_cqn <- counts_mat[keep, , drop = FALSE]
  gc_cqn <- gc_aligned[keep]
  len_cqn <- len_aligned[keep]

  # Run CQN normalization
  cqn_fit <- cqn(
    counts  = counts_cqn,
    x       = gc_cqn,
    lengths = len_cqn,
    verbose = FALSE
  )
  
  # Return CQN object
  return(cqn_fit)

}

plot_cqn <- function(cqn_df) {

  p <- cqn_df %>%
    pivot_longer(-ensg_id, names_to = "sample_id", values_to = "expr") %>%
    ggplot(aes(x = expr, color = sample_id)) +
    geom_density() +
    geom_vline(xintercept = 1.5, linetype = "dashed") +
    theme_minimal() +
    theme(legend.position = "none")

  return(p)

}

subset_cqn <- function(cqn_df, thresh = 1.5) {

  # Median expression per gene
  median_expr <- cqn_df %>%
    pivot_longer(-ensg_id, names_to = "sample_id", values_to = "expr") %>%
    group_by(ensg_id) %>%
    summarise(median = median(expr, na.rm = TRUE))
  
  # Filter to expressed genes via threshold
  expr_genes <- median_expr %>%
    filter(median >= thresh)

  # Subset CQN 
  expr_df <- cqn_df %>%
    inner_join(select(expr_genes, ensg_id), by = "ensg_id")

  # Return
  return(expr_df)

}

prep_deg <- function(cqn_df, samples_df, subjects_df) {

  merged_df <- cqn_df %>%
    pivot_longer(-ensg_id, names_to = "SAMPID", values_to = "expr") %>%
    left_join(samples_df %>% select(SAMPID, SUBJID, SMRIN, SMNABTCH, SMGEBTCH, SMTS), by = "SAMPID") %>%
    left_join(subjects_df, by = "SUBJID") %>%
    drop_na()

  return(merged_df)

}

run_deg <- function(input_df) {

  # Subsample genes for ease of run
  genes_to_keep <- input_df %>%
    select(ensg_id) %>%
    distinct() %>%
    slice_sample(n = 500)

  input_df <- input_df %>%
    inner_join(genes_to_keep, by = "ensg_id")

  # Run DEG
  deg_results <- input_df %>%
    group_split(ensg_id) %>%
    furrr::future_map_dfr(
      ~ {
        gene_id <- unique(.x$ensg_id)
        fit <- lm(expr ~ SMTS + SMRIN + SMNABTCH + SMGEBTCH + SEX + AGE, data = .x)
        broom::tidy(fit, conf.int = TRUE) %>%
          dplyr::filter(term == "SMTSBrain") %>%
          mutate(ensg_id = gene_id)
      }
    )

  deg_results
}

render_report <- function() {

# TODO: Automated rendering of the QMD report

}

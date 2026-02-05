make_cqn_matrix <- function(cqn_df) {

  cqn_mat <- cqn_df %>%
    select(-ensg_id) %>%
    as.matrix() %>%
    t()

  colnames(cqn_mat) <- cqn_df$ensg_id

  return(cqn_mat)

}

evaluate_softpowers <- function(cqn_mat) {

  powers <- c(1:10, seq(12, 30, by = 2))

  sft <- pickSoftThreshold(
    cqn_mat,
    powerVector = powers,
    networkType = "signed",
    verbose = 0
  )

  return(sft)

}

plot_softpowers <- function(sft) {
  
  plot_df <- sft$fitIndices

  p1 <- plot_df %>%
    ggplot(aes(x = Power, y = SFT.R.sq)) +
    geom_point(color = "dodgerblue") +
    geom_line(color = "dodgerblue") +
    geom_hline(yintercept = 0.8, color = "black", linetype = "dashed") +
    labs(x = "Soft Power Threshold", y = "Scale-Free Topology Fit (R^2)") +
    theme_minimal()

  p2 <- plot_df %>%
    ggplot(aes(x = Power, y = mean.k.)) +
    geom_point(color = "darkorange") +
    geom_line(color = "darkorange") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    labs(x = "Soft Power Threshold", y = "Average Connectivity") +
    theme_minimal()

  p3 <- cowplot::plot_grid(p1, p2, nrow = 1)

  return(p3)

}

get_module_eigengenes <- function(net, covars) {

  mes <- as_tibble(net$MEs, rownames = "SAMPID") %>%
    left_join(covars, by = "SAMPID")

  return(mes)

}

get_batch_pcs <- function(covars) {

  # Select batch variables, fill missing value with explicit label to
  # prevent dropping of rows and downstream mis-alignment
  batch_traits <- covars %>%
    select(SAMPID, SMNABTCH, SMGEBTCH) %>%
    mutate(SMNABTCH = factor(replace_na(as.character(SMNABTCH), "Unknown")),
           SMGEBTCH = factor(replace_na(as.character(SMGEBTCH), "Unknown")))
  
  # Convert to one-hot representation
  one_hot_traits <- model.matrix(~ SMNABTCH + SMGEBTCH - 1, data = batch_traits)
  rownames(one_hot_traits) <- batch_traits$SAMPID

  batch_pca <- prcomp(one_hot_traits, center = TRUE, scale. = FALSE)

  batch_pcs <- broom::tidy(batch_pca) %>%
    rename(SAMPID = row)

  return(batch_pcs)

}

format_traits_for_cor <- function(covars, batch_pcs) {

  # Subset covariates and convert for correlation
  covars_sub <- covars %>%
    select(SAMPID, SMTS, AGE, SEX, SMRIN) %>%
    mutate(SMTS = as.numeric(SMTS) - 1,
           SEX = as.numeric(SEX) - 1)

  # Select batch PCs for correlation
  batch_pcs_sub <- batch_pcs %>%
    filter(PC <= 3) %>%
    pivot_wider(id_cols = SAMPID,
                names_from = PC,
                values_from = value,
                names_prefix = "PC",
                names_sep = "")

  # Construct final matrix
  trait_mat <- covars_sub %>%
    left_join(batch_pcs_sub, by = "SAMPID") %>%
    select(-SAMPID) %>%
    as.matrix()
  rownames(trait_mat) <- covars_sub$SAMPID

  # Return matrix
  return(trait_mat)

}

correlate_modules <- function(mes) {

  p <- cor(mes) %>%
    pheatmap::pheatmap(
      border_color = NA,
      main = "Module eigengene correlation"
    )
  
  return(p)

}

correlate_module_traits <- function(mes, traits) {

  # Order traits to match MEs
  traits <- traits[rownames(mes), , drop = FALSE]

  # Run correlation
  cor_res <- cor(mes, traits, method = "pearson")

  cor_res <- cor_res %>%
    as_tibble(rownames = "module")

  # Return
  return(cor_res)

}

plot_module_size <- function(mod_colors) {

  plot_df <- table(mod_colors) %>%
    as.data.frame() %>%
    filter(mod_colors != 0)

  p <- plot_df %>%
    ggplot(aes(x = mod_colors, y = Freq, fill = mod_colors)) +
    geom_bar(stat = "identity") +
    labs(x = "Module", y = "Module Size (N)") +
    theme_minimal() +
    theme(legend.position = "none")

  return(p)

}

plot_eg_boxplot <- function(mes) {

  plot_df <- mes %>%
    pivot_longer(starts_with("ME"), names_to = "module", values_to = "value") %>%
    mutate(module = factor(module, levels = paste0("ME", seq(0,20))))

  p <- plot_df %>%
    ggplot(aes(x = SMTS, y = value, fill = SMTS)) +
    geom_boxplot(outlier.alpha = 0.2) +
    scale_fill_manual(values = c("darkorange","dodgerblue")) +
    facet_wrap(~ module, scales = "free") +
    labs(x = "Tissue", y = "Module Expression") +
    theme_minimal() +
    theme(legend.position = "none")

  return(p)

}

plot_eg_trait_cor_heatmap <- function(eg_trait_cor) {

  plot_df <- eg_trait_cor %>%
    pivot_longer(-module, names_to = "trait", values_to = "cor") %>%
    mutate(trait = factor(trait, levels = c("SMTS", "AGE", "SEX", "SMRIN", "PC1", "PC2", "PC3")),
           module = factor(module, levels = paste0("ME", seq(0,20))))

  p <- plot_df %>%
    ggplot(aes(x = trait, y = module)) +
    geom_tile(aes(fill = cor)) +
    scale_fill_gradient2() +
    labs(x = "Trait", y = "Module", fill = "Correlation") +
    theme_minimal() +
    theme(legend.position = "bottom")

  return(p)

}


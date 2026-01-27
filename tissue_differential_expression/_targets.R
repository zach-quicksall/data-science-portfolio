source("R/packages.R")
source("R/functions.R")
set.seed(2025)

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
  # reduce computational burden (i.e. Brain). Will also use
  # the data dictionaries to restrict samples metadata and
  # subject covariates to only those needed for this analysis.
  samples_cols = c("SAMPID","SMRIN","SMNABTCH","SMGEBTCH","SMTS","SMTSD"),
  subjects_cols = c("SUBJID","SEX","AGE"),

  subjects_sub = subjects_raw %>% select(all_of(subjects_cols)),
  samples_sub = samples_raw %>%
    select(all_of(samples_cols)) %>%
    filter(SMTS == "Brain"),
  
  # TODO: Subset counts data
  

)
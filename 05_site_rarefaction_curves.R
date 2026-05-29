script_dir <- {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
}
source(file.path(script_dir, "_shared.R"), encoding = "UTF-8")

ensure_packages(c("vegan"))

project_dir <- project_dir_from_script(script_dir)
data_dir <- data_dir_from_env(project_dir)
output_dir <- output_dir_from_env(project_dir, "site_rarefaction_curves")

raw_otu_file <- Sys.getenv("RAREFACTION_RAW_FILE", unset = file.path(data_dir, "rarefaction_raw.csv"))
cmbl_otu_file <- Sys.getenv("RAREFACTION_CMBL_FILE", unset = file.path(data_dir, "rarefaction_cmbl.csv"))
ncbi_otu_file <- Sys.getenv("RAREFACTION_NCBI_FILE", unset = file.path(data_dir, "rarefaction_ncbi.csv"))

read_otu_table <- function(file) {
  otu <- read_csv_checked(file, row.names = 1, check.names = FALSE)
  otu <- as.matrix(otu)
  storage.mode(otu) <- "numeric"
  otu[is.na(otu)] <- 0
  otu <- round(otu)

  otu_site <- t(otu)
  otu_site <- otu_site[rowSums(otu_site) > 0, , drop = FALSE]
  otu_site <- otu_site[, colSums(otu_site) > 0, drop = FALSE]

  otu_site
}

plot_rarecurve <- function(otu_site, title_text, line_col) {
  vegan::rarecurve(
    otu_site,
    step = 1000,
    label = FALSE,
    col = grDevices::adjustcolor(line_col, alpha.f = 0.35),
    xlab = "Sequencing depth (reads)",
    ylab = "Observed OTUs",
    main = title_text,
    lwd = 0.8
  )
}

otu_raw <- read_otu_table(raw_otu_file)
otu_cmbl <- read_otu_table(cmbl_otu_file)
otu_ncbi <- read_otu_table(ncbi_otu_file)

pdf(file.path(output_dir, "otu_rarefaction_curves_three_matrices.pdf"), width = 15, height = 5)
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))
plot_rarecurve(otu_raw, "Taxonomy-free raw OTUs", "#3B82F6")
plot_rarecurve(otu_cmbl, "CMBL-annotated OTUs", "#10B981")
plot_rarecurve(otu_ncbi, "NCBI-annotated OTUs", "#F59E0B")
dev.off()


script_dir_from_args <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)

  if (length(file_arg) > 0) {
    script_file <- sub("^--file=", "", file_arg[[1]])
    return(dirname(normalizePath(script_file, winslash = "/", mustWork = FALSE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

project_dir_from_script <- function(script_dir) {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
}

data_dir_from_env <- function(project_dir) {
  normalizePath(
    Sys.getenv("EDNA_DATA_DIR", unset = file.path(project_dir, "data")),
    winslash = "/",
    mustWork = FALSE
  )
}

output_dir_from_env <- function(project_dir, analysis_name) {
  default_dir <- file.path(project_dir, "outputs", analysis_name)
  output_dir <- normalizePath(
    Sys.getenv("EDNA_OUTPUT_DIR", unset = default_dir),
    winslash = "/",
    mustWork = FALSE
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir
}

ensure_packages <- function(packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Install the following R packages before running this script: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

read_csv_checked <- function(path, ...) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }

  read.csv(path, stringsAsFactors = FALSE, ...)
}

read_numeric_matrix_csv <- function(path) {
  data <- read_csv_checked(path, row.names = 1, check.names = FALSE)
  data[] <- lapply(data, function(x) as.numeric(as.character(x)))
  data[is.na(data)] <- 0
  data
}

clean_character_set <- function(x) {
  x <- trimws(as.character(x))
  unique(x[!is.na(x) & nzchar(x)])
}

align_matrix <- function(data, row_ids, col_ids) {
  out <- matrix(
    0,
    nrow = length(row_ids),
    ncol = length(col_ids),
    dimnames = list(row_ids, col_ids)
  )

  common_rows <- intersect(row_ids, rownames(data))
  common_cols <- intersect(col_ids, colnames(data))

  if (length(common_rows) > 0 && length(common_cols) > 0) {
    out[common_rows, common_cols] <- as.matrix(data[common_rows, common_cols, drop = FALSE])
  }

  as.data.frame(out, check.names = FALSE)
}

rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2, na.rm = TRUE))
}


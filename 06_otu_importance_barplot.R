script_dir <- {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
}
source(file.path(script_dir, "_shared.R"), encoding = "UTF-8")

ensure_packages(c("dplyr", "ggplot2", "RColorBrewer"))

project_dir <- project_dir_from_script(script_dir)
data_dir <- data_dir_from_env(project_dir)
output_dir <- output_dir_from_env(project_dir, "otu_importance_barplot")

importance_file <- Sys.getenv("OTU_IMPORTANCE_FILE", unset = file.path(data_dir, "otu_importance_table.tsv"))

read_importance_table <- function(path) {
  if (file.exists(path)) {
    return(read.delim(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (.Platform$OS.type == "windows") {
    message("Input file not found; trying to read tab-delimited data from the Windows clipboard.")
    return(read.delim("clipboard", header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE))
  }

  stop("Input file not found: ", path, call. = FALSE)
}

make_class_palette <- function(classes) {
  nature_colors <- c(
    Gastropoda = "#E64B35",
    Bivalvia = "#4DBBD5",
    Insecta = "#00A087",
    Clitellata = "#3C5488",
    Malacostraca = "#F39B7F",
    norank = "#8491B4",
    Neogastropoda = "#DC0000",
    Unionida = "#7E6148",
    Veneroida = "#B09C85"
  )

  class_colors <- nature_colors[names(nature_colors) %in% classes]
  new_classes <- classes[!classes %in% names(nature_colors)]

  if (length(new_classes) > 0) {
    palette_function <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
    extra_colors <- setNames(palette_function(length(new_classes)), new_classes)
    class_colors <- c(class_colors, extra_colors)
  }

  class_colors
}

otu_table <- read_importance_table(importance_file)

required_columns <- c("OTU", "Mean_importance_score", "Class", "Species")
missing_columns <- setdiff(required_columns, names(otu_table))
if (length(missing_columns) > 0) {
  stop("Importance table is missing columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

plot_data <- otu_table |>
  dplyr::select(OTU, Mean_importance_score, Class, Species) |>
  dplyr::rename(Importance = Mean_importance_score) |>
  dplyr::mutate(Latin_name = as.character(Species)) |>
  dplyr::arrange(Importance) |>
  dplyr::mutate(OTU = factor(OTU, levels = OTU))

class_colors <- make_class_palette(unique(plot_data$Class))

importance_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Importance, y = OTU, fill = Class)) +
  ggplot2::geom_col(width = 0.8) +
  ggplot2::geom_text(
    ggplot2::aes(x = Importance, label = Latin_name),
    hjust = -0.05,
    size = 3,
    fontface = "italic",
    family = "serif"
  ) +
  ggplot2::scale_fill_manual(values = class_colors) +
  ggplot2::labs(
    x = "Mean importance score",
    y = "",
    title = "",
    fill = "Class"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 7),
    axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
    axis.title.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
    legend.position = "right",
    legend.title = ggplot2::element_text(face = "bold", size = 10),
    legend.text = ggplot2::element_text(size = 8, face = "italic"),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_line(color = "gray90"),
    plot.margin = ggplot2::margin(t = 5, r = 70, b = 5, l = 5, unit = "pt")
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, max(plot_data$Importance) * 1.45),
    expand = c(0, 0)
  )

plot_height <- max(6, nrow(plot_data) * 0.22)

ggplot2::ggsave(
  file.path(output_dir, "otu_importance_barplot.png"),
  plot = importance_plot,
  width = 12,
  height = plot_height,
  dpi = 300
)

ggplot2::ggsave(
  file.path(output_dir, "otu_importance_barplot.pdf"),
  plot = importance_plot,
  width = 12,
  height = plot_height,
  dpi = 300
)

summary_table <- data.frame(
  Total_OTUs = nrow(plot_data),
  Class_count = length(unique(plot_data$Class))
)

write.csv(summary_table, file.path(output_dir, "otu_importance_summary.csv"), row.names = FALSE)
write.csv(as.data.frame(table(plot_data$Class)), file.path(output_dir, "otu_class_counts.csv"), row.names = FALSE)

print(summary_table)


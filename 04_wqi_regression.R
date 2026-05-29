script_dir <- {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
}
source(file.path(script_dir, "_shared.R"), encoding = "UTF-8")

ensure_packages(c("dplyr", "ggplot2", "ggpubr", "gridExtra", "irr"))

project_dir <- project_dir_from_script(script_dir)
data_dir <- data_dir_from_env(project_dir)
output_dir <- output_dir_from_env(project_dir, "wqi_regression_and_kappa")

prediction_file <- Sys.getenv(
  "WQI_PREDICTION_FILE",
  unset = file.path(data_dir, "未注释-WQI后25th预测值-前200重要OTU.csv")
)

wqi_env_file <- Sys.getenv("WQI_ENV_FILE", unset = file.path(data_dir, "WQI-理化(删0).csv"))
env_file <- Sys.getenv("ENV_FILE", unset = file.path(data_dir, "理化(删0).csv"))
classification_scheme <- Sys.getenv("WQI_CLASSIFICATION_SCHEME", unset = "none")

plot_prediction_scatter <- function(data, output_dir) {
  r2 <- cor(data$Observed_WQI, data$Predicted_WQI)^2
  rmse_value <- rmse(data$Observed_WQI, data$Predicted_WQI)

  plot <- ggpubr::ggscatter(
    data,
    x = "Observed_WQI",
    y = "Predicted_WQI",
    size = 2,
    color = "#2e2e2e",
    alpha = 0.7,
    add = "reg.line",
    add.params = list(
      color = "#2c91e0",
      fill = "lightblue",
      alpha = 0.3,
      size = 1.2
    ),
    conf.int = TRUE
  ) +
    ggplot2::annotate(
      "text",
      x = 40,
      y = 90,
      label = paste0("R2 = ", round(r2, 3), "\nRMSE = ", round(rmse_value, 3), "\np < 0.001"),
      size = 5,
      hjust = 0
    ) +
    ggplot2::xlab("Measured WQI") +
    ggplot2::ylab("Predicted WQI") +
    ggplot2::coord_cartesian(xlim = c(40, 100), ylim = c(40, 100)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      aspect.ratio = 1,
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1.2),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12)
    )

  metrics <- data.frame(
    R2 = r2,
    RMSE = rmse_value
  )

  ggplot2::ggsave(
    file.path(output_dir, "wqi_measured_vs_predicted.pdf"),
    plot,
    height = 6,
    width = 6,
    dpi = 300
  )

  write.csv(metrics, file.path(output_dir, "wqi_prediction_metrics.csv"), row.names = FALSE)

  list(plot = plot, metrics = metrics)
}

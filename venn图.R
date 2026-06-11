library (ggVennDiagram)  
library(ggplot2)
library(ggvenn)
library(eulerr)
library(grid)

# =========================
# load data
# =========================
set1 <- read.csv("CMBL_family_data.csv", header = TRUE)
set2 <- read.csv('NCBI_family_data.csv', header = TRUE)
set3 <- read.csv('morphology_family_data.csv', header = TRUE)

CMBL_families <- set1$family %>%
  na.omit() %>%
  trimws() %>%
  unique()

NCBI_families <- set2$family %>%
  na.omit() %>%
  trimws() %>%
  unique()

Morphology_families <- set3$family %>%
  na.omit() %>%
  trimws() %>%
  unique()

family_data <- list(
  CMBL = CMBL_families,
  NCBI = NCBI_families,
  Morphology = Morphology_families
)

fit <- euler(family_data)

print(fit)

plot(
  fit,
  fills = list(
    fill = c("#E68D3D", "#E26472", "#6270B7"),
    alpha = 0.55
  ),
    edges = list(
    col = "grey25",
    lwd = 1.2
  ),
  labels = list(
    font = 2,
    fontsize = 15,
    col = "grey10"
  ),
  quantities = list(
    fontsize = 14,
    font = 2,
    col = "grey10"
  ),
   legend = FALSE,
  main = NULL
)
############################################################
## Export high-resolution figure
############################################################

pdf(
  file = "Family_level_area_proportional_Venn.pdf",
  width = 7,
  height = 6
)

plot(
  fit,
  fills = list(
    fill = c("#E68D3D", "#E26472", "#6270B7"),
    alpha = 0.55
  ),
  edges = list(
    col = "grey25",
    lwd = 1.2
  ),
  labels = list(
    font = 2,
    fontsize = 15,
    col = "grey10"
  ),
  quantities = list(
    fontsize = 14,
    font = 2,
    col = "grey10"
  ),
  legend = FALSE,
  main = NULL
)

dev.off()


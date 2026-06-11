library (ggVennDiagram)  
library(ggplot2)
library(ggvenn)

library(eulerr)
library(grid)
# 加载必要的包
library(ggvenn)

library(ggVennDiagram)
library(ggplot2)

# 设置工作目录
setwd("E:/博1~博2/不同数据库对比")

############# 读取数据#########
set1 <- read.csv("CMBL科级数据.csv", header = TRUE)
set2 <- read.csv('NCBI科级数据.csv', header = TRUE)
set3 <- read.csv('浙江形态学科级数据.csv', header = TRUE)
# 提取家族名称（不需要转置）
CMBL_families <- set1$family
NCBI_families <- set2$family
Morphology_families <- set3$family

# 创建数据列表
family_data <- list(
  CMBL = CMBL_families,
  NCBI = NCBI_families,
  Morphology = Morphology_families
)
#绘图
# 绘制韦恩图（去除边框，只显示数字）
ggvenn(
  family_data,
  fill_color = c("#E68D3D", "#E26472", "#6270B7"),
  stroke_size = 0,  # 设置为0去除黑色边框
  stroke_linetype = 0,  # 去除线条类型
  set_name_size = 5,
  text_size = 5,
  show_percentage = FALSE  # 不显示百分比，只显示数字
) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_blank()  # 设置背景为空白
  )




############################################################
## 等比放大缩小
## CMBL vs NCBI vs Morphology family-level comparison
############################################################

library(eulerr)
library(dplyr)
library(grid)

# 设置工作目录
setwd("E:/博1~博2/不同数据库对比")

# =========================
# 1. 读取数据
# =========================
set1 <- read.csv("CMBL科级数据.csv", header = TRUE, stringsAsFactors = FALSE)
set2 <- read.csv("NCBI科级数据.csv", header = TRUE, stringsAsFactors = FALSE)
set3 <- read.csv("浙江形态学科级数据.csv", header = TRUE, stringsAsFactors = FALSE)

# =========================
# 2. 提取 family 名称
#    去除 NA、空值和重复值
# =========================
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

# =========================
# 3. 创建集合列表
# =========================
family_data <- list(
  CMBL = CMBL_families,
  NCBI = NCBI_families,
  Morphology = Morphology_families
)

# =========================
# 4. 拟合等比例面积 Euler / Venn 图
# =========================
fit <- euler(family_data)

# 查看拟合效果
print(fit)

# =========================
# 5. 高分期刊风格绘图
# =========================
plot(
  fit,
  
  # 填充颜色：柔和、低饱和，适合论文
  fills = list(
    fill = c("#E68D3D", "#E26472", "#6270B7"),
    alpha = 0.55
  ),
  
  # 圆圈边框
  edges = list(
    col = "grey25",
    lwd = 1.2
  ),
  
  # 集合名称
  labels = list(
    font = 2,
    fontsize = 15,
    col = "grey10"
  ),
  
  # 区域数字
  quantities = list(
    fontsize = 14,
    font = 2,
    col = "grey10"
  ),
  
  # 不显示百分比，只显示数量
  legend = FALSE,
  
  # 背景
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


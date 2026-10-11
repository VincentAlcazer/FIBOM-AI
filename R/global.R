#' Global parameters

#  library(BiocManager)
# options(repos = BiocManager::repositories())

#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom caret predict.train
#' @importFrom xgboost xgb.DMatrix


########## ========== Parameters
# pal <- MetBrewer::met.brewer("Hiroshige", 3, direction = -1)
#
# MetBrewer::met.brewer("Hiroshige", 10, direction = -1)


########## ========== Data

### === Model & predictions

data <- readRDS("data/251023_FIBOMAI_fibrosis_df.rds")

fibrosis_model <- readRDS("data/251023_FIBOMAI_XGB_model.rds")

pred_all <- readRDS("data/251023_pred_all.rds")

uncertainty_lookup <- readRDS("data/uncertainty_lookup.rds")

### === Params df

params_df <- data.frame(
  sen_threshold = 0.1166833,
  spe_threshold = 0.8925802
)

### === Datasets

### === Base PCA
training_data <- data %>%
  # Subset variables from final model
  select(Fibrosis = response, all_of(fibrosis_model$finalModel$xNames))

### === Training data pCA
## Get components
pca <- prcomp(as.matrix(training_data[,-1]), scale=T, center = T)

## Base plot
pca_plot <- pca$x[,1:2] %>% data.frame(check.names = F) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_density_2d(aes(color = after_stat(level)), linewidth = 1.5, bins = 25, alpha = 0.75) +
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(title = "Density plot - reference data")

### === Input ranges (outlier warnings)
## Sidebar input id -> column of the reference data (same units as the input)
input_vars <- c(
  age           = "BM_biopsy_age",
  hemoglobin    = "hemoglobine",
  hematocrite   = "hematocrite",
  hematies      = "hematies",
  MCV           = "vgm",
  MCH           = "tcmh",
  MCHC          = "ccmh",
  IDR           = "idr_cv",
  platelets     = "plaquettes",
  mpv           = "vpm",
  WBC           = "leucocytes",
  ANC           = "poly_neutrophiles",
  ALC           = "lymphocytes",
  mono          = "monocytes",
  eosino        = "poly_eosinophiles",
  baso          = "poly_basophiles",
  erythroblasts = "erythroblastes_percent",
  myelemia      = "myelemie_percent",
  myeloblasts   = "myeloblastes_percent"
)

input_labels <- c(
  age           = "Age (years)",
  hemoglobin    = "Hemoglobin (g/L)",
  hematocrite   = "Hematocrit (%)",
  hematies      = "RBC (10^12/L)",
  MCV           = "MCV (fL)",
  MCH           = "MCH (pg)",
  MCHC          = "MCHC (g/L)",
  IDR           = "RDW-CV (%)",
  platelets     = "Platelets (10^9/L)",
  mpv           = "MPV (fL)",
  WBC           = "Leukocytes (10^9/L)",
  ANC           = "Absolute neutrophils (10^9/L)",
  ALC           = "Absolute lymphocytes (10^9/L)",
  mono          = "Absolute monocytes (10^9/L)",
  eosino        = "Absolute eosinophils (10^9/L)",
  baso          = "Absolute basophils (10^9/L)",
  erythroblasts = "Erythroblasts (%)",
  myelemia      = "Myelemia (%)",
  myeloblasts   = "Myeloblasts (%)"
)

## Training ranges, checked client-side (see inst/app/www/fibomai.js)
valid_ranges <- lapply(names(input_vars), function(id) {
  rng <- range(data[[input_vars[[id]]]], na.rm = TRUE)
  list(min = signif(rng[1], 4), max = signif(rng[2], 4), label = input_labels[[id]])
})
names(valid_ranges) <- names(input_vars)


########## ========== Parameters

sidebar_split <- c("80%","20%")
checkbox_align <- c("margin-top: 20px; margin-left: -15px")

### Ggplot 2 default theme

default_theme <- theme_bw() + theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text = element_text(size = 14, color = "black"),
  axis.title = element_text(size = 16, face = "bold"),
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 14),
  strip.text.x = element_text(size = 16, face = "bold"),
  legend.position = "none"
)

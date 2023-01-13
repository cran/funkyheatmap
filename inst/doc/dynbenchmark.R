## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-data----------------------------------------------------------------
library(funkyheatmap)
library(kableExtra)

data("dynbenchmark_data")

## -----------------------------------------------------------------------------
data <- dynbenchmark_data$data
print(data[,1:12])

## -----------------------------------------------------------------------------
preview_cols <- c(
  "id",
  "method_source",
  "method_platform",
  "benchmark_overall_norm_correlation",
  "benchmark_overall_norm_featureimp_wcor",
  "benchmark_overall_norm_F1_branches",
  "benchmark_overall_norm_him",
  "benchmark_overall_overall"
)
kable(data[,preview_cols])

## -----------------------------------------------------------------------------
g <- funky_heatmap(data[,preview_cols])

## ----include=FALSE------------------------------------------------------------
# set width and height for upcoming plot
knitr::opts_chunk$set(fig.width = g$width, fig.height = g$height)

## ----heatmap_preview----------------------------------------------------------
g

## -----------------------------------------------------------------------------
column_info <- dynbenchmark_data$column_info
kable(column_info)

## -----------------------------------------------------------------------------
g <- funky_heatmap(data, column_info = column_info)

## ----include=FALSE------------------------------------------------------------
# set width and height for upcoming plot
knitr::opts_chunk$set(fig.width = g$width, fig.height = g$height)

## ----heatmap_withcolinfo------------------------------------------------------
g

## -----------------------------------------------------------------------------
column_groups <- dynbenchmark_data$column_groups
kable(column_groups)

## -----------------------------------------------------------------------------
row_info <- dynbenchmark_data$row_info
kable(row_info)

## -----------------------------------------------------------------------------
row_groups <- dynbenchmark_data$row_groups
kable(row_groups)

## -----------------------------------------------------------------------------
palettes <- dynbenchmark_data$palettes
print(palettes)

## ----make-plot----------------------------------------------------------------
g <- funky_heatmap(
  data = data,
  column_info = column_info,
  column_groups = column_groups,
  row_info = row_info,
  row_groups = row_groups,
  palettes = palettes,
  col_annot_offset = 3.2
)

## ----include=FALSE------------------------------------------------------------
# set width and height for upcoming plot
knitr::opts_chunk$set(fig.width = g$width, fig.height = g$height)

## ----heatmap------------------------------------------------------------------
g


## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries----------------------------------------------------------------
library(funkyheatmap)
library(dplyr)
library(tibble)

## ----summary-data-------------------------------------------------------------
data("scib_summary")
glimpse(scib_summary)

## ----summary-prep-------------------------------------------------------------
# A small helper function for creating rank labels for each column.
# It takes a scores, ranks them and returns a character vector with labels for
# the top 3 scores. Any additional arguments are passed to the `rank()`
# function.
label_top_3 <- function(scores, ...) {
  ranks <- rank(scores, ...)
  ifelse(ranks <= 3, as.character(ranks), "")
}

scib_summary_plot <- scib_summary |>
  # Create an ID column showing the final rank
  mutate(id = as.character(seq_len(nrow(scib_summary)))) |>
  # Set the labels for the scaling and features columns
  mutate(
    scaling = factor(
      scaling,
      levels = c("Unscaled", "Scaled"),
      labels = c("-", "+")
    ),
    features = factor(
      features,
      levels = c("Full", "HVG"),
      labels = c("FULL", "HVG")
    )
  ) |>
  # Create a column with paths to output images
  mutate(
    output_img = case_match(
      output,
      "Features"  ~ "images/matrix.png",
      "Embedding" ~ "images/embedding.png",
      "Graph"     ~ "images/graph.png"
    )
  ) |>
  # Create rank labels
  mutate(
    label_pancreas = label_top_3(rank_pancreas),
    label_lung_atlas = label_top_3(rank_lung_atlas),
    label_immune_cell_hum = label_top_3(rank_immune_cell_hum),
    label_immune_cell_hum_mou = label_top_3(rank_immune_cell_hum_mou),
    label_mouse_brain = label_top_3(rank_mouse_brain),
    label_simulations_1_1 = label_top_3(rank_simulations_1_1),
    label_simulations_2 = label_top_3(rank_simulations_2),
    package_label = label_top_3(-package_rank, ties.method = "min"),
    paper_label = label_top_3(-paper_rank, ties.method = "min"),
    time_label = label_top_3(-time_rank, ties.method = "min"),
    memory_label = label_top_3(-memory_rank, ties.method = "min")
  ) |>
  as.data.frame()

glimpse(scib_summary_plot)

## ----summary-cols-------------------------------------------------------------
column_info <- tribble(
                           ~ id,                 ~ name,  ~ geom,       ~ group, ~ palette, ~ width, ~ legend, ~ hjust, ~ overlay,
                           "id",                 "Rank",  "text",      "Method",        NA,       1,    FALSE,       0,     FALSE,
                       "method",               "Method",  "text",      "Method",        NA,       5,    FALSE,       0,     FALSE,
                   "output_img",               "Output", "image",      "Method",        NA,       1,    FALSE,     0.5,     FALSE,
                     "features",             "Features",  "text",      "Method",        NA,       2,    FALSE,     0.5,     FALSE,
                      "scaling",              "Scaling",  "text",      "Method",        NA,       1,     TRUE,     0.5,     FALSE,
             "overall_pancreas",             "Pancreas",   "bar",         "RNA",   "blues",     1.5,     TRUE,      NA,     FALSE,
               "label_pancreas",                     NA,  "text",         "RNA",        NA,      NA,    FALSE,     0.1,      TRUE,
           "overall_lung_atlas",                 "Lung",   "bar",         "RNA",   "blues",     1.5,     TRUE,      NA,     FALSE,
             "label_lung_atlas",                     NA,  "text",         "RNA",        NA,      NA,    FALSE,     0.1,      TRUE,
      "overall_immune_cell_hum",       "Immune (human)",   "bar",         "RNA",   "blues",     1.5,     TRUE,      NA,     FALSE,
        "label_immune_cell_hum",                     NA,  "text",         "RNA",        NA,      NA,    FALSE,     0.1,      TRUE,
  "overall_immune_cell_hum_mou", "Immune (human/mouse)",   "bar",         "RNA",   "blues",     1.5,     TRUE,      NA,     FALSE,
    "label_immune_cell_hum_mou",                     NA,  "text",         "RNA",        NA,      NA,    FALSE,     0.1,      TRUE,
          "overall_mouse_brain",          "Mouse brain",   "bar",         "RNA",   "blues",     1.5,     TRUE,      NA,     FALSE,
            "label_mouse_brain",                     NA,  "text",         "RNA",        NA,      NA,    FALSE,     0.1,      TRUE,
      "overall_simulations_1_1",                "Sim 1",   "bar", "Simulations",  "greens",     1.5,     TRUE,      NA,     FALSE,
        "label_simulations_1_1",                     NA,  "text", "Simulations",        NA,      NA,    FALSE,     0.1,      TRUE,
        "overall_simulations_2",                "Sim 2",   "bar", "Simulations",  "greens",     1.5,     TRUE,      NA,     FALSE,
          "label_simulations_2",                     NA,  "text", "Simulations",        NA,      NA,    FALSE,     0.1,      TRUE,
                "package_score",              "Package",   "bar",   "Usability", "oranges",     1.5,     TRUE,      NA,     FALSE,
                "package_label",                     NA,  "text",   "Usability",        NA,      NA,    FALSE,     0.1,      TRUE,
                  "paper_score",                "Paper",   "bar",   "Usability", "oranges",     1.5,     TRUE,      NA,     FALSE,
                  "paper_label",                     NA,  "text",   "Usability",        NA,      NA,    FALSE,     0.1,      TRUE,
                   "time_score",                 "Time",   "bar", "Scalability",   "greys",     1.5,     TRUE,      NA,     FALSE,
                   "time_label",                     NA,  "text", "Scalability",        NA,      NA,    FALSE,     0.1,      TRUE,
                 "memory_score",               "Memory",   "bar", "Scalability",   "greys",     1.5,     TRUE,      NA,     FALSE,
                 "memory_label",                     NA,  "text", "Scalability",        NA,      NA,    FALSE,     0.1,      TRUE,
)

column_info

## ----summary-col-groups-------------------------------------------------------
column_groups <- tribble(
        ~ group,     ~ palette,      ~ level1,
       "Method",      "Method",      "Method",
          "RNA",       "blues",         "RNA",
  "Simulations",      "greens", "Simulations",
    "Usability",     "oranges",   "Usability",
  "Scalability",       "greys", "Scalability",
)

column_groups

## ----summary-rows-------------------------------------------------------------
row_info <- data.frame(id = scib_summary_plot$id, group = NA_character_)

row_info

## ----summary-palettes---------------------------------------------------------
palettes <- tribble(
   ~ palette,                                     ~ colours,
  "features",                             c("green", "red"),
     "blues", grDevices::colorRampPalette(
                rev(RColorBrewer::brewer.pal(9, "Blues"))
              )(101),
    "greens", grDevices::colorRampPalette(
                rev(RColorBrewer::brewer.pal(9, "Greens"))
              )(101),
   "oranges", grDevices::colorRampPalette(
                rev(RColorBrewer::brewer.pal(9, "Oranges"))
              )(101),
     "greys", grDevices::colorRampPalette(
                rev(RColorBrewer::brewer.pal(9, "Greys"))
              )(101)
)

## ----summary-figure, fig.width=8, fig.height=8--------------------------------
funky_heatmap(
  scib_summary_plot,
  column_info = column_info,
  column_groups = column_groups,
  row_info = row_info,
  palettes = palettes,
  scale_column = FALSE,
  col_annot_offset = 4
)


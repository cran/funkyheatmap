## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-data----------------------------------------------------------------
library(funkyheatmap)
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

data("dynbenchmark_data")

data <- dynbenchmark_data

## ----method-grouping----------------------------------------------------------
row_info <-
  data %>%
  select(group, id)
row_groups <-
  data %>%
  transmute(
    group,
    Group = case_when(
      group == "cycle" ~ "Cyclic methods",
      TRUE ~ paste0(stringr::str_to_title(group), " methods")
    )
  ) %>%
  unique()

## ----column-info--------------------------------------------------------------
column_info <- tribble( # tribble_start
  ~group,                   ~id,                                            ~name,                      ~geom,        ~palette,         ~options,
  "method_characteristic",  "method_name",                                  "",                         "text",       NA,               list(hjust = 0, width = 6),
  "method_characteristic",  "method_priors_required_str",                   "Priors required",          "text",       NA,               list(width = 2),
  "method_characteristic",  "method_wrapper_type",                          "Wrapper type",             "text",       NA,               list(width = 2),
  "method_characteristic",  "method_platform",                              "Platform",                 "text",       NA,               list(width = 2),
  "method_characteristic",  "method_topology_inference",                    "Topology inference",       "text",       NA,               list(width = 2),
  "score_overall",          "summary_overall_overall",                      "Overall",                  "bar",        "overall",        list(width = 4, legend = FALSE),
  "score_overall",          "benchmark_overall_overall",                    "Accuracy",                 "bar",        "benchmark",      list(width = 4, legend = FALSE),
  "score_overall",          "scaling_pred_overall_overall",                 "Scalability",              "bar",        "scaling",        list(width = 4, legend = FALSE),
  "score_overall",          "stability_overall_overall",                    "Stability",                "bar",        "stability",      list(width = 4, legend = FALSE),
  "score_overall",          "qc_overall_overall",                           "Usability",                "bar",        "qc",             list(width = 4, legend = FALSE),
  "score_overall",          "control_label",                                "",                         "text",       NA,               list(overlay = TRUE),
  "benchmark_metric",       "benchmark_overall_norm_him",                   "Topology",                 "funkyrect",  "benchmark",      lst(),
  "benchmark_metric",       "benchmark_overall_norm_F1_branches",           "Branch assignment",        "funkyrect",  "benchmark",      lst(),
  "benchmark_metric",       "benchmark_overall_norm_correlation",           "Cell positions",           "funkyrect",  "benchmark",      lst(),
  "benchmark_metric",       "benchmark_overall_norm_featureimp_wcor",       "Features",                 "funkyrect",  "benchmark",      lst(),
  "benchmark_source",       "benchmark_source_real_gold",                   "Gold",                     "funkyrect",  "benchmark",      lst(),
  "benchmark_source",       "benchmark_source_real_silver",                 "Silver",                   "funkyrect",  "benchmark",      lst(),
  "benchmark_source",       "benchmark_source_synthetic_dyngen",            "dyngen",                   "funkyrect",  "benchmark",      lst(),
  "benchmark_source",       "benchmark_source_synthetic_dyntoy",            "dyntoy",                   "funkyrect",  "benchmark",      lst(),
  "benchmark_source",       "benchmark_source_synthetic_prosstt",           "PROSSTT",                  "funkyrect",  "benchmark",      lst(),
  "benchmark_source",       "benchmark_source_synthetic_splatter",          "Splatter",                 "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_cycle",                           "Cycle",                    "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_linear",                          "Linear",                   "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_bifurcation",                     "Bifurcation",              "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_convergence",                     "Convergence",              "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_multifurcation",                  "Multifurcation",           "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_tree",                            "Tree",                     "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_acyclic_graph",                   "Acyclic",                  "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_graph",                           "Connected",                "funkyrect",  "benchmark",      lst(),
  "benchmark_trajtype",     "benchmark_tt_disconnected_graph",              "Disconnected",             "funkyrect",  "benchmark",      lst(),
  "benchmark_execution",    "benchmark_overall_pct_errored_str",            "% Errored",                "text",       NA,               lst(hjust = 1),
  "benchmark_execution",    "benchmark_overall_error_reasons",              "Reason",                   "pie",        "error_reasons",  lst(),
  "scaling_predtime",       "scaling_pred_scoretime_cells1m_features100",   "1m \u00D7 100",            "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells1m_features100",   "",                         "text",       "white6black4",   lst(label = "scaling_pred_timestr_cells1m_features100", overlay = TRUE, size = 3, scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells100k_features1k",  "100k \u00D7 1k",           "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells100k_features1k",  "",                         "text",       "white6black4",   lst(label = "scaling_pred_timestr_cells100k_features1k", overlay = TRUE, size = 3, scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells10k_features10k",  "10k \u00D7 10k",           "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells10k_features10k",  "",                         "text",       "white6black4",   lst(label = "scaling_pred_timestr_cells10k_features10k", overlay = TRUE, size = 3, scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells1k_features100k",  "1k \u00D7 100k",           "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells1k_features100k",  "",                         "text",       "white6black4",   lst(label = "scaling_pred_timestr_cells1k_features100k", overlay = TRUE, size = 3, scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells100_features1m",   "100 \u00D7 1m",            "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predtime",       "scaling_pred_scoretime_cells100_features1m",   "",                         "text",       "white6black4",   lst(label = "scaling_pred_timestr_cells100_features1m", overlay = TRUE, size = 3, scale = FALSE),
  "scaling_predtime",       "benchmark_overall_time_predcor_str",           "Cor. pred. vs. real",      "text",       NA,               lst(size = 3),
  "scaling_predmem",        "scaling_pred_scoremem_cells1m_features100",    "1m \u00D7 100",            "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells1m_features100",    "",                         "text",       "white6black4",   lst(label = "scaling_pred_memstr_cells1m_features100", overlay = TRUE, size = 2, scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells100k_features1k",   "100k \u00D7 1k",           "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells100k_features1k",   "",                         "text",       "white6black4",   lst(label = "scaling_pred_memstr_cells100k_features1k", overlay = TRUE, size = 2, scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells10k_features10k",   "10k \u00D7 10k",           "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells10k_features10k",   "",                         "text",       "white6black4",   lst(label = "scaling_pred_memstr_cells10k_features10k", overlay = TRUE, size = 2, scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells1k_features100k",   "1k \u00D7 100k",           "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells1k_features100k",   "",                         "text",       "white6black4",   lst(label = "scaling_pred_memstr_cells1k_features100k", overlay = TRUE, size = 2, scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells100_features1m",    "100 \u00D7 1m",            "rect",       "scaling",        lst(scale = FALSE),
  "scaling_predmem",        "scaling_pred_scoremem_cells100_features1m",    "",                         "text",       "white6black4",   lst(label = "scaling_pred_memstr_cells100_features1m", overlay = TRUE, size = 2, scale = FALSE),
  "scaling_predmem",        "benchmark_overall_mem_predcor_str",            "Cor. pred. vs. real",      "text",       NA,               lst(size = 3),
  "stability",              "stability_him",                                "Topology",                 "funkyrect",  "stability",      lst(),
  "stability",              "stability_F1_branches",                        "Branch assignment",        "funkyrect",  "stability",      lst(),
  "stability",              "stability_correlation",                        "Cell positions",           "funkyrect",  "stability",      lst(),
  "stability",              "stability_featureimp_wcor",                    "Features",                 "funkyrect",  "stability",      lst(),
  "qc_category",            "qc_cat_availability",                          "Availability",             "funkyrect",  "qc",             lst(),
  "qc_category",            "qc_cat_behaviour",                             "Behaviour",                "funkyrect",  "qc",             lst(),
  "qc_category",            "qc_cat_code_assurance",                        "Code assurance",           "funkyrect",  "qc",             lst(),
  "qc_category",            "qc_cat_code_quality",                          "Code quality",             "funkyrect",  "qc",             lst(),
  "qc_category",            "qc_cat_documentation",                         "Documentation",            "funkyrect",  "qc",             lst(),
  "qc_category",            "qc_cat_paper",                                 "Paper",                    "funkyrect",  "qc",             lst(),
  "qc_category",            "control_label",                                "",                         "text",       NA,               list(overlay = TRUE, width = -6)
)

## ----column-groups------------------------------------------------------------
column_groups <- tribble(
  ~Experiment,    ~Category,                                      ~group,                   ~palette,
  "Method",       "\n",                                           "method_characteristic",  "overall",
  "Summary",      "Aggregated scores per experiment",             "score_overall",          "overall",
  "Accuracy",     "Per metric",                                   "benchmark_metric",       "benchmark",
  "Accuracy",     "Per dataset source",                           "benchmark_source",       "benchmark",
  "Accuracy",     "Per trajectory type",                          "benchmark_trajtype",     "benchmark",
  "Accuracy",     "Errors",                                       "benchmark_execution",    "benchmark",
  "Scalability",  "Predicted time\n(#cells \u00D7 #features)",    "scaling_predtime",       "scaling",
  "Scalability",  "Predicted memory\n(#cells \u00D7 #features)",  "scaling_predmem",        "scaling",
  "Stability",    "Similarity\nbetween runs",                     "stability",              "stability",
  "Usability",    "Quality of\nsoftware and paper",               "qc_category",            "qc"
)

## ----column-grouping----------------------------------------------------------
error_reasons <- tibble(
  name = c("pct_memory_limit", "pct_time_limit", "pct_execution_error", "pct_method_error"),
  label = c("Memory limit exceeded", "Time limit exceeded", "Execution error", "Method error"),
  colour = RColorBrewer::brewer.pal(length(name), "Set3")
)

palettes <- tribble(
  ~palette,             ~colours,
  "overall",            grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greys")[-1]))(101),
  "benchmark",          grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Blues") %>% c("#011636")))(101),
  "scaling",            grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Reds")[-8:-9]))(101),
  "stability",          grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlOrBr")[-7:-9]))(101),
  "qc",                 grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greens")[-1] %>% c("#00250f")))(101),
  "error_reasons",      error_reasons %>% select(label, colour) %>% deframe(),
  "white6black4",       c(rep("white", 3), rep("black", 7))# ,
  # "column_annotation",  c(overall = "#555555", benchmark = "#4292c6", scaling = "#f6483a", stability = "#fe9929", qc = "#41ab5d")
)

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


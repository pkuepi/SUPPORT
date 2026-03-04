rm(list = ls())

library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(dplyr)
library(tibble)
library(scales)

# ----------------------------
# Base paths (current project)
# ----------------------------
base_dir <- "/Validation/Parameters/Corr"
out_dir <- "/Validation/Figures"

# ----------------------------
# Global input settings
# ----------------------------
target_sheet_name <- "Corr"
target_vars <- c("Age", "SBP", "TC", "BMI")
file_pattern <- "\\.csv$"

# ----------------------------
# Panel definitions for Figure A/B/C
# ----------------------------
panel_configs <- list(
  list(
    panel_title = "(A) SUPPORT (East China) vs. CHERRY cohort",
    left_dir = file.path(base_dir, "CHERRY"),
    right_dir = file.path(base_dir, "East_SUPPORT"),
    subplot_titles = c(
      "CHERRY: Men (35-59 years)",
      "CHERRY: Men (60-84 years)",
      "CHERRY: Women (35-59 years)",
      "CHERRY: Women (60-84 years)",
      "SUPPORT: Men (35-59 years)",
      "SUPPORT: Men (60-84 years)",
      "SUPPORT: Women (35-59 years)",
      "SUPPORT: Women (60-84 years)"
    )
  ),
  list(
    panel_title = "(B) SUPPORT (National level) vs. CHARLS study",
    left_dir = file.path(base_dir, "CHARLS"),
    right_dir = file.path(base_dir, "SUPPORT_45"),
    subplot_titles = c(
      "CHARLS: Men (45-59 years)",
      "CHARLS: Men (60-84 years)",
      "CHARLS: Women (45-59 years)",
      "CHARLS: Women (60-84 years)",
      "SUPPORT: Men (45-59 years)",
      "SUPPORT: Men (60-84 years)",
      "SUPPORT: Women (45-59 years)",
      "SUPPORT: Women (60-84 years)"
    )
  ),
  list(
    panel_title = "(C) SUPPORT (National level) vs. CHNS study",
    left_dir = file.path(base_dir, "CHNS"),
    right_dir = file.path(base_dir, "SUPPORT_35"),
    subplot_titles = c(
      "CHNS: Men (35-59 years)",
      "CHNS: Men (60-84 years)",
      "CHNS: Women (35-59 years)",
      "CHNS: Women (60-84 years)",
      "SUPPORT: Men (35-59 years)",
      "SUPPORT: Men (60-84 years)",
      "SUPPORT: Women (35-59 years)",
      "SUPPORT: Women (60-84 years)"
    )
  )
)

# ----------------------------
# Utility functions
# ----------------------------
list_input_files <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    stop(sprintf("Input folder does not exist: %s", folder_path))
  }
  
  files <- list.files(
    path = folder_path,
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  files <- sort(files)
  
  if (length(files) == 0) {
    stop(sprintf("No input files found in: %s", folder_path))
  }
  
  files
}

import_smart_matrix <- function(file_path, sheet_name) {
  raw_data <- read.csv(file_path, check.names = FALSE)
  
  raw_data <- as.data.frame(raw_data)
  
  if (is.character(raw_data[[1]])) {
    first_col_name <- colnames(raw_data)[1]
    clean_data <- raw_data %>% column_to_rownames(var = first_col_name) %>% as.matrix()
  } else {
    clean_data <- as.matrix(raw_data)
  }
  
  clean_data
}

subset_corr_matrix <- function(full_mat, vars_to_keep) {
  mat <- as.data.frame(full_mat)
  
  # Some exports include an extra index column; remove it when detected.
  if (ncol(mat) == nrow(mat) + 1) {
    mat <- mat[, -1]
  }
  
  if (ncol(mat) != nrow(mat)) {
    stop(sprintf("Matrix is not square. nrow=%d, ncol=%d", nrow(mat), ncol(mat)))
  }
  
  rownames(mat) <- colnames(mat)
  mat <- as.matrix(mat)
  
  missing_vars <- setdiff(vars_to_keep, colnames(mat))
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing variables: %s", paste(missing_vars, collapse = ", ")))
  }
  
  mat[vars_to_keep, vars_to_keep]
}

create_heatmap <- function(corr_mat, title_text, show_x_text = TRUE, show_y_text = TRUE) {
  my_breaks <- seq(-0.2, 0.5, by = 0.1)
  my_limits <- c(-0.2, 0.5)
  
  p <- ggcorrplot(
    corr_mat,
    method = "square",
    type = "lower",
    show.diag = FALSE,
    lab = FALSE,
    lab_size = 3,
    tl.cex = 10,
    tl.col = "black",
    colors = c("#3B9AB2", "#F8F8F8", "#F21A00"),
    outline.color = "white"
  )
  
  p <- p + scale_fill_stepsn(
    colors = c("#3B9AB2", "white", "#F21A00"),
    breaks = my_breaks,
    limits = my_limits,
    values = scales::rescale(c(-0.2, 0, 0.5), from = my_limits),
    oob = scales::squish,
    name = "Correlation",
    guide = guide_colorsteps(
      show.limits = TRUE,
      barheight = grid::unit(8, "cm"),
      frame.colour = "black",
      ticks = TRUE
    )
  )
  
  p <- p + coord_cartesian()
  
  p <- p + labs(title = title_text) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 13),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 5)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")
    )
  
  if (show_x_text) {
    p <- p + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 15, face = "bold"),
      plot.margin = margin(t = 2, r = 2, b = 15, l = 2, unit = "pt")
    )
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  if (show_y_text) {
    p <- p + theme(
      axis.text.y = element_text(size = 15, face = "bold"),
      plot.margin = margin(t = 2, r = 2, b = 2, l = 15, unit = "pt")
    )
  } else {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  
  if (show_x_text && show_y_text) {
    p <- p + theme(plot.margin = margin(t = 2, r = 2, b = 15, l = 15, unit = "pt"))
  }
  
  p
}

build_panel <- function(panel_cfg) {
  files_left <- list_input_files(panel_cfg$left_dir)
  files_right <- list_input_files(panel_cfg$right_dir)
  all_files <- c(files_left, files_right)
  
  if (length(all_files) != 8) {
    stop(sprintf(
      "Panel '%s' requires exactly 8 files (4 + 4), but found %d.",
      panel_cfg$panel_title,
      length(all_files)
    ))
  }
  
  matrices <- lapply(all_files, import_smart_matrix, sheet_name = target_sheet_name)
  plot_data <- lapply(matrices, subset_corr_matrix, vars_to_keep = target_vars)
  
  plot_objs <- lapply(seq_along(plot_data), function(i) {
    is_left_col <- i %in% c(1, 5)
    is_bottom_row <- i %in% c(5, 6, 7, 8)
    
    create_heatmap(
      corr_mat = plot_data[[i]],
      title_text = panel_cfg$subplot_titles[i],
      show_x_text = is_bottom_row,
      show_y_text = is_left_col
    )
  })
  
  section_plot <- ggarrange(
    plotlist = plot_objs,
    ncol = 4,
    nrow = 2,
    align = "hv",
    common.legend = TRUE,
    legend = "right"
  )
  
  annotate_figure(
    section_plot,
    top = text_grob(panel_cfg$panel_title, color = "black", face = "bold", size = 18)
  )
}

# ----------------------------
# Build figure A/B/C
# ----------------------------
section_plots <- lapply(panel_configs, build_panel)

final_plot <- ggarrange(
  plotlist = section_plots,
  ncol = 1,
  nrow = 3,
  heights = c(1, 1, 1)
)

# Save high-resolution png (300 dpi), matching the original export style.
ggsave(
  filename = file.path(out_dir, "Figure_Correlation_Panel_ABC.png"),
  plot = final_plot,
  device = "png",
  width = 18,
  height = 30,
  dpi = 300,
  bg = "white"
)

print(final_plot)

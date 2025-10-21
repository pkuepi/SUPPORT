# =========================
# Figures for Demographic results
# For 7 regions
# =========================

rm(list = ls())

# install.packages("extrafont")


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
  library(scales) 
  library(extrafont)
  library(stringr)
})

font_import(pattern = "arial", prompt = FALSE)
loadfonts()

# --------- I/O ---------
file_path  <- "/Validation/Parameters/All_Regions_Combined.xlsx"
sheets     <- c("Age", "Rural", "Education")
output_names <- c("Age", "Settlement_type", "Education")
output_dir <- "/Validation/Figures"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --------- Visuals ---------
fill_vals <- c("Observed" = "#1f77b4", "Simulated" = "#ff7f0e")

row_yset <- function(v_million){
  ymax <- max(v_million, na.rm = TRUE)
  brks <- pretty(c(0, ymax), n = 5)
  list(breaks = brks, limits = c(0, max(brks)))
}

# Education recode
recode_education <- function(df){
  stopifnot(all(c("Region","Category","Observed","Simulated") %in% names(df)))
  
  df2 <- df %>%
    mutate(
      Category = as.character(Category),
      Category_new = case_when(
        Category %in% c("No_Schooling","Preschool","Primary") ~ "Primary and below",
        Category == "Junior_Secondary"                         ~ "Junior high school",
        Category == "Senior_Secondary"                         ~ "Senior high school",
        Category == "College"                                  ~ "Post-secondary non-tertiary education",
        Category %in% c("Bachelor","Master","Doctorate")       ~ "University and above",
        TRUE ~ Category
      )
    ) %>%
    group_by(Region, Category_new) %>%
    summarise(
      Observed  = sum(Observed,  na.rm = TRUE),
      Simulated = sum(Simulated, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(Category = Category_new) %>%
    mutate(
      Category = factor(
        Category,
        levels = c("Primary and below",
                   "Junior high school",
                   "Senior high school",
                   "Post-secondary non-tertiary education",
                   "University and above")
      )
    ) %>%
    arrange(Region, Category)
  
  df2
}



legend_tile_pretty <- function() {
  ggplot() +
    annotate("text", x = 0.05, y = 0.5, label = "Data source",
             fontface = "bold", size = 4.2, hjust = 0, family = "Arial") +
    geom_point(aes(x = 0.40, y = 0.5),
               shape = 22, size = 5.2, stroke = 0,
               colour = fill_vals["Observed"], fill = fill_vals["Observed"]) +
    annotate("text", x = 0.46, y = 0.5, label = "Observed",
             size = 4.0, hjust = 0, family = "Arial") +
    geom_point(aes(x = 0.72, y = 0.5),
               shape = 22, size = 5.2, stroke = 0,
               colour = fill_vals["Simulated"], fill = fill_vals["Simulated"]) +
    annotate("text", x = 0.78, y = 0.5, label = "Simulated",
             size = 4.0, hjust = 0, family = "Arial") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}


# --------- Main ---------
for (sheet in sheets) {
  
  df <- read_excel(file_path, sheet = sheet)
  stopifnot(all(c("Region","Category","Observed","Simulated") %in% names(df)))
  
  if (sheet == "Education") {
    df <- recode_education(df)
  } else {
    df$Category <- factor(df$Category, levels = unique(df$Category))
  }
  
  panel_order   <- c("Central", "East", "North", "Northeast", "Northwest",  "South", "Southwest")
  region_levels <- c("Central", "East", "North", "Northeast", "Northwest",  "South", "Southwest")
  
  
  df_long <- df %>%
    pivot_longer(c(Observed, Simulated), names_to = "Type", values_to = "Value") %>%
    mutate(
      Region = factor(Region, levels = region_levels),
      Type   = factor(Type, levels = c("Observed","Simulated")),
      ValueM = Value / 1e6
    )
  
  regions_row1 <- panel_order[1:4]
  regions_row2 <- panel_order[5:7]
  yset_row1 <- row_yset(df_long %>% filter(Region %in% regions_row1) %>% pull(ValueM))
  yset_row2 <- row_yset(df_long %>% filter(Region %in% regions_row2) %>% pull(ValueM))
  

  reg_all     <- c(regions_row1, regions_row2)
  letters_idx <- letters[seq_along(reg_all)]
  panels <- vector("list", 7)
  
  for (i in seq_along(reg_all)) {
    r <- reg_all[i]
    df_r <- df_long %>% filter(Region == r)
    
    title_txt <- paste0("(", letters_idx[i], ") ", r, " China")
    show_y <- i %in% c(1,5)   
    show_x <- i >= 4          
    yset   <- if (i <= 4) yset_row1 else yset_row2
    

    p <- ggplot(df_r, aes(Category, ValueM, fill = Type)) +
      geom_col(position = position_dodge(width = 0.50), width = 0.50, show.legend = FALSE) +
      scale_fill_manual(values = fill_vals) +
      scale_y_continuous(limits = yset$limits, breaks = yset$breaks, expand = c(0.02, 0)) +
      labs(title = title_txt, x = NULL, y = NULL) +
      theme_classic(base_size = 16, base_family = "Arial") +
      theme(
        axis.text.y  = if (show_y) element_text(size = 12, color = "black") else element_blank(),
        axis.ticks.y = element_line(),
        axis.text.x  = if (show_x) element_text(size = 12, color = "black") else element_blank(),
        axis.ticks.x = element_line(),
        plot.title   = element_text(hjust = 0.5, size = 15, face = "bold", lineheight = 1.1),
        plot.margin  = margin(t = 12, r = 8, b = if (show_x) 28 else 6, l = 44)
      )
    

    if (sheet %in% c("Age", "Education") && show_x) {
      p <- p + theme(
        axis.text.x = element_text(
          size = 12,
          angle = 45,
          hjust = 1, vjust = 1,
          color = "black"
        )
      )
    }
    

    if (sheet == "Education") {
      p <- p +
        scale_x_discrete(
          labels = function(x) {
            gsub("Post-secondary non-tertiary education",
                 "Post-secondary\nnon-tertiary education", x)
          },
          expand = expansion(mult = c(0.08, 0.12))
        )
      if (show_x) {
        bottom_margin <- if (i >= 5) 85 else 55  
        
        p <- p + 
          theme(
            plot.margin = margin(t = 12, r = 12, b = bottom_margin, l = 44),
            axis.text.x = element_text(
              size = 10,  
              angle = 45,
              hjust = 1, 
              vjust = 1,
              color = "black",
              lineheight = 0.9  
            )
          )
      }
    }
    
    panels[[i]] <- p
  }
  
  
  

  p_leg <- legend_tile_pretty()
  

  combined <- wrap_plots(c(panels, list(p_leg)), ncol = 4, nrow = 2)
  
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_plot(combined, 0, 0, 1, 1) +
    cowplot::draw_label("Population (million)",
                        x = 0.015, y = 0.5, angle = 90,
                        vjust = 0.5, hjust = 0.5,
                        fontface = "bold", size = 16,
                        fontfamily = "Arial")  
  
  final_plot <- final_plot + theme(text = element_text(family = "Arial"))
  height_out <- 9.5
  
  out_base <- gsub(" ", "_", output_names[match(sheet, sheets)])
  out_file <- file.path(output_dir,     paste0(out_base, "_by_Region.tif"))
  ggsave(out_file, final_plot, width = 16, height = height_out, dpi = 300)
  message("Saved: ", out_file)
}

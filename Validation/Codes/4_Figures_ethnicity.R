# =========================
# Figures for Ethnicity
# For 7 regions
# =========================

rm(list = ls())

# install.packages("extrafont")

library(extrafont)
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

font_import(pattern = "arial", prompt = FALSE)
loadfonts()

excel_path <- "/Validation/Parameters/All_Ethnicity_Combined.xlsx"
sheet_name <- "Ethnicity"

output_dir <- "/Validation/Figures"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_path <- file.path(output_dir, "Ethnicity_by_Region.tif")

df <- read_excel(excel_path, sheet = sheet_name)
df <- df %>%
  filter(
    Category != "Han"
  ) %>% 
  mutate(
    ObservedM  = Observed  / 1e6,
    SimulatedM = Simulated / 1e6
  )
region_levels <- c("Central", "East", "North", "Northeast", "Northwest",  "South", "Southwest")
df$Region <- factor(df$Region, levels = region_levels)


region_plots <- lapply(seq_along(region_levels), function(i) {
  r <- region_levels[i]
  df_r <- df %>% filter(Region == r)
  
  title_txt <- paste0("(", letters[i], ") ", r, " China")
  
  p <- ggplot(df_r, aes(x = SimulatedM, y = ObservedM)) +
    geom_point(
      shape = 21,
      size  = 1.5,
      stroke = 0.5,
      colour = "black",
      fill   = "steelblue"
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    labs(
      x = if (i %in% 4:7) "Simulated population count (million)" else "",
      y = NULL,                         
      title = title_txt                 
    ) +
    theme_classic(base_size = 12, base_family = "Arial") +
    theme(
      plot.title  = element_text(face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 10),
      plot.margin  = margin(t = 12, r = 12, b = if (i %in% 4:7) 28 else 8, l = 46),
    )

  return(p)
})


empty_plot <- ggplot() + theme_void()
all_items <- c(region_plots, list(empty_plot))


combined_plot <- wrap_plots(all_items, ncol = 4, nrow = 2)


final_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(combined_plot, 0, 0, 1, 1) +
  cowplot::draw_label(
    "Observed population count in census (million)",
    x = 0.022, y = 0.5, angle = 90,
    vjust = 0.5, hjust = 0.5,
    fontface = "bold", size = 16,
    fontfamily = "Arial"
  )

final_plot <- final_plot + theme(text = element_text(family = "Arial"))

ggsave(output_path, plot = final_plot, width = 16, height = 9.5, dpi = 300)

message("Saved to ", output_path)

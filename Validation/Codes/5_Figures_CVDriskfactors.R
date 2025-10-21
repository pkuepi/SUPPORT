# =========================
# Figures for CVD Risk Factors
# For 7 regions
# =========================
rm(list=ls())

# install.packages("extrafont")

library(extrafont)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

font_import(pattern = "arial", prompt = FALSE)
loadfonts()

excel_files <- c(
  "/Validation/Tables/Northeast_CVDriskFactor_ComparedWithNangang_category.xlsx",
  "/Validation/Tables/South_CVDriskFactor_ComparedWithLiubei_category.xlsx",
  "/Validation/Tables/South_CVDriskFactor_ComparedWithMeilan_category.xlsx",
  "/Validation/Tables/Central_CVDriskFactor_ComparedWithLiuyang_category.xlsx",
  "/Validation/Tables/Central_CVDriskFactor_ComparedWithHuixian_category.xlsx",
  "/Validation/Tables/East_CVDriskFactor_ComparedWithLicang_category.xlsx",
  "/Validation/Tables/East_CVDriskFactor_ComparedWithTongxiang_category.xlsx",
  "/Validation/Tables/East_CVDriskFactor_ComparedWithWuzhong_category.xlsx",
  "/Validation/Tables/Northwest_CVDriskFactor_ComparedWithMaiji_category.xlsx",
  "/Validation/Tables/Southwest_CVDriskFactor_ComparedWithPengzhou_category.xlsx"
)

out_dir <- "/Validation/Figures"
dir.create(out_dir, showWarnings = FALSE)

region_names <- c("Northeast", "South", "South", "Central", "Central", 
                  "East", "East", "East", "Southwest", "Northwest")

validation_sites <- c("Nangang, Heilongjiang", "Liubei, Guangxi", "Meilan, Hainan", 
                      "Liuyang, Hunan", "Huixian, Henan", "Licang, Qingdao", 
                      "Tongxiang, Zhejiang", "Wuzhong, Jiangsu", "Pengzhou, Sichuan", 
                      "Maiji, Gansu")



sheets <- c("SBP", "BMI", "Smoking", "Diabetes", "CHDrisk", "Strokerisk")

output_names <- c("SBP", "BMI", "Prevalence_of_Current_Smoking",
                  "History_of_Diabetes", "Ten-year_CHD_risk", "Ten-year_Stroke_risk")

y_labels <- c("Systolic blood pressure (mmHg)", "Body mass index (kg/m²)", "Prevalence of current smoking (%)", "History of diabetes (%)",
              "10-year Coronary heart disease risk (%)", "10-year Stroke risk (%)")

y_limits <- list(NULL, NULL, c(0, 70), c(0, 15), c(0, 10), c(0, 35))
y_breaks <- list(NULL, NULL, seq(0, 70, 10), seq(0, 15, 3), seq(0, 10, 2), seq(0, 35, 5))


facet_titles <- c(
  "(a) Nangang (Heilongjiang), Northeast China (Urban)",
  "(b) Liubei (Guangxi), South China (Urban)",
  "(c) Meilan (Hainan), South China (Urban)",
  "(d) Liuyang (Hunan), Central China (Rural)",
  "(e) Huixian (Henan), Central China (Rural)",
  "(f) Licang (Qingdao), East China (Urban)",
  "(g) Tongxiang (Zhejiang), East China (Rural)",
  "(h) Wuzhong (Jiangsu), East China (Urban)",
  "(i) Pengzhou (Sichuan), Southwest China (Rural)",
  "(j) Maiji (Gansu), Northwest China (Rural)"
)

facet_titles <- str_replace(facet_titles, ",\\s*", ",\n")

read_one <- function(file, title, sheet){
  stopifnot(file.exists(file))
  df <- read_excel(file, sheet = sheet) %>%
    select(Sex, Observed, Simulated) %>%
    pivot_longer(c(Observed, Simulated),
                 names_to = "Type", values_to = "Value")
  df$FacetLabel <- title
  df$Sex   <- factor(df$Sex, levels = unique(df$Sex))
  df$Type       <- factor(df$Type, levels = c("Observed", "Simulated"))
  df$ValueLabel <- sprintf("%.1f", df$Value)
  df
}


fill_cols <- c("Observed" = "#1f77b4", "Simulated" = "#ff7f0e")

make_plot <- function(sheet_name, y_title, fixed_ylim = NULL, fixed_breaks = NULL){

  dl <- mapply(
    read_one,
    file  = excel_files,
    title = facet_titles,
    MoreArgs = list(sheet = sheet_name),
    SIMPLIFY = FALSE
  )
  dat <- bind_rows(dl)
  dat$FacetLabel <- factor(dat$FacetLabel, levels = facet_titles)
  
  p <- ggplot(dat, aes(Sex, Value, fill = Type)) +
    geom_col(position = position_dodge(width = 0.6), width = 0.6) +
    scale_fill_manual(values = fill_cols, name = "Data source",
                      labels = c("Observed", "Simulated")) +
    geom_text(aes(label = ValueLabel),
              position = position_dodge(width = 0.6),
              vjust = -0.35, size = 3.5, color = "black",
              family = "Arial") +
    labs(x = NULL, y = y_title) +
    facet_wrap(~ FacetLabel, ncol = 5) +
    theme_classic(base_size = 16, base_family = "Arial") +
    theme(
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      strip.text  = element_text(size = 13, face = "bold", lineheight = 1.1),
      legend.position = "bottom",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text  = element_text(size = 12),
      plot.margin  = margin(t = 20, r = 20, b = 20, l = 20),
      axis.title.y = element_text(face = "bold", size = 16, family = "Arial",
                                  margin = margin(r = 8))
    )
  
  if (!is.null(fixed_ylim) && !is.null(fixed_breaks)) {
    p <- p + scale_y_continuous(limits = fixed_ylim, breaks = fixed_breaks,
                                expand = expansion(mult = c(0, 0.12)))
  } else if (!is.null(fixed_ylim)) {
    p <- p + scale_y_continuous(limits = fixed_ylim,
                                expand = expansion(mult = c(0, 0.12)))
  } else {
    p <- p + scale_y_continuous(expand = expansion(mult = c(0, 0.12)))
  }
  
  p
}


for (i in seq_along(sheets)) {
  sh_read   <- sheets[i]         
  sh_out    <- output_names[i]   
  yl        <- y_labels[i]
  ylim_i    <- y_limits[[i]]
  ybreak_i  <- y_breaks[[i]]
  
  plt <- make_plot(sh_read, yl, fixed_ylim = ylim_i, fixed_breaks = ybreak_i)
  
  out_file     <- file.path(out_dir,     paste0(sh_out, "_with_CKB.tif"))
  
  ggsave(out_file,     plt, width = 16, height = 9.5, dpi = 300,
         device = "tiff", type = "cairo")

  message("Saved: ", out_file)
}



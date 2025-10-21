# =========================
# Summarize CVD risk factors
# Standarlize prevalence and means using China's sixth national census
# South
# =========================
#======================== Basic setting ========================#
rm(list = ls())
gc()

library(dplyr) 
library(tidyr)
library(writexl)


# Set working directory and paths
base_path  <- "/Generation"
input_path <- file.path(base_path, "/SUPPORT/South_CVDriskfactor_ALLSex.rda")




cat("====================================================================\n")
cat("Starting CVD risk factor validation and calibration (R version)\n")
cat("Start time:", as.character(Sys.time()), "\n")
cat("====================================================================\n\n")

# --------- Read RDA input ---------
cat("=== Reading Data ===\n")
e <- new.env()
objs <- load(input_path, envir = e)
for (nm in objs) {
  if (is.data.frame(e[[nm]])) {
    data <- e[[nm]]
    break
  }
}
cat("Original data dimensions:", nrow(data), "rows", ncol(data), "columns\n")

#======================== applying the same inclusion criteria as the CKB study ========================#
cat("\n=== Urban_census Distribution Check ===\n")
urban_table <- table(data$Settlement_type, useNA = "always")
print(urban_table)

# Keep only urban population (Urban_census==1)
cat("\n=== Filtering Rural Population ===\n")
data <- data %>% filter(Settlement_type == 1)
cat("Data dimensions after filtering:", nrow(data), "rows", ncol(data), "columns\n")

# Remove 35-39 and 80-84 age groups
cat("\n=== Removing Specific Age Groups ===\n")
cat("Age group distribution before removal:\n")
print(table(data$Agegrp, useNA = "always"))

data <- data %>% filter(!(Agegrp %in% c(1, 10)))
cat("Data dimensions after removing 35-39 and 80-84 age groups:", nrow(data), "rows", ncol(data), "columns\n")
cat("Age group distribution after removal:\n")
print(table(data$Agegrp, useNA = "always"))

# Check CVD history
cat("\n=== CVD History Check ===\n")
cvd_table <- table(data$History_CVD, useNA = "always")
print(cvd_table)

cvd_no_history   <- sum(data$History_CVD == 0, na.rm = TRUE)
cvd_with_history <- sum(data$History_CVD == 1, na.rm = TRUE)
cat("No CVD history (History_CVD==0):", cvd_no_history, "\n")
cat("With CVD history (History_CVD==1):", cvd_with_history, "\n")

# Remove records with CVD history
data <- data %>% filter(History_CVD == 0)
cat("Data dimensions after removing CVD history:", nrow(data), "rows", ncol(data), "columns\n")


#======================== WHO Non-laboratory/BMI Model Calculation ========================#
cat("\n=== Variable Renaming and Centering ===\n")
data <- data %>%
  rename(
    ages  = Age 
  ) %>%
  mutate(
    censbp     = SBP - 120,
    cenages    = ages - 60,
    cenbmi     = BMI - 25,
    lpbmi_chd  = NA_real_,
    lpbmi_crbv = NA_real_,
    who_chdr_m2 = NA_real_,
    who_strr_m2 = NA_real_
  )

cat("Variable renaming and centering completed\n")

# Mutate Men as inputs for WHO Non-laboratory/BMI Model
data <- data %>%
  mutate(Men = ifelse(Sex == 1, 1, 0))
cat("Table for Men：\n")
print(table(data$Sex, data$Men, useNA = "always")) 


cat("\n=== WHO Non-laboratory/BMI Model Calculation ===\n")

# Male CHD linear predictor
data$lpbmi_chd[data$Men == 1] <- with(data[data$Men == 1, ], {
    0.073593  * cenages +
    0.0337219 * cenbmi +
    0.0133937 * censbp +
    0.5954767 * Smoking -
    0.0010432 * cenages * cenbmi -
    0.0001837 * cenages * censbp -
    0.0200831 * cenages * Smoking
})

# Female CHD linear predictor
data$lpbmi_chd[data$Men == 0] <- with(data[data$Men == 0, ], {
    0.1049418 * cenages +
    0.0257616 * cenbmi +
    0.016726  * censbp +
    1.093132  * Smoking -
    0.0006537 * cenages * cenbmi -
    0.0001966 * cenages * censbp -
    0.0343739 * cenages * Smoking
})

# Male stroke linear predictor
data$lpbmi_crbv[data$Men == 1] <- with(data[data$Men == 1, ], {
    0.097674  * cenages +
    0.0159518 * cenbmi +
    0.0227294 * censbp +
    0.4999862 * Smoking -
    0.0003516 * cenages * cenbmi -
    0.0004374 * cenages * censbp -
    0.0153895 * cenages * Smoking
})

# Female stroke linear predictor
data$lpbmi_crbv[data$Men == 0] <- with(data[data$Men == 0, ], {
    0.1046105 * cenages +
    0.0036406 * cenbmi +
    0.0216741 * censbp +
    0.7399405 * Smoking -
    0.0000129 * cenages * cenbmi -
    0.0005311 * cenages * censbp -
    0.0203997 * cenages * Smoking
})
cat("Linear predictor calculation completed\n")

#======================== Risk Probability Calculation ========================#
cat("\n=== Risk Probability Calculation ===\n")
data$who_chdr_m2[data$Men == 1] <- 1 - 0.9544258^exp(data$lpbmi_chd [data$Men == 1])
data$who_chdr_m2[data$Men == 0] <- 1 - 0.9887124^exp(data$lpbmi_chd [data$Men == 0])
data$who_strr_m2[data$Men == 1] <- 1 - 0.984826 ^exp(data$lpbmi_crbv[data$Men == 1])
data$who_strr_m2[data$Men == 0] <- 1 - 0.9885706^exp(data$lpbmi_crbv[data$Men == 0])
data$who_cvdr_m2 <- 1 - (1 - data$who_chdr_m2) * (1 - data$who_strr_m2)
cat("WHO risk calculation completed\n")

#======================== Apply CKB Calibration ========================#
cat("\n=== CKB Calibration Coefficients Application ===\n")
data <- data %>% mutate(cal_chd_m2 = NA_real_, cal_str_m2 = NA_real_)
data$cal_chd_m2[data$Men == 1] <- 1 - exp(-exp((0.94563699) + 1.58428440 * log(-log(1 - data$who_chdr_m2[data$Men == 1]))))
data$cal_chd_m2[data$Men == 0] <- 1 - exp(-exp( 0.68519174 + 0.98117212 * log(-log(1 - data$who_chdr_m2[data$Men == 0]))))
data$cal_str_m2[data$Men == 1] <- 1 - exp(-exp( 0.74242968 + 0.72411089 * log(-log(1 - data$who_strr_m2[data$Men == 1]))))
data$cal_str_m2[data$Men == 0] <- 1 - exp(-exp( 0.41378313 + 0.53948642 * log(-log(1 - data$who_strr_m2[data$Men == 0]))))
data$cal_cvd_m2 <- 1 - (1 - data$cal_chd_m2) * (1 - data$cal_str_m2)
cat("CKB calibration completed\n")


cat("Variable name restoration to uppercase completed\n")

#======================== Compare Baseline Characteristics ========================#
cat("\n=== Creating Gender-Stratified Variables ===\n")
data <- data %>%
  mutate(
    sbp_male   = ifelse(Men == 1, SBP, NA),
    sbp_female = ifelse(Men == 0, SBP, NA),
    bmi_male   = ifelse(Men == 1, BMI, NA),
    bmi_female = ifelse(Men == 0, BMI, NA),
    dm_male    = ifelse(Men == 1, Diabetes, NA),
    dm_female  = ifelse(Men == 0, Diabetes, NA),
    Smoking_male   = ifelse(Men == 1, Smoking, NA),
    Smoking_female = ifelse(Men == 0, Smoking, NA),
    cal_str_m2_male   = ifelse(Men == 1, cal_str_m2, NA),
    cal_str_m2_female = ifelse(Men == 0, cal_str_m2, NA),
    cal_chd_m2_male   = ifelse(Men == 1, cal_chd_m2, NA),
    cal_chd_m2_female = ifelse(Men == 0, cal_chd_m2, NA)
  )

cat("Gender-stratified variable creation completed\n")

#======================== Summarize Data by Age Group ========================#
cat("\n=== Summarizing Data by Age Group ===\n")
collapsed_data <- data %>%
  group_by(Agegrp) %>%
  summarise(
    sbp_male = mean(sbp_male, na.rm = TRUE),
    sbp_female = mean(sbp_female, na.rm = TRUE),
    bmi_male = mean(bmi_male, na.rm = TRUE),
    bmi_female = mean(bmi_female, na.rm = TRUE),
    dm_male = mean(dm_male, na.rm = TRUE),
    dm_female = mean(dm_female, na.rm = TRUE),
    Smoking_male = mean(Smoking_male, na.rm = TRUE),
    Smoking_female = mean(Smoking_female, na.rm = TRUE),
    cal_str_m2_male = mean(cal_str_m2_male, na.rm = TRUE),
    cal_str_m2_female = mean(cal_str_m2_female, na.rm = TRUE),
    cal_chd_m2_male = mean(cal_chd_m2_male, na.rm = TRUE),
    cal_chd_m2_female = mean(cal_chd_m2_female, na.rm = TRUE),
    .groups = 'drop'
  )
cat("Age group summarization completed\n")
print(collapsed_data)

#======================== Age Structure Weights ========================#
cat("\n=== Setting Age Structure Weights ===\n")
w_m <- c(0.093222871, 0.078813021, 0.059155082, 0.060209857, 
         0.043724393, 0.030408304, 0.024040383, 0.01652994)
w_f <- c(0.094000000, 0.079661165, 0.059017699, 0.061845755, 
         0.044325387, 0.031307274, 0.025471804, 0.019329172)
total_male   <- 0.406103851
total_female <- 0.414958256

collapsed_data$w_m <- w_m
collapsed_data$w_f <- w_f
stopifnot(abs(sum(w_m) - total_male) < 1e-12)
stopifnot(abs(sum(w_f) - total_female) < 1e-12)

cat("\n=== Age Adjustment Calculation ===\n")
sbp_male_mean   <- sum(collapsed_data$sbp_male   * collapsed_data$w_m, na.rm = TRUE) / total_male
sbp_female_mean <- sum(collapsed_data$sbp_female * collapsed_data$w_f, na.rm = TRUE) / total_female
bmi_male_mean   <- sum(collapsed_data$bmi_male   * collapsed_data$w_m, na.rm = TRUE) / total_male
bmi_female_mean <- sum(collapsed_data$bmi_female * collapsed_data$w_f, na.rm = TRUE) / total_female
dm_male_mean    <- sum(collapsed_data$dm_male    * collapsed_data$w_m, na.rm = TRUE) / total_male
dm_female_mean  <- sum(collapsed_data$dm_female  * collapsed_data$w_f, na.rm = TRUE) / total_female
Smoking_male_mean   <- sum(collapsed_data$Smoking_male   * collapsed_data$w_m, na.rm = TRUE) / total_male
Smoking_female_mean <- sum(collapsed_data$Smoking_female * collapsed_data$w_f, na.rm = TRUE) / total_female
chd_male_mean   <- sum(collapsed_data$cal_chd_m2_male   * collapsed_data$w_m, na.rm = TRUE) / total_male
chd_female_mean <- sum(collapsed_data$cal_chd_m2_female * collapsed_data$w_f, na.rm = TRUE) / total_female
stroke_male_mean   <- sum(collapsed_data$cal_str_m2_male   * collapsed_data$w_m, na.rm = TRUE) / total_male
stroke_female_mean <- sum(collapsed_data$cal_str_m2_female * collapsed_data$w_f, na.rm = TRUE) / total_female

cat("SBP male age-adjusted mean:",   round(sbp_male_mean, 6), "\n")
cat("SBP female age-adjusted mean:", round(sbp_female_mean, 6), "\n")
cat("BMI male age-adjusted mean:",   round(bmi_male_mean, 6), "\n")
cat("BMI female age-adjusted mean:", round(bmi_female_mean, 6), "\n")
cat("Diabetes male age-adjusted mean:",   round(dm_male_mean, 6), "\n")
cat("Diabetes female age-adjusted mean:", round(dm_female_mean, 6), "\n")
cat("Smoking male age-adjusted mean:",   round(Smoking_male_mean, 6), "\n")
cat("Smoking female age-adjusted mean:", round(Smoking_female_mean, 6), "\n")
cat("CHD risk male age-adjusted mean:",   round(chd_male_mean, 6), "\n")
cat("CHD risk female age-adjusted mean:", round(chd_female_mean, 6), "\n")
cat("Stroke risk male age-adjusted mean:",   round(stroke_male_mean, 6), "\n")
cat("Stroke risk female age-adjusted mean:", round(stroke_female_mean, 6), "\n")

#======================== Results Summary ========================#
cat("\n=== Final Results Summary (age-adjusted means) ===\n")
results_summary <- data.frame(
  Variable = c("SBP", "BMI", "Diabetes", "Smoking", "CHD_Risk", "Stroke_Risk"),
  Male     = c(sbp_male_mean, bmi_male_mean, dm_male_mean, Smoking_male_mean, chd_male_mean, stroke_male_mean),
  Female   = c(sbp_female_mean, bmi_female_mean, dm_female_mean, Smoking_female_mean, chd_female_mean, stroke_female_mean)
)
print(results_summary, row.names = FALSE)


#======================== Save as excel file ========================#

# Transform into long table：Sex, Variable, Value
results_long <- results_summary %>%
  pivot_longer(cols = c("Male", "Female"), 
               names_to = "Sex", 
               values_to = "Value") %>%
  dplyr::select(Sex, Variable, Value)

# Output
output_path <- "/Validation/Tables/South_withMeilan_Summary_CVDriskFactor.xlsx"
write_xlsx(results_long, output_path)


cat("\n====================================================================\n")
cat("CVD risk factor validation and calibration completed (R version)\n")
cat("Completion time:", as.character(Sys.time()), "\n")
cat("====================================================================\n")


cat("🎉 CVD risk factor validation and calibration successfully completed!\n")
cat("📊 Final results:\n")
print(results_summary)

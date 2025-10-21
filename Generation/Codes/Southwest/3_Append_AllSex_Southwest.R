#======================== Basic setting ========================#
rm(list = ls())
gc()

library(dplyr) 
library(haven)

# Set working directory and paths
base_path <- "/Generation"
female_path <- file.path(base_path, "Southwest_SBP_Diabetes_Female.rda")
male_path   <- file.path(base_path, "Southwest_SBP_Diabetes_Male.rda")
output_path <- "/Generation/Southwest_SBP_Diabetes_ALLSex.rda"

# Create output directories
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)



cat("====================================================================\n")
cat("Starting CVD risk factor data merging (RDA version)\n")
cat("Start time:", as.character(Sys.time()), "\n")
cat("====================================================================\n\n")

# Helper to load the first data.frame from RDA
load_rda_df <- function(path) {
  e <- new.env(parent = emptyenv())
  objs <- load(path, envir = e)
  for (nm in objs) {
    x <- get(nm, envir = e)
    if (is.data.frame(x)) return(x)
  }
  stop("No data.frame found in: ", path, " (objects: ", paste(objs, collapse=", "), ")")
}

#======================== Read female data ========================#
cat("=== Reading Female Data ===\n")
if (file.exists(female_path)) {
  female_data <- load_rda_df(female_path)
  cat("Female data read successfully\n")
  cat("Female data dimensions:", nrow(female_data), "rows", ncol(female_data), "columns\n")
} else {
  stop("Female data file does not exist:", female_path)
}

#======================== Read male data ========================#
cat("\n=== Reading Male Data ===\n")
if (file.exists(male_path)) {
  male_data <- load_rda_df(male_path)
  cat("Male data read successfully\n")
  cat("Male data dimensions:", nrow(male_data), "rows", ncol(male_data), "columns\n")
} else {
  stop("Male data file does not exist:", male_path)
}

#======================== Check data structure consistency ========================#
cat("\n=== Checking Data Structure Consistency ===\n")
female_cols <- names(female_data)
male_cols <- names(male_data)

cat("Female data column names:", paste(female_cols, collapse = ", "), "\n")
cat("Male data column names:", paste(male_cols, collapse = ", "), "\n")

if (identical(female_cols, male_cols)) {
  cat("✅ Column names are completely identical\n")
} else {
  cat("⚠️ Column names are not completely identical\n")
  only_female <- setdiff(female_cols, male_cols)
  only_male <- setdiff(male_cols, female_cols)
  if (length(only_female) > 0) cat("Columns only in female data:", paste(only_female, collapse = ", "), "\n")
  if (length(only_male) > 0) cat("Columns only in male data:", paste(only_male, collapse = ", "), "\n")
  common_cols <- intersect(female_cols, male_cols)
  cat("Common columns:", paste(common_cols, collapse = ", "), "\n")
  female_data <- female_data %>% select(all_of(common_cols))
  male_data   <- male_data %>% select(all_of(common_cols))
}

#======================== Merge data ========================#
cat("\n=== Merging Data ===\n")
combined_data <- bind_rows(female_data, male_data)
cat("Merging completed\n")
cat("Merged data dimensions:", nrow(combined_data), "rows", ncol(combined_data), "columns\n")

#======================== Count observations ========================#
cat("\n=== Data Count ===\n")
total_count <- nrow(combined_data)
cat("Total observations:", total_count, "\n")

if ("Sex" %in% names(combined_data)) {
  male_count <- sum(combined_data$Sex == 1, na.rm = TRUE)
  female_count <- sum(combined_data$Sex == 2, na.rm = TRUE)
  cat("Male observations:", male_count, "\n")
  cat("Female observations:", female_count, "\n")
  cat("Verification: Male + Female =", male_count + female_count, "(should equal total", total_count, ")\n")
}


#======================== Save merged data (RDA) ========================#
cat("\n=== Saving Data ===\n")
obj_name <- "Southwest_CVDriskfactor_ALLSex"
assign(obj_name, combined_data)
save(list = obj_name, file = output_path)
cat("Data saved to:", output_path, "\n")

cat("\n====================================================================\n")
cat("CVD risk factor data merging completed (RDA version)\n")
cat("Completion time:", as.character(Sys.time()), "\n")
cat("====================================================================\n")

cat("🎉 CVD risk factor data merging successfully completed!\n")
cat("📁 Output file:", output_path, "\n")
cat("📊 Merging results:\n")
cat("   - Total observations:", total_count, "\n")
cat("   - Number of variables:", ncol(combined_data), "\n")

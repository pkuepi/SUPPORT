#### Generation of synthetic population for SBP and Diabetes ####
#### North Male ####
#======================== Basic setting ========================#
rm(list = ls())

#install.packages("MASS")
library(readxl)
library(MASS)
library(labelled) 
library(dplyr)
library(haven)

region <- "North"   # region name
sex <- "Male"         # "Male" or "Female"

#======================== Input path(edit) ========================#
base_path <- "/Generation/Parameters"
pop_mean_path <- file.path(base_path, region)
#======================== Covariance matrix path ========================#
cov_path <- file.path(base_path)
#======================== Output path ========================#
output_path <- file.path("/Generation")
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) #confirm file creation


#======================== Read the Population Size and Means ========================#
sex_suffix <- ifelse(sex == "Male", "_Male.xlsx", "_Female.xlsx")

#number of population(age_group/table sheet)
pop_file <- file.path(pop_mean_path, paste0(region, "_Population_num", ifelse(sex=="Male","_Male.xlsx","_Female.xlsx"))) 
age_groups <- excel_sheets(pop_file)
num_people_list <- lapply(age_groups, function(sheet) read_excel(pop_file, sheet = sheet)[[1]])

#means(age_group/table sheet)
mean_file <- file.path(pop_mean_path, paste0(region, "_mean", ifelse(sex=="Male","_Male.xlsx","_Female.xlsx")))
mean_list <- lapply(age_groups, function(sheet) as.numeric(read_excel(mean_file, sheet = sheet)[1, ]))

#======================== Read Covariance Matrix ========================#
cov_file <- ifelse(sex == "Male",
                   file.path(cov_path, "cov_matrix_male_SBP_Diabetes.xlsx"),
                   file.path(cov_path, "cov_matrix_female_SBP_Diabetes.xlsx"))
cov_list <- lapply(age_groups, function(sheet) as.matrix(read_excel(cov_file, sheet = sheet, col_names = FALSE)))


#======================== Generate Individual Data ========================#
set.seed(1)
individual_data <- data.frame()

for (k in seq_along(age_groups)) {
  n <- num_people_list[[k]]
  mu <- mean_list[[k]]
  Sigma <- cov_list[[k]]
  
  biometrics <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
  
  group_data <- data.frame(
    Age_group = rep(age_groups[k], n),
    Region = rep(region,n),
    SBP = biometrics[, 1],
    Diabetes = biometrics[, 2]
  )
  
  individual_data <- rbind(individual_data, group_data)
}


#======================== Add ID and save ========================#
# Generate synthetic data with IDs
age_levels <- c("age35","age40","age45","age50","age55",
                "age60","age65","age70","age75","age80")

# Recode agegrp
age_cats <- c("35-39","40-44","45-49","50-54","55-59",
                            "60-64","65-69","70-74","75-79","80-84")

individual_data <- individual_data %>%
  mutate(
    Age_group = trimws(Age_group),
    Agegrp = as.integer(factor(Age_group, levels = age_levels, ordered = TRUE))
  ) %>% 
  mutate(
    # Agegrp label
    Agegrp = labelled(
      Agegrp,
      labels = setNames(seq_along(age_cats), age_cats)
    ),
    # Region label
    Region = labelled(3, labels = c(Central = 1, East = 2, North = 3, Northeast = 4, Northwest = 5, South = 6, Southwest = 7)),
  ) %>% 
  arrange(Agegrp) %>%
   # North ID 3e9
  mutate(
    ID = ID <- as.numeric(row_number() + 3e9)) %>%
  dplyr::select(-Age_group)

#Reorder columns
individual_data <- individual_data[, c("ID", setdiff(names(individual_data), "ID"))]

# Sex label
individual_data <- individual_data %>%
  mutate(
    Sex = labelled(1, labels = c(Male = 1, Female = 2))
  )


save_name <- paste0(region, "_", sex, "_individual_SBP_Diabetes_data.rda")
save_path <- file.path(output_path, save_name)
save(individual_data,file = save_path)

# ---------- helper: load the first data.frame object from an .rda file ----------
load_rda_df <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  e <- new.env(parent = emptyenv())
  objs <- load(path, envir = e)
  for (nm in objs) {
    x <- get(nm, envir = e)
    if (is.data.frame(x)) return(x)
  }
  stop("No data.frame object found in: ", path)
}

# ======================== I/O paths ======================== #
root_dir <- "/Generation"
in_rda   <- file.path(root_dir, "North_Male_individual_SBP_Diabetes_data.rda")
out_rda  <- file.path(root_dir, "North_SBP_Diabetes_Male.rda")


dir.create(dirname(out_rda), recursive = TRUE, showWarnings = FALSE)


cat("==============================================================================\n")
cat("(Male): keep ONLY original names; 20 groups = Age group x Sex\n")
cat("Start:", as.character(Sys.time()), "\n")
cat("In :", in_rda, "\n")
cat("Out:", out_rda, "\n")
cat("==============================================================================\n\n")

# ======================== 1) Ensure required variables exist ======================== #
df <- load_rda_df(in_rda)
cat("Loaded:", nrow(df), "rows,", ncol(df), "cols\n")
need_vars <- c("ID","Region","Sex","SBP","Diabetes","Agegrp") 
missing_vars <- setdiff(need_vars, names(df))
if (length(missing_vars)) {
  cat("Create missing vars as NA:", paste(missing_vars, collapse=", "), "\n")
  for (v in missing_vars) df[[v]] <- NA
}

# ======================== 2) Build 20 groups = Agegrp (1..10) x Sex (0/1) ======================== #
df <- df %>%
  mutate(
    Group20 = dplyr::case_when(
      !is.na(Agegrp) & !is.na(Sex) & Sex == 1 ~ as.integer(Agegrp),
      !is.na(Agegrp) & !is.na(Sex) & Sex == 2 ~ as.integer(Agegrp) + 10L,
      TRUE ~ NA_integer_
    )
  )
cat("Group20 table:\n"); print(table(df$Group20, useNA = "ifany"))

# ======================== 3) Two-stage rescale + binarize ======================== #
# Stage-1: rescale within group to [0,1]
# Stage-2: adjust to match the original group mean; then truncate to [0,1]
# Finally: binarize by comparing to runif, overwrite original column

two_stage_rescale_and_binarize <- function(data, var_name, group_var = "Group20") {
  stopifnot(var_name %in% names(data), group_var %in% names(data))
  x <- data[[var_name]]
  g <- data[[group_var]]
  
  set.seed(1)
  rnd <- runif(nrow(data))
  out <- x
  
  for (grp in sort(unique(na.omit(g)))) {
    idx <- which(g == grp & !is.na(x))
    if (length(idx) == 0) next
    
    xi <- x[idx]
    xmin <- suppressWarnings(min(xi, na.rm = TRUE))
    xmax <- suppressWarnings(max(xi, na.rm = TRUE))
    xmean <- suppressWarnings(mean(xi, na.rm = TRUE))
    rng <- xmax - xmin
    
    if (is.finite(rng) && rng > 0) {
      p1 <- (xi - xmin) / rng
    } else {
      p1 <- rep(0, length(xi))
    }
    
    m1 <- mean(p1, na.rm = TRUE)
    if (is.finite(m1) && m1 > 0) {
      p2 <- (p1 / m1) * xmean
    } else {
      p2 <- rep(xmean, length(p1))
    }
    p2[p2 < 0] <- 0
    p2[p2 > 1] <- 1
    
    out[idx] <- as.integer(p2 > rnd[idx])
    
    cat(sprintf("  %s | group=%02d: n=%d, xmin=%.3f, xmax=%.3f, mean=%.3f -> mean_p=%.3f\n",
                var_name, grp, length(idx), xmin, xmax, xmean, mean(p2, na.rm = TRUE)))
  }
  
  data[[var_name]] <- out
  data
}


df <- two_stage_rescale_and_binarize(df, "Diabetes", "Group20")

cat("Diabetes (after binarize):\n");   print(table(df$Diabetes, useNA="ifany"))


# ======================== 3b) Round SBP to specified precision ======================== #
cat("\nRounding SBP to integers\n")

df <- df %>%
    mutate(
        SBP = round(SBP, digits = 0),
    )

# Output summary statistics after rounding (optional, for verification)
cat("Summary after rounding:\n")
cat("SBP (integer): "); print(summary(df$SBP))



# ======================== 4) Standardize a few numeric/binary aliases ======================== #
df <- df %>% dplyr::select(
 "ID","Region","Sex","SBP","Diabetes","Agegrp"
)

cat("\nFinal dims:", nrow(df), "x", ncol(df), "\n")
cat("Any NA in ID:", any(is.na(df$ID)), " | Duplicated ID:", sum(duplicated(df$ID)), "\n")

# ======================== 5) Save RDA ======================== #
obj_name <- "North_SBP_Diabetes_Male"
assign(obj_name, df)
save(list = obj_name, file = out_rda)
cat("Saved:", out_rda, " (object:", obj_name, ")\n")

cat("\nDone:", as.character(Sys.time()), "\n")
# =========================
# Summarize CVD risk factors
# Input for Figures
# East
# =========================

rm(list = ls())  
library(openxlsx)
library(readxl)
library(writexl)
library(dplyr)



# Read the observed table and adjust some labels
data_observed <- read.xlsx("/Validation/Parameters/East_CVDriskFactor_ComparedWithWuzhong.xlsx", sheet = "original")

data_observed <- data_observed %>% 
  mutate(Sex = case_when(  
    Sex == "women" ~ "Female",
    Sex == "men" ~ "Male",
    TRUE ~ Sex  
  ))


# Read the simulated table and adjust some labels
data_simulated <- read.xlsx("/Validation/Tables/East_withWuzhong_Summary_CVDriskFactor.xlsx") 



data_simulated <- data_simulated %>% 
  rename(simulated = Value, Category = Variable)
data_simulated <- data_simulated %>% 
  mutate(
    simulated = ifelse(
      Category %in% c("SBP", "BMI"), 
      round(simulated, 1),           
      round(simulated * 100, 1)       
    )
  )%>%
  mutate(simulated = round(simulated, digits = 1))%>%
  mutate(Category = case_when(  
    Category == "SBP" ~ "SBP, mmHg",
    Category == "BMI" ~ "BMI, kg/m2",
    Category == "Smoking" ~ "Smoking, %",
    Category == "Diabetes" ~ "Diabetes, %",
    Category == "CHD_Risk" ~ "CHD",
    Category == "Stroke_Risk" ~ "Stroke",
    TRUE ~ Category
  ))




data_observed <- data_observed %>%
  left_join(
    data_simulated[, c("Sex", "Category", "simulated")],
    by = c("Sex", "Category")  
  ) %>%
  mutate(synpop = ifelse(!is.na(simulated), simulated, synpop)) %>%  
  dplyr::select(-simulated,-`ARE(%)`) 


# Compute absolute difference
data_observed <- data_observed %>% mutate(Absolute.difference = abs(real - synpop))
# Compute Relative Absolute Error (RAE%)
data_observed <- data_observed %>% mutate(`RAE(%)` = round(Absolute.difference/real*100,2))

# Save merged/updated table
write_xlsx(data_observed, "/Validation/Tables/East_CVDriskFactor_ComparedWithWuzhong_original.xlsx")



# Read source workbook for per-sheet updates
path <- "/Validation/Parameters/East_CVDriskFactor_ComparedWithWuzhong.xlsx"
sheet_names <- c("SBP","BMI","Diabetes","Smoking","CHDrisk" ,"Strokerisk")


# Mapping between display variable names and sheet names (adjust as needed)
var_mapping <- c(
  "SBP, mmHg" = "SBP",      
  "BMI, kg/m2" = "BMI", 
  "Diabetes, %" = "Diabetes",
  "Smoking, %" = "Smoking",
  "CHD" = "CHDrisk" ,
  "Stroke" ="Strokerisk"
)

# Loop through sheets and fill values
for (sheet in sheet_names) {
  df_sheet <- read_excel(path, sheet = sheet)
  
  df_sheet <-  df_sheet %>% 
    rename(Sex = Category) %>%  
    mutate(Sex = case_when(  
      Sex == "women" ~ "Female",
      Sex == "men" ~ "Male",
      TRUE ~ Sex  
    )) 
  
   # Look up the display variable corresponding to the current sheet
  target_var <- names(var_mapping)[var_mapping == sheet]
  
  if (length(target_var) > 0) {  
    df_sheet$Simulated <- data_simulated %>%
      filter(Category == target_var) %>% 
      pull(simulated) %>%                
      .[match(df_sheet$Sex, data_simulated$Sex[data_simulated$Category == target_var])]
    
     # Align Observed column for consistency and future-proofing
    df_sheet$Observed <- data_observed %>%
      filter(Category == target_var) %>%
      pull(real) %>%
      .[match(df_sheet$Sex, data_observed$Sex[data_observed$Category == target_var])]
    
    assign(paste0("df_", sheet), df_sheet)
  }
}



# Collect all updated sheets and save to a new workbook
result_sheets <- mget(paste0("df_", sheet_names))%>% 
  setNames(gsub("^df_", "", names(.)))


write_xlsx(result_sheets, "/Validation/Tables/East_CVDriskFactor_ComparedWithWuzhong_category.xlsx")



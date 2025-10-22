# Synthetic data Using Population Profiles for cardiOvascular Risk facTors (SUPPORT)

This repository includes parameters and codes of synthetic data generation and technical validation procedures for the manuscript "Synthetic data Using Population Profiles for cardiOvascular Risk facTors (SUPPORT) in mainland China" submitted to the *Scientific Data*.

## Overview

The SUPPORT dataset is a large-scale resource comprising 777,358,492 synthetic individuals aged 35–84 across seven geographic regions of China for 2020. Each individual has a detailed profile of sociodemographic attributes and major cardiovascular disease (CVD) risk factors, including blood pressure, cholesterol, body mass index, and diabetes history. The population was constructed using iterative proportional fitting and multivariate normal distribution sampling, integrating data from China's Seventh National Population Census, the Global Burden of Disease (GBD) study, and numerous health surveys. Technical validation against census statistics and independent cohorts, including the China Kadoorie Biobank, confirmed the high fidelity of the dataset to real-world population data. SUPPORT is open-source and can be extended with additional attributes. This resource enables robust, individual-level modeling of CVD and is publicly available at `https://doi.org/10.5281/zenodo.17406896`
        
Lists of provinces/autonomous region/municipality in seven regions of Mainland China*: 
- Central China: Henan, Hubei, Hunan
- East China: Shanghai, Jiangsu, Zhejiang, Anhui, Fujian, Jiangxi, Shandong 
- North China: Beijing, Tianjin, Hebei, Shanxi, Inner Mongolia 
- Northeast China: Liaoning, Jilin, Heilongjiang  
- Northwest China: Shaanxi, Gansu, Qinghai, Ningxia, Xinjiang  
- South China: Guangdong, Guangxi, Hainan  
- Southwest China: Chongqing, Sichuan, Guizhou, Yunnan, Xizang (Tibet)
* Note: The synthetic population refers to the population of the 31 provinces, autonomous regions and municipalities of the Chinese mainland, excluding residents in Hong Kong SAR, Macao SAR and Taiwan region.

`Reference: Zhang X, Lu J, Yang Y, Cui J, Zhang X, Xu W, et al. Cardiovascular disease prevention and mortality across 1 million urban populations in China: data from a nationwide population-based study. Lancet Public Health 2022;7:e1041-e1050. doi: 10.1016/S2468-2667(22)00170-0`

## Quick Start

In the following sections, we will describe the purposes of each major directory and the scripts in that folder. **The scripts and parameters here are used for reproduction of data generation and technical validation in the paper only.** Scripts were tested on R-4.5.1.

### 1.Data generation
As a demonstration, this script illustrates the process of synthetic data generation using a subset of variables SBP as an example of continuous variables and Diabetesas an example of categorical variables. Please run the R scripts for each region. Sociodemographic attributes (Region, Sex, and Agegrp) and CVD risk factors (SBP and Diabetes) will be generated, and the final data of all the seven regions will be saved at the `Generation` folder. 

If you want to run the codes, you need to: 

#### 1) Change the path

Please change the `path` to the location of the `SUPPORT` folder you downloaded and load the R function.

#### 2) Prepare R packages

Prerequisites: You may need to install the following R packages if you have not done so yet.

- dplyr
- readxl 
- labelled
- haven
- MASS: This package is specificly used for the generation of CVD risk factors

Then you can load the packages.

#### 3) Load the parameters

Please load the parameters needed for date generation.

#### 4) Create and assign attributes

Please run the R scripts `1_Demonstration_SBP_Diabetes_Generation_Male.R` and `2_Demonstration_SBP_Diabetes_Generation_Female.R` in each regional subfolder to generate sociodemographic attributes and CVD risk factors for males and females, respectively. 


### 2.Validation
Technical validation was designed to assess the fidelity of the synthetic population by comparing its statistical properties with those of observed real-world data from the China Kadoorie Biobank (CKB) study (source: Bull World Health Organ 2023;101:238-247.).  Tables and figures are save at subfolders `Table` and `Figure` in folder `Validation`, respectively. If you want to run the codes, you need to:

##### 1) Change the path

Please change the `path` to the location of the `SUPPORT` folder you downloaded and run the first chunk to load the R function.

#### 2) Prepare R packages

Prerequisites: You may need to install the following R packages if you have not done so yet.

- extrafont
- readxl
- ggplot2
- dplyr
- patchwork
- cowplot
- tidyr
- scales
- stringr
- openxlsx
- writexl

And then you can load the packages.

#### 3) Sociodemographic Data Validation

The results are saved as Excel tables in folder `Parameters`. Please run the `plot_demographics_exceptEthnicity`.R and `plot_ethnicity`.R to plot figures to visualize the simulated results from the SUPPORT data set alongside the observed data, the output figures including Fig. 3, and Fig. 4 in the manuscript as well as Fig. S1 and Fig S2 in the Supplementary.

#### 4) External CVD Risk Factor and Predicted Risk Validation

Ten R scripts containing `Validation_CVDriskfactors` (corresponding to the 10 sites of the CKB study) are set to calculate the mean or prevalence values of CVD risk factors of SBP, BMI, Diabetes, Smoking, 10-year CHD risk, 10-year stroke risk of the SUPPORT population according to the inclusion and exclusion criteria of the CKB study. Fig. 5, Fig. 6, Fig. 7 and Fig. 8 in the manuscript as well as Fig. S3 and Fig S4 in the Supplementary will be generated.

### If you are not interested in the details of this code, you can skip the following descriptions.

### 3.Descriptions of folders

#### Folder `Generation`
This folder contains 2 subfolders, including codes and parameters used to generate synthetic populations with CVD risk factors (SBP and Diabetes) across seven regions of China.

##### Folders  `Codes`
This folder contains 7 subfolders, each representing a region for synthetic population generation.

###### Folders `Central`, `East`, `North`, `Northeast`, `Northwest`, `South` and `Southwest`
Each region folder has the same structure. Using Central as an example:
- `1_Demonstration_SBP_Diabetes_Generation_Male_Central.R`: demonstrates generating CVD risk factors (SBP and diabetes) for males in Central.
- `2_Demonstration_SBP_Diabetes_Generation_Female_Central.R`: demonstrates generating CVD risk factors (SBP and diabetes) for females in Central.
- `3_Append_AllSex_Central.R`: combines the two .rda files produced in the previous steps.

##### Folder `Parameters`
This folder contains 7 subfolders and 2 .xlsx files. 
Each subfolder represents the population size and distribution of CVD risk factors for one region. 
Each .xlsx file (`cov_matrix_female_SBP_Diabetes.xlsx` and `cov_matrix_male_SBP_Diabetes.xlsx`) represents the covariance matrix for one sex.

###### Folders `Central`, `East`, `North`, `Northeast`, `Northwest`, `South` and `Southwest`
Each region folder has the same structure. Using Central as an example:
- `Central_mean_Female.xlsx` and `Central_mean_Male.xlsx`: contain age-specific mean SBP values and diabetes prevalence for each sex.
- `Central_Population_num_Female.xlsx` and `Central_Population_num_Male.xlsx`: provide sex-specific population counts by age group.

#### Folder `Validation`
This folder includes four subfolders containing the codes, parameters, and results for validating synthetic populations by CVD risk factors and demographic characteristics.

#####  Folder `Codes`
This folder contains 23 files for validation to calculate the mean or prevalence of CVD risk factors and estimate CHD and stroke risks using the WHO non-lab model, prepare cleaned and structured tables of CVD risk factor results for figure generation.
- `3_Figures_demographics.R` generates validation figures for demographic characteristics.
- `4_Figures_ethnicity.R` generates validation figures for ethnicity distributions.
- `5_Figures_CVDriskfactors.R` generates validation figures for CVD risk factors.

#####  Folder `Parameters` provides the parameter inputs for validation, including demographic baselines sourced from China’s Seventh National Population Census (2020) and CVD risk factor distributions sourced from the China Kadoorie Biobank (CKB).

##### Folder `Tables` presents the comparison results of CVD risk factors between SUPPORT v1.0 and the CKB across different regions and demographic groups.

##### Folder `Figures` provides visual validation results for demographic characteristics and cardiovascular disease (CVD) risk factors


## Repository Structure

```
SUPPORT/
├─Generation
│  ├─Codes
│  │  ├─Central
│  │  │      1_Demonstration_SBP_Diabetes_Generation_Male_Central.R
│  │  │      2_Demonstration_SBP_Diabetes_Generation_Female_Central.R
│  │  │      3_Append_AllSex_Central.R
│  │  │
│  │  ├─East
│  │  │      1_Demonstration_SBP_Diabetes_Generation_Male_East.R
│  │  │      2_Demonstration_SBP_Diabetes_Generation_Female_East.R
│  │  │      3_Append_AllSex_East.R
│  │  │
│  │  ├─North
│  │  │      1_Demonstration_SBP_Diabetes_Generation_Male_North.R
│  │  │      2_Demonstration_SBP_Diabetes_Generation_Female_North.R
│  │  │      3_Append_AllSex_North.R
│  │  │
│  │  ├─Northeast
│  │  │      1_Demonstration_SBP_Diabetes_Generation_Male_Northeast.R
│  │  │      2_Demonstration_SBP_Diabetes_Generation_Female_Northeast.R
│  │  │      3_Append_AllSex_Northeast.R
│  │  │
│  │  ├─Northwest
│  │  │      1_Demonstration_SBP_Diabetes_Generation_Male_Northwest.R
│  │  │      2_Demonstration_SBP_Diabetes_Generation_Female_Northwest.R
│  │  │      3_Append_AllSex_Northwest.R
│  │  │
│  │  ├─South
│  │  │      1_Demonstration_SBP_Diabetes_Generation_Male_South.R
│  │  │      2_Demonstration_SBP_Diabetes_Generation_Female_South.R
│  │  │      3_Append_AllSex_South.R
│  │  │
│  │  └─Southwest
│  │          1_Demonstration_SBP_Diabetes_Generation_Male_Southwest.R
│  │          2_Demonstration_SBP_Diabetes_Generation_Female_Southwest.R
│  │          3_Append_AllSex_Southwest.R
│  │
│  └─Parameters
│      │  cov_matrix_female_SBP_Diabetes.xlsx
│      │  cov_matrix_male_SBP_Diabetes.xlsx
│      │
│      ├─Central
│      │      Central_mean_Female.xlsx
│      │      Central_mean_Male.xlsx
│      │      Central_Population_num_Female.xlsx
│      │      Central_Population_num_Male.xlsx
│      │
│      ├─East
│      │      East_mean_Female.xlsx
│      │      East_mean_Male.xlsx
│      │      East_Population_num_Female.xlsx
│      │      East_Population_num_Male.xlsx
│      │
│      ├─North
│      │      North_mean_Female.xlsx
│      │      North_mean_Male.xlsx
│      │      North_Population_num_Female.xlsx
│      │      North_Population_num_Male.xlsx
│      │
│      ├─Northeast
│      │      Northeast_mean_Female.xlsx
│      │      Northeast_mean_Male.xlsx
│      │      Northeast_Population_num_Female.xlsx
│      │      Northeast_Population_num_Male.xlsx
│      │
│      ├─Northwest
│      │      Northwest_mean_Female.xlsx
│      │      Northwest_mean_Male.xlsx
│      │      Northwest_Population_num_Female.xlsx
│      │      Northwest_Population_num_Male.xlsx
│      │
│      ├─South
│      │      South_mean_Female.xlsx
│      │      South_mean_Male.xlsx
│      │      South_Population_num_Female.xlsx
│      │      South_Population_num_Male.xlsx
│      │
│      └─Southwest
│              Southwest_mean_Female.xlsx
│              Southwest_mean_Male.xlsx
│              Southwest_Population_num_Female.xlsx
│              Southwest_Population_num_Male.xlsx
│
└─Validation
    ├─Codes
    │      1_Central_withHuixian_Validation_CVDriskfactors.R
    │      1_Central_withLiuyang_Validation_CVDriskfactors.R
    │      1_East_withLicang_Validation_CVDriskfactors.R
    │      1_East_withTongxiang_Validation_CVDriskfactors.R
    │      1_East_withWuzhong_Validation_CVDriskfactors.R
    │      1_Northeast_withNangang_Validation_CVDriskfactors.R
    │      1_Northwest_withMaiji_Validation_CVDriskfactors.R
    │      1_Southwest_withPengzhou_Validation_CVDriskfactors.R
    │      1_South_withLiubei_Validation_CVDriskfactors.R
    │      1_South_withMeilan_Validation_CVDriskfactors.R
    │      2_Central_Validation_CVDriskfactor_InputForFigures_Huixian.R
    │      2_Central_Validation_CVDriskfactor_InputForFigures_Liuyang.R
    │      2_East_Validation_CVDriskfactor_InputForFigures_Licang.R
    │      2_East_Validation_CVDriskfactor_InputForFigures_Tongxiang.R
    │      2_East_Validation_CVDriskfactor_InputForFigures_Wuzhong.R
    │      2_Northeast_Validation_CVDriskfactor_InputForFigures_Nangang.R
    │      2_Northwest_Validation_CVDriskfactor_InputForFigures_Maiji.R
    │      2_Southwest_Validation_CVDriskfactor_InputForFigures_Pengzhou.R
    │      2_South_Validation_CVDriskfactor_InputForFigures_Liubei.R
    │      2_South_Validation_CVDriskfactor_InputForFigures_Meilan.R
    │      3_Figures_demographics.R
    │      4_Figures_ethnicity.R
    │      5_Figures_CVDriskfactors.R
    │
    ├─Figures
    │      Age_by_Region.tif
    │      BMI_with_CKB.tif
    │      Education_by_Region.tif
    │      Ethnicity_by_Region.tif
    │      History_of_Diabetes_with_CKB.tif
    │      Prevalence_of_Current_Smoking_with_CKB.tif
    │      Prevalence_of_Diabetes_with_CKB.tif
    │      Rural_by_Region.tif
    │      SBP_with_CKB.tif
    │      Settlement_type_by_Region.tif
    │      Ten-year_CHD_risk_with_CKB.tif
    │      Ten-year_Stroke_risk_with_CKB.tif
    │
    ├─Parameters
    │      Age_byRegion_Settlement_Sex.xlsx
    │      All_Ethnicity_Combined.xlsx
    │      All_Regions_Combined.xlsx
    │      Central_CVDriskFactor_ComparedWithHuixian.xlsx
    │      Central_CVDriskFactor_ComparedWithLiuyang.xlsx
    │      East_CVDriskFactor_ComparedWithLicang.xlsx
    │      East_CVDriskFactor_ComparedWithTongxiang.xlsx
    │      East_CVDriskFactor_ComparedWithWuzhong.xlsx
    │      Education_byRegion_Settlement_Sex.xlsx
    │      Ethnicity_byRegion_Settlement_Sex.xlsx
    │      Northeast_CVDriskFactor_ComparedWithNangang.xlsx
    │      Northwest_CVDriskFactor_ComparedWithMaiji.xlsx
    │      Southwest_CVDriskFactor_ComparedWithPengzhou.xlsx
    │      South_CVDriskFactor_ComparedWithLiubei.xlsx
    │      South_CVDriskFactor_ComparedWithMeilan.xlsx
    │
    └─Tables
            Central_CVDriskFactor_ComparedWithHuixian_category.xlsx
            Central_CVDriskFactor_ComparedWithLiuyang_category.xlsx
            East_CVDriskFactor_ComparedWithLicang_category.xlsx
            East_CVDriskFactor_ComparedWithTongxiang_category.xlsx
            East_CVDriskFactor_ComparedWithWuzhong_category.xlsx
            Northeast_CVDriskFactor_ComparedWithNangang_category.xlsx
            Northwest_CVDriskFactor_ComparedWithMaiji_category.xlsx
            Southwest_CVDriskFactor_ComparedWithPengzhou_category.xlsx
            South_CVDriskFactor_ComparedWithLiubei_category.xlsx
            South_CVDriskFactor_ComparedWithMeilan_category.xlsx
```

## Version History

- **v1.0**: Initial release with parameters and codes of the SUPPORT dataset across seven geographic regions of mainland China


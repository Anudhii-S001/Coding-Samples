## Author: Anudhii Sundaram 
## Date Created: April 09, 2024
## Date Modified: October 25, 2024

## R Version: 4.3.2 

## Purpose: DATA CLEANING, ANALYSIS AND VISUALIZATION FOR RMG PROJECT

## Brief Project Description: 
# The aim of the project is to evaluate the impact of a  training program rolled out by IFC and ILO in Bangladesh
# on working conditions and management practices in factories, along with worker productivity. 
# The program trained only female workers in a factory, who could then be promoted to supervisory roles.  

## Brief description of data: 
# The data used in this analysis comes from surveying the line operators and supervisors in the factory. 
# For the analysis, line operator and supervisor data were merged.   

## Brief variable description: 
# Outcomes: Working Conditions Index, Management Practices Index, and individual components comprising both indices. 
# Treatment: 'Trainee Status' is the treatment variable. 

## Clustered standard errors by supervisor's ID in all regression models.  
## Factory fixed effects included in all regression models. 
## Nearest Neighboring Matching used with two distance metrics: Mahalanobis distance and Propensity score. 

## This R script has the following sections:

########### Section A. DATA CLEANING AND MERGE 
# Section 1: Merging data sets 
#           Section 1.1: Importing Data sets 
#           Section 1.2: Merging Data sets
# Section 2: Data wrangling in the merged data set
#           Section 2.1: Treatment variable
#           Section 2.2: Outcome variables 
#           Section 2.3: Control variables 
# Section 3: Renaming and adding labels
#           Section 3.1: Renaming variables 
#           Section 3.2: Adding labels 
#           Section 3.3: Saving data set

########### Section B. DESCRIPTIVE TABLES AND PLOTS 
# Section 1: Summary Statistics and T-Test Table
#           Section 1.1: Summary Statistics Table for Supervisors 
#           Section 1.2: Summary Statistics Table for Line Operators 
# Section 2: Descriptive Plots 
#           Section 2.1: Descriptive Plots for Line Operators 
#           Section 2.2: T-test Plots for Management Practice Index sub-components 

########### Section C. DATA ANALYSIS 
# Section 1: OLS Fixed Effects Regression before Statistical Matching 
#            Section 1.1: Outcome Variable: Working Conditions Index 
#            Section 1.2: Outcome Variable: Management Practices Index
# Section 2: OLS Fixed Effects Regression after Statistical Matching 
#            Section 2.1: Nearest Neighbor Matching
#            Section 2.2: Regression after Nearest Neighbor Matching using Mahalanobis Distance 
#            Section 2.3: Regression after Nearest Neighbor Matching using Propensity Score
# Section 3: Logistic Fixed Effects Regression  
#            Section 3.1: Individual components of Working Conditions Index  
#            Section 3.2: Individual components of Management Practices Index 

########### Section D: POST ESTIMATION TABLES AND PLOTS 
# Section 1: Regression Tables
# Section 2: Regression Coefficient Plot


## Clearing the environment 
rm(list=ls())

######################################################################################
####################### SETTING WORKING DIRECTORY ####################################

# Get the username of the system
user <- Sys.info()[["user"]]

# Define the root directory based on the username
if (user == "anudhiisundaram") {
  root <- "/Users/anudhiisundaram/Desktop/Coding Samples/R/RMG"
} else {
  root <- readline(prompt = "Enter your working directory: ") # Allowing others to set manually
}

# Set the working directory
setwd(root)

# Check if the working directory is set correctly
print(getwd())

# Create a "tables" directory if it doesn't exist
tables_wd <- file.path(root, "tables")  # Defining the path to the tables folder
if (!dir.exists(tables_wd)) {
  dir.create(tables_wd, recursive = TRUE)
}

# Create a "plots" directory if it doesn't exist
plots_wd <- file.path(root, "plots")  # Defining the path to the plots folder
if (!dir.exists(plots_wd)) {
  dir.create(plots_wd, recursive = TRUE)
}

##############################################################################################



######################################################################################
########################### INSTALL PACKAGES #########################################

# Create a function to check and install packages
install_load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)  # Install if missing
    }
    library(pkg, character.only = TRUE)  # Load package
  }
}

# Define the required packages
packages <- c("dplyr", "tidyverse", "ggplot2", "stargazer", "haven", 
                       "writexl", "readxl", "expss", "sandwich", "lmtest", 
                       "ltm", "repmis", "MatchIt", "cobalt", "aod", "patchwork", 
                       "ggpubr")

# Run the function to install and load packages
install_load(packages)

######################################################################################


######################################################################################
####################### SECTION A: DATA CLEANING AND MERGE ###########################
######################################################################################


######################## SECTION 1: MERGING DATA SETS ################################
######################################################################################

################### 1.1 IMPORTING DATA SETS

## Import First Data Set: Supervisors' Data 
# Note: In addition to specifying main file path, specify the folder where the data set is stored.
# For example, here it is stored in the folder 'Data' within root. 
sup_data <- read_dta(file.path(root, "Data/clean_IFC_GEAR_trainee_comparison_sv_v3.dta"))

## Import Second Data Set: Line Operators' Data
lo_data <- read_dta(file.path(root, "Data/clean_IFC_GEAR_line_operator_v3.dta"))

################### 1.2 MERGING DATA SETS

## Add a prefix to all variables in both data sets for ease of merge later
sup_data <- sup_data |>  # Supervisors' data 
  rename_with(.fn = ~ paste0("sup_", .x)) 

# Add lo_ before all the variables in the line operators' data 
lo_data <- lo_data |>    # Line Operators' data
  rename_with(.fn = ~ paste0("lo_", .x)) 

## Rename the unique supervisor ID variable in both data sets for ease of merge later
sup_data <- sup_data |> # Supervisors' data 
  rename(id_sup_merge=sup_res_id)

lo_data <- lo_data |>   # Line Operators' data
  rename(id_sup_merge=lo_gi_sv_unique_id)

## Merge both data sets using the unique supervisor ID variable 
merged_data <- merge(sup_data, lo_data, by = "id_sup_merge")

## Drop repetitive variable 'line_code' 
merged_data <- merged_data |> 
  dplyr::select(-lo_line_code)


################## SECTION 2: DATA WRANGLING IN THE MERGED DATA SET ##################
######################################################################################


#################### 2.1 TREATMENT VARIABLE 

####### TRAINEE STATUS 

# Recode the treatment variable: trainee = 1 and comparison = 0
merged_data$sup_res_type <- replace(merged_data$sup_res_type, merged_data$sup_res_type==2, 0)


#################### 2.2 OUTCOME VARIABLES

####### 2.2.1 WORKING CONDITIONS INDEX 

## Create an index to measure working conditions at the factories in the study

# Recode values '-97' and '-99' with NA across survey response variables pertaining to working conditions
merged_data <- merged_data |>
  mutate(across(lo_wc_3a:lo_wc_3h, ~ ifelse(.x < 0, NA, .x)))

# Store all the survey response variables as a vector for reverse coding in the next step 
reverse_cols <- c("lo_wc_3a", "lo_wc_3b", "lo_wc_3d", "lo_wc_3e", "lo_wc_3f", "lo_wc_3g", 
                  "lo_wc_3h") 

# Reverse code the response scale for ease of interpretation
merged_data[,reverse_cols] <- 6 - merged_data[ , reverse_cols]

# Create a new variable through horizontal summation across survey response variables
merged_data$'lo_wc_3_sum' <- rowSums(merged_data[,c("lo_wc_3a", "lo_wc_3b", "lo_wc_3c", 
                                                    "lo_wc_3d","lo_wc_3e","lo_wc_3f", 
                                                    "lo_wc_3g", "lo_wc_3h")], 
                                     na.rm = T)  # Ignore missing values while performing calculation

# Relocate the new variable in the data frame
merged_data <- merged_data |> 
  relocate(lo_wc_3_sum, .after = lo_wc_3h)

# Create a function for standardization 
standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return(z) 
}

# Apply the standardization function to obtain a new variable 
merged_data$'lo_wc_3_zscore' <- standardize(merged_data$lo_wc_3_sum)

# Relocate the new variable in the data frame 
merged_data <- merged_data |> 
  relocate(lo_wc_3_zscore, .after = lo_wc_3_sum)

# Extract all the survey response variables in a separate data frame for Cronbach
# Alpha coefficient calculation in the next step
wc_response_data <- merged_data[,c("lo_wc_3a", "lo_wc_3b", "lo_wc_3c", "lo_wc_3d", 
                                   "lo_wc_3e", "lo_wc_3f", "lo_wc_3g", "lo_wc_3h")]

# Calculate Cronbach's Alpha coefficient for the survey response variables
# Note: Cronbach Alpha is a reliability coefficient and a measure of the internal 
# consistency of measures. 
cronbach.alpha(wc_response_data, CI = T, na.rm = T) # Ignore missing values while performing calculation


####### 2.2.2 MANAGEMENT PRACTICES INDEX

## Create an index to measure management practices of supervisors at factories in the study

# Recode values '-97' and '-99' with NA across the survey response variables pertaining to management practices
merged_data <- merged_data |>
  mutate(across(lo_sa_3a:lo_sa_3i, ~ ifelse(.x < 0, NA, .x)))

# Create a new variable through horizontal summation across survey response variables 
merged_data$'lo_sa_3_sum' <- rowSums(merged_data[,c("lo_sa_3a", "lo_sa_3b", "lo_sa_3c", 
                                                    "lo_sa_3d","lo_sa_3e","lo_sa_3f", 
                                                    "lo_sa_3g", "lo_sa_3h", "lo_sa_3i")], 
                                     na.rm = T) # Ignore missing values while performing calculation

# Relocate the new variable in the data frame 
merged_data <- merged_data |> 
  relocate(lo_sa_3_sum, .after = lo_sa_3i)

# Create a function for standardization
standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return(z) 
}

# Apply the standardization function to obtain a new variable 
merged_data$'lo_sa_3_zscore' <- standardize(merged_data$lo_sa_3_sum)

# Relocate the new variable in the data frame 
merged_data <- merged_data |> 
  relocate(lo_sa_3_zscore, .after = lo_sa_3_sum)

# Extract all the survey response variables in a separate data frame for Cronbach
# Alpha coefficient calculation in the next step
mp_response_data <- merged_data[,c("lo_sa_3a", "lo_sa_3b", "lo_sa_3c", "lo_sa_3d",
                                   "lo_sa_3e","lo_sa_3f", "lo_sa_3g", "lo_sa_3h", 
                                   "lo_sa_3i")]

# Calculate Cronbach's Alpha coefficient for the survey response variables
cronbach.alpha(mp_response_data, CI = T, na.rm = T) # Ignore missing values while performing calculation


#################### 2.3 CONTROL VARIABLES

####### 2.3.1 SUPERVISOR'S WORK EXPERIENCE IN MONTHS 

## Create a function for year to month conversion 

conversion = function(x){ 
  z <- (x*12)
  return(z) 
}

# Apply the conversion function to obtain a new variable 
merged_data$'sup_wh_8_year_new' <- conversion(merged_data$sup_wh_8_year)

# Relocate the new variable in the data frame
merged_data <- merged_data |> 
  relocate(sup_wh_8_year_new, .after = sup_wh_8_year)

# Create a new variable for the total number of months worked by supervisors 
merged_data$'sup_wh_8_total_months' <- rowSums(merged_data[,c("sup_wh_8_year_new", "sup_wh_8_month")])

# Relocate the new variable in the data frame
merged_data <- merged_data |> 
  relocate(sup_wh_8_total_months, .after = sup_wh_8_month)


####### 2.3.2 SUPERVISOR AND LINE OPERATOR'S SEX

## Recode the sex variable: female = 1 and male = 0

# Recode variable for supervisors
merged_data$sup_di_2 <- replace(merged_data$sup_di_2, merged_data$sup_di_2==1, 0)
merged_data$sup_di_2 <- replace(merged_data$sup_di_2, merged_data$sup_di_2==2, 1)

# Recode variable for line operators
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==1, 0)
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==2, 1)


####### 2.3.3 SUPERVISOR AND LINE OPERATOR'S EDUCATION STATUS

## Recode variable for certain values of 'total years of education' variable

# Recode variable for supervisors
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 77, 0, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 15, 17, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 14, 15, merged_data$sup_di_3)

# Recode variable for line operators
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 77, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 88, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 16, 10, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 15, 17, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 14, 15, merged_data$lo_di_3)


####### 2.3.4 SUPERVISOR AND LINE OPERATOR'S MARITAL STATUS

## Create binary variables for marital status: married = 1 and unmarried = 0

# Create variable for supervisors
merged_data$'sup_di_4_new' <- ifelse(merged_data$sup_di_4==2, "1", "0")

# Relocate the new variable in the data frame 
merged_data <- merged_data |> 
  relocate(sup_di_4_new, .after = sup_di_4)

# Create variable for line operators
merged_data$'lo_di_4_new' <- ifelse(merged_data$lo_di_4==2, "1", "0")

# Relocate the new variable in the data frame 
merged_data <- merged_data |> 
  relocate(lo_di_4_new, .after = lo_di_4)


####### 2.3.5 SUPERVISOR AND LINE OPERATOR'S EXPECTED YEARS IN THE FACTORY

# Replace negative values with NA for supervisors
merged_data$sup_wh_9_year <- ifelse(merged_data$sup_wh_9_year==-97, NA, merged_data$sup_wh_9_year)

# Replace negative values with NA for line operators
merged_data$lo_wh_6_year <- ifelse(merged_data$lo_wh_6_year==-97, NA, merged_data$lo_wh_6_year)


###################### SECTION 3: RENAMING AND ADDING LABELS #########################
######################################################################################

#################### 3.1 RENAME VARIABLES

# Rename variables in the merged data set for ease of use in the analysis
merged_data <- merged_data |> 
  rename(trainee_status = sup_res_type, wc_index = lo_wc_3_zscore, mp_index = lo_sa_3_zscore, 
         sup_workexper_mths = sup_wh_8_total_months, sup_age = sup_di_1, sup_gender = sup_di_2, 
         sup_educ = sup_di_3, sup_marital_status = sup_di_4_new, lo_age = lo_di_1, 
         lo_gender = lo_di_2, lo_educ = lo_di_3, lo_marital_status = lo_di_4_new, 
         factory_code = sup_gi_4, line_code = sup_line_code)

#################### 3.2 ADD LABELS TO VARIABLES

# Add labels to variables in the merged data set for better understanding
merged_data <- apply_labels(merged_data,
                            trainee_status = "Trainee or Comparison supervisor",
                            wc_index = "Working conditions index",
                            mp_index = "Management practices index", 
                            sup_workexper_mths = "Supervisor's work experience in months", 
                            sup_age = "Supervisor's age", 
                            sup_educ = "Supervisor's education", 
                            sup_marital_status = "Supervisor's marital status", 
                            lo_age = "Line operator's age", 
                            lo_gender = "Line operator's gender", 
                            lo_educ = "Line operator's education",
                            lo_marital_status = "Line operator's marital status", 
                            line_code = "Line Code") 


#################### 3.3 SAVE THE MERGED AND CLEANED DATA SET

# Save the data set as a .RData file
# Note: In addition to specifying main file path, specify the folder where the data set should be stored.
# For example, here it will be stored in the folder 'Data' within root. 
save(merged_data, file = file.path(root, "Data/merged_data.RData"))

# Save the data set as a .dta file
write_dta(merged_data, file.path(root, "Data/merged_data.dta"))


######################################################################################
#################### SECTION B: DESCRIPTIVE TABLES AND PLOTS #########################
######################################################################################

# Import merged data set
merged_data <- read_dta(file.path(root, "Data/merged_data.dta"))


################ SECTION 1: SUMMARY STATISTICS AND T-TEST TABLE ######################
######################################################################################

summary_stat <- function(data, vars) { 
  
  # Subset the data for treatment and control groups using the 'vars' vector
  treatment_data <- data[data$trainee_status == 1, vars, drop = FALSE]  # Treatment group
  control_data <- data[data$trainee_status == 0, vars, drop = FALSE]  # Control group

  # Ensure all variables in the data are numeric for summary statistics calculation in the next step
  treatment_data <- treatment_data |>        # Treatment group
    mutate_if(is.character, as.numeric) |>   # Convert character columns to numeric (if possible)
    mutate_if(is.factor, as.numeric)         # Convert factor columns to numeric (if possible)
  
  control_data <- control_data |>            # Control group
    mutate_if(is.character, as.numeric) |>   # Convert character columns to numeric (if possible)
    mutate_if(is.factor, as.numeric)         # Convert factor columns to numeric (if possible)
  
  # Compute summary statistics for the treatment and control groups
  means_treat <- colMeans(treatment_data, na.rm = TRUE) # Treatment mean (ignore missing values while performing calculation)
  sd_treat <- apply(treatment_data, 2, sd, na.rm = TRUE) # Treatment standard deviation (ignore missing values while performing calculation)
  
  means_control <- colMeans(control_data, na.rm = TRUE) # Control mean (ignore missing values while performing calculation)
  sd_control <- apply(control_data, 2, sd, na.rm = TRUE) # Control standard deviation (ignore missing values while performing calculation)
  
  # Compute p-values for each variable using t-test
  p_values <- sapply(vars, function(var) {     # Apply the function to all variables in the data
    t.test(treatment_data[[var]], control_data[[var]], var.equal = TRUE)$p.value  # Extract p value from t-test
  })
  
  # Combine results into a data frame 
  summary_table <- data.frame(
    Treatment_Mean = round(means_treat, 3),
    Treatment_SD = round(sd_treat, 3),
    Control_Mean = round(means_control, 3),
    Control_SD = round(sd_control, 3),
    P_Value = round(p_values, 3)  # Add p-values in the last column
  )
  
}


########################## 1.1 SUMMARY STATISTICS TABLE FOR SUPERVISORS

# Specify the variables to be included in the summary table
vars <- c("sup_age", "sup_gender", "sup_marital_status", "sup_di_5", "sup_di_11",  
          "sup_wh_6", "sup_wh_7_year", "sup_wh_8_year", "sup_wh_9_year", 
          "sup_wh_salary", "sup_wh_10")  

# Apply the summary_stat function 
summary_table <- summary_stat(
  data = merged_data,
  vars = vars
)

# Define new variable names for the summary table
var_names <- c(
  "sup_age" = "Age",
  "sup_gender" = "Gender",
  "sup_marital_status" = "Marital Status",
  "sup_di_5" = "Number of children", 
  "sup_di_11" = "Family size",
  "sup_wh_6" = "Number of factories worked", 
  "sup_wh_7_year" = "Year(s) in the current factory", 
  "sup_wh_8_year" = "Year(s) in the current position", 
  "sup_wh_9_year" = "Expected years in the factory",
  "sup_wh_salary" = "Salary in last position (BDT)",
  "sup_wh_10" = "Last month's salary (BDT)"
)

# Assign new row names based on the variable names in the data 
rownames(summary_table) <- var_names[rownames(summary_table)]

# Assign new column names 
colnames(summary_table) <- c("Treatment Mean", "Treatment SD", "Control Mean", 
                             "Control SD", "P-Value")

# Output the summary statistics table using stargazer
stargazer(summary_table, 
          type = "html", 
          summary = FALSE, 
          title = "Table 1: Summary Statistics and T-Test for Supervisors' Baseline Characteristics", 
          out = file.path(tables_wd,"summarystat_table1.html"))


########################## 1.2 SUMMARY STATISTICS TABLE FOR LINE OPERATORS

# Specify the variables to be included in the summary table
vars <- c("lo_age", "lo_gender", "lo_marital_status", "lo_di_5", "lo_di_11",  
          "lo_wh_3", "lo_wh_4_year", "lo_wh_5_year", "lo_wh_6_year", 
          "lo_wh_7")  

# Apply the summary_stat function 
summary_table <- summary_stat(
  data = merged_data,
  vars = vars
)

# Define new variable names for the summary table
var_names <- c(
  "lo_age" = "Age",
  "lo_gender" = "Gender",
  "lo_marital_status" = "Marital Status",
  "lo_di_5" = "Number of children", 
  "lo_di_11" = "Family size",
  "lo_wh_3" = "Number of factories worked", 
  "lo_wh_4_year" = "Year(s) in the current factory", 
  "lo_wh_5_year" = "Year(s) in the current position", 
  "lo_wh_6_year" = "Expected years in the factory",
  "lo_wh_7" = "Last month's salary (BDT)"
)

# Assign new row names based on the variable names in the data 
rownames(summary_table) <- var_names[rownames(summary_table)]

# Assign new column names 
colnames(summary_table) <- c("Treatment Mean", "Treatment SD", "Control Mean", 
                             "Control SD", "P-Value")

# Output the summary statistics table using stargazer
stargazer(summary_table, 
          type = "html", 
          summary = FALSE, 
          title = "Table 2: Summary Statistics and T-Test for Line Operators' Baseline Characteristics", 
          out = file.path(tables_wd,"summarystat_table2.html"))



###################### SECTION 2: DESCRIPTIVE PLOTS ##################################
######################################################################################

## Converting Trainee Status into Factor variable
merged_data$trainee_status <- ifelse(merged_data$trainee_status == 1, "Trainee", "Control")

########################## 2.1 DESCRIPTIVE PLOTS FOR LINE OPERATORS

# 1. Density Curve for Line Operator's Age by Supervisor's Trainee Status

plot1 <- ggplot(merged_data, aes(x = lo_age, fill = trainee_status)) +
  geom_density(alpha = 0.5) +                            # Add a density curve 
  ggtitle("Age") +                                       # Add the plot title 
  xlab("Age") +                                          # Label the x axis 
  ylab("Density") +                                      # Label the y axis 
  scale_fill_brewer(palette = "Pastel1") +               # Set the color scheme 
  theme(plot.title = element_text(size=10, face="bold"), # Adjust the text size of title
        axis.title.x = element_text(size=10),            # Adjust the text size of x axis
        axis.title.y = element_text(size=10)) +          # Adjust the text size of y axis
  labs(fill = "")                                        # Remove the title of the legend 
plot1

# 2. Stacked bar plot for Line Operator's Gender by Supervisor's Trainee Status 

## Converting line operator's gender into Factor variable
merged_data$lo_gender <- ifelse(merged_data$lo_gender == 1, "Female", "Male")

plot2 <- ggplot(merged_data, aes(x = trainee_status, fill = lo_gender)) +
  geom_bar(position = "fill", stat = "count") +  # Add a stacked bar plot
  ggtitle("Gender") +
  xlab("Trainee Status") +
  ylab("Percentage of Line Operators") +
  scale_y_continuous(labels = scales::percent) + # Format the y-axis labels as percentages
  scale_fill_brewer(palette = "Pastel1") +
  theme(plot.title = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) + 
  labs(fill = "") 
plot2

# 3. Stacked Bar plot for Line Operator's Marital Status 

merged_data$lo_marital_status <- ifelse(merged_data$lo_marital_status == 1, "Married", "Unmarried")

plot3 <- ggplot(merged_data, aes(x = trainee_status, fill = lo_marital_status)) +
  geom_bar(position = "fill", stat = "count") +  # Add a stacked bar plot
  ggtitle("Marital Status") +
  xlab("Trainee Status") +
  ylab("Percentage of Line Operators") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme(plot.title = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) + 
  labs(fill = "") 
plot3

# 4. Box plot for Line Operator's Years of Education by Supervisor's Trainee Status

plot4 <- ggplot(merged_data, aes(x=trainee_status, y=lo_educ, fill=trainee_status)) + 
  geom_boxplot() +                          # Add a box plot 
  scale_fill_brewer(palette = "Pastel1") +
  ggtitle("Educational level") + 
  xlab("Trainee Status") +  
  ylab("Years of Education") + 
  theme(plot.title = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) + 
  labs(fill = "") 
plot4

## Combine all the line operator plots 

# Save the plot as a JPEG file adjusting the width, height and resolution
jpeg(file.path(plots_wd, "lo_plot.jpeg"), 
     width=3000, height=2200, res=250)

# Generate the plot
lo_plot <- plot1 + plot2 + plot3 + plot4 +  # Add plots together
  plot_annotation(title = "Descriptive Plots for Line Operators by Supervisor's Trainee Status", # Add main plot title
                  theme = theme(plot.title = element_text(size=12, face="bold", hjust = 0.45)))  # Adjust the text size, font and position of title
lo_plot

# Close the graphics device
dev.off()


########################## 2.2 T-TEST PLOTS FOR MANAGEMENT PRACTICE INDEX SUB-COMPONENTS

## 5a. Supervisor is more confident

# Convert into a binary variable
merged_data$lo_sa_3a <- ifelse(merged_data$lo_sa_3a < 5, 0, 1)

# Generate the plot
plot5a <- ggplot(merged_data, aes(x = trainee_status, y = lo_sa_3a, fill = trainee_status)) +
  geom_bar(stat = 'summary', fun = mean) +                 # Add a bar chart that shows the mean of each group
  geom_errorbar(stat = 'summary', width = 0.2) +           # Add error bars 
  geom_signif(comparisons = list(c("Control", "Trainee")), # T-test between Treatment and Control
              map_signif_level = function(p) {             # Define how p-values are mapped to significance stars
                if (p < 0.01) "***"
                else if (p < 0.05) "**"
                else if (p < 0.1) "*"
                else "p > 0.1"
              }) + 
  ggtitle("Supervisor is more confident") + # Add the plot title
  xlab("Trainee Status") + ylab("Mean Proportion") +         # Add labels to x and y axis
  scale_fill_brewer(palette = "PuBu") + # Set the color scheme 
  theme(plot.title = element_text(size = 10, face = "bold"), # Adjust the text size of title
        axis.title.x = element_text(size = 10),              # Adjust the text size of x axis
        axis.title.y = element_text(size = 10)) +            # Adjust the text size of y axis
  labs(fill = "")                                            # Remove the title of the legend 
plot5a


## 5b. Supervisor is better at remaining calm in stressful situations 

# Convert into a binary variable
merged_data$lo_sa_3b <- ifelse(merged_data$lo_sa_3b < 5, 0, 1)

# Generate the plot
plot5b <- ggplot(merged_data, aes(x = trainee_status, y = lo_sa_3b, fill = trainee_status)) +
  geom_bar(stat = 'summary', fun = mean) +
  geom_errorbar(stat = 'summary', width = 0.2) +
  geom_signif(comparisons = list(c("Control", "Trainee")),
              map_signif_level = function(p) {
                if (p < 0.01) "***"
                else if (p < 0.05) "**"
                else if (p < 0.1) "*"
                else "p > 0.1"
              }) + 
  ggtitle("Supervisor is better at remaining calm in stressful situations") +
  xlab("Trainee Status") + ylab("Mean Proportion") + 
  scale_fill_brewer(palette = "PuBu") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) + 
  labs(fill = "") 
plot5b

## 5c. Supervisor is better at motivating operators

# Convert into a binary variable
merged_data$lo_sa_3c <- ifelse(merged_data$lo_sa_3c < 5, 0, 1)

# Generate the plot
plot5c <- ggplot(merged_data, aes(x = trainee_status, y = lo_sa_3c, fill = trainee_status)) +
  geom_bar(stat = 'summary', fun = mean) +
  geom_errorbar(stat = 'summary', width = 0.2) +
  geom_signif(comparisons = list(c("Control", "Trainee")),
              map_signif_level = function(p) {
                if (p < 0.01) "***"
                else if (p < 0.05) "**"
                else if (p < 0.1) "*"
                else "p > 0.1"
              }) + 
  ggtitle("Supervisor is better at motivating operators") +
  xlab("Trainee Status") + ylab("Mean Proportion") + 
  scale_fill_brewer(palette = "PuBu") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) + 
  labs(fill = "") 
plot5c

## 5d. Supervisor is better at helping operators improve their skills

# Convert into a binary variable
merged_data$lo_sa_3g <- ifelse(merged_data$lo_sa_3g < 5, 0, 1)

# Generate the plot
plot5d <- ggplot(merged_data, aes(x = trainee_status, y = lo_sa_3g, fill = trainee_status)) +
  geom_bar(stat = 'summary', fun = mean) +
  geom_errorbar(stat = 'summary', width = 0.2) +
  geom_signif(comparisons = list(c("Control", "Trainee")),
              map_signif_level = function(p) {
                if (p < 0.01) "***"
                else if (p < 0.05) "**"
                else if (p < 0.1) "*"
                else "p > 0.1"
              }) + 
  ggtitle("Supervisor is better at helping operators improve their skills") +
  xlab("Trainee Status") + ylab("Mean Proportion") + 
  scale_fill_brewer(palette = "PuBu") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) + 
  labs(fill = "") 
plot5d

## 5e. Supervisor is better at correcting mistakes and ensuring product quality

# Convert into a binary variable
merged_data$lo_sa_3d <- ifelse(merged_data$lo_sa_3d < 5, 0, 1)

# Generate the plot
plot5e <- ggplot(merged_data, aes(x = trainee_status, y = lo_sa_3d, fill = trainee_status)) +
  geom_bar(stat = 'summary', fun = mean) +
  geom_errorbar(stat = 'summary', width = 0.2) +
  geom_signif(comparisons = list(c("Control", "Trainee")),
              map_signif_level = function(p) {
                if (p < 0.01) "***"
                else if (p < 0.05) "**"
                else if (p < 0.1) "*"
                else "p > 0.1"
              }) + 
  ggtitle("Supervisor is better at correcting mistakes and ensuring product quality") +
  xlab("Trainee Status") + ylab("Mean Proportion") + scale_fill_brewer(palette = "PuBu") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) + labs(fill = "") 
plot5e

## 5f. Supervisor is better at meeting production targets

# Convert into a binary variable
merged_data$lo_sa_3i <- ifelse(merged_data$lo_sa_3i < 5, 0, 1)

# Generate the plot
plot5f <- ggplot(merged_data, aes(x = trainee_status, y = lo_sa_3i, fill = trainee_status)) +
  geom_bar(stat = 'summary', fun = mean) +
  geom_errorbar(stat = 'summary', width = 0.2) +
  geom_signif(comparisons = list(c("Control", "Trainee")),
              map_signif_level = function(p) {
                if (p < 0.01) "***"
                else if (p < 0.05) "**"
                else if (p < 0.1) "*"
                else "p > 0.1"
              }) + 
  ggtitle("Supervisor is better at meeting production targets") +
  xlab("Trainee Status") + ylab("Mean Proportion") + 
  scale_fill_brewer(palette = "PuBu") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) + 
  labs(fill = "") 
plot5f

## Combine all the plots of the sub-components

# Save the plot as a JPEG file adjusting for the width, height and resolution
jpeg(file.path(plots_wd, "ttest_plot.jpeg"), 
     width=4200, height=3000, res=250)

# Generate the plot
ttest_plot <- plot5a + plot5b + plot5c + plot5d + plot5e + plot5f +    # Add plots together
  plot_annotation(title = "T-test plots for sub-components of Management Practices Index by Trainee's Status", # Main plot title
                  theme = theme(plot.title = element_text(size=12, face="bold", hjust = 0.45))) # Adjust for text size, font, and position of title
ttest_plot

# Close the graphics device
dev.off()


######################################################################################
######################### SECTION C: DATA ANALYSIS ###################################
######################################################################################

# Import merged data set
merged_data <- read_dta(file.path(root, "Data/merged_data.dta"))


####### SECTION 1: OLS FIXED EFFECTS REGRESSION BEFORE STATISTICAL MATCHING ##########
######################################################################################

# Define a function to calculate clustered standard errors for each fitted linear regression 
# model

cluster_se <- function(model, cluster_var, model_name) { 
  
  # Calculate clustered standard errors
  cl_cov <- vcovCL(model, cluster = cluster_var)
  se <- sqrt(diag(cl_cov))
  
  # Assign standard errors output directly to the environment
  assign(paste0("se_", model_name), se, envir = .GlobalEnv)
}


########################## 1.1 OUTCOME VARIABLE: WORKING CONDITIONS INDEX

# Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_1 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_gender + lo_educ + 
                lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_1)

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_1, ~id_sup_merge, "model_1")

# Same as Model 1 plus interaction with supervisor's work experience 
model_2 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_gender + lo_educ + 
                lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_2)  

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_2, ~id_sup_merge, "model_2")


################# ADDITIONAL REGRESSIONS WITH SAMPLE RESTRCITIONS 

########## 1.1.1 Restrict the sample to female line operators 

# Filter for female line operators
female_lo_data <- merged_data |> 
  filter(lo_gender==1)

# Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_3 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_educ + lo_marital_status  + 
                factor(factory_code), 
              data = female_lo_data)
summary(model_3)

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_3, ~id_sup_merge, "model_3")

# Same as Model 3 plus interaction with supervisor's work experience 
model_4 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_educ + lo_marital_status  + 
                factor(factory_code), 
              data = female_lo_data)
summary(model_4)  

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_4, ~id_sup_merge, "model_4")

########## 1.1.2 Add a dummy controlling for female comparison supervisors 

# Create a variable for female comparison supervisor 
merged_data$'sup_comp_female' <- ifelse(merged_data$sup_gender == 1 & merged_data$trainee_status == 0, 1, 0)

# Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_5 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_comp_female + 
                sup_age + sup_educ + sup_marital_status + lo_age + lo_gender + 
                lo_educ + lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_5)

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_5, ~id_sup_merge, "model_5")

# Same as Model 5 plus interaction with supervisor's work experience 
model_6 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_comp_female + 
                sup_age + sup_educ + sup_marital_status + lo_age + lo_gender + 
                lo_educ + lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_6)  

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_6, ~id_sup_merge, "model_6")


########################## 1.2 OUTCOME VARIABLE: MANAGEMENT PRACTICES INDEX

# Regression of Trainee Status of Supervisors on Operator Reported Outcomes of Management Practices
model_7 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_gender + lo_educ + 
                lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_7)

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_7, ~id_sup_merge, "model_7")

# Same as Model 7 plus interaction with supervisor's work experience
model_8 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_gender + lo_educ + 
                lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_8)  

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_8, ~id_sup_merge, "model_8")


################# ADDITIONAL REGRESSIONS WITH SAMPLE RESTRCITIONS 

########## 1.2.1 Restrict the sample to female line operators 

# Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_9 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_educ + lo_marital_status  + 
                factor(factory_code), 
              data = female_lo_data)
summary(model_9)

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_9, ~id_sup_merge, "model_9")

# Same as Model 9 plus interaction with supervisor's work experience
model_10 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + 
                 sup_marital_status + lo_age + lo_educ + lo_marital_status  + 
                 factor(factory_code), 
               data = female_lo_data)
summary(model_10) 

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_10, ~id_sup_merge, "model_10")


########## 1.2.2 Add a dummy controlling for female comparison supervisors 

# Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_11 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_comp_female + 
                 sup_age + sup_educ + sup_marital_status + lo_age + lo_gender + 
                 lo_educ + lo_marital_status  + factor(factory_code), 
               data = merged_data)
summary(model_11)

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_11, ~id_sup_merge, "model_11")

# Same as Model 11 plus interaction with supervisor's work experience and supervisor characteristics
model_12 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_comp_female + 
                 sup_age + sup_educ + sup_marital_status + lo_age + lo_gender + 
                 lo_educ + lo_marital_status  + factor(factory_code), 
               data = merged_data)
summary(model_12)  

# Apply the function to calculate clustered standard errors by supervisor's ID
cluster_se(model_12, ~id_sup_merge, "model_12")


####### SECTION 2: OLS FIXED EFFECTS REGRESSION AFTER STATISTICAL MATCHING ###########
######################################################################################

########################## 2.1 NEAREST NEIGHBOR MATCHING 

# Set the seed to ensure same matching results are obtained each time the 'matchit' function is called
set.seed(123)

################# 2.1.1 MAHALNOBIS DISTANCE 

# Perform Nearest Neighbor Matching using Mahalanobis distance metric
# Note: We are performing Mahalanobis distance matching within propensity score calipers to ensure 
# the common support assumption holds.  

nn_maha_match <- matchit(trainee_status ~ sup_workexper_mths + sup_age + sup_educ, 
                         data = merged_data,
                         method = "nearest",  # Method of Matching is Nearest Neighbor 
                         distance = "glm",    # Propensity scores estimated using a generalized linear model
                         mahvars =  ~ sup_workexper_mths + sup_age + sup_educ, # Specify variables for Mahalanobis distance matching
                         discard = "control", # Dropping unmatched control observations
                         m.order = "random",  # The order that Matching takes place
                         estimand = "ATT",    # Estimand is Average Treatment Effect on the Treated
                         ratio = 1,           # Number of control units matched to each treated unit
                         replace = F)         # Matching done without replacement

# Extract matched data to be used for regression later
nn_maha_data <- match.data(nn_maha_match)

# Add covariate labels and store them in a data frame to create covariate balance plot
cov_label <- data.frame(old = c("sup_workexper_mths", "sup_age", "sup_educ"),
                new = c("Supervisor's work experience", "Supervisor's age", "Supervisor's education"))

## Create the covariance balance plot

# Save the plot as a JPEG file adjusting for width, height and resolution
jpeg(file.path(plots_wd, "balance_plot_maha.jpeg"), 
     width = 1600, height = 960, res = 150)

# Generate the plot
love.plot(nn_maha_match, var.order = "unadjusted",  
          var.names = cov_label, drop.distance = TRUE, abs = FALSE, 
          binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))

# Close the graphics device
dev.off()

## Create the plot for Common Support Assumption

# Save the plot as a JPEG file adjusting for width, height and resolution
jpeg(file.path(plots_wd, "commonsupport_plot_maha.jpeg"),
     pointsize = 4, width = 1600, height = 960, res = 150)

# Generate the plot
bal.plot(nn_maha_match, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))

# Close the graphics device
dev.off()


################# 2.1.2 PROPENSITY SCORE 

### Perform Nearest Neighbor Matching using Propensity Score
nn_ps_match <- matchit(trainee_status ~ sup_workexper_mths + sup_age + sup_educ, 
                       data = merged_data,
                       method = "nearest",  # Method of Matching is Nearest Neighbor 
                       distance = "glm",    # Propensity scores estimated using a generalized linear model 
                       discard = "control", # Dropping unmatched control observations
                       m.order = "random",  # The order that Matching takes place
                       estimand = "ATT",    # Estimand is Average Treatment Effect on the Treated
                       ratio = 1,           # Number of control units matched to each treated unit
                       replace = F)         # Matching done without replacement

### Extract matched data to be used for regression later
nn_ps_data <- match.data(nn_ps_match)

## Create the covariance balance plot

# Save the plot as a JPEG file adjusting for width, height and resolution
jpeg(file.path(plots_wd, "balance_plot_ps.jpeg"), 
     width = 1600, height = 960, res = 150)

# Generate the plot
love.plot(nn_maha_match, var.order = "unadjusted",  
          var.names = cov_label, drop.distance = TRUE, abs = FALSE, 
          binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))

# Close the graphics device
dev.off()

## Create the plot for Common Support Assumption

# Save the plot as a JPEG file adjusting for width, height and resolution
jpeg(file.path(plots_wd, "commonsupport_plot_ps.jpeg"),
     pointsize = 4, width = 1600, height = 960, res = 150)

# Generate the plot
bal.plot(nn_maha_match, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))

# Close the graphics device
dev.off()


########################## 2.2 REGRESSION AFTER NEAREST NEIGHBOR MATCHING USING MAHALANOBIS DISTANCE

################# 2.2.1 OUTCOME VARIABLE: WORKING CONDITIONS INDEX 

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
nn_maha_wc_att_mod1 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + 
                            sup_educ + sup_marital_status + lo_age + lo_gender + 
                            lo_educ + lo_marital_status  + factor(factory_code),
                          data = nn_maha_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_maha_wc_att_mod2 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + 
                            sup_educ + sup_marital_status + lo_age + lo_gender + 
                            lo_educ + lo_marital_status  + factor(factory_code), 
                          data = nn_maha_data, weights = weights)


# Clustered covariance matrix estimation for both models by supervisor's ID
nn_maha_wc_mod1_v <- vcovCL(nn_maha_wc_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_maha_wc_mod2_v <- vcovCL(nn_maha_wc_att_mod2, cluster = ~ id_sup_merge + subclass) 

# Calculate clustered standard errors for both models by supervisor's ID
nn_maha_wc_mod1_se <- sqrt(diag(nn_maha_wc_mod1_v))
nn_maha_wc_mod2_se <- sqrt(diag(nn_maha_wc_mod2_v))

################# 2.2.2 OUTCOME VARIABLE: MANAGEMENT PRACTICES INDEX 

## Regression of Trainee Status of Supervisors on Operator Reported Outcomes of Management Practices
nn_maha_mp_att_mod1 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + 
                            sup_educ + sup_marital_status + lo_age + lo_gender + 
                            lo_educ + lo_marital_status  + factor(factory_code),
                          data = nn_maha_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_maha_mp_att_mod2 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + 
                            sup_educ + sup_marital_status + lo_age + lo_gender + 
                            lo_educ + lo_marital_status  + factor(factory_code), 
                          data = nn_maha_data, weights = weights)

# Clustered covariance matrix estimation for both models by supervisor's ID
nn_maha_mp_mod1_v <- vcovCL(nn_maha_mp_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_maha_mp_mod2_v <- vcovCL(nn_maha_mp_att_mod2, cluster = ~ id_sup_merge + subclass) 

# Calculate clustered standard errors for both models by supervisor's ID
nn_maha_mp_mod1_se <- sqrt(diag(nn_maha_mp_mod1_v))
nn_maha_mp_mod2_se <- sqrt(diag(nn_maha_mp_mod2_v))


########################## 2.3 REGRESSION AFTER NEAREST NEIGHBOR MATCHING USING PROPENSITY SCORE

################# 2.3.1 OUTCOME VARIABLE: WORKING CONDITIONS INDEX 

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
nn_ps_wc_att_mod1 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + 
                          sup_educ + sup_marital_status + lo_age + lo_gender + 
                          lo_educ + lo_marital_status  + factor(factory_code),
                        data = nn_ps_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_ps_wc_att_mod2 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + 
                          sup_educ + sup_marital_status + lo_age + lo_gender + 
                          lo_educ + lo_marital_status  + factor(factory_code), 
                        data = nn_ps_data, weights = weights)

# Clustered covariance matrix estimation for both models by supervisor's ID
nn_ps_wc_mod1_v <- vcovCL(nn_ps_wc_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_ps_wc_mod2_v <- vcovCL(nn_ps_wc_att_mod2, cluster = ~ id_sup_merge + subclass) 

# Calculate clustered standard errors for both models by supervisor's ID
nn_ps_wc_mod1_se <- sqrt(diag(nn_ps_wc_mod1_v))
nn_ps_wc_mod2_se <- sqrt(diag(nn_ps_wc_mod2_v))

################# 2.3.2 OUTCOME VARIABLE: MANAGEMENT PRACTICES INDEX 

## Regression of Trainee Status of Supervisors on Operator Reported Outcomes of Management Practices
nn_ps_mp_att_mod1 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + 
                          sup_educ + sup_marital_status + lo_age + lo_gender + 
                          lo_educ + lo_marital_status  + factor(factory_code),
                        data = nn_ps_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_ps_mp_att_mod2 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + 
                          sup_educ + sup_marital_status + lo_age + lo_gender + 
                          lo_educ + lo_marital_status  + factor(factory_code), 
                        data = nn_ps_data, weights = weights)

# Clustered covariance matrix estimation for both models by supervisor's ID
nn_ps_mp_mod1_v <- vcovCL(nn_ps_mp_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_ps_mp_mod2_v <- vcovCL(nn_ps_mp_att_mod2, cluster = ~ id_sup_merge + subclass) 

# Calculate clustered standard errors for both models by supervisor's ID
nn_ps_mp_mod1_se <- sqrt(diag(nn_ps_mp_mod1_v))
nn_ps_mp_mod2_se <- sqrt(diag(nn_ps_mp_mod2_v))


################ SECTION 3: LOGISTIC FIXED EFFECTS REGRESSION ########################
######################################################################################

# Define a function to calculate Wald Chi Square Test Statistic, odds ratio estimates and 
# clustered standard errors for each fitted logistic regression model 

logit_model <- function(model, cluster_var, model_name) { 
  
  # Calculate Wald Chi-Square Test Statistic
  chi2_stat <- round(
    wald.test(b = coef(model), Sigma = vcov(model), 
              Terms = 2:length(coef(model)))[["result"]][["chi2"]], 3
    )
  
  # Convert estimates into odds ratio
  odds_ratios <- exp(coef(model))
  
  # Calculate clustered standard errors
  cl_cov <- vcovCL(model, cluster = cluster_var)
  se_values <- sqrt(diag(cl_cov))
  
  # Convert standard errors into odds ratio standard errors
  se_or <- se_values * odds_ratios
  
  # Assign each output directly to the environment
  assign(paste0("chi2_", model_name), chi2_stat, envir = .GlobalEnv)
  assign(paste0("or_logit_", model_name), odds_ratios, envir = .GlobalEnv)
  assign(paste0("se_or_logit_", model_name), se_or, envir = .GlobalEnv)
  
}


########################## 3.1 INDIVIDUAL COMPONENTS OF WORKING CONDITIONS INDEX 

############### 3.1.1 Compared to typical supervisor in factory, to what extent did or 
## does your supervisor give extra support to less skilled operators?

## Convert into a binary variable for logistic regression
merged_data$lo_wc_3a <- ifelse(merged_data$lo_wc_3a < 5, 0, 1)

## Convert into a factor and add labels to factor levels 
merged_data$'lo_wc_3a_bin' <- factor(merged_data$lo_wc_3a, levels = c(0,1), labels = c("Much less", "Much more"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_1 <- glm(lo_wc_3a_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_1)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_1, ~id_sup_merge, "model_1")

## Same as Model 1 plus interaction with supervisor's work experience 
logit_model_2 <- glm(lo_wc_3a_bin ~ trainee_status*sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_2)  

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_2, ~id_sup_merge, "model_2")


############### 3.1.2 Compared to typical supervisor, to what extent did or does your 
# supervisor use praise to motivate operators?

## Convert into a binary variable for logistic regression
merged_data$lo_wc_3b <- ifelse(merged_data$lo_wc_3b < 5, 0, 1)

## Convert into a factor and adding labels to factor levels 
merged_data$'lo_wc_3b_bin' <- factor(merged_data$lo_wc_3b, levels = c(0,1), labels = c("Much less", "Much more"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_3 <- glm(lo_wc_3b_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_3)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_3, ~id_sup_merge, "model_3")

## Same as Model 3 plus interaction with supervisor's work experience 
logit_model_4 <- glm(lo_wc_3b_bin ~ trainee_status*sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_4) 

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_4, ~id_sup_merge, "model_4")


############### 3.1.3 Compared to typical supervisor, to what extent did or does your 
# supervisor use shouting or abusive language to motivate operators?

# Convert into a binary variable for logistic regression
merged_data$lo_wc_3c <- ifelse(merged_data$lo_wc_3c < 5, 0, 1)

# Convert into a factor and adding labels to factor levels 
merged_data$'lo_wc_3c_bin' <- factor(merged_data$lo_wc_3c, levels = c(0,1), labels = c("Much more", "Much less"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_5 <- glm(lo_wc_3c_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_5)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_5, ~id_sup_merge, "model_5")

## Same as Model 5 plus interaction with supervisor's work experience 
logit_model_6 <- glm(lo_wc_3c_bin ~ trainee_status*sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_6)  

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_6, ~id_sup_merge, "model_6")


############### 3.1.4 Compared to typical supervisor, to what extent did or does your 
# supervisor involve sewing operators in solving problems on the line?

# Convert into a binary variable for logistic regression
merged_data$lo_wc_3d <- ifelse(merged_data$lo_wc_3d < 5, 0, 1)

# Convert into a factor and adding labels to factor levels 
merged_data$'lo_wc_3d_bin' <- factor(merged_data$lo_wc_3d, levels = c(0,1), labels = c("Much less", "Much more"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_7 <- glm(lo_wc_3d_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_7)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_7, ~id_sup_merge, "model_7")

## Same as Model 7 plus interaction with supervisor's work experience 
logit_model_8 <- glm(lo_wc_3d_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                     + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_8)  

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_8, ~id_sup_merge, "model_8")


########################## 3.2 INDIVIDUAL COMPONENTS OF MANAGEMENT PRACTICES INDEX 

############### 3.2.1 Compared to typical supervisor, my supervisor is more confident 

# Convert into a binary variable for logistic regression
merged_data$lo_sa_3a <- ifelse(merged_data$lo_sa_3a < 5, 0, 1)

# Convert into a factor and adding labels to factor levels 
merged_data$'lo_sa_3a_bin' <- factor(merged_data$lo_sa_3a, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_9 <- glm(lo_sa_3a_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                       sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                       lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_9)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_9, ~id_sup_merge, "model_9")

## Same as Model 9 plus interaction with supervisor's work experience 
logit_model_10 <- glm(lo_sa_3a_bin ~ trainee_status*sup_workexper_mths + sup_age + 
                        sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                        lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_10)  

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_10, ~id_sup_merge, "model_10")


############### 3.2.2 Compared to typical supervisor, my supervisor is better at 
# remaining calm in stressful situations

# Convert into a binary variable for logistic regression
merged_data$lo_sa_3b <- ifelse(merged_data$lo_sa_3b < 5, 0, 1)

# Convert into a factor and adding labels to factor levels 
merged_data$'lo_sa_3b_bin' <- factor(merged_data$lo_sa_3b, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_11 <- glm(lo_sa_3b_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                        sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                        lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_11)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_11, ~id_sup_merge, "model_11")

## Same as Model 11 plus interaction with supervisor's work experience 
logit_model_12 <- glm(lo_sa_3b_bin ~ trainee_status*sup_workexper_mths + sup_age + 
                        sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                        lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_12)  

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_12, ~id_sup_merge, "model_12")


############### 3.2.3 Compared to typical supervisor, my supervisor is better at 
# motivating operators

# Convert into a binary variable for logistic regression
merged_data$lo_sa_3c <- ifelse(merged_data$lo_sa_3c < 5, 0, 1)

# Convert into a factor and adding labels to factor levels 
merged_data$'lo_sa_3c_bin' <- factor(merged_data$lo_sa_3c, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_13 <- glm(lo_sa_3c_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                        sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                        lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_13)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_13, ~id_sup_merge, "model_13")

## Same as Model 13 plus interaction with supervisor's work experience 
logit_model_14 <- glm(lo_sa_3c_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                      + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_14)  

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_14, ~id_sup_merge, "model_14")


############### 3.2.4 Compared to typical supervisor, my supervisor is better at 
# correcting mistakes and ensuring product quality

# Convert into a binary variable for logistic regression
merged_data$lo_sa_3d <- ifelse(merged_data$lo_sa_3d < 5, 0, 1)

# Convert into a factor and adding labels to factor levels 
merged_data$'lo_sa_3d_bin' <- factor(merged_data$lo_sa_3d, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_15 <- glm(lo_sa_3d_bin ~ trainee_status + sup_workexper_mths + sup_age + 
                        sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                        lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_15)

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_15, ~id_sup_merge, "model_15")

## Same as Model 23 plus interaction with supervisor's work experience 
logit_model_16 <- glm(lo_sa_3d_bin ~ trainee_status*sup_workexper_mths + sup_age + 
                        sup_educ + sup_marital_status + lo_age + lo_gender + lo_educ + 
                        lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_16)  

# Apply the logit_model function for chi sq stat and odds ratio estimates and standard errors
logit_model(logit_model_16, ~id_sup_merge, "model_16")


######################################################################################
################# SECTION D: POST ESTIMATION TABLES AND PLOTS ########################
######################################################################################

####################### SECTION 1: REGRESSION TABLES #################################
######################################################################################


############## TABLE 1: Impact of Training on Working Conditions Index - Before Matching

# Create a list of regression models and standard errors pre matching 
wc_prematch_models <- list(model_1, model_2, model_3, model_4, model_5, model_6)
wc_prematch_se <- list(se_model_1, se_model_2, se_model_3, se_model_4, se_model_5, se_model_6)

## Create a row of control means for the regression table

# Filter for the control group
control_data <- merged_data |>
  filter(trainee_status == 0)

# Compute control mean for working conditions index
wc_control_mean <- round(mean(control_data$wc_index, na.rm = TRUE), 3) # Ignore missing values while performing calculation

# Replicate the control mean six times and store as a vector
control_means <- c("Control Mean", rep(wc_control_mean, 6))

# Create a row of fixed effects for the regression table and store as a vector
factory_fixed_effects <- c("Factory Fixed Effects", rep("Yes", 6))

# Create a regression table  
stargazer(wc_prematch_models,   # list of linear models 
          se = wc_prematch_se,  # list of standard errors
          type = "html",        # Save the table in an html format 
          dep.var.labels.include = FALSE,   
          dep.var.caption = "Standardized values of working conditions index",
          title = "Table 1: Impact of Training on Working Conditions Index - Before Matching",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", 
                               "Comparison Supervisor = Female", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", 
                               "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",  # Omit the variable from the main table
          add.lines = list(control_means, factory_fixed_effects),
          out = file.path(tables_wd, "wc_prematch_table.html")) # Save the table in the directory

## Append note to the regression table

# Define the note to include CSS styling for text wrapping, width constraints and space
note_text <- '<div style="max-width: 100%; text-align: left; margin: 0.5; font-size: 14px; line-height: 1.4; margin-top: 5px; padding-top: 0px;">
              <p> Columns (1) and (2) show OLS fixed effects regression results before matching. In addition to all the controls in the first model, 
              the second model includes the interaction between the trainee status and supervisors work experience. Columns (3) and (4) show results for the same 
              model but restrict the sample to female line operators. Columns (5) and (6) show results after adding a dummy for female comparison supervisors. 
              The standard errors are robust and clustered at the supervisors level.</p>
              </div>'

# Read the existing HTML file
html_content <- readLines(file.path(tables_wd, "wc_prematch_table.html"))

# Find the position where the table ends (</table>)
table_end_index <- which(grepl("</table>", html_content))

# Insert the note directly after the table
if (length(table_end_index) > 0) {
  html_content <- append(html_content, note_text, after = table_end_index)
}

# Write the modified HTML back to the regression table
writeLines(html_content, file.path(tables_wd, "wc_prematch_table.html"))


############## TABLE 2: Impact of Training on Management Practices Index - Before Matching

# Create a list of regression models and standard errors pre matching
mp_prematch_models <- list(model_7, model_8, model_9, model_10, model_11, model_12)
mp_prematch_se <- list(se_model_7, se_model_8, se_model_9, se_model_10, se_model_11, se_model_12)

## Create a row of control means for the regression table

# Compute control mean for management practices index
mp_control_mean <- round(mean(control_data$mp_index, na.rm = TRUE), 3) # Ignore missing values while performing calculation

# Replicate the control mean six times and store as a vector
control_means <- c("Control Mean", rep(mp_control_mean, 6))

# Create a row of fixed effects for the regression table and store as a vector
factory_fixed_effects <- c("Factory Fixed Effects", rep("Yes", 6))

# Create a regression table 
stargazer(mp_prematch_models,  # list of linear models 
          se = mp_prematch_se, # list of standard errors
          type = "html",       # Save the table in an html format
          dep.var.labels.include = FALSE,
          dep.var.caption = "Standardized values of management practices index",
          title = "Table 2: Impact of Training on Management Practices Index - Before Matching", 
          covariate.labels = c("Trainee Status", "Supervisor's work experience", 
                               "Comparison Supervisor = Female", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", 
                               "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",  # Omit the variable from the main table
          add.lines = list(control_means, factory_fixed_effects),
          out = file.path(tables_wd, "mp_prematch_table.html")) # Save the table in the directory

## Append note to the regression table

# Define the note to include CSS styling for text wrapping, width constraints and space
note_text <- '<div style="max-width: 100%; text-align: left; margin: 0.5; font-size: 14px; line-height: 1.4; margin-top: 5px; padding-top: 0px;">
              <p> Columns (1) and (2) show OLS fixed effects regression results before matching. In addition to all the controls in the first model, 
              the second model includes the interaction between the trainee status and supervisors work experience. Columns (3) and (4) show results for the same 
              model but restrict the sample to female line operators. Columns (5) and (6) show results after adding a dummy for female comparison supervisors. 
              The standard errors are robust and clustered at the supervisors level.</p>
              </div>'

# Read the existing HTML file
html_content <- readLines(file.path(tables_wd, "mp_prematch_table.html"))

# Find the position where the table ends (</table>)
table_end_index <- which(grepl("</table>", html_content))

# Insert the note directly after the table
if (length(table_end_index) > 0) {
  html_content <- append(html_content, note_text, after = table_end_index)
}

# Write the modified HTML back to the regression table
writeLines(html_content, file.path(tables_wd, "mp_prematch_table.html"))


########################### TABLE 3: Impact of Training on Working Conditions Index - After Matching

## Create a list of regression models and standard errors post matching
wc_postmatch_models <- list(nn_maha_wc_att_mod1, nn_maha_wc_att_mod2, nn_ps_wc_att_mod1, nn_ps_wc_att_mod2)
wc_postmatch_se <- list(nn_maha_wc_mod1_se, nn_maha_wc_mod2_se,nn_ps_wc_mod1_se, nn_ps_wc_mod2_se)

## Create a row of control means for the regression table

# Replicate the control mean six times and store as a vector
control_means <- c("Control Mean", rep(wc_control_mean, 4))

# Create a row of fixed effects for the regression table and store as a vector
factory_fixed_effects <- c("Factory Fixed Effects", rep("Yes", 4))

# Create a regression table 
stargazer(wc_postmatch_models,  # list of linear models post matching
          se = wc_postmatch_se, # list of standard errors
          type = "html",        # Save the table in an html format
          dep.var.labels.include = FALSE,
          column.labels = c("Mahalanobis", "Propensity Score"), 
          column.separate = c(2,2),
          dep.var.caption = "Standardized values of working conditions index",
          title = "Table 3: Impact of Training on Working Conditions Index - After Matching",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", 
                               "Comparison Supervisor = Female", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", 
                               "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",  # Omit the variable from the main table
          add.lines = list(control_means, factory_fixed_effects),
          out = file.path(tables_wd,"wc_postmatch_table.html")) # Save the table in the directory

## Append note to the regression table

# Define the note to include CSS styling for text wrapping, width constraints and space
note_text <- '<div style="max-width: 75%; text-align: left; margin: 0.5; font-size: 14px; line-height: 1.4; margin-top: 5px; padding-top: 0px;">
              <p> Columns (1) and (2) show OLS fixed effects regression results after Mahalanobis Distance matching. In addition to all the controls in the
              first model, the second model includes the interaction between the trainee status and supervisors work experience. Columns (3) and (4) show
              results for the same models using Propensity Score matching.
              The standard errors are robust and clustered at the supervisors level.</p>
              </div>'

# Read the existing HTML file
html_content <- readLines(file.path(tables_wd, "wc_postmatch_table.html"))

# Find the position where the table ends (</table>)
table_end_index <- which(grepl("</table>", html_content))

# Insert the note directly after the table
if (length(table_end_index) > 0) {
  html_content <- append(html_content, note_text, after = table_end_index)
}

# Write the modified HTML back to the regression table
writeLines(html_content, file.path(tables_wd, "wc_postmatch_table.html"))


########################### TABLE 4: Impact of Training on Management Practices Index - After Matching

## Create a list of regression models and standard errors post matching
mp_postmatch_models <- list(nn_maha_mp_att_mod1, nn_maha_mp_att_mod2, nn_ps_mp_att_mod1, nn_ps_mp_att_mod2)
mp_postmatch_se <- list(nn_maha_mp_mod1_se, nn_maha_mp_mod2_se,nn_ps_mp_mod1_se, nn_ps_mp_mod2_se)

## Create a row of control means for the regression table

# Replicate the control mean six times and store as a vector
control_means <- c("Control Mean", rep(mp_control_mean, 4))

# Create a row of fixed effects for the regression table and store as a vector
factory_fixed_effects <- c("Factory Fixed Effects", rep("Yes", 4))

# Create a regression table 
stargazer(mp_postmatch_models,  # list of linear models post matching
          se = mp_postmatch_se, # list of standard errors
          type = "html",        # Save the table in an html format
          dep.var.labels.include = FALSE,
          column.labels = c("Mahalanobis", "Propensity Score"), 
          column.separate = c(2,2),
          dep.var.caption = "Standardized values of management practices index",
          title = "Table 4: Impact of Training on Management Practices Index - After Matching", 
          covariate.labels = c("Trainee Status", "Supervisor's work experience", 
                               "Comparison Supervisor = Female", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", 
                               "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",  # Omit the variable from the main table
          add.lines = list(control_means, factory_fixed_effects),
          out = file.path(tables_wd,"mp_postmatch_table.html")) # Save the table in the directory

## Append note to the regression table

# Define the note to include CSS styling for text wrapping, width constraints and space
note_text <- '<div style="max-width: 70%; text-align: left; margin: 0.5; font-size: 14px; line-height: 1.4; margin-top: 5px; padding-top: 0px;">
              <p> Columns (1) and (2) show OLS fixed effects regression results after Mahalanobis Distance matching. In addition to all the controls in the
              first model, the second model includes the interaction between the trainee status and supervisors work experience. Columns (3) and (4) show
              results for the same models using Propensity Score matching.
              The standard errors are robust and clustered at the supervisors level.</p>
              </div>'

# Read the existing HTML file
html_content <- readLines(file.path(tables_wd, "mp_postmatch_table.html"))

# Find the position where the table ends (</table>)
table_end_index <- which(grepl("</table>", html_content))

# Insert the note directly after the table
if (length(table_end_index) > 0) {
  html_content <- append(html_content, note_text, after = table_end_index)
}

# Write the modified HTML back to the regression table
writeLines(html_content, file.path(tables_wd, "mp_postmatch_table.html"))


########################### TABLE 5: Impact of Training on Working Conditions Index Sub-components

## Create a list of logit models, odds ratio estimates, and standard errors 

wc_logit_models <- list(logit_model_1, logit_model_2, logit_model_3, logit_model_4, 
                        logit_model_5, logit_model_6, logit_model_7, logit_model_8)

wc_logit_coeff <- list(or_logit_model_1, or_logit_model_2, or_logit_model_3, or_logit_model_4, 
                       or_logit_model_5, or_logit_model_6, or_logit_model_7, or_logit_model_8)

wc_logit_se <- list(se_or_logit_model_1, se_or_logit_model_2, se_or_logit_model_3, 
                    se_or_logit_model_4, se_or_logit_model_5, se_or_logit_model_6, 
                    se_or_logit_model_7, se_or_logit_model_8)

## Create a row of control means for the regression table

control_means <-c(mean(control_data$lo_wc_3a, na.rm = TRUE), mean(control_data$lo_wc_3a, na.rm = TRUE), 
                  mean(control_data$lo_wc_3b, na.rm = TRUE), mean(control_data$lo_wc_3b, na.rm = TRUE),
                  mean(control_data$lo_wc_3c, na.rm = TRUE), mean(control_data$lo_wc_3c, na.rm = TRUE),
                  mean(control_data$lo_wc_3d, na.rm = TRUE), mean(control_data$lo_wc_3d, na.rm = TRUE))
control_means <- round(control_means, 3) 
control_means <- c("Control Mean", control_means)

# Create a row of fixed effects for the regression table and store as a vector
factory_fixed_effects <- c("Factory Fixed Effects", rep("Yes", 8))

# Create a regression table 
stargazer(wc_logit_models,
          se = wc_logit_se, 
          coef = wc_logit_coeff,
          p.auto = FALSE, 
          type = "html", 
          dep.var.caption = "Operator Reported Outcomes of Working Conditions",
          dep.var.labels.include = FALSE,
          column.labels = c("Supervisor gives extra support to less skilled operators", 
                            "Supervisor uses praise to motivate operators",
                            "Supervisor uses less shouting or abusive language", 
                            "Supervisor involve operators in solving problems on the line"),
          column.separate = c(2,2,2,2),
          title = "Table 5: Impact of Training on Working Conditions Index Sub-Components",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Supervisor's age", "Supervisor's education", 
                               "Supervisor's marital status", "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects), 
          out = file.path(tables_wd,"wc_logit_table.html"))

## Append note to the regression table

# Define the note to include CSS styling for text wrapping, width constraints and space
note_text <- '<div style="max-width: 100%; text-align: left; margin: 0.5; font-size: 14px; line-height: 1.4; margin-top: 5px; padding-top: 0px;">
              <p> The estimates are from logistic regression models and the even columns shows results from models that include interaction between the trainee status and supervisors work experience. 
                  The estimates are in odds ratios and using the delta method, the standard errors also correspond to the odds ratios.
                  For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.</p>
              </div>'

# Read the existing HTML file
html_content <- readLines(file.path(tables_wd, "wc_logit_table.html"))

# Find the position where the table ends (</table>)
table_end_index <- which(grepl("</table>", html_content))

# Insert the note directly after the table
if (length(table_end_index) > 0) {
  html_content <- append(html_content, note_text, after = table_end_index)
}

# Write the modified HTML back to the regression table
writeLines(html_content, file.path(tables_wd, "wc_logit_table.html"))


########################### TABLE 6: Impact of Training on Management Practices Index Sub-components

## Create a list of logit models, odds ratio estimates, and standard errors 

mp_logit_models <- list(logit_model_9, logit_model_10, logit_model_11, logit_model_12, 
                          logit_model_13, logit_model_14, logit_model_15, logit_model_16)

mp_logit_coeff <- list(or_logit_model_9, or_logit_model_10, or_logit_model_11, or_logit_model_12, 
                         or_logit_model_13, or_logit_model_14, or_logit_model_15, or_logit_model_16)

mp_logit_se <- list(se_or_logit_model_9, se_or_logit_model_10, se_or_logit_model_11, 
                    se_or_logit_model_12, se_or_logit_model_13, se_or_logit_model_14, 
                    se_or_logit_model_15, se_or_logit_model_16)

## Create a row of control means for the regression table

control_means <-c(mean(control_data$lo_sa_3a, na.rm = TRUE), mean(control_data$lo_sa_3a, na.rm = TRUE), 
                  mean(control_data$lo_sa_3b, na.rm = TRUE), mean(control_data$lo_sa_3b, na.rm = TRUE),
                  mean(control_data$lo_sa_3c, na.rm = TRUE), mean(control_data$lo_sa_3c, na.rm = TRUE),
                  mean(control_data$lo_sa_3d, na.rm = TRUE), mean(control_data$lo_sa_3d, na.rm = TRUE))
control_means <- round(control_means, 3) 
control_means <- c("Control Mean", control_means)

# Create a row of fixed effects for the regression table and store as a vector
factory_fixed_effects <- c("Factory Fixed Effects", rep("Yes", 8))

# Create a regression table 
stargazer(mp_logit_models,
          se = mp_logit_se, 
          coef = mp_logit_coeff,
          p.auto = FALSE, 
          type = "html", 
          dep.var.caption = "Operator Reported Outcomes of Management Practices",
          dep.var.labels.include = FALSE,
          column.labels = c("Supervisor is more confident", 
                            "Supervisor is better at remaining calm under stressful situations",
                            "Supervisor is better at motivating operators", 
                            "Supervisor is better at correcting mistakes"),
          column.separate = c(2,2,2,2),
          title = "Table 6: Impact of Training on Management Practices Index Sub-Components",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Supervisor's age", "Supervisor's education", 
                               "Supervisor's marital status", "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects), 
          out = file.path(tables_wd,"mp_logit_table.html"))


## Append note to the regression table

# Define the note to include CSS styling for text wrapping, width constraints and space
note_text <- '<div style="max-width: 100%; text-align: left; margin: 0.5; font-size: 14px; line-height: 1.4; margin-top: 5px; padding-top: 0px;">
              <p> The estimates are from logistic regression models and the even columns shows results from models that include interaction between the trainee status and supervisors work experience. 
                  The estimates are in odds ratios and using the delta method, the standard errors also correspond to the odds ratios.
                  For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.</p>
              </div>'

# Read the existing HTML file
html_content <- readLines(file.path(tables_wd, "mp_logit_table.html"))

# Find the position where the table ends (</table>)
table_end_index <- which(grepl("</table>", html_content))

# Insert the note directly after the table
if (length(table_end_index) > 0) {
  html_content <- append(html_content, note_text, after = table_end_index)
}

# Write the modified HTML back to the regression table
writeLines(html_content, file.path(tables_wd, "mp_logit_table.html"))


################## SECTION 2: REGRESSION COEFFICIENT PLOT ###########################
######################################################################################

################# REGRESSION COEFFICIENT PLOT FOR THE LOGIT MODELS 

# Define a function to create a table for the plot

coeff_plot_table <- function(models) {
  
  # Initialize an empty list to store results
  results <- list()
  
  # Loop over each model name
  for (model in models) {
    
    # Extract Coefficient Estimate 
    coeff_var <- paste0("or_", model)
    if (!exists(coeff_var, envir = .GlobalEnv)) {      # First check if the vector exists in the environment
      stop(paste("Odds ratio vector", coeff_var, "does not exist in the environment."))
    }
    coeff_ate <- get(coeff_var, envir = .GlobalEnv)    # Extract the estimates vector from the environment
    
    # Extract Standard Error
    se_var <- paste0("se_or_", model)
    if (!exists(se_var, envir = .GlobalEnv)) {         # First check if the vector exists in the environment
      stop(paste("Standard error vector", se_var, "does not exist in the environment."))
    }
    se_ate <- get(se_var, envir = .GlobalEnv)          # Extract the standard errors vector from the environment
    
    
    # Define a function to rename outcome variables for the output table
    rename_model <- function(model) {
      if (model == "logit_model_1") return("Supervisor gives extra support to less skilled operators")
      if (model == "logit_model_2") return("Supervisor gives extra support to less skilled operators")
      if (model == "logit_model_3") return("Supervisor uses praise to motivate operators")
      if (model == "logit_model_4") return("Supervisor uses praise to motivate operators")
      if (model == "logit_model_5") return("Supervisor uses less shouting or abusive language")
      if (model == "logit_model_6") return("Supervisor uses less shouting or abusive language")
      if (model == "logit_model_7") return("Supervisor involves operators in solving problems")
      if (model == "logit_model_8") return("Supervisor involves operators in solving problems")
      if (model == "logit_model_9") return("Supervisor is more confident")
      if (model == "logit_model_10") return("Supervisor is more confident")
      if (model == "logit_model_11") return("Supervisor is better at remaining calm under stress")
      if (model == "logit_model_12") return("Supervisor is better at remaining calm under stress")
      if (model == "logit_model_13") return("Supervisor is better at motivating operators")
      if (model == "logit_model_14") return("Supervisor is better at motivating operators")
      if (model == "logit_model_15") return("Supervisor is better at correcting mistakes")
      if (model == "logit_model_16") return("Supervisor is better at correcting mistakes")
      
      return(outcome)
    }
    
    # Store results in a data frame for each model
    results[[model]] <- data.frame(
      Outcome = rename_model(model),               # Apply the function to rename models
      Estimate = coeff_ate[2],                     # Extract coefficient estimate for treatment
      Upper_CI = coeff_ate[2] + 1.96 * se_ate[2],  # Calculate upper confidence interval
      Lower_CI = coeff_ate[2] - 1.96 * se_ate[2]   # Calculate lower confidence interval
    )
  }
  
  # Combine results from all models into a single data frame
  plot_table <- do.call(rbind, results)
  
  # Create an alternating vector for type of regression model
  n <- 16       # Define the length of the vector
  Model <- rep(c("Without interaction", "With interaction"), length.out = n)
  
  # Add the alternating vector to the table
  plot_table <- cbind(plot_table, Model)
  
  return(plot_table)
}


# Define a vector of logit models
models <- c("logit_model_1", "logit_model_2", "logit_model_3", "logit_model_4", 
            "logit_model_5", "logit_model_6", "logit_model_7", "logit_model_8", 
            "logit_model_9", "logit_model_10", "logit_model_11", "logit_model_12", 
            "logit_model_13", "logit_model_14", "logit_model_15", "logit_model_16")

# Apply the function
coeff_table <- coeff_plot_table(models)

# Define file path for saving the plot
plot_filename <- file.path(plots_wd, "logit_coeff_plot.jpeg")

# Create the plot
plot <- ggplot(coeff_table, aes(x = Estimate, 
                                y = factor(Outcome, levels = unique(Outcome)),  
                                color = Model,          # Color vary by type of Model
                                shape = Model)) +       # Add shape aesthetic
  geom_point(size = 3) +                                # Points for ATE estimates
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) +  # Add error bars
  geom_vline(xintercept = 1, color = "black", linetype = "dashed") +     # Vertical line at 1
  theme_classic() +                               # Set the theme
  labs(title = "ATE for sub-components of Working Conditions and Management Practices Index",
       x = "Odds Ratio Estimates",                # x axis label
       y = "Outcomes") +                          # y axis label 
  scale_color_brewer(palette = "Set1") +          # Set the color scheme
  scale_shape_manual(values = c(16, 17)) +        # Define different shapes for the points
  theme(plot.title = element_text(face = "bold"), # Adjust text size and style of plot title
        axis.text.x = element_text(size = 10),    # Adjust text size of x axis
        axis.text.y = element_text(size = 10))    # Adjust text size of y axis

# Add a note at the bottom of the plot
plot <- plot + plot_annotation(
  caption = "Note: The estimates are from the logistic regression models with and without the treatment and supervisor work experience interaction. 
             For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
  theme = theme(plot.caption = element_text(hjust = 0.9, size = 9))) # Adjust text size and position of note

# Save the plot adjusting for its width, height and resolution
ggsave(filename = plot_filename, plot = plot, width = 15, height = 8, dpi = 200)


######################################################################################
############################### END OF SCRIPT ########################################
######################################################################################
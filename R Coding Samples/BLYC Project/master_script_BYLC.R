## Author: Anudhii Sundaram 
## Date Created: November 01, 2024
## Date Modified: January 20, 2025

## R Version: 4.3.2 

## Purpose: HETEROGENEITY ANALYSIS FOR BYLC PROJECT

## Brief Project Description: 
# The aim of the project is to evaluate the impact of a soft-skills training program
# on the employment and soft-skills related outcomes of the participants.
# Researchers, with the support of IPA, conducted a randomized evaluation of the 
# soft skills training program for Bangladeshi university graduates entering the job-market. 

## Brief description of data: 
# Data used in the analysis comes from three waves of surveys to track participants' progress. 
# This includes the baseline survey and two follow-up surveys. 

## Brief variable description: 
# Outcomes: In this script, the main outcomes of interest are employment in paid work, 
# socio-emotional skills index, inter-personal skills index and intra-personal skill index. 
# Treatment: Soft skills training program  

## This R script has the following sections:

########### Section 1. HETEROGENEITY ANALYSIS USING CAUSAL FOREST 

########### Section 2. ATE/HTE ESTIMATE PLOTS AND HEATMAPS  
#           Section 2.1: ATE/HTE Estimate Plots
#           Section 2.2: Heatmaps by Age and Years to Graduation


## Clearing the environment 
rm(list=ls())

######################################################################################
####################### SETTING WORKING DIRECTORY ####################################

# Get the username of the system
user <- Sys.info()[["user"]]

# Define the root directory based on the username
if (user == "anudhiisundaram") {
  root <- "/Users/anudhiisundaram/Desktop/Coding Samples/R/BYLC"
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
packages <- c("grf", "dplyr", "tidyverse", "ggplot2", "stargazer", "haven", 
                       "patchwork")

# Run the function to install and load packages
install_load(packages)


######################################################################################
############# SECTION 1: HETEROGENEITY ANALYSIS USING CAUSAL FOREST ##################
######################################################################################

# Import survey data set
survey_data <- read_dta(file.path(root, "Data/merged_BL_FL_FL2_final_updated.dta"))

# Define a function for causal forest analysis 

cf_analysis <- function(data, outcome_var, treatment_var = "treatment", 
                        cluster_var = "bylc_batch", control_vars, 
                        heterogeneity_vars = NULL) {
  
  # Set the seed for reproducibility
  set.seed(123)
  
  # Select relevant variables only 
  relevant_vars <- c(outcome_var, treatment_var, cluster_var, control_vars)
  if (!is.null(heterogeneity_vars)) {    # If heterogeneity_vars list is not NULL, then include those variables also 
    relevant_vars <- unique(c(relevant_vars, heterogeneity_vars))
  }
  
  # Subset data to include only relevant variables 
  data <- data[, relevant_vars]
  data <- na.omit(data)   # Remove rows with missing values to run the causal forest model
  
  # Prepare input parameters for causal forest
  X <- as.matrix(data[, control_vars])       # X is a matrix of control variables
  Y <- as.numeric(data[[outcome_var]])       # Y is a vector for outcome variable
  W <- as.numeric(data[[treatment_var]])     # W is a vector for treatment variable  
  clusters <- as.vector(data[[cluster_var]]) # Vector of cluster variable 
  
  # Run regression forests to estimate Y_hat and W_hat
  Y_forest <- regression_forest(X, Y, clusters = clusters, equalize.cluster.weights = FALSE)
  Y_hat <- predict(Y_forest)$predictions   # Estimates of the expected responses E[Y|Xi], marginalizing over treatment
  W_forest <- regression_forest(X, W, clusters = clusters, equalize.cluster.weights = FALSE)
  W_hat <- predict(W_forest)$predictions   # Estimates of the treatment propensities E[W|Xi]
  
  # Run the causal forest model with all the input parameters
  cf <- causal_forest(X, Y, W, 
                      Y.hat = Y_hat,       # Regression forest estimates of Y_hat
                      W.hat = W_hat,       # Regression forest estimates of W_hat
                      clusters = clusters, # Vector specifying the cluster 
                      num.trees = 500,     # Number of trees grown in the forest
                      honesty = TRUE,      # Use sub-sample splitting to enable standard statistical inference
                      equalize.cluster.weights = FALSE, # Weight clusters as per their size
                      min.node.size = 10)  # Minimum number of observations in each tree leaf
  
  # Define a function to rename outcome variables for the final output table
  rename_outcome <- function(outcome) {
    if (outcome == "fl_any_employ") return("Employment in paid work")
    if (outcome == "fl_ses_ipa") return("Socio-emotional skills index") 
    if (outcome == "fl_intra_ipa") return("Intra-personal skills index") 
    if (outcome == "fl_inter_ipa") return("Inter-personal skills index") 
    
    return(outcome)  
  }
  
  
  ## Create plot to test Overlap Assumption
  
  # Define file path for saving the plot
  overlap_filename <- file.path(plots_wd, paste0("overlap_plot_", outcome_var, ".jpeg"))
  
  # Create a histogram showing the distribution of estimated propensity scores 
  overlap_plot <- ggplot(data.frame(W_hat = cf$W.hat, W = factor(cf$W.orig))) +
    geom_histogram(aes(x = W_hat, y = stat(density), fill = W), # Plot histogram
                   alpha = 0.3,                  # Adjust the transparency of the bars
                   position = "identity") +      # Ensure bars overlap and not stack
    geom_density(aes(x = W_hat, color = W)) +    # Add density curve over the histogram 
    xlim(0, 1) +                                 # Ensure propensity scores are within their range 
    labs(title = paste("Causal forest propensity scores for", rename_outcome(outcome_var)), # Plot title
         caption = "The propensity scores are learned via GRF's regression forest",         # Add caption to title
         x = "Propensity Score") +      # x axis label 
    theme_minimal()                     # Set the theme
  
  # Save the plot
  ggsave(filename = overlap_filename, plot = overlap_plot, width = 15, height = 8, dpi = 200)
  
  
  ## Create histogram of out-of-bag predictions
  
  # Extract out-of-bag predictions
  tau_hat <- predict(cf)$predictions
  
  # Define file path for saving the plot
  oob_filename <- file.path(plots_wd, paste0("oob_hist_", outcome_var, ".jpeg"))
  
  # Adjust the width, height and resolution of the jpeg file
  jpeg(oob_filename, pointsize = 12, width = 1900, height = 1500, res = 200)
  
  # Generate the histogram
  hist(tau_hat, main = paste("Histogram of out-of-bag ATE estimates for", rename_outcome(outcome_var)), 
       xlab = "Estimated CATE")
  
  # Close the graphics device
  dev.off()
  
  
  # Best linear predictor analysis (Omnibus test for heterogeneity)
  calibration_results <- test_calibration(cf)
  
  # Define function for creating ATE table
  ate_table <- function(ate, group) {
    t_value <- ate[["estimate"]] / ate[["std.err"]]  # Calculate t-value
    p_value <- 2 * (1 - pnorm(abs(t_value)))         # Calculate p-value
    significance <- ifelse(p_value < 0.01, "***",    # Assign values to significance stars
                           ifelse(p_value < 0.05, "**",
                                  ifelse(p_value < 0.1, "*", "")))
    
    # Extract estimates and attach significance stars to estimates
    estimate <- paste0(round(ate[["estimate"]], 3), significance) 
    
    # Create a data frame to generate the ATE table later
    data.frame(                                      
      Group = group,
      Estimate = estimate,
      Std.Error = round(ate[["std.err"]], 3),
      T.value = round(t_value, 3),
      Upper.CI = round(ate[["estimate"]] + 1.96 * ate[["std.err"]], 3),
      Lower.CI = round(ate[["estimate"]] - 1.96 * ate[["std.err"]], 3)
    )
  }
  
  # Define a mapping function for renaming group names in the output table
  rename_group <- function(group) {
    if (grepl("female = 1", group)) return("Female")
    if (grepl("female = 0", group)) return("Male")
    if (grepl("public_educ_inst = 1", group)) return("Degree from Public University")
    if (grepl("public_educ_inst = 0", group)) return("Degree from Private University")
    if (grepl("dhaka = 0", group)) return("Not from Dhaka")
    if (grepl("dhaka = 1", group)) return("From Dhaka")
    if (grepl("parent_working = 1", group)) return("Parent working")
    if (grepl("parent_working = 0", group)) return("Parent not working")
    if (grepl("educ_inst_subcat_rev = 1", group)) return("Degree in Engineering/ Sciences")
    if (grepl("educ_inst_subcat_rev = 2", group)) return("Degree in Social Science/Humanities ")
    if (grepl("educ_inst_subcat_rev = 3", group)) return("Degree in Business/Management")
    if (grepl("age_dummy = 0", group)) return("Below average age")
    if (grepl("age_dummy = 1", group)) return("Above average age")
    if (grepl("bl_yrstograd_binary = 1", group)) return("Graduated")
    if (grepl("bl_yrstograd_binary = 0", group)) return("Not Graduated")
    
    return(group)  
  }
  
  ## Average and Heterogeneous Treatment Effect Tables using heterogeneity_vars for subgroups
  ## Doubly robust ATE estimate (forest-based AIPW)
  
  # Compute the overall Average Treatment Effect (ATE) and store it in a summary table
  ate_summary_table <- ate_table(average_treatment_effect(cf), "All (Average Treatment Effect)")
  
  # If heterogeneity_vars are provided, iterate over them to add to ATE table
  if (!is.null(heterogeneity_vars)) {    # First check if any heterogeneity variables are provided
    
    # Iterate over each variable in the heterogeneity_vars list
    for (heterogeneity_var in heterogeneity_vars) {
      
      # Extract all unique values of the current heterogeneity variable
      unique_values <- unique(data[[heterogeneity_var]])
      
      # Iterate over each unique value of the heterogeneity variable
      for (value in unique_values) {
        
        # Subset data for each unique value the variable takes 
        subset_data <- data[[heterogeneity_var]] == value
        # Generate a descriptive group name for this subset 
        group_name <- paste(heterogeneity_var, "=", value)
        # Apply the rename_group function defined above
        renamed_group <- rename_group(group_name)  
        
        # Compute the ATE for the specific subgroup and append it to the ATE summary table
        ate_summary_table <- rbind(ate_summary_table,
                                   ate_table(average_treatment_effect(cf, subset = subset_data), 
                                             renamed_group))
      }
    }
  }
  
  # Define the file path for the ATE and HTE summary table 
  ate_table_filename <- file.path(tables_wd, paste0("ate_summary_", outcome_var, ".html"))
  
  # Output the ATE and HTE summary table using stargazer
  stargazer(ate_summary_table, 
            type = "html", 
            summary = FALSE, 
            rownames = FALSE,
            column.labels = c("Estimate", "Standard Error", "T Value", "Upper CI", "Lower CI"),
            title = paste("Average and Heterogeneous Treatment Effect Estimates:", 
                          rename_outcome(outcome_var)), 
            out = ate_table_filename)
  
  # Add inline CSS to customize column widths directly in the HTML file
  cat('<style>
        th:first-child, td:first-child { width: 25%; } /* First column: 25% */
        th:not(:first-child), td:not(:first-child) { width: 15%; } /* Other columns: 15% each */
     </style>',
      file = ate_table_filename, append = TRUE)
  
  # Return the results as a list of tables
  return(list(
    ate_summary = ate_summary_table,
    calibration = calibration_results
  ))
}


##################### APPLY THE FUNCTION TO DIFFERENT OUTCOME VARIABLES 

## 1. fl_any_employ - Employment in paid work

# Define parameters
outcome_var <- "fl_any_employ"                 # Outcome variable
control_vars <- "bl_any_employ"                # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis function
cf_results <- cf_analysis(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)

# Average Treatment Effect summary table
print(cf_results$ate_summary)

# Calibration cf_results table
print(cf_results$calibration)

## 2. fl_ses_ipa - Socio-emotional skills index 

# Define parameters
outcome_var <- "fl_ses_ipa"                    # Outcome variable
control_vars <- "bl_ses_ipa"                   # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis function
cf_results <- cf_analysis(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)

# Average Treatment Effect summary table
print(cf_results$ate_summary)

# Calibration cf_results table
print(cf_results$calibration)

## 3. fl_inter_ipa - Inter-personal skills index

# Define parameters
outcome_var <- "fl_inter_ipa"                  # Outcome variable
control_vars <- "bl_inter_ipa"                 # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis function
cf_results <- cf_analysis(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)

# Average Treatment Effect summary table
print(cf_results$ate_summary)

# Calibration cf_results table
print(cf_results$calibration)

## 4. fl_intra_ipa - Intra-personal skills index

# Define parameters
outcome_var <- "fl_intra_ipa"                  # Outcome variable
control_vars <- "bl_intra_ipa"                 # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis function
cf_results <- cf_analysis(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)

# Average Treatment Effect summary table
print(cf_results$ate_summary)

# Calibration cf_results table
print(cf_results$calibration)



######################################################################################
################ SECTION 2: ATE/HTE ESTIMATE PLOTS AND HEATMAPS ######################
######################################################################################


########################## 2.1 ATE/HTE ESTIMATE PLOTS

# Define function to plot ATE estimates with confidence intervals
cf_estimate_plot <- function(ate_results, outcome_var) {
  
  # Extract estimates and confidence intervals
  ate_results <- cf_results$ate_summary
  
  # Convert Estimate column to numeric 
  ate_results <- ate_results |>
    mutate(Estimate = as.numeric(gsub("\\*", "", Estimate))) # Remove significance stars ***
  
  # Reorder outcomes for better visualization
  ate_results$Group <- factor(ate_results$Group, levels = rev(unique(ate_results$Group)))
  
  # Define a function to rename outcome variables for the output table
  rename_outcome <- function(outcome) {
    if (outcome == "fl_any_employ") return("Employment in paid work")
    if (outcome == "fl_ses_ipa") return("Socio-emotional skills index") 
    if (outcome == "fl_intra_ipa") return("Intra-personal skills index") 
    if (outcome == "fl_inter_ipa") return("Inter-personal skills index") 
    
    return(outcome)  
  }
  
  ## Create coefficient estimate plot
  
  # Define file path for saving the plot
   plot_filename <- file.path(plots_wd, paste0("coeff_plot_", outcome_var, ".jpeg"))
  
  # Create the plot
  plot <- ggplot(ate_results, aes(x = Estimate, y = Group)) +
    geom_point(color = "blue", size = 3) +           # Points for ATE estimates
    geom_errorbarh(aes(xmin = Lower.CI, xmax = Upper.CI), height = 0.2, color = "blue") +  # Add error bars
    geom_vline(xintercept = 0, color = "red") +      # Vertical line at 0
    theme_minimal() +                                # Set the theme
    labs(title = paste("ATE and HTE for", rename_outcome(outcome_var)), # Plot title
         x = "Coefficient Estimates",                # x axis label
         y = rename_outcome(outcome_var)) +          # y axis label
    theme(plot.title = element_text(face = "bold"),  # Adjust text size of plot
          axis.text.x = element_text(size = 10),     # Adjust text size of x axis
          axis.text.y = element_text(size = 10))     # Adjust text size of y axis
  
  # Save the plot adjusting for its width, height and resolution 
  ggsave(filename = plot_filename, plot = plot, width = 15, height = 8, dpi = 200)
  
  # Assign plot to the environment so it can be accessed later
  assign(paste0("coeff_plot_", outcome_var), plot, envir = .GlobalEnv)
  
}


## Apply the function to different outcome variables 

cf_estimate_plot(cf_results, "fl_any_employ")
cf_estimate_plot(cf_results, "fl_ses_ipa")
cf_estimate_plot(cf_results, "fl_intra_ipa")
cf_estimate_plot(cf_results, "fl_inter_ipa")


## Combine all the plots 

# Define file path for saving the combined plot
plot_filename <- file.path(plots_wd, "combined_coeff_plot.jpeg")

# Adjust the width, height and resolution of the jpeg file
jpeg(plot_filename, width=3800, height=3000, res=250)

# Generate the plot
combined_coeff_plot <- coeff_plot_fl_any_employ + coeff_plot_fl_inter_ipa + # Add all plots together
  coeff_plot_fl_intra_ipa + coeff_plot_fl_ses_ipa +
  plot_annotation(title = "ATE and HTE Coefficient Estimates for different outcomes", # Add main plot title
                  theme = theme(plot.title = element_text(size=14, face="bold", hjust = 0.45))) # Adjust text size, font and position of title
combined_coeff_plot

# Close the graphics device
dev.off()


########################## 2.2 HEATMAPS BY AGE AND YEARS TO GRADUATION

# Define a causal forest analysis function and a heatmap function within the function

cf_heatmap <- function(data, outcome_var, treatment_var = "treatment",
                                  cluster_var = "bylc_batch", 
                                  control_vars, heterogeneity_vars, 
                                  age_var = "age", grad_year_var = "bl_yrstograd",
                                  num_quantiles = 4) {
  
  # Set the seed for reproducibility
  set.seed(123)
  
  # Select relevant variables only
  relevant_vars <- c(outcome_var, treatment_var, cluster_var, control_vars, 
                     age_var, grad_year_var)
  if (!is.null(heterogeneity_vars)) {       # If heterogeneity_vars list is not NULL, then include those variables also 
    relevant_vars <- unique(c(relevant_vars, heterogeneity_vars))
  }
  
  # Subset data to include only relevant variables 
  data <- data[, relevant_vars]
  data <- na.omit(data)                      # Remove rows with missing values to run the causal forest model
  
  # Prepare input parameters for causal forest 
  X <- as.matrix(data[, control_vars])       # X is a matrix of control variables
  Y <- as.numeric(data[[outcome_var]])       # Y is a vector for outcome variable
  W <- as.numeric(data[[treatment_var]])     # W is a vector for treatment variable  
  clusters <- as.vector(data[[cluster_var]]) # Vector of cluster variable 
  
  # Run regression forests to estimate Y_hat and W_hat
  Y_forest <- regression_forest(X, Y, clusters = clusters, equalize.cluster.weights = FALSE)
  Y_hat <- predict(Y_forest)$predictions   # Estimates of the expected responses E[Y|Xi], marginalizing over treatment
  W_forest <- regression_forest(X, W, clusters = clusters, equalize.cluster.weights = FALSE)
  W_hat <- predict(W_forest)$predictions   # Estimates of the treatment propensities E[W|Xi]
  
  # Run the causal forest model using the input parameters
  cf <- causal_forest(X, Y, W, 
                      Y.hat = Y_hat,       # Regression forest estimates of Y_hat
                      W.hat = W_hat,       # Regression forest estimates of W_hat
                      clusters = clusters, # Vector specifying the cluster 
                      num.trees = 500,     # Number of trees grown in the forest
                      honesty = TRUE,      # Use sub-sample splitting to enable standard statistical inference
                      equalize.cluster.weights = FALSE, # Weight clusters as per their size
                      min.node.size = 10)  # Minimum number of observations in each tree leaf
  
  
  # Extract out-of-bag predictions 
  data$tau_hat <- predict(cf)$predictions  
  
  # Function to generate heatmap using Age Quantiles × Years to Graduation
  plot_heatmap <- function(data, age_var, grad_year_var, tau_hat, outcome_var, 
                               num_quantiles) {
    
    # Generate unique quantile breaks for Age
    age_quantiles <- unique(quantile(data[[age_var]],                    # Ensure all quantile values are unique
                                     probs = seq(0, 1, 1/num_quantiles), # Divide data into equal quantiles
                                     na.rm = TRUE))                      # Remove missing values to avoid errors
    
    # If quantile-based binning fails, create evenly spaced bins
    if (length(age_quantiles) < num_quantiles + 1) {
      
      # Generate num_quantiles + 1 break points from the minimum to the maximum of age_var
      age_quantiles <- seq(min(data[[age_var]], na.rm = TRUE), max(data[[age_var]], na.rm = TRUE), 
                           length.out = num_quantiles + 1)
    }
    
    # Assign age into quantile bins
    data <- data |>
      # Create a new variable for age quantile 
      mutate(Age_Quantile = cut(!!sym(age_var), breaks = age_quantiles,     # Divide the age variable into bins defined by age_quantiles
                                include.lowest = TRUE, labels = FALSE)) |>  # Ensure lowest value is included in the first bin
      # Group by the age and years to graduation variable and calculate group summaries
      group_by(Age_Quantile, !!sym(grad_year_var)) |>
      # Compute the mean of tau_hat for each group
      summarise(Mean_ATE = mean(!!sym(tau_hat), na.rm = TRUE), .groups = "drop")
    
    # Function to rename outcome variables
    rename_outcome <- function(outcome) {
      if (outcome == "fl_any_employ") return("Employment in paid work")
      if (outcome == "fl_ses_ipa") return("Socio-emotional skills index")
      if (outcome == "fl_intra_ipa") return("Intra-personal skills index")
      if (outcome == "fl_inter_ipa") return("Inter-personal skills index")
      
      return(outcome)  
    }
    
    # Generate the Heatmap
    heatmap_plot <- ggplot(data, aes(x = as.numeric(Age_Quantile), 
                                     y = as.numeric(!!sym(grad_year_var)), 
                                     fill = Mean_ATE)) +
      geom_tile() +                                 # Add tiles for heatmap
      scale_fill_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "red"), # Set the color scheme 
                           name = "ATE") +       
      theme_minimal() +                             # Set the theme
      labs(title = paste("ATE on", rename_outcome(outcome_var), "by Age Quantiles x Years to Graduation"),
           x = "Age (Quantiles)",                   # x axis label 
           y = "Years to Graduation") +             # y axis label 
      theme(plot.title = element_text(size = 12, face = "bold"), # Adjust text size and font of plot title
            axis.text.x = element_text(size = 10),  # Adjust text size of x axis
            axis.text.y = element_text(size = 10))  # Adjust text size of y axis
    
    # Define the file path for saving the heatmap
    heatmap_filename <- file.path(plots_wd, paste0("heatmap_", outcome_var, ".jpeg"))
    
    # Save the heatmap adjusting for its width, height and resolution
    ggsave(filename = heatmap_filename, plot = heatmap_plot, width = 8, height = 6, dpi = 300)
    
    return(list(plot = heatmap_plot, filename = heatmap_filename))
  }
  
  # Apply the plot_heatmap function  
  heatmap <- plot_heatmap(data, age_var, grad_year_var, "tau_hat", outcome_var, num_quantiles)
  
  # Assign the generated heatmap to the environment so it can be accessed later
  assign(paste0("heatmap_", outcome_var), heatmap$plot, envir = .GlobalEnv)
  
}


##################### APPLY THE FUNCTION TO DIFFERENT OUTCOME VARIABLES 


### 1. fl_any_employ 

# Define parameters
outcome_var <- "fl_any_employ"                 # Outcome variable
control_vars <- "bl_any_employ"                # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis and heatmap function
cf_results <- cf_heatmap(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)


### 2. fl_ses_ipa 

# Define parameters
outcome_var <- "fl_ses_ipa"                 # Outcome variable
control_vars <- "bl_ses_ipa"                # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis and heatmap function
cf_results <- cf_heatmap(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)


### 3. fl_inter_ipa 

# Define parameters
outcome_var <- "fl_inter_ipa"                 # Outcome variable
control_vars <- "bl_inter_ipa"                # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis and heatmap function
cf_results <- cf_heatmap(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)


### 4. fl_intra_ipa 

# Define parameters
outcome_var <- "fl_intra_ipa"                 # Outcome variable
control_vars <- "bl_intra_ipa"                # Control for baseline outcome
heterogeneity_vars <- c("female", "public_educ_inst", "dhaka", "parent_working", 
                        "educ_inst_subcat_rev", "age_dummy", "bl_yrstograd_binary")  # Variables for heterogeneity analysis

# Apply the causal forest analysis and heatmap function
cf_results <- cf_heatmap(
  data = survey_data,
  outcome_var = outcome_var,
  control_vars = control_vars,
  heterogeneity_vars = heterogeneity_vars
)


## Combine all the heatmaps 

# Define file path for saving the combined heatmaps
plot_filename <- file.path(plots_wd, "combined_heatmap_plot.jpeg")

# Adjust the width, height and resolution of the jpeg file
jpeg(plot_filename, width=3800, height=3000, res=250)

# Generate the plot 
combined_heatmap <- heatmap_fl_any_employ + heatmap_fl_inter_ipa + heatmap_fl_intra_ipa + 
                    heatmap_fl_ses_ipa +    # Add all plots together
  plot_annotation(title = "ATE by Age Quantiles x Years to Graduation for different outcomes",  # Add main plot title
                  theme = theme(plot.title = element_text(size=16, face="bold", hjust = 0.45))) # Adjust text size, font and position of title
combined_heatmap

# Close the graphics device
dev.off()


######################################################################################
############################### END OF SCRIPT ########################################
######################################################################################
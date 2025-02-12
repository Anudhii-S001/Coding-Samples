## Author: Anudhii Sundaram 
## Date Created: December 05, 2024
## Date Modified: December 20, 2025

## R Version: 4.3.2 

## Purpose: Aggregating the NDC Indicator for Climate Policy and creating a heatmap 

## Brief Project Description: 
# The broader aim of the project is to create a Green Jobs Index for low- and middle-income countries. 
# To begin with, the project is focusing on building this index for the African countries. 
# The index is built based on eight pillars that range from green job creation and carbon intensity 
# of jobs to climate policy in countries and green skills. 

## Brief description of data: 
# For the climate policy pillar of the index, currently we are using data from the Climate Watch
# on the Nationally Determined Contributions of countries. 

## This R script has only one section:

########### Section 1. PILLAR - CLIMATE POLICY
#           Section 1.1: Recode Variables
#           Section 1.2: Aggregation and Normalization  
#           Section 1.3: Choropleth Maps of African countries and the World
#           Section 1.4: Table with Rankings of African countries 


## Clearing the environment 
rm(list=ls())

######################################################################################
####################### SETTING WORKING DIRECTORY ####################################

# Get the username of the system
user <- Sys.info()[["user"]]

# Define the root directory based on the username
if (user == "anudhiisundaram") {
  root <- "/Users/anudhiisundaram/Desktop/Coding Samples/R/JLA"
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
              "lubridate", "rworldmap", "RColorBrewer")

# Run the function to install and load packages
install_load(packages)


######################################################################################
################# SECTION 1: PILLAR - CLIMATE POLICY #################################
######################################################################################

# Import data set 
cw_ndc_data <- read_dta(file.path(root, "Data/CW_NDC_enhancement_tracker.dta"))
cw_ndc_data <- cw_ndc_data %>% 
  select(-AX) # Remove this column from data 

########################## 1.1 RECODE VARIABLES 

# NOTE: In this section, primarily, we are creating binary variables for the different sub-indicators.  

############# 1. NDC revised or not by country: New or Updated NDC = 1 otherwise 0
cw_ndc_data$'revision_ndc' <- ifelse(cw_ndc_data$submission_label=="New or Updated NDC", 1, 0)

############# 2. Reduced Greenhouse Gas Emissions by 2030 in NDC: Updated NDC with Reduced Total Emissions = 1 otherwise 0 
cw_ndc_data$'mitigation_ndc' <- ifelse(cw_ndc_data$ndce_status_2020_label=="Submitted New or Updated NDC with Reduced Total Emissions", 1, 0)

############# 3. Paris Accord Agreement: Joined Agreement = 1 otherwise 0
cw_ndc_data$'pa_status_ndc' <- ifelse(cw_ndc_data$pa_status_label=="Joined Agreement", 1, 0)

############# 4. Share of Greenhouse Gas Emissions by country
cw_ndc_data <- cw_ndc_data %>% 
  mutate(ghg_share_2021=case_when(ndce_ghg >= 0 & ndce_ghg < 0.005 ~ 5, 
                                  ndce_ghg >= 0.005 & ndce_ghg < 0.01 ~ 4, 
                                  ndce_ghg >= 0.01 & ndce_ghg < 0.015 ~ 3, 
                                  ndce_ghg >= 0.015 & ndce_ghg < 0.02 ~ 2, 
                                  ndce_ghg >= 0.02 ~ 1))

# Create a binary variable: If global share of emission is less than 1% = 1 and otherwise 0
cw_ndc_data$ghg_share_2021 <- ifelse(cw_ndc_data$ghg_share_2021==5 | 
                                     cw_ndc_data$ghg_share_2021==4, 1, 0)

############# 5. Years since the NDC was last updated 

# Convert into standard date format
cw_ndc_data$ndce_date <- as.Date(cw_ndc_data$ndce_date, format = "%m/%d/%Y")

# Extract year from dates
cw_ndc_data$'ndce_year' <- year(cw_ndc_data$ndce_date)

# Relocate new variable
cw_ndc_data <- cw_ndc_data %>% 
  relocate(ndce_year, .after = ndce_date)

# Create a new variable for years since the NDC was last updated
cw_ndc_data$'ndc_update_years' <- (2024 - cw_ndc_data$ndce_year)

# Create a binary variable: If NDC was updates less than 3 years ago = 1 otherwise 0
cw_ndc_data$ndc_update_years <- ifelse(cw_ndc_data$ndc_update_years < 3, 1, 0)

############# 6. Net Zero Status: If Net Zero Targets mentioned in Policy Document/Law/Political Pledge = 1 otherwise 0
cw_ndc_data$'nz_status_ndc' <- ifelse(cw_ndc_data$nz_status_label=="Net-zero Target in Policy Document" | 
                                      cw_ndc_data$nz_status_label=="Net-zero Target in Law" |
                                      cw_ndc_data$nz_status_label=="Net-zero Target in Political Pledge", 1, 0)

############# 7. Net Zero Target Year (or should countries with a target be 1 and the ones with missing values be 0?)
cw_ndc_data$'nz_target_year_ndc' <- ifelse(cw_ndc_data$nz_year_label=="Already achieved and commit to maintain" | 
                                           cw_ndc_data$nz_year_label=="Target Year Before 2050", 1, 0)

############# 8. Net Zero Target GHG coverage 
cw_ndc_data$'nz_ghg_ndc' <- ifelse(cw_ndc_data$nz_ghg_label=="All GHGs Covered", 1, 0)

############# 9. Net Zero Target Sectors covered 
cw_ndc_data$'nz_sec_ndc' <- ifelse(cw_ndc_data$nz_sec_label=="All Domestic Sectors Covered", 1, 0)


########################## 1.2 AGGREGATION AND NORMALIZATION  

# Replace NAs with 0 for a few sub-indicators
cw_ndc_data <- cw_ndc_data %>% 
  replace_na(list(ghg_ndc = 0, sectoral_ndc = 0, policy_ndc = 0, adaptation_ndc = 0, 
                  information_ndc = 0, ndc_update_years = 0))

# Create a new variable with horizontal summation across the 14 sub-indicators 
cw_ndc_data <- cw_ndc_data %>%
  mutate(sum_ndc = rowSums(across(c(ghg_ndc: nz_sec_ndc)), na.rm=TRUE))

# Define a function for Normalization 
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply the function to create a new normalized variable
cw_ndc_data$'normalized_ndc' <- normalize(cw_ndc_data$sum_ndc)

# Check the minimum and maximum value to ensure the new variable is within the valid range 
min(cw_ndc_data$normalized_ndc)
max(cw_ndc_data$normalized_ndc)


########################## 1.3 CHOROPLETH MAPS OF THE WORLD AND AFRICAN COUNTRIES

############# 1.3.1 CHOROPLETH MAP OF THE WORLD

# Merge NDC data with world map data using country ISO code
world_map_sf <- joinCountryData2Map(cw_ndc_data, joinCode = "ISO3", nameJoinColumn = "ISO")

# Define a custom color palette from purple (low) to yellow (high)
purple_to_yellow <- colorRampPalette(c("purple", "yellow"))

# Define breaks
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)     

# Define file path for saving the heatmap
world_filename <- file.path(plots_wd, "world_map_ndc.jpeg")

# Adjust the width, height and resolution of the jpeg file
jpeg(world_filename, pointsize = 12, width=1900, height=1500, res=200)

# Generate the Map
mapCountryData(world_map_sf, nameColumnToPlot = "normalized_ndc", catMethod = "fixedWidth",
               numCats = 5,  colourPalette = purple_to_yellow(length(breaks) - 1),
               mapTitle = "Variation in Normalized NDC Scores across countries", 
               addLegend = TRUE, missingCountryCol = "gray")

# Close the graphics device 
dev.off()

############# 1.3.2 CHOROPLETH MAP OF AFRICAN COUNTRIES

# Define a list of ISO3 codes for African countries
africa_iso3 <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD", "COM", 
                 "COG", "CIV", "COD", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", 
                 "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", 
                 "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", 
                 "SOM", "ZAF", "SSD", "SDN", "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE")

# Filter world map shape file to include only African countries
africa_map_sf <- world_map_sf[world_map_sf$ISO3 %in% africa_iso3, ]

# Define file path for saving the heatmap
africa_filename <- file.path(plots_wd, "africa_map_ndc.jpeg")

# Define a custom color palette from purple (red) to green (high)
red_to_green <- colorRampPalette(c("red", "green"))

# Adjust the width, height and resolution of the jpeg file
jpeg(africa_filename, pointsize = 12, width=1900, height=1500, res=200)

# Generate the Map
mapCountryData(africa_map_sf, nameColumnToPlot = "normalized_ndc", catMethod = "fixedWidth",
               numCats = 5,  colourPalette = red_to_green(length(breaks) - 1),
               mapTitle = "Variation in Normalized NDC Scores across African countries", 
               addLegend = TRUE, missingCountryCol = "gray")

# Close the graphics device 
dev.off()


########################## 1.4 TABLE WITH RANKINGS OF AFRICAN COUNTRIES 

# Filter data for African countries using the shape file
africa_data <- africa_map_sf@data

# Select variables relevant for creating a country rankings table 
africarank_ndc_data <- africa_data %>%
  select(ADMIN, ISO3, normalized_ndc)

# Rank all African countries based on the normalized score  
africarank_ndc_data <- arrange(africarank_ndc_data, desc(normalized_ndc)) %>%
  mutate(rank_ndc = 1:nrow(africarank_ndc_data)) # Create a new variable that shows the rank

# Order countries in the data based on their rank
africarank_ndc_data <- africarank_ndc_data[order(africarank_ndc_data$rank_ndc, decreasing = FALSE),]

# Assign new column names 
colnames(africarank_ndc_data) <- c("Country", "ISO Code", "Normalized NDC Score", "Rank")

# Output a Country Rankings table using stargazer
stargazer(africarank_ndc_data, 
          summary = FALSE,
          type = "html",
          title = "Table 1: Rank of African Countries by Normalized NDC Score", 
          out = file.path(tables_wd, "africarank_ndc_table.html"))


######################################################################################
############################### END OF SCRIPT ########################################
######################################################################################


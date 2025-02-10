/*******************************************************************************
Author: Anudhii Sundaram 

Date Created: Jan 20, 2024
Date Modified: May 15, 2024

Stata Version 18.0

Purpose: Data Cleaning, Analysis and Visulization 

Project Description: 
The project aims to examine the impact of an early childhood development program - 'Integrated Child 
Development Services' - in India on pre-adolescent learning outcomes using data from the Indian Human 
Development Survey. The focus is particularly on the heterogeneity in treatment effects to target the 
intervention to subgroups that benefit the most from it. 

Brief description of ICDS: 
In 1975, the Government of India initiated the Integrated Child Development Services (ICDS) scheme. 
It offers nutritional support, health assessments, referral services, immunizations, and non-formal 
preschool education to children aged 0 to 6 years. 

Brief description of the Indian Human Development Survey: 
IHDS is a nation-wide survey encompassing both rural and urban regions, comprising over 1,500 villages,
970 urban blocks and over 41,000 households. The survey includes a diverse range of questions on topics
such as healthcare, education, employment, marriage, and gender relations. IHDS encompasses two waves: 
the first wave of survey was held in 2004–05 and the second wave was conducted in 2011–12. In the second 
wave, about 85 per cent of the households from the first wave were re-interviewed. 

Brief description of Data used for analysis: 
For the purpose of this study, three datasets from both the waves of IHDS were combined using a common 
variable `household ID' to obtain a panel data set consisting of data from both the waves. The sample is
restricted to include only those children whose households were part of the first wave of survey and then
re-interviewed in the second wave. The information about whether a child received any ICDS intervention is 
obtained from the eligible woman in the household in the first wave and is matched with the learning outcomes
of children in the second wave. The final dataset is a panel, comprising relevant variables from both the waves
of the survey. However, the study is cross-sectional since the learning outcomes of children are only recorded 
in the second wave.

Brief description of Variables: 
Outcomes: Standardized Reading and Math Scores of children
Treatment: Whether the child received any ICDS intervention

This Script has the following sections: 
*** Section 1: Data Merge
*** Section 2: Data Cleaning
*** Section 3: Summary Statistics and T-test Table
*** Section 4: Regression Analysis and Propensity Score Matching 
***       4.1. Average Treatment Effects
***       4.2. Heterogeneous Treatment Effects
*** Section 5: Post-Estimation Plots

*******************************************************************************/


clear all
set more off
set scheme s2color
pause on
set maxvar 30000


********************************************************************************
*************************** GLOBAL DIRECTORY *********************************** 

display "`c(username)'" // Displays the username of your computer 

* Define the working directory and global root variable
if "`c(username)'" == "anudhiisundaram" { 
	
	* The main directory
    global root "/Users/anudhiisundaram/Desktop/Coding Samples/Stata/ICDS"
	
	* The directory for importing and saving data sets 
	global data "$root/data"
	
	* The directory for saving tables 
	global tables "$root/tables"
	
	* The directory for saving plots 
	global plots "$root/plots"

}


********************************************************************************
**************************  SECTION 1. DATA MERGE ******************************
********************************************************************************

* Import Datasets 
********************************************************************************

* Import First Dataset (Household)
use         "$data/hhwide.dta"
tempfile     first_dataset         // Create a temporary file to store the first dataset
save        `first_dataset'

* Import Second Dataset (Eligible Women)
clear                              // Clear the data in memory
use         "$data/ewwide.dta"
tempfile     second_dataset        // Create a temporary file to store the second dataset
save        `second_dataset'

* Import Third Dataset (Individual)
clear                              // Clear the data in memory
use         "$data/indwide.dta"
keep if      RO5>=8 & RO5<=11      // Keep only children in the age group of 8 to 11 years 
tempfile     third_dataset        // Create a temporary file to store the third dataset
save        `third_dataset'


* Merge datasets
********************************************************************************

* Merge the first and second data set  
use        `first_dataset', clear               // Load the first dataset 
merge       m:1 HHBASE using `second_dataset' // Many-to-one merge on household ID

* Check the results of the merge
tab         _merge

* Optional: Keep only matched records
keep if     _merge == 3                    // Keep only observations that matched (optional)

* Drop the merge indicator if not needed
drop        _merge

* Create temp file for storing this merged data
tempfile    1_2_merge_dataset // Create a temporary file to store the first dataset

* Save the temp file
save       `1_2_merge_dataset'

* Load the data from the first merge 
use        `1_2_merge_dataset', clear 
merge       m:m HHBASE using `third_dataset' // Many-to-many merge on household ID

* Check the results of the merge
tab         _merge

* Optional: Keep only matched records
keep if     _merge == 3 // Keep only observations that matched (optional)

* Drop the merge indicator if not needed
drop        _merge

* Save the final merged dataset
save        "$data/merged_dataset.dta", replace


********************************************************************************
*************************  SECTION 2. DATA CLEANING ****************************
********************************************************************************

* Import merged survey data 
use         "$data/merged_dataset.dta", clear

* Keep only children currently enrolled in school to control for school and teacher specfic attributes 
keep if     ED5==1

* Drop all variables not relevant for analysis 
drop        PSUID HHSPLITID IDPERSON IDHH MB14 AD6 AD7 URBAN EW6 XRO4 CH2 ID11 ///
            HHBASE GROUPS6 PBASE XBMI WT ED5 XLB48

* Create a variable for receiving any of the ICDS services (Treatment variable) 
gen         icds_treatment = .
replace     icds_treatment = 1 if (XLB49A1 == 1) | (XLB49B1 == 1) | (XLB49C1 == 1) | ///
                                  (XLB49D1 == 1) | (XLB49E1 == 1)
replace     icds_treatment = 0 if (XLB49A1 == 0) & (XLB49B1 == 0) & (XLB49C1 == 0) & ///
                                  (XLB49D1 == 0) & (XLB49E1 == 0)
								  
* Define value label for the treatment variable 
label       define treatment 1 "Yes" 0 "No"
label       values icds_treatment treatment   // Assign label to the variable 

* Drop observations if treatment is missing in the data
drop if     icds_treatment == .
		  
* Create a variable for standardized reading score (First outcome variable) 
egen        std_reading_score = std(TA8B)

* Create a variable for standardized math score (Second outcome variable)
egen        std_math_score = std(TA9B)

* Drop observations if data is missing for any of the outcome variables 
drop if     std_reading_score == .   // Standardized Reading Score 
drop if     std_math_score == .     // Standardized Math Score 

* Generate logged version of household expenditure 
gen         log_hh_expend = log(XCOTOTAL)

	  
** Create dummies for the following variables:
********************************************************************************

* Child's sex
recode       XRO3 (2 = 1 "Female") (1 = 0 "Male"), gen(child_sex)

* Child's family type 
recode       XFAMCAT (3 = 1 "Nuclear") (4/5 = 0 "Joint"), gen(child_family_type)

* Child's mother's health 
recode       XEW10 (1/3 = 1 "Good") (4/5 = 0 "Poor") (missing = .), gen(mother_health)

* Child's teacher is regular 
recode       CH6 (3 = 1 "Regular") (1/2 = 0 "Irregular") (missing = .), gen(teacher_regular)

* Child's teacher is good 
recode       CH10 (1/2 = 1 "Yes") (3/4 = 0 "No") (missing = .), gen(teacher_good)

* Availability of toilet in child's household 
recode       XSA4 (2/4 = 1 "Yes") (1 = 0 "No") (missing = .), gen(hh_toilet_avail)


** Create categorical variables for the following:  
********************************************************************************

* Child's school type 
recode       CS4 (1/3 = 1 "Public") (4 = 2 "Private") (5/7 = 3 "Other") (missing = .), gen(school_type)

* Household's religion 
recode       XID11 (1 = 1 "Hindu") (2 = 2 "Muslim") (3 = 3 "Christian") (4/9 = 4 "Other"), gen(hh_religion)

* Household's caste
recode       XGROUPS6 (2 = 1 "Forward caste") (3/4 = 2 "Backward caste") (5/7 = 3 "Other"), gen(hh_caste) 

* State ID 
recode       STATEID (1 = 1 "Jammu & Kashmir") (2 = 2 "Himachal Pradesh") (3 = 3 "Punjab") ///
                     (5 = 4 "Uttrakhand") (6 = 5 "Haryana") (8 = 6 "Rajasthan") (9 = 7 "Uttar Pradesh") ///
                     (10 = 8 "Bihar") (11/18 = 9 "Northeast") (19 = 10 "West Bengal") (20 = 11 "Jharkhand") ///
                     (21 = 12 "Orissa") (22 = 13 "Chhattisgarh") (23 = 14 "Madhya Pradesh") (24 = 15 "Gujarat") ///
                     (27 = 16 "Maharashtra") (28 = 17 "Andhra Pradesh") (29 = 18 "Karnataka") (30 = 19 "Goa") ///
                     (32 = 20 "Kerala") (33 = 21 "Tamil Nadu") (4 = 22 "Union Territories") (7 = 22 "Union Territories") ///
		     (25 = 22 "Union Territories") (26 = 22 "Union Territories") (31 = 22 "Union Territories") ///
		     (34 = 22 "Union Territories") (35 = 22 "Union Territories"), gen(stateid)
					
order        stateid         // Move the variable to the beginning of the data set

* Define value label for whether household is below poverty line 
label        define poor 1 "Below poverty line" 0 "Above poverty line"
label        values XPOOR poor

* Drop old variables 
drop         XLB49A1 XLB49B1 XLB49C1 XLB49D1 XLB49E1 TA8B TA9B XCOTOTAL XRO3 XFAMCAT XEW10 ///
             CH6 CH10 XSA4 CS4 XID11 XGROUPS6 STATEID 

* Rename remaining variables 
rename      (XWA5A  XFU1 XFU2 XURBAN XEW6 XEW8 XEW9 XSPED6 XRO5 XPOOR RO5 CS14Y) ///
            (hh_water_avail hh_electricity_avail hh_meals_perday hh_urban mother_age ///
             mother_educ mother_surviving_children father_educ child_age_wave1 hh_poor ///
	     child_age_wave2 midday_meal)	  
		  
* Convert all variable names into lowercase  		  
rename       *, lower	

* Save the cleaned dataset
save        "$data/merged_cleaned_dataset.dta", replace


********************************************************************************
*************** SECTION 3: SUMMARY STATISTICS AND T-TEST TABLE *****************
********************************************************************************

* Import cleaned survey data 
use         "$data/merged_cleaned_dataset.dta", clear
 

* Define the list of control variables 
local       control_vars "child_age_wave1 child_age_wave2 child_family_type child_sex mother_age mother_educ mother_health mother_surviving_children father_educ school_type teacher_good teacher_regular midday_meal hh_caste hh_religion hh_meals_perday hh_poor hh_toilet_avail hh_electricity_avail hh_water_avail hh_urban log_hh_expend"

* Loop over each control variable
foreach var in `control_vars' {

            * Perform a t-test on the variable of interest by the grouping variable
            ttest `var', by(icds_treatment)
            
            * Extract values for the treatment group (icds_treatment == 1)
            local mean_treatment = r(mu_1)         // Mean 
            local sd_treatment = r(sd_1)           // Standard deviation 
            local obs_treatment = r(N_1)           // Number of observations 
        
            * Extract values for the treatment group (icds_treatment == 0)
            local mean_control = r(mu_2)            // Mean 
            local sd_control = r(sd_2)              // Standard deviation 
            local obs_control = r(N_2)              // Number of observations 
        
            * Extract the p-value for the t-test
            local p_value = r(p)                    // p-value for the difference in means
            
            * Create new variables to hold the extracted values
             gen `var'_mean_t = `mean_treatment'
             gen `var'_sd_t = `sd_treatment'
             gen `var'_obs_t = `obs_treatment'
	         gen `var'_mean_c = `mean_control'
             gen `var'_sd_c = `sd_control'
             gen `var'_obs_c = `obs_control'
	         gen `var'_pvalue = `p_value'

} // End of loop 


** Create the summary statistics and t-test table in Latex 
********************************************************************************

file open   table using "$tables/sum_stat_table.tex", write replace
 
file write  table "\documentclass[12pt]{article}" _n
file write  table "\usepackage{graphicx}" _n
file write  table "\usepackage{caption}" _n
file write  table "\usepackage{booktabs}" _n
file write  table "\usepackage{multirow}" _n
file write  table "\usepackage{makecell}" _n
file write  table "\renewcommand{\arraystretch}{1.5}" _n   
file write  table "\setlength{\arrayrulewidth}{0.5mm}" _n
 
 
file write  table "\begin{document}" _n
 
* Start the table
file write  table "\begin{table}[ht]" _n
file write  table " \hspace*{-1in}" _n
file write  table " \begin{minipage}{\textwidth} " _n

* Customize table header in Latex 
file write  table "    \centering" _n
file write  table "    \caption{Summary Statistics and T-test}" _n
file write  table "    \begin{tabular}{lccc ccc c}" _n
file write  table "        \toprule" _n
file write  table "        & \multicolumn{3}{c}{\textbf{Treatment}} & \multicolumn{3}{c}{\textbf{Control}} & \\" _n
file write  table "        \cmidrule(lr){2-4} \cmidrule(lr){5-7}" _n
file write  table "        \textbf{Variable} & \textbf{Mean} & \textbf{Std. Dev.} & \textbf{N} & \textbf{Mean} & \textbf{Std. Dev.} & \textbf{N} & \textbf{P-value} \\" _n
file write  table "        \midrule" _n

* Write data rows (need to define the variables in Stata first)
* Extract values of mean, standard deviation, number of observations and p value 

* child_age_wave1
file write table "        Child's Age Wave 1 & `=string(round(child_age_wave1_mean_t, 0.001))' & `=string(round(child_age_wave1_sd_t, 0.001))' & `=string(round(child_age_wave1_obs_t, 0.001))' & `=string(round(child_age_wave1_mean_c, 0.001))' & `=string(round(child_age_wave1_sd_c, 0.001))' & `=string(round(child_age_wave1_obs_c, 0.001))' & `=string(round(child_age_wave1_pvalue, 0.001))' \\" _n

* child_age_wave2
file write table "        Child's Age Wave 2 & `=string(round(child_age_wave2_mean_t, 0.001))' & `=string(round(child_age_wave2_sd_t, 0.001))' & `=string(round(child_age_wave2_obs_t, 0.001))' & `=string(round(child_age_wave2_mean_c, 0.001))' & `=string(round(child_age_wave2_sd_c, 0.001))' & `=string(round(child_age_wave2_obs_c, 0.001))' & `=string(round(child_age_wave2_pvalue, 0.001))' \\" _n

* child_family_type
file write table "        Child's Family Type & `=string(round(child_family_type_mean_t, 0.001))' & `=string(round(child_family_type_sd_t, 0.001))' & `=string(round(child_family_type_obs_t, 0.001))' & `=string(round(child_family_type_mean_c, 0.001))' & `=string(round(child_family_type_sd_c, 0.001))' & `=string(round(child_family_type_obs_c, 0.001))' & `=string(round(child_family_type_pvalue, 0.001))' \\" _n

* child_sex
file write table "        Child's Sex & `=string(round(child_sex_mean_t, 0.001))' & `=string(round(child_sex_sd_t, 0.001))' & `=string(round(child_sex_obs_t, 0.001))' & `=string(round(child_sex_mean_c, 0.001))' & `=string(round(child_sex_sd_c, 0.001))' & `=string(round(child_sex_obs_c, 0.001))' & `=string(round(child_sex_pvalue, 0.001))' \\" _n

* mother_age
file write table "        Mother's age & `=string(round(mother_age_mean_t, 0.001))' & `=string(round(mother_age_sd_t, 0.001))' & `=string(round(mother_age_obs_t, 0.001))' & `=string(round(mother_educ_mean_c, 0.001))' & `=string(round(mother_age_sd_c, 0.001))' & `=string(round(mother_age_obs_c, 0.001))' & `=string(round(mother_age_pvalue, 0.001))' \\" _n

* mother_educ
file write table "        Mother's education & `=string(round(mother_educ_mean_t, 0.001))' & `=string(round(mother_educ_sd_t, 0.001))' & `=string(round(mother_educ_obs_t, 0.001))' & `=string(round(mother_educ_mean_c, 0.001))' & `=string(round(mother_educ_sd_c, 0.001))' & `=string(round(mother_educ_obs_c, 0.001))' & `=string(round(mother_educ_pvalue, 0.001))' \\" _n

* mother_health
file write table "        Mother's health & `=string(round(mother_health_mean_t, 0.001))' & `=string(round(mother_health_sd_t, 0.001))' & `=string(round(mother_health_obs_t, 0.001))' & `=string(round(mother_health_mean_c, 0.001))' & `=string(round(mother_health_sd_c, 0.001))' & `=string(round(mother_health_obs_c, 0.001))' & `=string(round(mother_health_pvalue, 0.001))' \\" _n

* mother_surviving_children
file write table "        Number of surviving children & `=string(round(mother_surviving_children_mean_t, 0.001))' & `=string(round(mother_surviving_children_sd_t, 0.001))' & `=string(round(mother_surviving_children_obs_t, 0.001))' & `=string(round(mother_surviving_children_mean_c, 0.001))' & `=string(round(mother_surviving_children_sd_c, 0.001))' & `=string(round(mother_surviving_children_obs_c, 0.001))' & `=string(round(mother_surviving_children_pvalue, 0.001))' \\" _n

* father_educ
file write table "        Father's education & `=string(round(father_educ_mean_t, 0.001))' & `=string(round(father_educ_sd_t, 0.001))' & `=string(round(father_educ_obs_t, 0.001))' & `=string(round(father_educ_mean_c, 0.001))' & `=string(round(father_educ_sd_c, 0.001))' & `=string(round(father_educ_obs_c, 0.001))' & `=string(round(father_educ_pvalue, 0.001))' \\" _n

* school_type
file write table "        Child's school type & `=string(round(school_type_mean_t, 0.001))' & `=string(round(school_type_sd_t, 0.001))' & `=string(round(school_type_obs_t, 0.001))' & `=string(round(school_type_mean_c, 0.001))' & `=string(round(school_type_sd_c, 0.001))' & `=string(round(school_type_obs_c, 0.001))' & `=string(round(school_type_pvalue, 0.001))' \\" _n

* teacher_good
file write table "        Child's teacher is good & `=string(round(teacher_good_mean_t, 0.001))' & `=string(round(teacher_good_sd_t, 0.001))' & `=string(round(teacher_good_obs_t, 0.001))' & `=string(round(teacher_good_mean_c, 0.001))' & `=string(round(teacher_good_sd_c, 0.001))' & `=string(round(teacher_good_obs_c, 0.001))' & `=string(round(teacher_good_pvalue, 0.001))' \\" _n

* teacher_regular
file write table "        Child's teacher is regular & `=string(round(teacher_regular_mean_t, 0.001))' & `=string(round(teacher_regular_sd_t, 0.001))' & `=string(round(teacher_regular_obs_t, 0.001))' & `=string(round(teacher_regular_mean_c, 0.001))' & `=string(round(teacher_regular_sd_c, 0.001))' & `=string(round(teacher_regular_obs_c, 0.001))' & `=string(round(teacher_regular_pvalue, 0.001))' \\" _n

* midday_meal
file write table "        Receives midday meals & `=string(round(midday_meal_mean_t, 0.001))' & `=string(round(midday_meal_sd_t, 0.001))' & `=string(round(midday_meal_obs_t, 0.001))' & `=string(round(midday_meal_mean_c, 0.001))' & `=string(round(midday_meal_sd_c, 0.001))' & `=string(round(midday_meal_obs_c, 0.001))' & `=string(round(midday_meal_pvalue, 0.001))' \\" _n

* hh_caste
file write table "        HH Caste & `=string(round(hh_caste_mean_t, 0.001))' & `=string(round(hh_caste_sd_t, 0.001))' & `=string(round(hh_caste_obs_t, 0.001))' & `=string(round(hh_caste_mean_c, 0.001))' & `=string(round(hh_caste_sd_c, 0.001))' & `=string(round(hh_caste_obs_c, 0.001))' & `=string(round(hh_caste_pvalue, 0.001))' \\" _n

* hh_religion
file write table "        HH Religion & `=string(round(hh_religion_mean_t, 0.001))' & `=string(round(hh_religion_sd_t, 0.001))' & `=string(round(hh_religion_obs_t, 0.001))' & `=string(round(hh_religion_mean_c, 0.001))' & `=string(round(hh_religion_sd_c, 0.001))' & `=string(round(hh_religion_obs_c, 0.001))' & `=string(round(hh_religion_pvalue, 0.001))' \\" _n

* hh_meals_perday
file write table "       HH Number of meals/day & `=string(round(hh_meals_perday_mean_t, 0.001))' & `=string(round(hh_meals_perday_sd_t, 0.001))' & `=string(round(hh_meals_perday_obs_t, 0.001))' & `=string(round(hh_meals_perday_mean_c, 0.001))' & `=string(round(hh_meals_perday_sd_c, 0.001))' & `=string(round(hh_meals_perday_obs_c, 0.001))' & `=string(round(hh_meals_perday_pvalue, 0.001))' \\" _n

* hh_poor
file write table "        HH Poor & `=string(round(hh_poor_mean_t, 0.001))' & `=string(round(hh_poor_sd_t, 0.001))' & `=string(round(hh_poor_obs_t, 0.001))' & `=string(round(hh_poor_mean_c, 0.001))' & `=string(round(hh_poor_sd_c, 0.001))' & `=string(round(hh_poor_obs_c, 0.001))' & `=string(round(hh_poor_pvalue, 0.001))' \\" _n

* hh_toilet_avail
file write table "        HH Toilet available & `=string(round(hh_toilet_avail_mean_t, 0.001))' & `=string(round(hh_toilet_avail_sd_t, 0.001))' & `=string(round(hh_toilet_avail_obs_t, 0.001))' & `=string(round(hh_toilet_avail_mean_c, 0.001))' & `=string(round(hh_toilet_avail_sd_c, 0.001))' & `=string(round(hh_toilet_avail_obs_c, 0.001))' & `=string(round(hh_toilet_avail_pvalue, 0.001))' \\" _n

* hh_electricity_avail
file write table "        HH Electricity available & `=string(round(hh_electricity_avail_mean_t, 0.001))' & `=string(round(hh_electricity_avail_sd_t, 0.001))' & `=string(round(hh_electricity_avail_obs_t, 0.001))' & `=string(round(hh_electricity_avail_mean_c, 0.001))' & `=string(round(hh_electricity_avail_sd_c, 0.001))' & `=string(round(hh_electricity_avail_obs_c, 0.001))' & `=string(round(hh_electricity_avail_pvalue, 0.001))' \\" _n

* hh_water_avail
file write table "        HH Water available & `=string(round(hh_water_avail_mean_t, 0.001))' & `=string(round(hh_water_avail_sd_t, 0.001))' & `=string(round(hh_electricity_avail_obs_t, 0.001))' & `=string(round(hh_water_avail_mean_c, 0.001))' & `=string(round(hh_water_avail_sd_c, 0.001))' & `=string(round(hh_water_avail_obs_c, 0.001))' & `=string(round(hh_water_avail_pvalue, 0.001))' \\" _n

* hh_urban
file write table "        HH Residence & `=string(round(hh_urban_mean_t, 0.001))' & `=string(round(hh_urban_sd_t, 0.001))' & `=string(round(hh_urban_obs_t, 0.001))' & `=string(round(hh_urban_mean_c, 0.001))' & `=string(round(hh_urban_sd_c, 0.001))' & `=string(round(hh_urban_obs_c, 0.001))' & `=string(round(hh_urban_pvalue, 0.001))' \\" _n

* log_hh_expend
file write table "        Log HH expenditure & `=string(round(log_hh_expend_mean_t, 0.001))' & `=string(round(log_hh_expend_sd_t, 0.001))' & `=string(round(hh_electricity_avail_obs_t, 0.001))' & `=string(round(log_hh_expend_mean_c, 0.001))' & `=string(round(log_hh_expend_sd_c, 0.001))' & `=string(round(log_hh_expend_obs_c, 0.001))' & `=string(round(log_hh_expend_pvalue, 0.001))' \\" _n

file write  table "        \bottomrule" _n

* Close the table
file write  table "    \end{tabular}" _n
file write  table "    \end{minipage}" _n
file write  table "\end{table}" _n
file write  table "\end{document}" _n

* Close the file
file close  table


********************************************************************************
********* SECTION 4: REGRESSION ANALYSIS AND PROPENSITY SCORE MATCHING *********
********************************************************************************	

* Import cleaned survey data 
use         "$data/merged_cleaned_dataset.dta", clear
 

* 4.1 Average Treatment Effects  
********************************************************************************

* Define global controls for the regression
global       controls "i.child_sex child_age_wave2 i.child_family_type mother_age mother_educ i.mother_health mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal wtew i.stateid"

*** Table 1: Impact of ICDS on Pre-adolescent Learning Outcomes (Pre and Post Matching)

* Reading Score 
reg           std_reading_score i.icds_treatment $controls, vce(cluster hhid)

* Store the regression results
estimates     store reading_model

* Math Score
quietly reg   std_math_score i.icds_treatment $controls, vce(cluster hhid)

* Store the regression results
estimates     store math_model


* Propensity Score Matching 
********************************************************************************

/*
NOTE: 
There can be several disadvantages of using propensity score matching which need to be 
considered before using it in the final analysis:
1. The success of PSM depends on how well the model is specified i.e. which covariates are 
included as controls for matching. Otherwise it can give biased estimates.
2. PSM only controls for observed confounders. 
3. As matching reduces the number of available observations, there can be a significant 
loss in sample size, which can consequently affect the statistical power of the study. 
4. It is quite possible that the quality of matches is poor, as different matching methods such as 
kernel, caliper and nearest neighbor have their own strengths and weaknesses. So the choice of 
matching method needs to be carefully determined. 
5. Using psmatch2 for statistcal matching has an important disadvatage that while calculating 
standard errors, it does not account for the propensity scores. 
6. Lastly, exisiting research on running regression on matched sample has mixed opinions on how 
reliable this form of analysis is. However, PSM is still widely used in observational studies. 
*/

* ssc install psmatch2 // Run this command if the package is not installed

* Create a temp file for storing any changes made to the data set while matching
tempfile     `temp_psmatch'

* Define global controls for matching 
global       match_controls "i.child_sex child_age_wave2 i.child_family_type mother_age mother_educ i.mother_health mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal"

* Set the seed for reproducibility of matching results 
set seed 123

* One-to-one Propensity Score Matching using the logit model 
psmatch2     icds_treatment $match_controls, out(std_reading_score) logit common ate

* Graph of Propensity Score Histogram by Treatment Status 
psgraph 

* Export the graph to a specific folder
graph        export "$plots/ps_graph.png", replace width(1500) height(1200)

** Test of Balance and Matching

* Table showing the individual covariate imbalance indicators
pstest       $match_controls, both table 
 
* Graphical summary of covariate imbalance via a dot chart
pstest       $match_controls, both graph ///
                        scale(0.6) 

* Export the graph to a specific folder
graph        export "$plots/covariate_imbalance_dotplot.png", replace width(1500) height(1200)

* Graphical summary of covariate imbalance via a histogram
pstest       $match_controls, both hist ///
                        scale(0.6)
						
* Export the graph to a specific folder
graph        export "$plots/covariate_imbalance_hist.png", replace width(1500) height(1500)
						
						
* Run weighted regression on the matched sample for Reading Score
reg          std_reading_score i.icds_treatment $controls [pweight=_weight], ///
             vce(cluster hhid)
			 
* Store the regression results
estimates     store reading_psmatch
			 
* Run weighted regression on the matched sample for Math Score
reg          std_math_score i.icds_treatment $controls [pweight=_weight], ///
             vce(cluster hhid)
			 
* Store the regression results
estimates     store math_psmatch			 

* Save temp file
save        `temp_psmatch', replace


* Output LaTeX table with results for Standardized Reading and Math Score

esttab       reading_model1 math_model1 reading_psmatch math_psmatch using /// List the regression models
             "$tables/ate_results_match.tex", replace ///
              star(* 0.10 ** 0.05 *** 0.01) /// Add significance stars to coefficients
              prehead(" \begin{tabular}{l c c c c} \toprule & \multicolumn{2}{c}{\specialcell{Pre-Matching}} & \multicolumn{2}{c}{\specialcell{Post-Matching}} \\\midrule & \specialcell{Standardized Reading Score} & \specialcell{Standardized Math Score} & \specialcell{Standardized Reading Score} & \specialcell{Standardized Math Score} \\\midrule") /// Customize table header in Latex 
              mtitles("(1)" "(2)" "(3)" "(4)") /// Add titles for model columns
              fragment nonumbers nolines noobs label /// Adjust specifications to customize table
              cells(b(fmt(3) star) se(par fmt(3) )) /// Format coefficients and standard errors with 3 decimal places
              collabels(none) /// 
              keep(1.icds_treatment 1.child_sex 1.child_family_type mother_age mother_educ ///
                   father_educ 2.school_type 2.hh_caste 1.hh_urban 1.hh_poor 2.hh_religion ///
                   3.hh_religion log_hh_expend) /// Keep only the specified terms in the table
              varlabels(1.icds_treatment "Received ICDS intervention" /// Add labels to terms
                        1.child_sex "Child's sex = Female" ///
                        1.child_family_type "Child's family type = Nuclear" ///
                        mother_age "Mother's Age" ///
                        mother_educ "Mother's Education" ///
                        father_educ "Father's Education" ///
                        2.school_type "School's type = Private" /// 
                        2.hh_caste "Household caste = Backward Caste" ///
                        1.hh_urban "Household residence = Urban" ///
                        1.hh_poor "Household below poverty line" ///
                        2.hh_religion "Household religion = Muslim" ///
                        3.hh_religion "Household religion = Christian" ///
                        log_hh_expend "Log Household Consumption Expenditure") ///
              stats(blank N r2, fmt(%9.0g %9.2f) /// Add additional statistics and labels
              labels(\hline "Number of Observations" "R-squared")) /// 
              postfoot("\bottomrule \end{tabular} ") // Add footer at the bottom of the table and close the table


    	 		   
* 4.2 Heterogeneous Treatment Effects  
********************************************************************************

* Import cleaned survey data 
use         "$data/merged_cleaned_dataset.dta", clear
 
/*
Note: 
Adjust the global controls defined above accordingly before running regressions for different variables under 
heterogeneity analysis. For example, in the first case, we remove child_sex from global controls before running 
the regression models. We add the variable back while running other regressions. 
*/

* Update global controls for the regression to avoid collinearity issues
global       controls "i.child_sex child_age_wave2 i.child_family_type mother_age mother_educ i.mother_health mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail i.hh_water_avail i.hh_poor i.hh_urban i.hh_caste i.hh_religion log_hh_expend i.midday_meal wtew i.stateid"


** 1. Heterogeneity by Child-specific characteristics
********************************************************************************

* 1.1. By child's sex: Female = 1 Male = 0

* Standardized Reading Score 						  
quietly reg    std_reading_score i.icds_treatment##i.child_sex $controls, /// 
               vce(cluster hhid)
								 
* Store the regression results
estimates     store reading_model_childsex								 

* Standardized Math Score 						  						  
quietly reg    std_math_score i.icds_treatment##i.child_sex $controls, ///
               vce(cluster hhid)
								 
* Store the regression results
estimates     store math_model_childsex	


* 1.2 By child's school type: Public = 1 Private = 2  Other = 3

* Standardized Reading Score 						  
quietly reg    std_reading_score i.icds_treatment##ib3.school_type $controls, /// Set the third category as reference 
                                   vce(cluster hhid)
							  
* Store the regression results
estimates      store reading_model_schooltype								 
						  
* Standardized Math Score 						  						  
quietly reg    std_math_score i.icds_treatment##ib3.school_type $controls, /// Set the third category as reference
                                   vce(cluster hhid)

* Store the regression results
estimates     store math_model_schooltype


** 2. Heterogeneity by Household-specific characteristics
********************************************************************************

* 2.1 By household's residence: Urban = 1 Rural = 0 

* Standardized Reading Score 						  
quietly reg    std_reading_score i.icds_treatment##ib1.hh_urban $controls, /// Set urban as the reference category 
               vce(cluster hhid)
								 
* Store the regression results
estimates     store reading_model_urban								 

* Standardized Math Score 						  						  
quietly reg    std_math_score i.icds_treatment##ib1.hh_urban $controls, /// Set urban as the reference category 
               vce(cluster hhid)
								 
* Store the regression results
estimates     store math_model_urban	
								   
								   
* 2.2 By household's social caste: Forward = 1 Backward = 2 Other = 3
								   
* Standardized Reading Score 						  
quietly reg    std_reading_score i.icds_treatment##ib3.hh_caste $controls, /// Set the third category as reference
               vce(cluster hhid)
								 
* Store the regression results
estimates     store reading_model_caste								 

* Standardized Math Score 						  						  
quietly reg    std_math_score i.icds_treatment##ib3.hh_caste $controls, /// Set the third category as reference
               vce(cluster hhid)
								 
* Store the regression results
estimates     store math_model_caste								   


* 2.3 By household's economic status: Low = 1 High = 0

* First create a binary variable for household's economic status
egen           quintile_hh_expend=cut(log_hh_expend), group(5) // Divide the expenditure variable into quintiles 
recode         quintile_hh_expend (3/4 = 0 "High Economic Status") (0/2 = 1 "Low Economic Status"), /// Recode variable such that the top two quintiles indicate high economic status
               gen(hh_economic_status)

* Standardized Reading Score 						  
quietly reg    std_reading_score i.icds_treatment##i.hh_economic_status $controls, /// 
               vce(cluster hhid)
								 
* Store the regression results
estimates      store reading_model_econstatus								 

* Standardized Math Score 						  						  
quietly reg    std_math_score i.icds_treatment##i.hh_economic_status $controls, ///
               vce(cluster hhid)
								 
* Store the regression results
estimates     store math_model_econstatus


* Output LaTeX table with results for Standardized Reading Score
esttab        reading_model_childsex reading_model_schooltype  reading_model_urban /// List the regression models
              reading_model_caste reading_model_econstatus using "$tables/hte_reading_results.tex", replace ///
 	      star(* 0.10 ** 0.05 *** 0.01) /// Add signficance stars to coefficients
              prehead(" \begin{tabular}{l c c c c c} \toprule & \multicolumn{5}{c}{\specialcell{Standardized Reading Score}} \\\midrule") /// Customize table header in Latex
	      mtitles("(1)" "(2)" "(3)" "(4)" "(5)") /// Add titles for model columns 
	      fragment nonumbers nolines noobs label /// Adjust specifications to customize table 
              cells(b(fmt(3) star) se(par fmt(3) )) /// Format coefficients and standard errors with 3 decimal places
	      collabels(none) /// 
              keep(1.icds_treatment 1.icds_treatment#1.child_sex 1.icds_treatment#1.school_type ///
		   1.icds_treatment#2.school_type 1.icds_treatment#0.hh_urban 1.icds_treatment#1.hh_caste ///
		   1.icds_treatment#2.hh_caste 1.icds_treatment#1.hh_economic_status) /// Keep only the specified terms in the table
	      varlabels(1.icds_treatment "Received ICDS intervention (Treatment)" /// Add labels to terms
	                1.icds_treatment#1.child_sex "Treatment x Child's Sex = Female" ///
			1.icds_treatment#1.school_type "Treatment x School Type = Public" ///
			1.icds_treatment#2.school_type "Treatment x School Type = Private" ///
			1.icds_treatment#0.hh_urban "Treatment x Household Residence = Rural" ///
			1.icds_treatment#1.hh_caste "Treatment x Household Caste = Forward" ///
			1.icds_treatment#2.hh_caste "Treatment x Household Caste = Backward" ///
			1.icds_treatment#1.hh_economic_status "Treatment x Household Economic Status = Low") ///
		stats(blank N r2, fmt(%9.0g %9.4f) /// Add additional statistics and labels
		labels(\hline "Number of Observations" "R-squared")) /// 
                postfoot("\bottomrule \end{tabular} ") // Add footer at the bottom of the table and close the table


* Output LaTeX table with results for Standardized Math Score
esttab        math_model_childsex math_model_schooltype  math_model_urban /// List the regression models
              math_model_caste math_model_econstatus using "$tables/hte_math_results.tex", replace ///
 	      star(* 0.10 ** 0.05 *** 0.01) /// Add signficance stars to coefficients
              prehead(" \begin{tabular}{l c c c c c} \toprule & \multicolumn{5}{c}{\specialcell{Standardized Math Score}} \\\midrule") /// Customize table header in Latex
	      mtitles("(1)" "(2)" "(3)" "(4)" "(5)") /// Add titles for model columns 
	      fragment nonumbers nolines noobs label /// Adjust specifications to customize table 
              cells(b(fmt(3) star) se(par fmt(3) )) /// Format coefficients and standard errors with 3 decimal places
	      collabels(none) /// 
              keep(1.icds_treatment 1.icds_treatment#1.child_sex 1.icds_treatment#1.school_type ///
		   1.icds_treatment#2.school_type 1.icds_treatment#0.hh_urban 1.icds_treatment#1.hh_caste ///
		   1.icds_treatment#2.hh_caste 1.icds_treatment#1.hh_economic_status) /// Keep only the specified terms in the table
	       varlabels(1.icds_treatment "Received ICDS intervention (Treatment)" /// Add labels to terms
	                 1.icds_treatment#1.child_sex "Treatment x Child's Sex = Female" ///
			 1.icds_treatment#1.school_type "Treatment x School Type = Public" ///
			 1.icds_treatment#2.school_type "Treatment x School Type = Private" ///
			 1.icds_treatment#0.hh_urban "Treatment x Household Residence = Rural" ///
			 1.icds_treatment#1.hh_caste "Treatment x Household Caste = Forward" ///
			 1.icds_treatment#2.hh_caste "Treatment x Household Caste = Backward" ///
			 1.icds_treatment#1.hh_economic_status "Treatment x Household Economic Status = Low") ///
		stats(blank N r2, fmt(%9.0g %9.4f) /// Add additional statistics and labels
		labels(\hline "Number of Observations" "R-squared")) /// 
                postfoot("\bottomrule \end{tabular} ") // Add footer at the bottom of the table and close the table

		   
********************************************************************************
******************* SECTION 5: POST ESTIMATION PLOTS ***************************
********************************************************************************	

* Import cleaned survey data 
use         "$data/merged_cleaned_dataset.dta", clear

/*
Note: Most the of variables used in this data are either binary or categorical variables
for which heterogenous treatment effects were calculated. Household consumption expenditure is 
a continuous varibale in the data that serves as an important indicator of child's household's 
economic status. To examine the variation in treatment effect more closely, the following 
quantile-based plots are created. 
*/

* Define global controls excluding log_hh_expend
global       controls "i.child_sex child_age_wave2 i.child_family_type mother_age  i.mother_health mother_educ mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail i.hh_water_avail i.hh_poor i.hh_urban i.hh_caste i.hh_religion i.midday_meal wtew i.stateid"

* Create quintiles for household consumption expenditure
xtile        q_hh_expend = log_hh_expend, nq(5)

* Create matrices to store coefficients and standard errors for both outcome variables
matrix       coef_m = J(5, 2, .)
matrix       se_m = J(5, 2, .)

* List both th outcome variables
local        outcomes "std_reading_score std_math_score"

* Loop over both the outcome variables
foreach outcome in `outcomes' {
             * Determine the column index for the outcome variable
             local col = cond("`outcome'" == "std_reading_score", 1, 2)
            
                * Loop over the quintiles
                foreach quintiles in 1 2 3 4 5 {
                * Run the regression for each quintile
                reg `outcome' i.icds_treatment $controls if q_hh_expend == `quintiles', vce(cluster hhid)
        
                * Save the coefficient and standard error into the respective matrix
                matrix coef_m[`quintiles', `col'] = _b[1.icds_treatment]
                matrix se_m[`quintiles', `col'] = _se[1.icds_treatment]
    } // End of quintile loop
} // End of outcome loop

* Create a new dataset to store coefficients and standard errors for plotting
clear
set          obs 5
gen          quintile = _n       // Variable for quintiles
gen          coef_reading = .    // Variable for Reading Score coefficients
gen          se_reading = .     // Variable for Reading Score standard errors
gen          coef_math = .      // Variable for Math Score coefficients
gen          se_math = .        // Variable for Math Score standard errors

* Assign coefficients and standard errors from the matrices
forval quintile = 1/5 {
             replace coef_reading = coef_m[`quintiles', 1] in `quintiles'
             replace se_reading = se_m[`quintiles', 1] in `quintiles'
             replace coef_math = coef_m[`quintiles', 2] in `quintiles'
             replace se_math = se_m[`quintiles', 2] in `quintiles'
} // End of loop

* Calculate lower and upper bounds for the error bars
gen          l_coef_reading = coef_reading - se_reading
gen          u_coef_reading = coef_reading + se_reading
gen          l_coef_math = coef_math - se_math
gen          u_coef_math = coef_math + se_math
		 
		 
* Plot for Standardized Reading Score	   
twoway      (scatter coef_reading quintile, msymbol(O) mcolor(red)) /// Add scatter points 
            (line coef_reading quintile, lcolor(red) lwidth(medium)) /// Add line connecting the points
            (rcap l_coef_reading u_coef_reading quintile, lcolor(black)) /// Add error bars
            , xlabel(1(1)5) ylabel(, grid) /// Format x and y axis labels
            yline(0, lpattern(dash) lcolor(black))  /// Add a vertical dashed line 
            title("Variation in Treatment Effect for Standardized Reading Score", size(4)) /// Add plot title 
            xtitle("Household Consumption Expenditure Quintiles") ytitle("Coefficient") /// Add x and y axis labels 
	    legend(off) /// Remove legend 
            scale(0.8) // Adjust scale
	   
* Save the plot in Stata to access it later			 
graph       save hhexpend_reading_plot.gph, replace	   
	   
* Plot for Standardized Math Score	   
twoway     (scatter coef_math quintile, msymbol(O) mcolor(red)) /// Add scatter points 
           (line coef_math quintile, lcolor(red) lwidth(medium)) /// Add line connecting the points
           (rcap l_coef_math u_coef_math quintile, lcolor(black)) /// Add error bars 
           , xlabel(1(1)5) ylabel(, grid) /// Format x and y axis labels
           yline(0, lpattern(dash) lcolor(black))  /// Add a vertical dashed line 
           title("Variation in Treatment Effect for Standardized Math Score", size(4)) /// Add plot title 
           xtitle("Household Consumption Expenditure Quantiles") ytitle("Coefficient") /// Add x and y axis labels
	   legend(off) /// Remove legend 
           scale(0.8) // Adjust scale
	   
* Save the plot in Stata to access it later			 
graph       save hhexpend_math_plot.gph, replace	

* Combine both the plots 
graph       combine hhexpend_reading_plot.gph hhexpend_math_plot.gph, row(2)

* Export the combined plot to a specific folder
graph       export "$plots/combined_hhexpend_plot.png", replace width(1500) height(1200)
	   

********************************************************************************
************************** END OF SCRIPT ***************************************
********************************************************************************	
	   

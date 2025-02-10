/******************************************************************************

Stata Version 18.0

Purpose: Data Cleaning, Analysis and Visualization

Author: Anudhii Sundaram 

Date Created: January 22, 2025
Date Modified: January 30, 2025

Project Description: 
The aim of the project is to study a large-scale randomized controlled trial that  provided a universal cash transfer of USD 416 (MWK 701,000) to around 70,000
individuals over the course of 12 months in Malawi. Rollout was randomized across 
Group Village Headman geographic clusters (GVHs). The study leverages the randomization in the timing of the receipt of the cash transfer. 

Brief Description of Data: 
Study uses a panel data set which covers information on household outcomes across 8 waves. This includes two baseline surveys and six follow-up surveys.  

Brief Description of Variables: 
In this script, the outcomes of interest are household consumption and spending. 
Since it is a rollout RCT, the study leverages randomization in the timing of the 
receipt of the transfers to identify the effect of treatment. 

This script contains the following sections:
*** Section 1: Data Cleaning
*** Section 2: Summary Statistics and T-test Table
*** Section 3: Uncontrolled Treatment Effects Plots
*** Section 4: Two-way Fixed Effects Regression
*** Section 5: Impulse Response Function Plots 


*******************************************************************************/

clear all
set more off
set scheme s2color
pause on
set maxvar 30000


********************************************************************************
*************************** GLOBAL DIRECTORY *********************************** ********************************************************************************

display "`c(username)'" // Displays the username of your computer 

* Define the working directory and global root variable
if "`c(username)'" == "anudhiisundaram" { 
	
	* The main directory
    global root "/Users/anudhiisundaram/Desktop/Coding Samples/Stata/Malawi_CashTransfer"
	
	* The directory for importing and saving data sets 
	global data "$root/data"
	
	* The directory for saving tables 
	global tables "$root/tables"
	
	* The directory for saving plots 
	global plots "$root/plots"

}

********************************************************************************
*************************  SECTION 1. DATA CLEANING ****************************
********************************************************************************


* Import Datasets 
********************************************************************************

* Import First Dataset 
use       "$data/gender.dta"
tempfile  first_dataset       // Create a temporary file to store the first dataset
save     `first_dataset'

* Import Second Dataset
clear                          // Clear the data in memory
use       "$data/no_PII_exp.dta"
tempfile  second_dataset      // Create a temporary file to store the second dataset
save     `second_dataset'


* Merge datasets
********************************************************************************

/*
NOTE: To perform the merge using id and month, we have to clean the month variable in the first data set.
*/


* Load the first dataset for cleaning
use      `first_dataset', clear 

* Check for duplicates in id and month 
bysort   id month: gen dup_count = _N
list     id month dup_count if dup_count > 1

* Drop the variable created for duplicate count
drop     dup_count

* Generate a new numeric variable for month
gen      clean_monthyear = .

* Replace it with corresponding numeric values
replace  clean_monthyear = 1  if (month == "October23")
replace  clean_monthyear = 1  if (month == "oct23")
replace  clean_monthyear = 2  if (month == "nov23")
replace  clean_monthyear = 3  if (month == "dec23")
replace  clean_monthyear = 4  if (month == "jan24")
replace  clean_monthyear = 5  if (month == "feb24")
replace  clean_monthyear = 6  if (month == "mar24")
replace  clean_monthyear = 7  if (month == "apr24")
replace  clean_monthyear = 8  if (month == "may24")
replace  clean_monthyear = 9  if (month  == "jun24")
replace  clean_monthyear = 10 if (month  == "jul24")
replace  clean_monthyear = 11 if (month  == "aug24")
replace  clean_monthyear = 12 if (month  == "sep24")
replace  clean_monthyear = 13 if (month  == "oct24")
replace  clean_monthyear = 13 if (month == "October24")
replace  clean_monthyear = 14 if (month  == "nov24")

* Change the variable type to float
destring clean_monthyear, replace

* Defining labels
label    define month_label 1 "oct23" 2 "nov23" 3 "dec23" 4 "jan24" ///
                            5 "feb24" 6 "mar24" 7 "apr24" 8 "may24" ///
                            9 "jun24" 10 "jul24" 11 "aug24" 12 "sep24" ///
                            13 "oct24" 14 "nov24"

* Assign labels to the numeric variable
label    values clean_monthyear month_label

* Drop the uncleaned month variable 
drop     month 

* Rename the cleaned month variable 
rename   clean_monthyear interview_month

* Check for inconsistencies in the cleaned month variable 
tab      interview_month

* Save first dataset after cleaning the month variable
save     `first_dataset', replace


* Load the first dataset for merge
use      `first_dataset', clear 

* Merge the first data set with the second dataset using the common month variable
merge     1:1 id interview_month using `second_dataset'

* Check the results of the merge
tab       _merge

* Optional: Keep only matched records
keep      if _merge == 3 

* Drop the merge indicator if not needed
drop      _merge



* Check for unreasonable values in ag_income, non_ag_income, and wage_income
********************************************************************************

* Run basic descriptive statistics to identify outliers
summarize ag_income non_ag_income wage_income, detail

* List negative values 
list      ag_income non_ag_income wage_income ///
          if ag_income < 0 | non_ag_income < 0 | wage_income < 0

* Replace negative values with missing  
foreach   var of varlist ag_income non_ag_income wage_income {
          replace `var' = . if `var' < 0
       }
	   	  	   
* Double check for negative values 	   
summarize ag_income non_ag_income wage_income, detail

	   
* Winsorize tot_cons and tot_spend at the 95th percentile
********************************************************************************

* Compute 95th percentile of tot_cons and tot_spend
egen     p95_tot_cons = pctile(tot_cons), p(95)    // Total consumption 
egen     p95_tot_spend = pctile(tot_spend), p(95)  // Total spending 

* Generating new variables equal to tot_cons and tot_spend
gen      tot_cons_w = tot_cons                     // Total consumption 
gen      tot_spend_w = tot_spend                   // Total spending

* Replace with 95th percentile for values greater than p95_tot_cons and p95_tot_spend
replace  tot_cons_w = p95_tot_cons if tot_cons > p95_tot_cons     // Total consumption
replace  tot_spend_w = p95_tot_spend if tot_spend > p95_tot_spend // Total spending

* Drop redundant variables 
drop     p95_tot_cons p95_tot_spend


* Clean gender_string variable
********************************************************************************

* Check for inconsistencies in the variable
tab      gender_string 

* Replace inconsistent values with missing
replace  gender_string = "" if (gender_string == "3")
replace  gender_string = "" if (gender_string == ".")

* Check if any IDs have multiple gender values
bysort   id (gender_string): /// 
gen      flag1_gender = gender_string != gender_string[_n-1] if _n > 1

tab      id if flag1_gender == 1

* Ensure that each ID has a unique gender
egen     gender_cleaned = mode(gender_string), by(id) 

* Cheching again for inconsistencies in the cleaned variable
bysort   id (gender_cleaned): /// 
gen      flag2_gender = gender_cleaned != gender_cleaned[_n-1] if _n > 1

tab      id if flag2_gender == 1

* Generate a numeric variable for gender using gender_cleaned
gen      gender_num = .
replace  gender_num = 1 if (gender_cleaned == "female")
replace  gender_num = 0 if (gender_cleaned == "male")

label    define gender_label 1 "female" 0 "male"
label    values gender_num gender_label

* Check for missing values in gender_num 
br       id gender_string gender_cleaned if (gender_num == .)

/*
NOTE: There are 223 observations with missing values in gender_num and these are missing either because they were missing in the original gender_string variable or because the mode function used to created gender_cleaned variable could not pick one value. In the latter case, this is mainly because there are an equal number of male/female values within an ID. For instance, for ID = 1735, there three males and three females recorded under the original gender_string variable so the mode function could not pick one value.  
*/

* Drop redundant variables
drop     gender_string gender_cleaned flag1_gender flag2_gender

* Save the final merged and cleaned dataset
save     "$data/merged_dataset.dta", replace



********************************************************************************
*************** SECTION 2. SUMMARY STATISTICS AND T-TEST TABLE *****************
********************************************************************************

* Import merged data set 
use       "$data/merged_dataset.dta", clear


* Average the dataset (over time) to one observation per individual
********************************************************************************

* Create a temporary file
tempfile  temp_avg

* Collapse all numeric variables while keeping gender
collapse  (mean) ag_income non_ag_income wage_income tot_cons_w tot_spend_w ///
          (first) gender_num, by(id)

* Add value labels to gender_num
label      values gender_num gender_label

* Save the collapsed dataset to the temporary file
save      `temp_avg', replace


* Generate a summary statistics and t-test table by recipient's gender 
********************************************************************************

* Load the temporary file
use       `temp_avg', clear

* Define the list of control variables 
local      control_vars "ag_income non_ag_income wage_income tot_cons_w tot_spend_w "

* Loop over each control variable
foreach    var in `control_vars' { 

          * Perform a t-test on the variable of interest by the grouping variable
           ttest `var', by(gender_num)

           * Extract values for females in the sample (gender_num == 1)
           local mean_female = r(mu_1)         // Mean 
           local sd_female = r(sd_1)           // Standard deviation 
           local obs_female = r(N_1)           // Number of observations 
       
           * Extract values for males in the sample (gender_num == 0)
           local mean_male = r(mu_2)            // Mean 
           local sd_male = r(sd_2)              // Standard deviation 
           local obs_male = r(N_2)              // Number of observations 
       
           * Extract the p-value for the t-test
           local p_value = r(p)                 // p-value for the difference in means
     
 
           * Creating new variables to hold the extracted values
           gen `var'_mean_f = `mean_female'
           gen `var'_sd_f = `sd_female'
           gen `var'_obs_f = `obs_female'
	       gen `var'_mean_m = `mean_male'
           gen `var'_sd_m = `sd_male'
           gen `var'_obs_m = `obs_male'
	       gen `var'_pvalue = `p_value'

} // End of loop 


** Create the summary statistics and t-test table in Latex 
********************************************************************************

file open  table using "$tables/sum_stat_table.tex", write replace

file write table "\documentclass[12pt]{article}" _n
file write table "\usepackage{graphicx}" _n
file write table "\usepackage{caption}" _n
file write table "\usepackage{booktabs}" _n
file write table "\usepackage{multirow}" _n
file write table "\usepackage{makecell}" _n
file write table "\renewcommand{\arraystretch}{1.5}" _n   
file write table "\setlength{\arrayrulewidth}{0.5mm}" _n

file write table "\begin{document}" _n

* Start the table
file write table "\begin{table}[ht]" _n
file write table " \hspace*{-1in}" _n
file write table " \begin{minipage}{\textwidth} " _n

* Customize table header in Latex 
file write table "    \centering" _n
file write table "    \caption{Summary Statistics and T-test}" _n 
file write table "    \small " _n
file write table "    \begin{tabular}{lccc ccc c}" _n
file write table "        \toprule" _n
file write table "        & \multicolumn{3}{c}{\textbf{Women}} & \multicolumn{3}{c}{\textbf{Men}} & \\" _n
file write table "        \cmidrule(lr){2-4} \cmidrule(lr){5-7}" _n
file write table "        \textbf{Variable} & \textbf{Mean} & \textbf{Std. Dev.} & \textbf{N} & \textbf{Mean} & \textbf{Std. Dev.} & \textbf{N} & \textbf{P-value} \\" _n
file write table "        \midrule" _n

* Write data rows (need to define the variables in Stata first)
* Extract values of mean, standard deviation, number of observations and p value 

* ag_income
file write table "        Household Agricultural Income & `=string(round(ag_income_mean_f, 0.001))' & `=string(round(ag_income_sd_f, 0.001))' & `=string(round(ag_income_obs_f, 0.001))' & `=string(round(ag_income_mean_m, 0.001))' & `=string(round(ag_income_sd_m, 0.001))' & `=string(round(ag_income_obs_m, 0.001))' & `=string(round(ag_income_pvalue, 0.001))' \\" _n

* non_ag_income
file write table "        Household Non-agricultural Income & `=string(round(non_ag_income_mean_f, 0.001))' & `=string(round(non_ag_income_sd_f, 0.001))' & `=string(round(non_ag_income_obs_f, 0.001))' & `=string(round(non_ag_income_mean_m, 0.001))' & `=string(round(non_ag_income_sd_m, 0.001))' & `=string(round(non_ag_income_obs_m, 0.001))' & `=string(round(non_ag_income_pvalue, 0.001))' \\" _n

* wage_income
file write table "        Household Wage Income & `=string(round(wage_income_mean_f, 0.001))' & `=string(round(wage_income_sd_f, 0.001))' & `=string(round(wage_income_obs_f, 0.001))' & `=string(round(wage_income_mean_m, 0.001))' & `=string(round(wage_income_sd_m, 0.001))' & `=string(round(wage_income_obs_m, 0.001))' & `=string(round(wage_income_pvalue, 0.001))' \\" _n

* tot_cons_w
file write table "        Household Consumption & `=string(round(tot_cons_w_mean_f, 0.001))' & `=string(round(tot_cons_w_sd_f, 0.001))' & `=string(round(tot_cons_w_obs_f, 0.001))' & `=string(round(tot_cons_w_mean_m, 0.001))' & `=string(round(tot_cons_w_sd_m, 0.001))' & `=string(round(tot_cons_w_obs_m, 0.001))' & `=string(round(tot_cons_w_pvalue, 0.001))' \\" _n

* tot_spend_w
file write table "        Household Spending & `=string(round(tot_spend_w_mean_f, 0.001))' & `=string(round(tot_spend_w_sd_f, 0.001))' & `=string(round(tot_spend_w_obs_f, 0.001))' & `=string(round(tot_spend_w_mean_m, 0.001))' & `=string(round(tot_spend_w_sd_m, 0.001))' & `=string(round(tot_spend_w_obs_m, 0.001))' & `=string(round(tot_spend_w_pvalue, 0.001))' \\" _n

* Add note at the bottom of the table 
file write table "        \bottomrule" _n
file write table "        \multicolumn{8}{c}{Notes: Household spending and consumption variables were winsorized at the 95th percentile.} \\" _n

* Close the table
file write table "    \end{tabular}" _n
file write table "    \end{minipage}" _n
file write table "\end{table}" _n
file write table "\end{document}" _n

* Close the file
file close table


/*
NOTE:
From the summary statistics table, it is clear that in the given sample, women tend
to have higher household agricultural income, non-agricultural income, wage income,
consumption, and spending compared to men. All the differences reported are statistically
significant, as indicated by the p-values, suggesting that these differences are not 
due to random chance. Agricultural income seems to be the main source of income for 
both men and women in the sample.
*/



********************************************************************************
************** SECTION 3. UNCONTROLLED TREATMENT EFFECTS PLOTS *****************
********************************************************************************

* Importing the merged dataset 
use       "$data/merged_dataset.dta", clear


* Create plots to show the change in tot_cons_w and tot_spend_w around the timing of treatment
********************************************************************************

* Calculate Averages by Wave and Treatment Cohort
collapse   (mean) tot_spend_w tot_cons_w, by(treat treat_timing)

/*
NOTE: We are generating twoway line plots to visualize total spending (tot_spend_w) and 
total consumption (tot_cons_w) over survey waves (treat) for different treatment cohorts 
based on when they received the transfer (treat_timing). Treatment cohort is defined as 
groups receiving treatment in the same month. We want to show the change in outcomes of 
interest around the timing of treatment.
*/


* 1. Plot for Winsorized Household Spending 
	   
twoway     (line tot_spend_w treat if treat_timing == "Dec23", /// Plot total spending over survey waves for given cohort     
           lcolor(blue) lwidth(medium) lpattern(solid) mcolor(blue)) /// Adjust the line color, width and pattern 
           (line tot_spend_w treat if treat_timing == "Feb24", /// 
		   lcolor(red) lwidth(medium) lpattern(solid) mcolor(red)) ///
           (line tot_spend_w treat if treat_timing == "Mar24", ///
		   lcolor(green) lwidth(medium) lpattern(solid) mcolor(green)) ///
           (line tot_spend_w treat if treat_timing == "Apr24", ///
		   lcolor(orange) lwidth(medium) lpattern(solid) mcolor(orange)) ///
           (line tot_spend_w treat if treat_timing == "May24", ///
		   lcolor(purple) lwidth(medium) lpattern(solid) mcolor(purple)) ///
           (line tot_spend_w treat if treat_timing == "Jun24", ///
		   lcolor(black) lwidth(medium) lpattern(solid) mcolor(black)), ///
     legend(order(1 "Dec23" 2 "Feb24" 3 "Mar24" 4 "Apr24" 5 "May24" 6 "Jun24") position(1)) /// Adjust legend labels and position
     title("Total Spending Over Survey Waves by Treatment Cohort", size(4)) /// Add title
     xtitle("Survey Wave", size(3)) ytitle("Average Total Spending", size(3)) ///  Label the x and y axis
     xlabel(0(1)7, valuelabel angle(45)) /// Adjust the angle of value labels 
	 xline(1, lpattern(dash) lcolor(black))  /// Add a vertical dashed line at "Post transfer"
     scale(0.8) // Adjust the scale of the plot
	   
	   
* Export the graph to a specific folder (plots)
graph      export "$plots/total_spending_plot.png", replace width(1200) height(800)



* 2. Plot for Winsorized Household Consumption 
	 
twoway     (line tot_cons_w treat if treat_timing == "Dec23", /// Plot total consumption over survey waves for given cohort     
           lcolor(blue) lwidth(medium) lpattern(solid) mcolor(blue)) /// Adjust the line color, width and pattern 
           (line tot_cons_w treat if treat_timing == "Feb24", ///
	       lcolor(red) lwidth(medium) lpattern(solid) mcolor(red)) ///
           (line tot_cons_w treat if treat_timing == "Mar24", ///
	       lcolor(green) lwidth(medium) lpattern(solid) mcolor(green)) ///
           (line tot_cons_w treat if treat_timing == "Apr24", ///
	       lcolor(orange) lwidth(medium) lpattern(solid) mcolor(orange)) ///
           (line tot_cons_w treat if treat_timing == "May24", ///
	       lcolor(purple) lwidth(medium) lpattern(solid) mcolor(purple)) ///
           (line tot_cons_w treat if treat_timing == "Jun24", ///
	       lcolor(black) lwidth(medium) lpattern(solid) mcolor(black)), ///
  legend(order(1 "Dec23" 2 "Feb24" 3 "Mar24" 4 "Apr24" 5 "May24" 6 "Jun24")  position(1)) /// Adjust legend labels and position
  title("Total Consumption Over Survey Waves by Treatment Cohort", size(4)) ///
  xtitle("Survey Wave", size(3)) ytitle("Average Total Consumption", size(3)) /// Label the x and y axis
  xlabel(0(1)7, valuelabel angle(45)) /// Adjust the angle of value labels 
  xline(1, lpattern(dash) lcolor(black)) /// Add the vertical dashed line at "Post transfer"
  scale(0.8) // Adjust the scale of the plot
	   	 
	   
* Export the graph to a specific folder (plots)
graph     export "$plots/total_consumption_plot.png", replace width(1200) height(800)
	
   	
	
********************************************************************************
**************** SECTION 4. TWO-WAY FIXED EFFECTS REGRESSION *******************
********************************************************************************

   
* Importing the merged dataset 
use       "$data/merged_dataset.dta", clear

* Generate logged versions of the winsorized tot_cons and tot_spend variables. 
gen       log_tot_spend_w = ln(tot_spend_w + 1) // Total spending 
gen       log_tot_cons_w  = ln(tot_cons_w + 1)   // Total consumption

/*
NOTE: 
Log transformation is used to transform skewed data to approximately follow a
normal distribution so that standard statistical inference can be carried out. In the 
case of both the spending and consumption variables, their distribution is skewed, so 
taking the log of variables, can make the analysis and inference more reliable. Add a 
constant 1 because there are many observations with '0' in both the variables.  
*/

* Implement the two-way fixed effects model 
********************************************************************************

* Create indicator variables based on the treat variable (time since treatment)
gen       period_0_2_months  =  (treat == 2)
gen       period_2_3_months  =  (treat == 3)
gen       period_4_5_months  =  (treat == 4)
gen       period_6_7_months  =  (treat == 5)
gen       period_8_9_months  =  (treat == 6)
gen       period_9_10_months =  (treat == 7)

/*
NOTE: 
The treat variable is used to indicate how many months since the transfer 
each observation corresponds to.
*/

* Create a unique time variable for each individual
bysort    id (treat): gen time_id = _n

/*
NOTE: 
We want to create a unique time variable for each individual, so that each combination 
of id and time (based on treat) is unique. The treat variable represents the months 
after the transfer, so we can use it as the time variable in the panel data structure.
*/

* Set the panel data structure using 'id' (individual identifier) and 'time_id' (unique time variable)
xtset     id time_id

* Create a composite GVH × month variable for clustering based on treat_timing
gen       gvh_month = gvh * 100 + ///
          cond(treat_timing == "Dec23", 1, ///
          cond(treat_timing == "Feb24", 2, ///
          cond(treat_timing == "Mar24", 3, ///
          cond(treat_timing == "Apr24", 4, ///
          cond(treat_timing == "May24", 5, ///
          cond(treat_timing == "Jun24", 6, .))))))
		  
/*
NOTE:
Given the structure of the data, we will get more robust estimates of standard errors 
if we consider correlation of errors within a GVH over time, as well as correlation 
of errors across GVHs in a month. Thus, the standard errors will be clustered at the 
gvh × month level. There is a possibility that the outcomes of interest for households
within a GVH may be correlated across months due to impact of some local conditions. 
And it is also possible that in a given month there is a national shock that affects 
the outcomes of households across GVHs.
*/		  
	
	
* Run regression two-way fixed effects regression	
********************************************************************************

/*
NOTE:
To identify the treatment coefficient estimate, it is important for the parallel 
trends assumption to hold, which implies that without the cash transfer, the 
difference in outcomes between the treatment and control households would have been 
constant. In the absence of this assumption, there is a possibility that the observed 
difference in trends is because of factors other than the intervention.
*/

** 1. Household Consumption 

* Regression for log_tot_cons_w with clustering at the GVH × month level
xtreg     log_tot_cons_w i.gvh##(period_0_2_months period_2_3_months period_4_5_months ///
          period_6_7_months period_8_9_months period_9_10_months), ///
		  fe cluster(gvh_month)
		  
* Store the regression results
estimates store consumption_model

** 2. Household Spending 

* Regression for log_tot_spend_w with clustering at the GVH × month level
xtreg      log_tot_spend_w i.gvh##(period_0_2_months period_2_3_months period_4_5_months ///
           period_6_7_months period_8_9_months period_9_10_months), ///
		   fe cluster(gvh_month)

* Store the regression results
estimates  store spending_model


* Output LaTeX table with results for Household Consumption and Household Spending
esttab     consumption_model spending_model using "$tables/reg_results.tex", replace ///
 	       star(* 0.10 ** 0.05 *** 0.01) /// Add signficance stars to coefficients
           prehead(" \begin{tabular}{l c c} \toprule & \specialcell{Log Household Consumption} & \specialcell{Log Household Spending} \\\midrule") /// Customize table header in Latex 
		   mtitles("(1)" "(2)") /// Add titles for model columns 
	       fragment nonumbers nolines noobs label /// Adjust specifications to customize table 
           cells(b(fmt(3) star) se(par fmt(3) )) /// Format coefficients and standard errors with 3 decimal places
		   collabels(none) /// 
           keep(1.period_0_2_months 1.period_2_3_months 1.period_4_5_months 1.period_6_7_months ///
	            1.period_8_9_months 1.period_9_10_months) /// Keep only the specified terms in the table
	       varlabels(1.period_0_2_months "0-2 months post cash transfer" /// Add labels to terms
	                 1.period_2_3_months "2-3 months post cash transfer" ///
	       		     1.period_4_5_months "4-5 months post cash transfer" ///
	       		     1.period_6_7_months "6-7 months post cash transfer" ///
	       		     1.period_8_9_months "8-9 months post cash transfer" ///
	       		     1.period_9_10_months "9-10 months post cash transfer") ///
		   stats(blank N r2, fmt(%9.0g %9.2f) labels(\hline "Number of observations" "R-squared")) /// Add additional statistics and labels
           postfoot("\bottomrule \end{tabular} ") // Add footer at the bottom of the table and close the table
		   	 

			 
********************************************************************************
**************** SECTION 5. IMPULSE RESPONSE FUNCTION PLOTS ************************
********************************************************************************
			 
/*
NOTE:
Impulse Response Function shows the effect of a shock (in this case the cash transfer) over time.
In this section, we will create impulse response function plots for both the outcome variables.  
*/


* Create a temp file for storing any changes made to the data set while creating the plot
tempfile     temp_irf

* Define outcome variables
local        outcome_vars log_tot_cons_w log_tot_spend_w
local        model_names consumption_model spending_model
local        coef_vars coefficient_cons coefficient_spend

* Loop over outcome variables to run regressions and store coefficients
forvalues i = 1/2 {
                local outcome_var : word `i' of `outcome_vars'
                local model_name : word `i' of `model_names'
                local coef_var : word `i' of `coef_vars'
            
                * Run the two-way fixed effects regression model
                xtreg `outcome_var' i.gvh##(period_0_2_months period_2_3_months period_4_5_months ///
                         period_6_7_months period_8_9_months period_9_10_months), ///
                         fe cluster(gvh_month)
            
                * Store the regression results
                estimates store `model_name'
            
                * Store the coefficient estimates in a matrix
                matrix b = e(b)
            
                * Extract specific coefficient estimates from the regression
                scalar coef_0_2  = b[1, "1.period_0_2_months"]
                scalar coef_2_3  = b[1, "1.period_2_3_months"]
                scalar coef_4_5  = b[1, "1.period_4_5_months"]
                scalar coef_6_7  = b[1, "1.period_6_7_months"]
                scalar coef_8_9  = b[1, "1.period_8_9_months"]
                scalar coef_9_10 = b[1, "1.period_9_10_months"]
	            
	           * Create a variable for time period after transfer
               capture confirm variable period
               if _rc {
                        gen     period = .
                        replace period = 1 in 1
                        replace period = 2 in 2
                        replace period = 3 in 3
                        replace period = 4 in 4
                        replace period = 5 in 5
                        replace period = 6 in 6

               * Define and assign labels for the variable 
               label define period_label 1 "0-2 months" 2 "2-3 months" 3 "4-5 months" ///
                                        4 "6-7 months" 5 "8-9 months" 6 "9-10 months" 
               label values period period_label
             }

                * Create a variable to store coefficient estimates
                gen     `coef_var' = .
                replace `coef_var' = coef_0_2  if (period == 1)
                replace `coef_var' = coef_2_3  if (period == 2)
                replace `coef_var' = coef_4_5  if (period == 3)
                replace `coef_var' = coef_6_7  if (period == 4)
                replace `coef_var' = coef_8_9  if (period == 5)
                replace `coef_var' = coef_9_10 if (period == 6)

} // End of main loop 


* Plot the Impulse Response Function (IRF) for Household Consumption
twoway       (line coefficient_cons period, sort), ///
             title("Impulse Response Function for Log Household Consumption", size(6)) /// Add title
             ylabel(, grid) xlabel(, valuelabel) /// Adjust x and y labels
			 ytitle("Coefficient Estimate", size(5)) /// Label y axis
             xtitle("Time Period After Cash Transfer", size(5)) /// Label x axis
			 legend(off) /// Remove legend
	         scale(0.6) // Adjust scale of plot			 
			 		 
			 
* Save the plot in Stata to access it later			 
graph        save irf_consumption_plot.gph, replace

* Export the plot to a specific folder 
graph        export "$plots/irf_consumption_plot.png", replace width(1200) height(800)
			 
			 
* Plot the Impulse Response Function (IRF) for Household Spending
twoway       (line coefficient_spend period, sort), ///
             title("Impulse Response Function for Log Household Spending", size(6)) /// Add title
             ylabel(, grid) xlabel(, valuelabel) /// Adjust x and y labels
			 ytitle("Coefficient Estimate", size(5)) /// Label y axis
             xtitle("Time Period After Cash Transfer", size(5)) /// Label x axis
			 legend(off) /// Remove legend
	         scale(0.6) // Adjust scale of plot			 
			 		 
* Save the plot in Stata to access it later			 
graph       save irf_spending_plot.gph, replace
				 
* Export the plot to a specific folder 
graph       export "$plots/irf_spending_plot.png", replace width(1200) height(800)


* Combine both the Impulse Response Function plots 
graph       combine irf_consumption_plot.gph irf_spending_plot.gph, row(2)

* Export the combined plot to a specific folder
graph       export "$plots/combined_irf_plot.png", replace width(1200) height(1200)

* Save temp file
save        `temp_irf', replace

/*
NOTE:
The impulse response function plots of household consumption and spending are almost identical.  
*/


********************************************************************************
**************************** END OF SCRIPT *************************************
********************************************************************************


************************************************************
* Julia DeChellis - Union Density & Income Inequality (2000–2023)
* Stata Code 
************************************************************

version 18
clear all
set more off
capture log close

cd "~/Downloads"

************************************************************
* 1. IMPORT AND CLEAN DATASETS
************************************************************

*----------------------------*
* 1A. Gini (after-tax)
*----------------------------*
clear
import excel "gini_aftertax_provinces_2000_2023.xlsx", firstrow clear
keep province year gini_aftertax
destring year gini_aftertax, replace force
sort province year
save "gini.dta", replace

*----------------------------*
* 1B. Union Density
*----------------------------*
clear
import excel "uniondensity_provinces_2000_2023.xlsx", firstrow clear
keep province year union_status
rename union_status union_density
destring year union_density, replace force
sort province year
save "union.dta", replace

*----------------------------*
* 1C. Unemployment Rate
*----------------------------*
clear
import excel "unemployment_rate_annual_2000_2023.xlsx", firstrow clear
keep province year avg_unemp
rename avg_unemp unemployment_rate
destring year unemployment_rate, replace force
sort province year
save "unemp.dta", replace

*----------------------------*
* 1D. Real GDP
*----------------------------*
clear
import excel "gdp_real_2000_2023.xlsx", firstrow clear
keep province year gdp_annual
rename gdp_annual gdp_real
destring year gdp_real, replace force
sort province year
save "gdp.dta", replace

*----------------------------*
* 1E. Population
*----------------------------*
clear
import excel "pop_2000_2023.xlsx", firstrow clear
keep province year population
destring year population, replace force
sort province year
save "pop.dta", replace

*----------------------------*
* 1F. Education (low education share)
*----------------------------*
clear
import excel "education_low_2000_2023.xlsx", firstrow clear
keep province year below_upper_secondary
rename below_upper_secondary education_low
destring year education_low, replace force
sort province year
save "edu.dta", replace

************************************************************
* 2. MERGE ALL DATASETS INTO MASTER PANEL
************************************************************

use "gini.dta", clear
merge 1:1 province year using "union.dta", nogen
merge 1:1 province year using "unemp.dta", nogen
merge 1:1 province year using "gdp.dta",  nogen
merge 1:1 province year using "pop.dta",  nogen
merge 1:1 province year using "edu.dta",  nogen

save "master_481_panel.dta", replace

************************************************************
* 3. LABEL VARIABLES
************************************************************

label variable gini_aftertax      "After-tax Gini"
label variable union_density      "Union density (%)"
label variable unemployment_rate  "Unemployment rate (%)"
label variable gdp_real           "Real GDP (millions)"
label variable population         "Population"
label variable education_low      "Low education (%)"

************************************************************
* 4. PANEL SETUP
************************************************************

use "master_481_panel.dta", clear
encode province, gen(prov_id)
xtset prov_id year

************************************************************
* 5. SCALE GDP/POP FOR INTERPRETABILITY
************************************************************

gen gdp_real_m = gdp_real / 1000       
gen population_m = population / 1000    

************************************************************
* 6. DESCRIPTIVE STATISTICS
************************************************************

ssc install estout, replace

estpost summarize gini_aftertax union_density unemployment_rate gdp_real population education_low
esttab ., cells("mean sd min max") label nomtitle nonumber noobs title("Descriptive Statistics (2000–2023)")

************************************************************
* 7. CORRELATION TABLE
************************************************************

corr union_density unemployment_rate gdp_real population education_low

************************************************************
* 8. REGRESSION MODELS
************************************************************

*----------------------------*
* 8A. BASELINE: PROVINCE FE ONLY
*----------------------------*

xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low, fe cluster(prov_id)

*----------------------------*
* 8B. PREFERRED MODEL: PROVINCE FE + YEAR FE
*----------------------------*

xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year, fe cluster(prov_id)


use "master_481_panel.dta", clear
encode province, gen(prov_id)
xtset prov_id year


gen gdp_real_m = gdp_real / 1000
gen population_m = population / 1000


************************************************************
* 9. ROBUSTNESS CHECKS
************************************************************

* 9A. TIME SPLIT ROBUSTNESS
display "=== EARLY PERIOD (2000–2010) ==="
xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year if year <= 2010, fe cluster(prov_id)

display "=== LATE PERIOD (2011–2023) ==="
xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year if year >= 2011, fe cluster(prov_id)


* 9B. FIXED EFFECTS VARIATIONS
display "=== PROVINCE FE ONLY ==="
xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low, fe cluster(prov_id)

display "=== YEAR FE ONLY ==="
reg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year, cluster(prov_id)

display "=== POOLED OLS (NO FE) ==="
reg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low, cluster(prov_id)


* 9C. DROP PROVINCES ROBUSTNESS
display "=== DROP QC ==="
xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year if province != "QC", fe cluster(prov_id)

display "=== DROP AB ==="
xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year if province != "AB", fe cluster(prov_id)


* lag of union_density (L.union_density)

xtreg gini_aftertax L.union_density unemployment_rate gdp_real_m population_m education_low i.year, fe cluster(prov_id)

************************************************************
* 10. Exporting Tables
************************************************************

***************
* TABLE 2: BASELINE FE
***************

eststo clear
eststo baseline: xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low, ///
    fe cluster(prov_id)

esttab baseline using "table2_baseline.rtf", ///
    replace rtf ///
    title("Table 2. Baseline Provincial Fixed Effects Regression") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label nonumber ///
    stats(N r2_within, fmt(0 3) labels("Observations" "Within R-squared"))

***************
* TABLE 3: PREFERRED FE + YEAR FE
***************

eststo clear
eststo preferred: xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year, ///
    fe cluster(prov_id)

esttab preferred using "table3_preferred.rtf", ///
    replace rtf ///
    label ///
    drop(*.year) ///
    title("Table 3. Preferred FE Model with Year Fixed Effects") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nonumber ///
    stats(N r2_within, fmt(0 3) labels("Observations" "Within R-squared"))

***************
* TABLE 4: LAGGED UNION DENSITY MODEL
***************

eststo clear
eststo lagged: xtreg gini_aftertax L.union_density unemployment_rate gdp_real_m population_m education_low i.year, ///
    fe cluster(prov_id)

esttab lagged using "table4_lagged.rtf", ///
    replace rtf ///
    label ///
    drop(*.year) ///
    varlabels(L.union_density "Lagged union density (%)") ///
    title("Table 4. Lagged Union Density Regression") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nonumber ///
    stats(N r2_within, fmt(0 3) labels("Observations" "Within R-squared"))

************************************************************
* ROBUSTNESS CHECKS — EXPORT TABLES A1–A4
************************************************************

label var union_density "Union density (%)"
label var unemployment_rate "Unemployment (%)"
label var gdp_real_m "Real GDP (thousands)"
label var population_m "Population (thousands)"
label var education_low "Low education (%)"
label var gini_aftertax "After-tax Gini"
label var lag_union "Lagged union density (%)"

************************************************************
* TABLE A1 — TIME SPLIT ROBUSTNESS (2000–2010 vs. 2011–2023)
************************************************************
eststo clear

eststo early: xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year ///
    if year <= 2010, fe cluster(prov_id)

eststo late: xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low i.year ///
    if year >= 2011, fe cluster(prov_id)

esttab early late using "Table_A1_TimeSplit.rtf", ///
    replace title("Table A1. Time-Split Robustness (2000–2010 vs. 2011–2023)") ///
    b(3) se(3) label se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N, fmt(%9.0g) labels("Observations")) ///
    nonotes note("Standard errors clustered at the provincial level.")

************************************************************
* TABLE A2 — FIXED EFFECTS STRUCTURE VARIATIONS
************************************************************
eststo clear

eststo provFE: xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low, ///
    fe cluster(prov_id)

eststo yearFE: reg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low ///
    i.year, cluster(prov_id)

eststo pooled: reg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low, ///
    cluster(prov_id)

esttab provFE yearFE pooled using "Table_A2_FE_Variations.rtf", ///
    replace title("Table A2. Fixed-Effects Structure Variations") ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N, fmt(%9.0g) labels("Observations")) ///
    nonotes note("Models vary FE structure: Province FE only, Year FE only, and Pooled OLS.")

************************************************************
* TABLE A3 — PROVINCE EXCLUSION ROBUSTNESS
************************************************************
eststo clear

eststo dropQC: xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low ///
    i.year if province != "QC", fe cluster(prov_id)

eststo dropAB: xtreg gini_aftertax union_density unemployment_rate gdp_real_m population_m education_low ///
    i.year if province != "AB", fe cluster(prov_id)

esttab dropQC dropAB using "Table_A3_ProvinceExclusion.rtf", ///
    replace title("Table A3. Province Exclusion Robustness Tests") ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N, fmt(%9.0g) labels("Observations")) ///
    nonotes note("Models exclude Quebec and Alberta in separate regressions.")

************************************************************
* TABLE A4 — LAGGED UNION DENSITY ROBUSTNESS
************************************************************
eststo clear

eststo lagModel: xtreg gini_aftertax L.union_density unemployment_rate gdp_real_m population_m education_low ///
    i.year, fe cluster(prov_id)

esttab lagModel using "Table_A4_LaggedUnionDensity.rtf", ///
    replace title("Table A4. Lagged Union Density Robustness Test") ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N, fmt(%9.0g) labels("Observations")) ///
    nonotes note("Province and year fixed effects included.")

*****************************************
** END OF FILE ******
*****************************************





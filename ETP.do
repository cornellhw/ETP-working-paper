* Load the dataset ETP_DATA_2.dta
use "ETP_DATA_2.dta", clear

* Parallel Trends Test
* Generate variables for years before and after policy implementation
foreach year_diff in 11 10 9 8 7 6 5 4 3 2 1 0 -1 -2 -3 -4 {
    local year_var "pre`year_diff'"
    if `year_diff' < 0 local year_var "post`=-year_diff'"
    gen `year_var' = 0
    replace `year_var' = 1 if year + `year_diff' == functionpolicy_time
}

* Set panel data structure
xtset id year

* Winsorize the countryC02 variable to reduce extreme values
winsor2 countryC02, cut(91 93)

* Regression analysis and store results
reghdfe countryC02_w industry gdp2 gdp agriculture cityzenmoney publicin publicout hospitals pre9-pre1 current post1-post4, absorb(year county)
est sto reg

* Plot coefficients
coefplot reg, keep(pre* current post*) vertical recast(connect) yline(0) xline(9, lp(dash)) ciopts(lpattern(dash) recast(rcap) msize(medium))

* Benchmark Regression Analysis
* Regression analysis of countryC02 and etp, and output results
reghdfe countryC02 etp, absorb(year county) vce(robust)
outreg2 using bench.doc, replace se bdec(3) sdec(3) adjr2 e(r2) keep(etp) addtext(Year FE, YES, County FE, YES) title(Benchmark regression results)

* Add control variables for regression analysis, and output results
reghdfe countryC02 etp gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households, absorb(year county) vce(robust)
outreg2 using bench.doc, append se bdec(3) sdec(3) adjr2 e(r2) keep(etp gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households) addtext(Year FE, YES, County FE, YES) title(Benchmark regression results)

* Regression analysis with clustered standard errors at the county level, and output results
reghdfe countryC02 etp gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households, absorb(year county) cluster(county)
outreg2 using bench.doc, append se bdec(3) sdec(3) adjr2 e(r2) keep(etp gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households) addtext(Year FE, YES, County FE, YES) title(Benchmark regression results)

* Mechanism Analysis
* Generate interaction terms and perform regression analysis
foreach var in hospitals publicout industry {
    gen t`var' = etp * `var'
    reghdfe countryC02 etp t`var' industry gdp gdp2 agriculture cityzenmoney publicin publicout hospitals, absorb(year county) vce(robust)
    outreg2 using media.doc, append se bdec(3) sdec(3) adjr2 e(r2) keep(etp t`var' industry gdp gdp2 agriculture cityzenmoney publicin publicout hospitals) addtext(Year FE, YES, County FE, YES) title(Benchmark regression results)
}

* Three-Stage Mechanism Analysis
* Perform three-stage regression analysis for hospitals, publicout, industry
foreach var in hospitals publicout industry {
    * First stage
    reghdfe `var' etp gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households, absorb(year county) vce(robust)
    outreg2 using medregression.doc, append se bdec(3) sdec(3) adjr2 e(r2) keep(etp `var' publicout industry) addtext(Year FE, YES, County FE, YES, Control variables, YES) title(Mechanism analysis results)

    * Second stage
    reghdfe countryC02 etp `var' gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households, absorb(year county) vce(robust)
    outreg2 using medregression.doc, append se bdec(3) sdec(3) adjr2 e(r2) keep(etp `var' publicout industry) addtext(Year FE, YES, County FE, YES, Control variables, YES) title(Mechanism analysis results)
}

* Placebo Test
forvalues i = 1/1000 {
    * Generate a random treatment variable
    gen random_esg = runiform()
    sort random_esg
    gen random_id1 = _n

    * Save random treatment data
    preserve
    keep random_id1 etp
    rename etp random_treat
    rename random_id1 id1
    save "random_treat.dta", replace
    restore

    * Merge random treatment data
    drop random_esg random_id1 etp
    rename obs_id1 id1
    save "newdata.dta", replace
    use "newdata.dta", clear
    merge 1:1 id1 using "random_treat.dta", nogen

    * Perform regression analysis
    xtset id year
    reghdfe countryC02 random_treat gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households, absorb(year county) vce(robust)
    gen _b_random_treat = _b[random_treat]
    gen _se_random_treat = _se[random_treat]

    * Save regression results
    keep _b_random_treat _se_random_treat
    duplicates drop _b_random_treat, force
    save "placebo`i'.dta", replace
}

* Combine placebo test results
use "placebo1.dta", clear
forvalues i = 2/1000 {
    append using "placebo`i'.dta"
}
gen tvalue = _b_random_treat / _se_random_treat
kdensity tvalue, xtitle("t value") ytitle("distribution") saving(placebo_test)

* Clean up temporary files
forvalues i = 1/1000 {
    erase "placebo`i'.dta"
}
erase "newdata.dta"
erase "random_treat.dta"

* Robustness Check
* Shorten the sample time range for robustness check
drop if year > 2013
save "ETP_DATA_00-13.dta", replace

use "ETP_DATA_00-13.dta", clear
* Repeat the steps of benchmark regression analysis

* PSM-DID Analysis
* PSM-DID Analysis using ETP_DATA_2.dta
use "ETP_DATA_2.dta", clear

* Set graph output style
graph set window fontface "Times New Roman"
graph set window fontfacesans "宋体"
set scheme s1color

* Define global macros
global xlist "gdp agriculture cityzenmoney publicin welfare households publicout"  
global regopt "absorb(year county) vce(robust)"

* Cross-sectional matching
* Caliper nearest neighbor matching 1:1 without replacement
set seed 0000
gen norvar_1 = rnormal()
sort norvar_1

psmatch2 treat $xlist, outcome(countryC02) logit neighbor(1) noreplacement caliper(0.05)

* P-value calculation
set seed 10101
bootstrap r(att) r(atu) r(ate), reps(50): psmatch2 treat $xlist, outcome(countryC02) ties ate logit common

* Balance test
pstest, both graph saving(balancing_assumption, replace)
eststo
esttab using table.rtf, b(3) se(3) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) nogaps r2 replace

* Common support graph
psgraph, saving(common_support, replace)
graph export "common_support.emf", replace

* Regression results comparison
* PSM-DID1 (Using samples with non-missing weights)
reghdfe countryC02 etp $xlist if _weight != ., $regopt
outreg2 using PSM-DID.doc, replace se bdec(3) sdec(3) adjr2 e(r2) keep(etp $xlist) addtext(Year FE, YES, County FE, YES) title(PSM-DID Regression Results)

* PSM-DID2 (Using samples satisfying common support assumption)
reghdfe countryC02 etp $xlist if _support == 1, $regopt
outreg2 using PSM-DID.doc, append se bdec(3) sdec(3) adjr2 e(r2) keep(etp $xlist) addtext(Year FE, YES, County FE, YES) title(PSM-DID Regression Results)

* PSM-DID3 (Using frequency weights regression)
gen weight = _weight * 2
replace weight = 1 if ets == 1 & _weight != .
gen inweight = round(weight)
drop weight
rename inweight weight
reghdfe countryC02 etp $xlist [fweight = weight], $regopt
outreg2 using PSM-DID.doc, append tstat bdec(3) tdec(3) adjr2 e(N) keep(etp $xlist) addtext(Year FE, YES, County FE, YES) title(PSM-DID Regression Results)


* Heterogeneity Analysis
* Divide regions based on provinces and perform region-wise regression analysis
foreach region in 1 2 3 4 5 6 7 {
    replace region = `region' if province == "北京市" | province == "天津市" | ... (other province conditions)
    reghdfe countryC02 etp gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households if region == `region', absorb(year county) vce(robust)
    outreg2 using heter.doc, append se bdec(3) sdec(3) adjr2 e(r2) keep(etp gdp gdp2 agriculture cityzenmoney publicin welfare sufficiencyrate households) addtext(Year FE, YES, County FE, YES, Region, `region') title(heter regression results)
}

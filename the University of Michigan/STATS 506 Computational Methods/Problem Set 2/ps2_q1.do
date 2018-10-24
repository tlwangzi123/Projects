* Analysis of the RECS 2015 data.
*
* The RECS 2015 Data used in this script can be found at the link below:
*   https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
*
* Author: Zi Wang (tlwangzi@umich.edu)
* Updated: Oct 18, 2018

clear

// Obtain or restore data
import delimited "https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv"
save data1, replace

// Estimate the four national means for residential energy consumption
collapse (mean) kwh cufeetngsph gallonlp gallonfo [pweight = nweight]
rename kwh usage_kwh
rename cufeetngsph usage_cuf
rename gallonlp usage_glp
rename gallonfo usage_gfo
generate w = 1
save summarydata1, replace

// Keep variables which are useful to calculate standard errors
clear
use data1
keep doeid kwh cufeetngsph gallonlp gallonfo brrwt1-brrwt96

// Convert long data to wide data
reshape long brrwt, i(doeid) j (w)

// Calculate standard errors for the four estimates
collapse (mean) kwh cufeetngsph gallonlp gallonfo[pweight=brrwt], by(w)
merge 1:1 w using summarydata1
generate adj_kwh=4*(kwh-usage_kwh[1])^2  // If e = 0.5, 1/(1-e)^2 = 4
generate adj_cuf=4*(cufeetngsph-usage_cuf[1])^2
generate adj_glp=4*(gallonlp-usage_glp[1])^2
generate adj_gfo=4*(gallonfo-usage_gfo[1])^2
collapse (mean) adj_kwh adj_cuf adj_glp adj_gfo
generate w=1

// Merge all of the results into one data 
merge 1:1 w using summarydata1
replace adj_kwh = adj_kwh^0.5
replace adj_cuf = adj_cuf^0.5
replace adj_glp = adj_glp^0.5
replace adj_gfo = adj_gfo^0.5

// Rename variables to show four kinds of residential energy consumption
// Generate their codes to sort their in logical order
generate Kind_kwh ="Electricity usage"
generate code_kwh = 1
generate Kind_cuf ="Natural gas usage"
generate code_cuf = 2
generate Kind_glp ="Propane usage"
generate code_glp = 3
generate Kind_gfo ="Fuel oil or kerosene usage"
generate code_gfo = 4

// Convert wide data to long data
reshape long Kind usage code adj, i(w) j(cat) string

// Generate the final CSV
rename usage Estimate 
rename adj SE
sort code
keep Kind Estimate SE
order Kind Estimate SE
export delimited using "recs2015_usage.csv", replace









preserve

// We will use these variable names repeatedly.
local vars = "kwh cufeetng gallonlp gallonfo"

// keep primary energy use variables
keep doeid nweight `vars'
save recs2015_fuels.dta, replace

// generate contributions to estimate for national total

foreach var in `vars' {
  generate t`var'=`var'*nweight
}

// compute point estimates for national totals
keep doeid t*
collapse (sum) t*

// add a fake variable to merge on later and save
generate fid=0
save recs2015_fuels_petotal.dta, replace

// keep replicate weights and reshape to long
restore
keep doeid brrwt1-brrwt96

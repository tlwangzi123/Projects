* Analysis of the 2005-2006 NHANES ORAL Health data 
* and the NHANES 2005-2006 Demographics Data using logistic regression         
*
* The 2005-2006 NHANES ORAL Health data used in this script
* can be found at the link below:
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT
* The 2005-2006 NHANES Demographics data used in this script
* can be found at the link below:
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT
*
* Author: Zi Wang (tlwangzi@umich.edu)
* Updated: Oct 18, 2018

// Generate a log
log using ps2_q2.log, text replace 	

// Obtain or restore data
clear
import sasxport "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT"
save oral, replace
import sasxport "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT"

/* a. */
// Merge these two data
merge 1:1 seqn using oral
save syn, replace

/* b. */
* Manipulate data
// recode permanent root fragments as permanent and 
// drop individuals for whom this tooth was not assessed
drop if (ohx04htc==9)|(ohx04htc==.)|(ridagemn==.)

// Convert ohx04htc to a two-values variable(represents the pro) to 
// use logitic regression
generate sign= (ohx04htc !=1)

// Collapse ‘Other Hispanic’ and ‘Other’, label the four levels
keep sign ridagemn ridreth1 riagendr indfmpir sdmvpsu wtmec2yr sdmvstra
sort ridagemn
replace ridreth1 = 2 if ridreth1 == 5 
label define ridreth1_codes 1 "Mex" 2 "Other" 3 "White" 4 "Black", replace
label values ridreth1 ridreth1_codes

// Label gender 
label define riagendr_codes 1 "Male" 2 "Female", replace
label values riagendr riagendr_codes
save syn_select, replace

// Logistic regression to estimate the relationship between age (in months) and 
// variable ohx04htc: the probability that an individual has lose the
// upper right 2nd bicuspid
logit sign ridagemn
outreg2 using myreg1.doc, replace ctitle(Model sign~ridagemn)
estat ic
* BIC: 1533.407

* Estimate the ages at which 25% of individuals 
* lose their primary upper right 2nd bicuspid
generate age_25 = round((log(0.25/(1-0.25)) - _b[_cons])/_b[ridagemn])
display age_25

* Estimate the ages at which 50% of individuals 
* lose their primary upper right 2nd bicuspid
generate age_50 = round((log(0.50/(1-0.50)) - _b[_cons])/_b[ridagemn])
display age_50

* Estimate the ages at which 75% of individuals 
* lose their primary upper right 2nd bicuspid
generate age_75 = round((log(0.75/(1-0.75)) - _b[_cons])/_b[ridagemn])
display age_75

* Taking the floor in years of the 25%-ile
generate year_25 = floor(age_25/12)
display year_25

* Taking the celling in years of the 75%-ile
generate year_75 = ceil(age_75/12)
display year_75

/* c. */
// Add gender to the model
logit sign ridagemn i.riagendr
outreg2 using myreg2.doc, replace ctitle(Model ridagemn~riagendr)
estat ic
* BIC: 1542.055

// Use White as the reference, Creat other three indicators
tab ridreth1, gen(dummy_ridreth1)
generate mex = dummy_ridreth11
generate black = dummy_ridreth14
generate other = dummy_ridreth12 

// Add category Mexican American
logit sign ridagemn i.mex // Mex
outreg2 using myreg3.doc, replace ctitle(Model sign~ridagemn+mex)
estat ic
* BIC: 1542.285

// Add category Non-Hispanic Black
logit sign ridagemn i.black  // Black 
outreg2 using myreg4.doc, replace ctitle(Model sign~ridagemn+black)
estat ic
* BIC: 1529.281, retain Black

// Add category Other
logit sign ridagemn i.black i.other  // Other 
outreg2 using myreg5.doc, replace ctitle(Model sign~ridagemn+black+other)
estat ic
* BIC: 1536.103

// Remove NAs in poverty income ratio 
drop if (indfmpir==.)
save syn_select, replace

// Add poverty income ratio to the model
logit sign ridagemn i.black indfmpir
outreg2 using myreg6.doc, replace ctitle(Model sign~ridagemn+black+indfmpir)
estat ic
* BIC: 1462.895, retain PIR

* Final model: sign~ridagemn+black+indfmpir 
logit sign ridagemn i.black indfmpir   
outreg2 using myreg7.doc, replace ctitle(Model sign~ridagemn+black+indfmpir)

/* d. */
// Adjusted Predictions at the mean (for other values) 
// at the representative ages: 8, 9, 10, 11, 12
margins, at(ridagemn=(96 108 120 132 144)) atmeans post
outreg2 using margin1.doc, replace ctitle(Adjusted predictions)
quietly:logit sign ridagemn black indfmpir 
quietly:margins, at(ridagemn=(96 108 120 132 144)) atmeans 
marginsplot

// The marginal effects at the mean of black 
// at the same representative ages
quietly:logit sign ridagemn i.black indfmpir 
margins, dydx(black) at (ridagemn=(96 108 120 132 144)) atmeans post
outreg2 using margin2.doc, replace ctitle(The marginal effects at the mean)
quietly:logit sign ridagemn i.black indfmpir 
quietly:margins, dydx(i.black) at (ridagemn=(96 108 120 132 144)) atmeans
marginsplot

// Average Marginal Effect of ridrethBlack at the representative ages
quietly:logit sign ridagemn i.black indfmpir 
margins, dydx(black) at (ridagemn=(96 108 120 132 144)) post
outreg2 using margin3.doc, replace ctitle(Average marginal effect)
quietly:logit sign ridagemn i.black indfmpir 
quietly:margins, dydx(i.black) at (ridagemn=(96 108 120 132 144))
marginsplot

/* e */
// set up the survey weights
svyset sdmvpsu [pweight=wtmec2yr], strata(sdmvstra) vce(linearized)

// Refit  final model from part c using svy
svy:logit sign ridagemn black indfmpir
outreg2 using myreg7.doc, append ctitle(Model svy:sign~ridagemn+black+indfmpir)

// Comment on differences
log close
exit

## Analysis of the RECS 2015 data.
##
## The RECS 2015 Data used in this script can be found at the link below:
##   https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Nov 19, 2018

# Libraries: ------------------------------------------------------------------
library(data.table)

# Multiplier for confidence level: --------------------------------------------
m = qnorm(.975)

# Obtain or restore data: -----------------------------------------------------
recs = 
  fread('https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv')

# Replicate weights: ----------------------------------------------------------
weight = recs[,c(1,475:571)] 
weights_long = 
  melt(weight[, -c('NWEIGHT'), with = FALSE], id.vars = c('DOEID'), 
       variable.name = 'repl', value.name = 'w')

# Division map: ---------------------------------------------------------------
divisions = c(
  'New England',
  'Middle Atlantic',
  'East North Central',
  'West North Central',
  'South Atlantic',
  'East South Central',
  'West South Central',
  'Mountain North',
  'Mountain South',
  'Pacific'
)

## Part a.
# Point estimate: -------------------------------------------------------------
p_stucco = recs[,.(DOEID, DIVISION, NWEIGHT, WALLTYPE)
               ][,`:=`(division = factor(DIVISION, 1:10, divisions))
               ][,.(p_stucco =  
                      sum( NWEIGHT*100*{WALLTYPE == 4} ) / sum(NWEIGHT)), 
                 by =division]

# Estimates from replicate weights: -------------------------------------------
p_stucco_r = merge(weights_long,
                   recs[,.(DOEID, DIVISION, NWEIGHT, WALLTYPE)
                       ][,`:=`(division = factor(DIVISION, 1:10, divisions))],
                   by = 'DOEID', all =TRUE)
p_stucco_r = p_stucco_r[,.(r_stucco =  sum( w*100*{WALLTYPE == 4} ) / sum(w)), 
                        by = .(division, repl)]

# Compute standard errors: ----------------------------------------------------
p_stucco_r = merge(p_stucco_r, p_stucco, by = 'division')
p_stucco = p_stucco_r[,.(p_stucco = p_stucco[1],
                         se_stucco = 2*sqrt( mean( {r_stucco - p_stucco}^2))),
                      by = division] 
p_stucco = p_stucco[,`:=`(lwr = pmax(p_stucco - m*se_stucco, 0), 
                          upr = p_stucco + m*se_stucco)
                   ][,`:=`(ci = sprintf('%4.1f (%4.1f, %4.1f)', p_stucco,
                                        lwr,upr))][]

## Part b.
### Average total electricity usage in kilowatt hours in each division
# Point estimate: -------------------------------------------------------------
kwh = recs[,.(DOEID, DIVISION, NWEIGHT, KWH)
          ][,`:=`(division = factor(DIVISION, 1:10, divisions))
          ][,.(kwh =  sum( NWEIGHT*KWH ) / sum(NWEIGHT)), by = division]

# Estimates from replicate weights: -------------------------------------------
kwh_r = merge(weights_long,
              recs[,.(DOEID, DIVISION, NWEIGHT, KWH)
                  ][,`:=`(division = factor(DIVISION, 1:10, divisions))],
              by = 'DOEID', all =TRUE)
kwh_r = kwh_r[,.(r_kwh =  sum( w*KWH ) / sum(w)), 
              by = .(division, repl)]

# Compute standard errors: ----------------------------------------------------
kwh_r = merge(kwh_r, kwh, by = 'division')
kwh = kwh_r[,.(kwh = kwh[1],
            se_kwh = 2*sqrt( mean( {r_kwh - kwh}^2))), 
            by = division] 
kwh = kwh[,`:=`(lwr = pmax(kwh - m*se_kwh, 0), 
          upr = kwh + m*se_kwh)
         ][,`:=`(ci = sprintf('%4.1f (%4.1f, %4.1f)', kwh,
                              lwr, upr))][]

### Average total electricity usage in kilowatt hours in each division
### stratified by urban and rural status
# Point estimate: -------------------------------------------------------------
kwh_div = recs[,.(DOEID, DIVISION, NWEIGHT, KWH, UATYP10)
              ][,`:=`(division = factor(DIVISION, 1:10, divisions),
                      urban = UATYP10 %in% c('U', 'C'))
              ][,.(kwh_div =  sum( NWEIGHT*KWH ) / sum(NWEIGHT)), 
                by = .(division, urban)]

# Estimates from replicate weights: -------------------------------------------
kwh_div_r = merge(weights_long,
                  recs[,.(DOEID, DIVISION, NWEIGHT, KWH, UATYP10)
                      ][,`:=`(division = factor(DIVISION, 1:10, divisions),
                              urban = UATYP10 %in% c('U', 'C'))],
                  by = 'DOEID', all =TRUE)
kwh_div_r = kwh_div_r[,.(r_kwh_div =  sum( w*KWH ) / sum(w)), 
                      by = .(division, urban, repl)]

# Compute standard errors: ----------------------------------------------------
kwh_div_r = merge(kwh_div_r, kwh_div, by = c('division','urban'))
kwh_div = kwh_div_r[,.(kwh_div = kwh_div[1],
                       se_kwh_div = 2*sqrt( mean({r_kwh_div - kwh_div}^2))), 
                    by = .(division, urban)]
kwh_div = kwh_div[,`:=`(lwr = pmax(kwh_div - m*se_kwh_div, 0), 
                        upr = kwh_div + m*se_kwh_div)
                 ][,`:=`(ci = sprintf('%4.1f (%4.1f, %4.1f)', kwh_div,
                              lwr, upr))][]


## Part c.
### The proportion of homes with internet access urban and rural areas: -------
int_div = recs[,.(DOEID, DIVISION, NWEIGHT, INTERNET, UATYP10)
              ][,`:=`(division = factor(DIVISION, 1:10, divisions),
                      urban = UATYP10 %in% c('U', 'C'))
              ][,.(int_div =  sum( NWEIGHT*100*INTERNET ) / sum(NWEIGHT)), 
                by = .(division, urban)]

# Estimates from replicate weights: -------------------------------------------
int_div_r = merge(weights_long,
                  recs[,.(DOEID, DIVISION, NWEIGHT, INTERNET, UATYP10)
                      ][,`:=`(division = factor(DIVISION, 1:10, divisions),
                              urban = UATYP10 %in% c('U', 'C'))],
                  by = 'DOEID', all =TRUE)
int_div_r = int_div_r[,.(r_int_div =  sum( w*100*INTERNET ) / sum(w)), 
                      by = .(division, urban, repl)]

# Compute standard errors: ----------------------------------------------------
int_div_r = merge(int_div_r, int_div, by = c('division','urban'))
int_div = int_div_r[,.(int_div = int_div[1],
                       se_int_div = 2*sqrt( mean({r_int_div - int_div}^2))), 
                    by = .(division, urban)]
int_div = int_div[,`:=`(lwr = pmax(int_div - m*se_int_div, 0), 
                        upr = int_div + m*se_int_div)
                 ][,`:=`(ci = sprintf('%4.1f (%4.1f, %4.1f)', int_div,
                              lwr, upr))][]

# Point estimate for difference: ---------------------------------------------
int = recs[,.(DOEID, w= NWEIGHT, x = 100*INTERNET,
              division = factor(DIVISION, 1:10, divisions),
              urban = UATYP10 %in% c('U','C'))]
pe = int[,.(est = sum(w*x) / sum(w)), by = .(division, urban)]
pe = dcast(pe, division ~ urban, value.var = "est")
pe = pe[,`:=`(est = `TRUE` - `FALSE`)][]

# Replicate estimates for difference: ----------------------------------------
pe_r = int[, -c('w'), with = FALSE]
pe_r = merge(pe_r, weights_long, by = 'DOEID')
pe_r = pe_r[,.(r = sum(w*x)/sum(w)), by = .(division, urban, repl)]
pe_r = dcast(pe_r, division + repl ~ urban, value.var = "r")
pe_r = pe_r[,`:=`(r = `TRUE` - `FALSE`)][]

# Std error and confidence interval for differnce: ---------------------------
int_disp = merge(pe_r, pe, by = 'division')
int_disp = int_disp[,.(est = est[1],
                       se = 2*sqrt(mean((r-est)^2))),
                    by = division
                   ][,`:=`(ci = sprintf('%4.1f%%(%4.1f,%4.1f)',
                                est, est- m*se, est + m*se))][]

# Join urban & rural estimates to differences: -------------------------------
int_div1 = int_div[,.(division, urban, ci)]
int_div1 = dcast(int_div1, division ~ urban, value.var = "ci")
int_disp = merge(int_disp, int_div1, by = 'division')
int_disp = int_disp[,.(division, est, se, 
                       Rural = `FALSE`, Urban = `TRUE`, Diff = ci)]


## Part D.
### Question: 
### What percent of homes have rented(KOWNRENT == 2) within each division? 
### Which division has the highest proportion? Which has the lowest?
# Point estimate: -------------------------------------------------------------
p_rent = recs[,.(DOEID, DIVISION, NWEIGHT, KOWNRENT)
             ][,`:=`(division = factor(DIVISION, 1:10, divisions))
             ][,.(p_rent =  sum( NWEIGHT*100*{KOWNRENT == 2} ) / sum(NWEIGHT)), 
               by =division]

# Estimates from replicate weights: -------------------------------------------
p_rent_r = merge(weights_long,
                 recs[,.(DOEID, DIVISION, NWEIGHT, KOWNRENT)
                     ][,`:=`(division = factor(DIVISION, 1:10, divisions))],
                 by = 'DOEID', all =TRUE)
p_rent_r = p_rent_r[,.(r_rent =  sum( w*100*{KOWNRENT == 2} ) / sum(w)), 
                    by = .(division, repl)]

# Compute standard errors: ----------------------------------------------------
p_rent_r = merge(p_rent_r, p_rent, by = 'division')
p_rent = p_rent_r[,.(p_rent = p_rent[1],
                     se_rent = 2*sqrt( mean( {r_rent - p_rent}^2))),
                  by = division] 
p_rent = p_rent[,`:=`(lwr = pmax(p_rent - m*se_rent, 0), 
                      upr = p_rent + m*se_rent)
               ][,`:=`(ci = sprintf('%4.1f (%4.1f, %4.1f)', p_rent,
                            lwr,upr))][]
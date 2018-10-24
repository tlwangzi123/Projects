## Analysis of the RECS 2015 data.
##
## The RECS 2015 Data used in this script can be found at the link below:
##   https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Oct 02, 2018

# Libraries: ------------------------------------------------------------------
library(tidyverse)

ps1_q3 <- function(){
# Obtain or restore data: -----------------------------------------------------
recs_tib = readr::read_delim(
  "https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv",
  delim = ",")

# Convert weights to long: ----------------------------------------------------
weights_long = recs_tib %>% select(DOEID,NWEIGHT,BRRWT1:BRRWT96) %>%
  gather(key = 'repl',value = 'w',BRRWT1:BRRWT96 )

## a.
# Give the estimates of percent of homes have stucco construction  
# as the major outside wall material within each division: --------------------
recs_wall_prop = recs_tib %>% group_by(DIVISION) %>% 
  mutate(total = sum(NWEIGHT)) %>% 
  filter(WALLTYPE == 4) %>%
  mutate(count = sum(NWEIGHT),per = count/total*100) %>% 
  distinct(DIVISION,per) %>% 
  arrange(DIVISION)

# Select DOEID, DIVISION, WALLTYPE in recs_tib: -------------------------------
recs_wall = recs_tib %>% 
  group_by(DIVISION) %>% 
  select(DOEID,DIVISION,WALLTYPE)

# Join DIVISION, WALLTYPE  to weights: ---------------------------------------- 
recs_wall_rep = weights_long %>%
  left_join(recs_wall %>% mutate(DOEID = as.integer(DOEID)),by='DOEID')

# Compute the percent in each division in each replicate subsample: ----------- 
recs_wall_rep_prop = recs_wall_rep %>% 
  group_by(DIVISION,repl) %>%
  mutate(total = sum(w)) %>% 
  filter(WALLTYPE == 4) %>% 
  mutate(count = sum(w), per_r = count/total*100) %>%
  select(DIVISION,repl,per_r) %>% 
  distinct(DIVISION,repl,per_r) %>% 
  arrange(DIVISION,repl)

# Join the estimate of the percent in each division to recs_wall_rep_prop: ---- 
recs_wall_rep_prop = recs_wall_rep_prop %>%
  ungroup %>%
  left_join(recs_wall_prop,by='DIVISION')

# Compute standard errors for the estimate of percent in each division: -------
R = 96
e = 0.5
recs_wall_summary = recs_wall_rep_prop %>%
  group_by(DIVISION) %>%
  mutate(std_err = sqrt(sum((per-per_r)^2)/(R*((1-e)^2)))) %>%
  distinct(DIVISION,per,std_err)

# Compute the position of the highest proportion and the lowest proportion: ---
recs_wall_max = recs_wall_summary[which.max(recs_wall_summary$per),1]
recs_wall_min = recs_wall_summary[which.min(recs_wall_summary$per),1]


## b.
# Give estimates of average total electricity usage in kilowatt hours  
# in each division: -----------------------------------------------------------
recs_elec_estimate = recs_tib %>% 
  group_by(DIVISION) %>% 
  mutate(count = sum(NWEIGHT),total_elec = sum(KWH*NWEIGHT),
         aver = total_elec/count) %>% 
  distinct(DIVISION,aver) %>% 
  arrange(DIVISION)

# Select DOEID, DIVISION, KWH in recs_tib: ------------------------------------
recs_elec = recs_tib %>% 
  group_by(DIVISION) %>% 
  select(DOEID,DIVISION,KWH)

# Join DIVISION, KWH to weights: ---------------------------------------------- 
recs_elec_rep = weights_long %>%
  left_join(recs_elec %>% mutate(DOEID = as.integer(DOEID)),by='DOEID')

# Compute the average electricity usage in each division 
# in each replicate subsample: ------------------------------------------------
recs_elec_rep_prop = recs_elec_rep %>% 
  group_by(DIVISION,repl) %>%
  mutate(total_elec_r = sum(w*KWH),aver_r = total_elec_r/sum(w)) %>% 
  distinct(DIVISION,repl,aver_r) %>% 
  arrange(DIVISION,repl)

# Join the estimate of the average electricity usage in each division
# to recs_elec_rep_prop: ------------------------------------------------------
recs_elec_rep_prop = recs_elec_rep_prop %>%
  ungroup %>%
  left_join(recs_elec_estimate,by = 'DIVISION')

# Compute standard errors for the estimate of average electricity usage 
# in each division: -----------------------------------------------------------
R = 96
e = 0.5
recs_elec_summary = recs_elec_rep_prop %>%
  group_by(DIVISION) %>%
  mutate(std_err = sqrt(sum((aver-aver_r)^2)/(R*((1-e)^2)))) %>%
  distinct(DIVISION,aver,std_err)

## Give estimates of average total electricity usage in kilowatt hours  
## in each division startified by urban and rural status: ---------------------
recs_elec_stra_estimate = recs_tib %>% 
  group_by(DIVISION,UATYP10) %>% 
  mutate(count = sum(NWEIGHT),total_elec = sum(KWH*NWEIGHT),
         aver = total_elec/count) %>% 
  distinct(DIVISION,UATYP10,aver) %>% 
  arrange(DIVISION,UATYP10)
recs_elec_stra_estimate

# Select DOEID, DIVISION, UATYP10, KWH in recs_tib: ---------------------------
recs_elec = recs_tib %>% 
  group_by(DIVISION) %>% 
  select(DOEID,DIVISION,UATYP10,KWH)

# Join DIVISION, UATYP10, KWH to weights: -------------------------------------
recs_elec_rep = weights_long %>%
  left_join(recs_elec %>% mutate(DOEID = as.integer(DOEID)),by='DOEID')

# Compute the average electricity usage in each division 
# startified by urban and rural status in the each replicate subsample: -------
recs_elec_rep_prop=recs_elec_rep %>% 
  group_by(DIVISION,repl,UATYP10) %>%
  mutate(total_elec_r = sum(w*KWH),aver_r = total_elec_r/sum(w)) %>% 
  distinct(DIVISION,repl,UATYP10,aver_r) %>% 
  arrange(DIVISION,repl,UATYP10)

# Join the estimate of the the average electricity usage in each division 
# startified by urban and rural status to recs_elec_rep_prop: -----------------
recs_elec_rep_prop = recs_elec_rep_prop %>%
  ungroup %>%
  left_join(recs_elec_stra_estimate,by = c('DIVISION','UATYP10'))

# Compute standard errors for the estimate of average electricity usage 
# in each division startified by urban and rural status: ----------------------
R = 96
e = 0.5
recs_elec_summary_stra = recs_elec_rep_prop %>%
  group_by(DIVISION,UATYP10) %>%
  mutate(std_err = sqrt(sum((aver-aver_r)^2)/(R*((1-e)^2)))) %>%
  distinct(DIVISION,UATYP10,aver,std_err)


## c.
# Give estimates of the proportion of homes with internet access 
# in urban areas: ------------------------------------------------------------- 
recs_int_dis_U = recs_tib %>% 
  group_by(DIVISION,UATYP10) %>% 
  filter(UATYP10 == "U") %>%
  mutate(count = sum(NWEIGHT),total_int = sum(INTERNET*NWEIGHT),
         perU = total_int/count) %>%
  ungroup %>%
  distinct(DIVISION,perU) %>% 
  arrange(DIVISION)

# Select DOEID, DIVISION, UATYP10, INTERNET in recs_tib: ----------------------
recs_int = recs_tib %>% 
  group_by(DIVISION) %>% 
  select(DOEID,DIVISION,UATYP10,INTERNET)

# Join DIVISION, UATYP10, INTERNET to weights: --------------------------------
recs_int_rep = weights_long %>%
  left_join(recs_int %>% mutate(DOEID = as.integer(DOEID)),by = 'DOEID')

# Compute the proportion of homes with internet access in urban areas
# in each replicate subsample: ------------------------------------------------
recs_int_rep_prop=recs_int_rep %>% 
  group_by(DIVISION,repl,UATYP10) %>%
  filter(UATYP10 == "U") %>%
  mutate(total_int_r = sum(w*INTERNET),perU_r = total_int_r/sum(w)) %>% 
  distinct(DIVISION,repl,UATYP10,perU_r) %>% 
  arrange(DIVISION,repl,UATYP10)

# Join the estimate of the proportion of homes with internet access 
# in urban areas to recs_int_rep_prop: ----------------------------------------
recs_int_rep_prop = recs_int_rep_prop %>%
  ungroup %>%
  left_join(recs_int_dis_U,by = c('DIVISION'))

# Compute standard errors for the estimate of the proportion of homes 
# with internet access in urban areas: ----------------------------------------
R = 96
e = 0.5
recs_int_summary_U = recs_int_rep_prop %>%
  group_by(DIVISION) %>%
  mutate(std_err = sqrt(sum((perU-perU_r)^2)/(R*((1-e)^2)))) %>%
  transmute(perU = perU*100,std_err = std_err*100) %>%
  distinct(DIVISION,perU,std_err)

# Give estimates of the proportion of homes with internet access 
# in rural areas: -------------------------------------------------------------
recs_int_dis_R = recs_tib %>% 
  group_by(DIVISION,UATYP10) %>% 
  filter(UATYP10 == "R") %>%
  mutate(count = sum(NWEIGHT),total_int = sum(INTERNET*NWEIGHT),
         perR = total_int/count) %>%
  ungroup %>%
  distinct(DIVISION,perR) %>% 
  arrange(DIVISION)

# Select DOEID, DIVISION, UATYP10, INTERNET in recs_tib: ----------------------
recs_int = recs_tib %>% 
  group_by(DIVISION) %>% 
  select(DOEID,DIVISION,UATYP10,INTERNET)

# Join DIVISION, UATYP10, INTERNET to weights: --------------------------------
recs_int_rep = weights_long %>%
  left_join(recs_int %>% mutate(DOEID = as.integer(DOEID)),by = 'DOEID')

# Compute the proportion of homes with internet access in rural areas
# in each replicate subsample: ------------------------------------------------
recs_int_rep_prop = recs_int_rep %>% 
  group_by(DIVISION,repl,UATYP10) %>%
  filter(UATYP10 == "R") %>%
  mutate(total_int_r = sum(w*INTERNET),perR_r = total_int_r/sum(w)) %>% 
  distinct(DIVISION,repl,UATYP10,perR_r) %>% 
  arrange(DIVISION,repl,UATYP10)

# Join the estimate of the proportion of homes with internet access 
# in rural areas to recs_int_rep_prop: ----------------------------------------
recs_int_rep_prop = recs_int_rep_prop %>%
  ungroup %>%
  left_join(recs_int_dis_R,by = c('DIVISION'))

# Compute standard errors for the estimate of the proportion of homes 
# with internet access in rural areas: ----------------------------------------
R = 96
e = 0.5
recs_int_summary_R = recs_int_rep_prop %>%
  group_by(DIVISION) %>%
  mutate(std_err = sqrt(sum((perR-perR_r)^2)/(R*((1-e)^2)))) %>%
  transmute(perR = perR*100,std_err = std_err*100) %>%
  distinct(DIVISION,perR,std_err)

# Compute disparity between urban and rural areas 
# in terms of the proportion of homes with internet access: ------------------- 
diff = abs(recs_int_summary_R$perR-recs_int_summary_U$perU)

# find the division which has the largest disparity: --------------------------
rec_int_diff_max = which.max(diff)

# import the results of question 3: -------------------------------------------
q3 = list(recs_wall_summary,recs_wall_max,recs_wall_min,recs_elec_summary,
          recs_elec_summary_stra,recs_int_summary_U,recs_int_summary_R,
          rec_int_diff_max)
q3
}
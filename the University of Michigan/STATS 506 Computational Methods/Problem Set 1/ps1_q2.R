## Analysis of data about flights originating in New York City,
## NY (NYC) in 2013 and 2014.
##
## Data about flights originating in New York City, NY (NYC) in 2013 
## used in this script can be found in the nycflights13 R package. 
## Data about flights originating in New York City, NY (NYC) in 2014 
## used in this script can be found at the link below: 
##   https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Oct 02, 2018

# Libraries: ------------------------------------------------------------------
library(nycflights13)
library(tidyverse)

ps1_q2 <- function(){
# Obtain or restore data: -----------------------------------------------------
data(airlines)
data(airports)
data(flights)
nyc_2014 = readr::read_delim(
  "https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv",
           delim = ",")

## a.
# Compute carrier code for airlines that were responsible for at least 
# 1% of the flights departing any of the three NYC airports 
# between January 1 and October 31, 2013: -------------------------------------
air_code = flights %>% 
  filter(month <= 10) %>% 
  mutate(tol = n()) %>% 
  group_by(carrier) %>% 
  mutate(count = n()) %>% 
  filter(count >= 0.01*tol) %>% 
  distinct(carrier) 

# Convert the carrier code in the results to the airline name: ----------------
air_name = airlines %>% 
  filter(carrier %in% air_code$carrier) %>% 
  arrange(carrier) %>% 
  select(name)


## b.
# Compute the number and percent of annual flights among the airlines 
# from part ¡°a¡± (shown as carrier code) in the first 10 months of 2013, 
# also give the upper bond and the lower bond 
# for each 95% CI of each percent: --------------------------------------------
num_2013 = flights %>%
  filter(month <= 10) %>% 
  mutate(total = n()) %>% 
  group_by(carrier) %>% 
  filter(carrier %in% air_code$carrier) %>%
  mutate(count = n(),per = count/total,pct = per*100,
         std_err = sqrt(per*(1-per)/total),
         lwr = (per-qnorm(.975)*std_err)*100,
         upr = (per+qnorm(.975)*std_err)*100) %>% 
  select(carrier,count,pct,lwr,upr) %>%
  distinct(carrier,count,pct,lwr,upr) %>% 
  arrange(carrier)

# Convert the carrier code in the results to the airline name: ----------------
for (i in 1:dim(num_2013)[1])
{
  for (j in 1:dim(airlines)[1])
    if (airlines$carrier[j] == num_2013$carrier[i]) num_2013$carrier[i] = airlines$name[j]
}

# Compute the number and percent of annual flights among the airlines 
# from part ¡°a¡± (shown as carrier code) in the first 10 months of 2014, 
# also give the upper bond and the lower bond 
# for each 95% CI of each percent: --------------------------------------------
num_2014 = nyc_2014 %>% 
  filter(month <= 10) %>% 
  mutate(total = n()) %>% 
  group_by(carrier) %>% 
  filter(carrier %in% air_code$carrier) %>%
  mutate(count_2014 = n(),per = count_2014/total,pct_2014 = per*100,
         std_err = sqrt(per*(1-per)/total),
         lwr_2014 = (per-qnorm(.975)*std_err)*100,
         upr_2014 = (per+qnorm(.975)*std_err)*100) %>% 
  select(carrier,count_2014,pct_2014,lwr_2014,upr_2014) %>%
  distinct(carrier,count_2014,pct_2014,lwr_2014,upr_2014) %>% 
  arrange(carrier)

# Convert the carrier code in the results to the airline name: ----------------
for (i in 1:dim(num_2014)[1])
{
  for (j in 1:dim(airlines)[1])
    if (airlines$carrier[j] == num_2014$carrier[i]) 
      num_2014$carrier[i] = airlines$name[j]
}

# Notice that "Endeavor Air Inc." has no flights in 2014,add a row in num_2014- 
num_2014_EA = num_2013 %>%
  filter(carrier == "Endeavor Air Inc.") %>%
  mutate(pct_2014 = pct-pct, lwr_2014 = pct_2014, upr_2014 = pct_2014,
         count_2014 = pct_2014)
num_2014_EA = num_2014_EA %>% 
  select(carrier,pct_2014,lwr_2014,upr_2014,count_2014)
num_2014 = rbind(num_2014,num_2014_EA)
  
# Compute the change in percent between 2013 and 2014 with 95% CI :------------
num_diff = num_2014 %>%
  left_join(num_2013,by='carrier')
num_diff = num_diff %>% 
  mutate(diff = pct_2014-pct,
        lwr_diff = diff-sqrt((pct_2014-lwr_2014)^2+(pct-lwr)^2),
        upr_diff = diff+sqrt((pct_2014-lwr_2014)^2+(pct-lwr)^2),
        change = count_2014-count) %>% 
  select(carrier,diff,lwr_diff,upr_diff,change)

# Convert the carrier code in the results to the airline name: ----------------
for (i in 1:dim(num_diff)[1])
{
  for (j in 1:dim(airlines)[1])
    if (airlines$carrier[j] == num_diff$carrier[i]) 
      num_diff$carrier[i] = airlines$name[j]
}

# Compute the airlines showed the largest increase and decrease: --------------
num_diff_max = num_diff[which.max(num_diff$change),1]
num_diff_min = num_diff[which.min(num_diff$change),1]

# Format the number of annual flights and its change: ------------------------- 
num_2013 = num_2013 %>% 
  transmute(count=format(count),pct,lwr,upr)
num_2014 = num_2014 %>% 
  transmute(count=format(count_2014),pct_2014,lwr_2014,upr_2014)
num_diff = num_diff %>% 
  transmute(change=format(change),diff,lwr_diff,upr_diff)

# The total num of flights in the first 10 months of 2014 
# is less than that of 2013.: -------------------------------------------------


## c.
# Collect all combination of airlines and airports together: ------------------
fil_sel = flights %>% select(carrier,origin)
nyc_sel = nyc_2014 %>% select(carrier,origin)
fil_tol = rbind(fil_sel,nyc_sel)

# Compute the percent of flights each airline 
# (shown as carrier code) is responsible for among of the three NYC airports,
# also give the upper bond and the lower bond 
# for each 95% CI of each percent: --------------------------------------------
per_tol = fil_tol %>%
  filter((origin == "JFK")|(origin == "EWR")|(origin == "LGA")) %>%
  group_by(origin) %>%
  mutate(total = n()) %>% 
  group_by(origin, carrier) %>% 
  filter(carrier %in% air_code$carrier) %>%
  mutate(count = n(),per = count/total,pct = per*100,std_err = sqrt(per*(1-per)/total),
         lwr = (per-qnorm(.975)*std_err)*100,upr = (per+qnorm(.975)*std_err)*100) %>% 
  select(carrier,origin,pct,lwr,upr) %>%
  distinct(carrier,origin,pct,lwr,upr) %>% 
  arrange(carrier)

# Convert the carrier code in the results to the airline name: ----------------
for (i in 1:dim(per_tol)[1])
{
  for (j in 1:dim(airlines)[1])
    if (airlines$carrier[j] == per_tol$carrier[i]) 
      per_tol$carrier[i] = airlines$name[j]
} 

# Find the airline which is the largest carrier at JFK airport: ---------------
per_tol_JFK = per_tol %>%
  filter(origin == "JFK") %>%
  select(carrier,origin,pct)
per_tol_JFK_max = per_tol_JFK[which.max(per_tol_JFK$pct),1]

# Find the airline which is the largest carrier at EWR airport: ---------------
per_tol_EWR = per_tol %>%
  filter(origin == "EWR") %>%
  select(carrier,origin,pct)
per_tol_EWR_max = per_tol_EWR[which.max(per_tol_EWR$pct),1]

# Find the airline which is the largest carrier at LGA airport: ---------------
per_tol_LGA = per_tol %>%
  filter(origin == "LGA") %>%
  select(carrier,origin,pct)
per_tol_LGA_max = per_tol_LGA[which.max(per_tol_LGA$pct),1]

# import the results of question 2: -------------------------------------------
q2 = list(air_name,num_2013,num_2014,num_diff,num_diff_max,num_diff_min,
          per_tol,per_tol_JFK_max,per_tol_EWR_max,per_tol_LGA_max)
q2
}
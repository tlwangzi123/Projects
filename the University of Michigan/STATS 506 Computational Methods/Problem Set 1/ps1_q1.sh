#!/bin/bash

## Analysis of the RECS 2015 data.
##
## The RECS 2015 Data used in this script can be found at the link below:
##   https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Oct 02, 2018

## Part A
### i.
cat ./recs2015_public_v3.csv|cut -b 10|grep ^3|wc -l

### ii.
cat ./recs2015_public_v3.csv|cut -d , -f 1,479-575 >recs_comp.txt|gzip recs_comp.txt


## Part B
### i.
for i in {1..4}
do
cat ./recs2015_public_v3.csv|cut -b 10|grep ^$i | wc -l
done

###ii.
cat ./recs2015_public_v3.csv |cut -d , -f 2,3 |tail -n +2 |sort -n -k 4 -t'"'|uniq   > region_division.txt
cat region_division.txt

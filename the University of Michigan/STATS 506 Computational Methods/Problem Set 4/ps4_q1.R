## Use SQL to construct a table from the Lahman baseball data.
##
## The Lahman baseball script used in this script 
## can be found at the Lahman package
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Dec 10, 2018

# Libraries: ------------------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(Lahman)
library(RSQLite)

# Empty the global environment
rm(list = ls())

# Create a local SQLlite database of the Lahman data
lahman = lahman_sqlite()

# main code
result = lahman %>%
  tbl(sql('
          SELECT Player, Debut, `Country of Birth`, Hits
          FROM(
            SELECT m.playerID, debut Debut, m.nameFirst ||" "|| m.nameLast as Player, b.Hits as Hits, m.birthCountry as `Country of Birth`
            FROM master m
            LEFT JOIN 
              (SELECT playerID, sum(H) Hits 
               FROM batting 
               GROUP BY playerID
              ) b
              ON b.playerID == m.playerID
            GROUP BY birthCountry
            HAVING Hits == max(Hits) AND Hits >= 200 
            ORDER BY -Hits)
          ')) %>% collect()
result

## Translate an analysis from a script written in Stata in various options.
## The Stata script can be found at the link below:
##   https://github.com/jbhender/Stats506_F18/tree/master/solutions/PS3
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Nov 19, 2018

# Libraries: ------------------------------------------------------------------
library(data.table)
library(dplyr)
data(mtcars)
fwrite(mtcars, file = 'mtcars.csv')

## a.
# Import data and select key items
df = fread('mtcars.csv')
df = df[,.(mpg,cyl,disp,hp,wt)]

# Center disp, hp, and wt within cyl and then computer regression coefficients.
beta_cyl = df[order(cyl)
             ][,`:=`(disp_gc = disp - mean(disp),
                     hp_gc = hp - mean(hp),
                     wt_gc = wt - mean(wt)), by = cyl
             ][,.(dispXmpg = sum(mpg*disp_gc), disp_sq = sum(disp_gc*disp_gc),
                  hpXmpg = sum(mpg*hp_gc), hp_sq = sum(hp_gc*hp_gc),
                  wtXmpg = sum(mpg*wt_gc), wt_sq = sum(wt_gc*wt_gc)), by = cyl
             ][,`:=`(beta_cyl_disp = dispXmpg / (disp_sq),
                     beta_cyl_hp = hpXmpg / (hp_sq),
                     beta_cyl_wt = wtXmpg / (wt_sq))
             ][,.(cyl, beta_cyl_disp, beta_cyl_hp, beta_cyl_wt)]
beta_cyl

# Export data
fwrite(beta_cyl,file='mpg_betas_by_cyl.csv')


## b. 
compute_reg_coef = function(df, response, predictor, group){
beta_cyl = df[,.(get(response),get(predictor),get(group))
             ][,.(response = V1, predictor = V2, group = V3)
             ][order(group)
             ][,`:=`(predictor_gc = predictor - mean(predictor)), by = group
             ][,.(preXres = sum(response * predictor_gc),
                  predictor_sq = sum(predictor_gc * predictor_gc)), by = group
             ][,.(group, beta_group_predictor = preXres / predictor_sq)]
beta_cyl
}

## Test the function
# beta_cyl_disp
beta1 = compute_reg_coef(df, "mpg", "disp", "cyl")

# beta_cyl_hp
beta2 = compute_reg_coef(df, "mpg", "hp", "cyl")

# beta_cyl_wt
beta3 = compute_reg_coef(df, "mpg", "wt", "cyl")

# Compare the difference between results in (a)
c(max(beta1[,2] - beta_cyl[,2]),
  max(beta2[,2] - beta_cyl[,3]),
  max(beta3[,2] - beta_cyl[,4]))


## c.
# Center disp, hp, and wt within cyl and then computer regression coefficients.
beta_cyl_dplyr = df %>%
  group_by(cyl) %>%
  mutate(disp_gc = disp - mean(disp),
         hp_gc = hp - mean(hp),
         wt_gc = wt - mean(wt),
         dispXmpg = mpg*disp_gc, disp_sq = disp_gc*disp_gc,
         hpXmpg = mpg*hp_gc, hp_sq = hp_gc*hp_gc,
         wtXmpg = mpg*wt_gc, wt_sq = wt_gc*wt_gc) %>%
  summarize_at(vars(dispXmpg:wt_sq), sum) %>%
  mutate(beta_cyl_disp = dispXmpg / (disp_sq),
         beta_cyl_hp = hpXmpg / (hp_sq),
         beta_cyl_wt = wtXmpg / (wt_sq)) %>%
  select(cyl, beta_cyl_disp, beta_cyl_hp, beta_cyl_wt)


## d.
compute_reg_coef_dplyr = function(df, response, predictor, group){
  # quote the variable with quo(), then unquote it in the dplyr call with !!
  response = enquo(response)
  predictor = enquo(predictor)
  group = enquo(group)
  
  # Use quo_name() to convert the input expression to a string,
  #   so we create the new names by pasting together strings
  predictor_gc = paste0(quo_name(predictor),"_gc")
  preXres = paste0(quo_name(predictor), "X", quo_name(response))
  predictor_sq = paste0(quo_name(predictor),"_sq")
  beta_group_predictor = 
    paste0("beta_",quo_name(group),"_",quo_name(predictor))
  beta_cyl = df %>%
    group_by(!!group) %>%
    
    # !! mean_name = mean(!! expr) is not valid R code, 
    #   so we need to use the := helper 
    # Use quo_name() to convert the input expression to a string,
    #   and then use .data[[]] to use the column we want
    mutate(!!predictor_gc := !!predictor - mean(!!predictor),
           !!preXres := sum(!!response * .data[[quo_name(predictor_gc)]]),
           !!predictor_sq := sum(.data[[quo_name(predictor_gc)]] *
                                 .data[[quo_name(predictor_gc)]])) %>%
    mutate(!!beta_group_predictor := .data[[quo_name(preXres)]] / 
                                     .data[[quo_name(predictor_sq)]]) %>%
    arrange(!!group) %>%
    ungroup %>%
    distinct(!!group,.data[[!!beta_group_predictor]])
  beta_cyl
}

## Test the function
# beta_cyl_disp
beta1_dplyr = compute_reg_coef_dplyr(df, mpg, disp, cyl)

# beta_cyl_hp
beta2_dplyr = compute_reg_coef_dplyr(df, mpg, hp, cyl)

# beta_cyl_wt
beta3_dplyr = compute_reg_coef_dplyr(df, mpg, wt, cyl)

# Compare the difference between results in (a)
c(max(beta1_dplyr$beta_cyl_disp - beta_cyl[,2]),
  max(beta2_dplyr$beta_cyl_hp - beta_cyl[,3]),
  max(beta3_dplyr$beta_cyl_wt - beta_cyl[,4]))

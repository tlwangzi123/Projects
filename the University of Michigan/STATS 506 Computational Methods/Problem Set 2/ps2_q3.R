## Analysis of the 2005-2006 NHANES ORAL Health data 
## and the NHANES 2005-2006 Demographics Data using logistic regression
##
## The 2005-2006 NHANES ORAL Health data used in this script
## can be found at the link below:
## https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT
## The 2005-2006 NHANES Demographics data used in this script
## can be found at the link below:
## https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Oct 18, 2018
# Libraries: ------------------------------------------------------------------
library(margins)
library(tidyverse)
library(prediction)
library(foreign)

# Manipulate ohx04htc, making it a two-values variable: -----------------------
trans = function(x){
  if (x == 1) ans = 0
     else ans = 1
  ans     
}

trans_all_elem = function(x){
  sapply(x,trans)
}

# Manipulate ridreth1, collapsing ¡®Other Hispanic¡¯ and ¡®Other¡¯: ---------------
col = function(x){
  if (x == 5) ans = 2
     else ans = x
  ans     
}

col_all_elem = function(x){
  sapply(x,col)
}

# Obtain or restore data: -----------------------------------------------------
health = read.xport("OHX_D.XPT")
demo = read.xport("DEMO_D.XPT")

## a.
# Merge these two data: -------------------------------------------------------
syn = demo %>%
  left_join(health,by = 'SEQN')

## b.
# Recode permanent root fragments as permanent and 
# drop individuals for whom this tooth was not assessed: ----------------------
syn_select = syn %>%
  select(RIDAGEMN,OHX04HTC,RIAGENDR,RIDRETH1,INDFMPIR) %>%
  filter(!is.na(RIDAGEMN)) %>%
  filter(OHX04HTC != 9)

# Convert ohx04htc to a two-values variable to use logitic regression: --------
syn_select = syn_select %>%
  arrange(RIDAGEMN) %>%
  mutate(sign = trans_all_elem(OHX04HTC))

# Logistic regression to estimate the relationship between age (in months) and 
# variable ohx04htc: the probability that an individual has lose the
# upper right 2nd bicuspid: ---------------------------------------------------
log_reg = glm(sign ~ RIDAGEMN,family = binomial(link = 'logit'),
              data = syn_select)
BIC_orig = BIC(log_reg)

# Estimate the ages at which 25% of individuals 
# lose their primary upper right 2nd bicuspid: --------------------------------
prop_1 = 0.25
age_25 = round((log(prop_1/(1-prop_1)) - log_reg$coef[1])/log_reg$coef[2])

# Estimate the ages at which 50% of individuals 
# lose their primary upper right 2nd bicuspid: --------------------------------
prop_2 = 0.50
age_50 = round((log(prop_2/(1-prop_2)) - log_reg$coef[1])/log_reg$coef[2])

# Estimate the ages at which 75% of individuals 
# lose their primary upper right 2nd bicuspid: --------------------------------
prop_3 = 0.75
age_75 = round((log(prop_3/(1-prop_3)) - log_reg$coef[1])/log_reg$coef[2])

# Taking the floor in years of the 25%-ile: -----------------------------------
year_25 = floor(age_25/12)

# Taking the ceiling in years of the 75%-ile: ---------------------------------
year_75 = ceiling(age_75/12)

## c.
# Add gender to the model: ----------------------------------------------------
syn_select$RIAGENDR = factor(syn_select$RIAGENDR,labels = c("Male","Female"))
log_reg_gen = glm(sign ~ RIDAGEMN + RIAGENDR,
                  family = binomial(link = 'logit'),data = syn_select)
BIC_gen = BIC(log_reg_gen) 
# BIC_gen > BIC_orig, so do not retain it

# Collapse ¡®Other Hispanic¡¯ and ¡®Other¡¯, label the four levels: ---------------
syn_select_1 = syn_select %>%
  mutate(RIDRETH = col_all_elem(RIDRETH1)) 
syn_select_1$RIDRETH = factor(syn_select_1$RIDRETH,
                              labels = c("Mex","Other","White","Black"))
syn_select_1$RIDRETH = relevel(syn_select_1$RIDRETH,ref = "White")

# Use White as the reference, Creat other three indicators: -------------------
dummy_ridreth = model.matrix(~ RIDRETH,syn_select_1)
colnames(dummy_ridreth) = c("White","Mex",'Other',"Black")
syn_select_1 = cbind(syn_select_1,dummy_ridreth)

# Add category Mexican American: ---------------------------------------------- 
log_reg_W_Mex = glm(sign ~ RIDAGEMN + Mex,
                    family = binomial(link = 'logit'),data = syn_select_1)
BIC_W_Mex = BIC(log_reg_W_Mex) 
# Do not improve BIC, so do not retain Mex

# Add category Non-Hispanic Black: --------------------------------------------
log_reg_W_Black = glm(sign ~ RIDAGEMN + Black,
                      family = binomial(link = 'logit'),data = syn_select_1)
BIC_W_Black = BIC(log_reg_W_Black) 
# Improve BIC, so retain Black

# Add category Others: -------------------------------------------------------- 
log_reg_W_Black_Other = glm(sign ~ RIDAGEMN + Black + Other,
                            family = binomial(link = 'logit'),
                            data = syn_select_1)
BIC_W_Black_Other = BIC(log_reg_W_Black_Other)
# Do not improve BIC, so do not retain Other

syn_select_2 = syn_select_1 %>%
  filter (!is.na(INDFMPIR))
# Add poverty income ratio to the model: --------------------------------------
log_reg_W_Black_PIR = glm(sign ~ RIDAGEMN + Black + INDFMPIR,
                          family = binomial(link = 'logit'),
                          data = syn_select_2)
BIC_W_Black_PIR = BIC(log_reg_W_Black_PIR)
# Improve BIC, so retain PIR

## d.
### 1.
# Adjusted predictions at the mean (for other values) 
# at the representative ages(From 8 to 12): -----------------------------------
q_tot = c(96,108,120,132,144)
syn_select_apm = 
  tibble(RIDAGEMN = rep(q_tot,each = 1),
         Black = rep(mean(syn_select_2$Black),5),
         INDFMPIR = rep(mean(syn_select_2$INDFMPIR),5)
  )
adp = predict(log_reg_W_Black_PIR, syn_select_apm,type="response",se = TRUE)

### 2.
# The marginal effects at the mean of Black 
# at the same representative ages: --------------------------------------------
q_tot = c(96,108,120,132,144)
syn_select_mem = 
  tibble(RIDAGEMN = rep(q_tot,each = 2),
         Black = rep(c(0,1),5),
         INDFMPIR = rep(mean(syn_select_2$INDFMPIR),10)
  )
adm = predict(log_reg_W_Black_PIR, syn_select_mem,type="response",se = TRUE)
trans = matrix(0,5,4)
amm= matrix(0,5,2)
for (i in 1:10)
{
  if (i %% 2 == 1) {
    trans[(i+1)/2,1] = as.numeric(adm$fit[i])
    trans[(i+1)/2,3] = as.numeric(adm$se.fit[i])
  }
  if (i %% 2 == 0) {
    trans[i/2,2] = as.numeric(adm$fit[i])
    trans[i/2,4] = as.numeric(adm$se.fit[i])
  }
}
for (i in 1:5){
  amm[i] = trans[i,2]-trans[i]
  amm[i,2] = sqrt(trans[i,4]^2 + trans[i,3]^2)
}

### 3.
# The average marginal effect of Black at the representative ages: ------------
q_tot = c(96,108,120,132,144)
log_reg_W_Black_PIR_1 = glm(sign ~ RIDAGEMN + factor(Black) + INDFMPIR,
                            family = binomial(link = 'logit'),
                            data = syn_select_2)
mem0 = margins(log_reg_W_Black_PIR_1, at = list(RIDAGEMN = q_tot))
mem0
# From the results of mem0, we can see the value of 
# the average marginal effects
syn_select_ame = 
  tibble(RIDAGEMN = c(96,108,120,132,144),
         Black = c(0.06706,0.10515,0.12193,0.10039,0.06189))
ame = amm         




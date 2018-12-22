## Run parallel Monte Carlo Simulations using doParallel Package
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Dec 10, 2018

# Libraries: ------------------------------------------------------------------
library(parallel)
library(doParallel)
library(tidyverse)

# Empty the global environment
rm(list = ls())

source('./ps4_q2_funcs.R')

# Specify cores to use in the cluster
ncores = 4   

# set up a cluster called 'cl'
cl = makeCluster(ncores)

# register the cluster
registerDoParallel(cl)

fun_q2b = function(rho, sigma, mc_rep, beta){
  # Run Parallel Monte Carlo Simulations for fixed rho and sigma
  S = matrix(0, nrow = p, ncol = p)   
  S1 = matrix(rho*0.01, nrow = 10, ncol = 10)
  S[1:10,1:10] =S1
  diag(S) = 1                   # Coviance matrix of X
  R = chol(S)
  X = rnorm(n*p)               
  dim(X) = c(n,p)
  X = X %*% R                   # X
  result = MC_analysis(X, beta, sigma, mc_rep)
  
  alpha = 0.05                  # Confidence level
  indice = 1:10                 # indice
  method = c('holm', 'bonferroni', 'BH', 'BY')
  
  partial = lapply(method, function(x) 
  {evaluate(apply(result, 2, p.adjust, method = x), indice) %>%
      mutate(rho = rho, sigma = sigma, method = x)})
  res_q2b = as.tibble(do.call('rbind', lapply(partial, function(x) x))) %>%
    select(rho, sigma, metric, method, est, se)
  res_q2b
}


n = 1e3                         # n
p = 1e2                         # p
mc_rep = 1e4                    # mc_rep
beta = rep(0,p)
for (i in 1:10) beta[i] = 0.1   # beta
rho = seq(-0.75, 0.75, 0.25)    # rho
sigma = c(0.25, 0.5, 1)         # sigma

# Use foreach loops to run simulations of 4 metrics
# for 4 multiple comparison methods
fit = foreach(i = 1:3,.packages = c('tidyverse')) %dopar%{
  lapply(rho,function(x) fun_q2b(x, sigma[i], mc_rep, beta))
}

results_q4b= NULL
for (i in 1:7){
results_q4b = rbind(results_q4b,
                    as.tibble(do.call('rbind',lapply(fit, function(x) x[[i]]))))
}
print(results_q4b)

# Save the data results_q4b to results_q4b.RData
save(results_q4b, file = './results_q4b.RData')

## shut down the cluster after use ##
stopCluster(cl)

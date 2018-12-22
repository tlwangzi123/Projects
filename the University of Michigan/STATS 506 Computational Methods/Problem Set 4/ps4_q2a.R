## Run parallel Monte Carlo Simulations using parallel package
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Dec 10, 2018


# Libraries: ------------------------------------------------------------------
library(parallel)
library(tidyverse)

# Empty the global environment
rm(list = ls())

source('./ps4_q2_funcs.R')

fun_q2a = function(rho, sigma, mc_rep, beta){
 
  # Run Parallel Monte Carlo Simulations for fixed rho and sigma
  S = matrix(0, nrow = p, ncol = p)   
  S1 = matrix(rho*0.01, nrow = 10, ncol = 10)
  S[1:10,1:10] =S1
  diag(S) = 1                  # Coviance matrix of X
  R = chol(S)
  X = rnorm(n*p)               
  dim(X) = c(n,p)
  X = X %*% R                  # X
  result = MC_analysis(X, beta, sigma, mc_rep)
    
  alpha = 0.05                 # Confidence level
  indice = 1:10                # indice
  
  # Run Parallel Monte Carlo Simulations of 4 metrics
  # in 4 multiple comparison methods
  method = c('holm', 'bonferroni', 'BH', 'BY')
  partial = mclapply(method, function(x) 
    {evaluate(apply(result, 2, p.adjust, method = x), indice) %>%
       mutate(rho = rho, sigma = sigma, method = x)})
  res_q2a = as.tibble(do.call('rbind', mclapply(partial, function(x) x))) %>%
    select(rho, sigma, metric, method, est, se)
  res_q2a
}

n = 1e3                        # n
p = 1e2                        # p
sigma = 1                      # sigma
mc_rep = 1e4                   # mc_rep
beta = rep(0,p)
for (i in 1:10) beta[i] = 0.1  # beta
rho = seq(-0.75, 0.75, 0.25)   # rho
fit = mclapply(rho,function(x) fun_q2a(x, sigma, mc_rep, beta))
results_q4a = as.tibble(do.call('rbind', mclapply(fit, function(x) x)))
## Modification of ps4_q2a.R
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Dec 10, 2018

# Libraries: ------------------------------------------------------------------
library(future)
library(parallel)
library(tidyverse)

# Empty the global environment
rm(list = ls())

source('./ps4_q2_funcs.R')

# Default arguments
# In order to produce a table to answer c., we set the default of sigma 
# as c(0.25,0.5,1) so that we can source ps4_q2c.R in Rmarkdown 
# and combine the 10 first rows of each situation to show
args_list = list(
  sigma = c(0.25, 0.5, 1),
  mc_rep = 1e4,
  n_cores = 4
)

# get parameters from command line
args = commandArgs(trailingOnly = TRUE)
print(args)

# functions for finding named arguments
args_to_list = function(args){
  ind = grep('=', args)  
  args_list = strsplit(args[ind], '=')
  names(args_list) = sapply(args_list, function(x) x[1])

  args_list = lapply(args_list, function(x) as.numeric(x[2]))
  args_list
}

# get named arguments
args_list_in = args_to_list(args)

# update non default arguments
ignored = c()
for ( arg in names(args_list_in) ) {
 # Check for unknown argument
 if ( is.null(args_list[[arg]]) ) {
    ignored = c(ignored, arg)
 } else{
   # update if known
   args_list[[arg]] = args_list_in[[arg]]
 }
}

# Print warning message about unknown arguments
if ( length(ignored) > 0 ) {
  cat('Ignoring unkown arguments:',paste(ignored,collapse=', '), '\n')
}

fun_q2c = function(rho, sigma, mc_rep, beta){
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
        mutate(rho = rho, sigma = sigma, method = x)}, mc.cores = n_cores)
  res_q2c = as.tibble(do.call('rbind', lapply(partial, function(x) x))) %>%
    select(rho, sigma, metric, method, est, se)
  res_q2c
}


sigma = args_list$sigma        # sigma
mc_rep = args_list$mc_rep      # mc_rep
n_cores = args_list$n_cores    # n_cores

n = 1e3                        # n
p = 1e2                        # p
beta = rep(0,p)
for (i in 1:10) beta[i] = 0.1  # beta
rho = seq(-0.75, 0.75, 0.25)   # rho

plan(multicore)
fit1 = list()
fit2 = list()

for (j in 1:length(sigma))
{
for (i in 1:7)
{
  fit1[[i+(j-1)*7]] = future(fun_q2c(rho[i], sigma[j], mc_rep, beta))
}
}

results_q4c = as.tibble(do.call('rbind', lapply(fit1, function(x) value(x))))
results_q4c


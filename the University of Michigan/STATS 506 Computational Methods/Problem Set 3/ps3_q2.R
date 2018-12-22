## A Monte Carlo study in R to compare the performance of different methods 
## that adjust for multiple comparisons.
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Nov 19, 2018

# Libraries: ------------------------------------------------------------------
library(data.table)
library(tidyverse)

## Part a.
MC_analysis = function(X, beta, sigma, mc_rep){
  result = matrix(0, nrow = p, ncol = mc_rep)
  var = rep(0,p)
  QR = qr(t(X) %*% X)
  R = qr.R(QR)
  Q = qr.Q(QR)
  for (i in 1:mc_rep){
    Y = X %*% beta 
    Y1 = rnorm(n, mean = 0, sd = sigma)
    dim(Y1) = c(n,1)
    Y = Y + Y1
    # i.
    beta_hat = solve(R, t(Q) %*% t(X) %*% Y)
    ## ii.
    Y_hat = X %*% beta_hat
    sigmasq_hat = sum((Y-Y_hat)^2)/(n-p)
    ## iii.
    v = (sigmasq_hat) * solve(R) %*% t(Q)
    var = diag(v)
    ## iv.
    Z = beta_hat/sqrt(var)
    result[,i] = 2*(1-pnorm(abs(Z)))
  }
  result
}

# Test the function:MC_analysis()
n = 1000
p = 100
mc_rep = 1
sigma = 1
beta = rep(0,p)
for (i in 1:10) beta[i] = sigma^2
X = rnorm(n*p)
dim(X) = c(n,p)
set.seed(42)
test = MC_analysis(X, beta, sigma, mc_rep)
set.seed(42)
Y = X%*%beta + rnorm(n, mean = 0, sd = sigma)
fit0 = lm(Y ~ 0 + X)
p_value = summary(fit0)$coefficients[,'Pr(>|t|)']
ans = max(abs(test[,1]-p_value))

## Part b.
n = 1e3; p = 1e2; r = .1; rho = -.1
beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)

# X ~ N(0, Sigma): -----------------------------------------------------------
Sigma = p*diag(p)/2

# Ensure it is symmetric, then rescale to give variances all equal to one
Sigma[lower.tri(Sigma)] = rho
Sigma = {Sigma + t(Sigma)} / p
R = chol(Sigma)

# Here is an X for testing: ---------------------------------------------------
X = matrix( rnorm(n*p), n, p) %*%  R
result = MC_analysis(X, beta, sigma = 1, mc_rep = 1e4)

## Part c.
evaluate = function(result, indice){
  alpha = 0.05
  m = qnorm(.975)
  p = nrow(result)
  mc_rep = ncol(result)
  FWER = rep(0,mc_rep)
  FDR = rep(0,mc_rep)
  Sensitivity = rep(0,mc_rep)
  Specificity = rep(0,mc_rep)
  for (j in 1:mc_rep)
  {
    tp = 0  # true positive
    tn = 0  # true negative
    fp = 0  # false positive
    fn = 0  # false negative
    fn = sum(result[indice, j] >= alpha)  # H0 is False, and accept H0
    tp = sum(result[indice, j] < alpha)   # H0 is false, and reject H0
    tn = sum(result[-indice, j] >= alpha) # H0 is true, and accept H0
    fp = sum(result[-indice, j] < alpha)  # H0 is true, and reject H0
    FWER[j] = (fp>=1)
    FDR[j] = fp/(fp+tp)
    Sensitivity[j] = tp/(tp+fn)
    Specificity[j] = tn/(tn+fp)
  }
  res = tibble(quan = c('The family wise error rate',
                        'The false discovery rate',
                        'The sensitivity',
                        'The specificity'),
               est = c(mean(FWER), mean(FDR), 
                       mean(Sensitivity), mean(Specificity)),
               se =  c(sd(FWER)/sqrt(mc_rep), 
                       sd(FDR)/sqrt(mc_rep), 
                       sd(Sensitivity)/sqrt(mc_rep), 
                       sd(Specificity)/sqrt(mc_rep)))
  res = res %>%
    group_by(quan) %>%
    mutate(lwr = pmax(est - m*se, 0), upr = pmin(est + m*se, 1),
           ci = sprintf('%4.3f (%4.3f, %4.3f)', est, lwr, upr)) %>%
    ungroup %>%
    transmute(Quantities = quan, ci)
  res
}

## Part d.
alpha = 0.05 # Confidence level
indice = vector()
for (i in 1:p)
  if (beta[i]!=0) indice = cbind(indice,i)
fit1 = evaluate(result,indice) %>%
  transmute(Quantities, "Uncorrected(95% CI)" = ci)
result_adj_bon = apply(result, 2,function(x) p.adjust(x,method = 'bonferroni'))
fit2 = evaluate(result_adj_bon,indice) %>%
  transmute(Quantities, "Bonferroni(95% CI)" = ci)
result_adj_holm = apply(result, 2, function(x) p.adjust(x,method = 'holm'))
fit3 = evaluate(result_adj_holm,indice) %>%
  transmute(Quantities, "Holm(95% CI)" = ci)
result_adj_bh = apply(result, 2, function(x) p.adjust(x,method = 'BH'))
fit4 = evaluate(result_adj_bh,indice) %>%
  transmute(Quantities, "BH Correction(95% CI)" = ci)
result_adj_by = apply(result, 2, function(x) p.adjust(x,method = 'BY'))
fit5 = evaluate(result_adj_by,indice) %>%
  transmute(Quantities, "BY Correction(95% CI)" = ci)

## Part e.
fit = fit1 %>%
  left_join(fit2, by = "Quantities") %>%
  left_join(fit3, by = "Quantities") %>%
  left_join(fit4, by = "Quantities") %>%
  left_join(fit5, by = "Quantities")
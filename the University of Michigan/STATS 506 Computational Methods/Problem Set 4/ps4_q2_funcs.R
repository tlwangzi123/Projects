## Two functions about Monte Carlo Simulations
##
## Author: Zi Wang (tlwangzi@umich.edu)
## Updated: Dec 10, 2018


MC_analysis = function(X, beta, sigma, mc_rep){
  QR = qr(crossprod(X))
  R = qr.R(QR)
  Q = qr.Q(QR)
  QX = X %*% Q
  XtXinv = solve(R, t(Q))
  
  # Generate mc_rep copies of Y at once, each in a column.
  Y = as.numeric(X %*% beta) + rnorm(n*mc_rep)
  dim(Y) = c(n, mc_rep)
  
  # estimate betas and residual standard errors
  beta_hat = solve(qr.R(QR), crossprod( QX, Y ) )
  
  # iii.
  v = colSums( {Y - as.numeric(X %*% beta_hat)}^2 / {n - p})
  var = sqrt( diag(XtXinv) * rep(v, each = p) )
  
  # p-value
  matrix( 2*pt( abs(beta_hat / var ), df = n-p, lower.tail = FALSE ), p, mc_rep ) 
}

evaluate = function(result, indice){
  alpha = 0.05
  m = qnorm(.975)
  p = nrow(result)
  mc_rep = ncol(result)
  result = result < alpha
  fn = colSums(!result[indice, ])  # H0 is False, and accept H0:false negative 
  tp = colSums(result[indice, ])   # H0 is false, and reject H0:true positive 
  tn = colSums(!result[-indice, ]) # H0 is true, and accept H0:true negative
  fp = colSums(result[-indice, ])  # H0 is true, and reject H0:false positive
  FWER = (fp>=1)
  FDR = ifelse(fp+tp > 0, fp/(fp+tp), 0)
  Sensitivity = tp/(tp+fn)
  Specificity = tn/(tn+fp)
  res = tibble(metric = c('FWER',
                          'FDR',
                          'Sensitivity',
                          'Specificity'),
               est = c(mean(FWER), mean(FDR), 
                       mean(Sensitivity), mean(Specificity)),
               se =  c(sqrt(mean(FWER)*{1-mean(FWER)}) /sqrt(mc_rep), 
                       sd(FDR)/sqrt(mc_rep), 
                       sd(Sensitivity)/sqrt(mc_rep), 
                       sd(Specificity)/sqrt(mc_rep)))
  res
}

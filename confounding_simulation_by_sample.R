set.seed(1)

nvals = seq(10, 5000, 10)
pvals = c()

for(n in nvals){
  temp = seq(1, n, 1)
  ice = temp + rnorm(n, 0, n/10)
  #sub_temp = rnorm(n, 0, 1)
  sub_temp = temp + rnorm(n, 0, n/50)
  #sub_ice = ice + rnorm(n, 0, 30)
  #pool = temp*3 + rnorm(n, 0, 1)
  pool = temp*3 + rnorm(n,0,n/10)
  
  mod = lm(pool ~ sub_temp + ice)
  p = summary(mod)$coef['ice',4]
  pvals = c(pvals, p)
}

plot(nvals, log10(pvals))
abline(h=log10(0.05), col='red')
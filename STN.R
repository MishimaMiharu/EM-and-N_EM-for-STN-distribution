STN = function(n,mu,sig,nu,lam){
  tau = 0; eta = 0; y = 0
  for (i in 1:n) {
    tau[i] = rgamma(1,nu/2,nu/2)
    eta[i] = rnorm(1,0,sqrt((tau[i]+lam^2)/tau[i]))
    while (eta[i]<0) {
      eta[i] = rnorm(1,0,sqrt((tau[i]+lam^2)/tau[i]))
    }
    y[i] = rnorm(1,mu+sig*lam*eta[i]/(tau[i]+lam^2),sqrt(sig^2/(tau[i]+lam^2)))
  }
  return(y)
}

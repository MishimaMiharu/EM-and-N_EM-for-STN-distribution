llf = function(n,y,mu,sig,nu,lam){
  u = (y-mu)/sig
  result = n*log(2)-n*log(sig)+sum(log(dt(u,nu)))+sum(log(pnorm(lam*u)))
  return(result)
}


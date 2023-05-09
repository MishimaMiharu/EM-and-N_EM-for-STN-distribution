# define the parameter

mu = 1; sig = 2; nu = 6; lam = 3; n = 200

# MM algorithm

LL=0; MU=0; SI=0; NU=0; LA=0; TI=0; LLF=0; k = 2


for (h in 1:100) {

  # generating sample of Y
  y = STN(n,mu,sig,nu,lam)
  LLF[h]=llf(n,y,mu,sig,nu,lam)
  ## initialize
  mue = mean(y); sige = sd(y); nue = 2; lame = 1; t=1
  
  llv = llf(n,y,mue,sige,nue,lame)
  
  ## algorithm
  start_time = Sys.time()
  while (t==1 || (t < 10000 & abs(llv[t]-llv[t-1])>10^(-7))) {
    # initialization #
    Tau = 0; TauY_1 = 0; Y_1 = 0; G_1 = 0; TauY_2 = 0;
    G_1Y_1 = 0; Y_2 = 0; Kap = 0;
    # expectation #
    for (i in 1:n) {
      u = (y[i]-mue[t])/sige[t]
      elogtau = digamma((nue[t]+1)/2) - log((nue[t]+u^2)/2)
      etau = (nue[t]+1)/(nue[t]+u^2)
      egamma_1 = lame[t]*u+dnorm(lame[t]*u)/pnorm(lame[t]*u)
      # summation #
      Tau = Tau + etau
      TauY_1 = TauY_1 + etau*y[i]
      Y_1 = Y_1 + y[i]
      G_1 = G_1 + egamma_1
      TauY_2 = TauY_2 + etau*(y[i])^2
      G_1Y_1 = G_1Y_1 + egamma_1*y[i]
      Y_2 = Y_2 + y[i]^2
      Kap = Kap + elogtau
    }
    # iteration #
    # mu #
    mue[t+1] = (TauY_1+lame[t]^2*Y_1-sige[t]*lame[t]*G_1)/(n*lame[t]^2+Tau)
    #mue[t+1]=mue[t]
    # sigma #
    sigc0 = TauY_2-2*mue[t+1]*TauY_1+mue[t+1]^2*Tau
    sigc0 = sigc0+lame[t]^2*Y_2-2*lame[t]^2*mue[t+1]*Y_1+n*(lame[t]*mue[t+1])^2
    sigc1 = -lame[t]*G_1Y_1+lame[t]*mue[t+1]*G_1
    sigc2 = -n
    sige[t+1]=(-sigc1-sqrt(sigc1^2-4*sigc0*sigc2))/(2*sigc2)
    # lambda #
    beta = (G_1Y_1-mue[t+1]*G_1)/(Y_2-2*mue[t+1]*Y_1+n*mue[t+1]^2)
    lame[t+1] = sige[t+1]*beta
    # nu #
    c = -1-(Kap-Tau)/n-log(nue[t]/2)+digamma(nue[t]/2)
    for (j in 0:k) {
      c = c+1/(j+nue[t]/2)
    }
    
    if(k==0) {nue[t+1] = 2/c}
    else if(k==1) {nue[t+1] = (2-c+sqrt(c^2+4))/c}
    else{
      coef = cf(c,k+1)
      nupo <- function(x) {
        result=0
        for (j in 1:length(coef)) {
          result=result+coef[j]*x^(j-1)
        }
        return(result)
      }
      nue[t+1]=2*uniroot(nupo,c(0,100))$root
    }
    llv[t+1] = llf(n,y,mue[t+1],sige[t+1],nue[t+1],lame[t+1])
    t = t+1
  }
  end_time=Sys.time()
  leng = length(llv)
  TI[h]=end_time-start_time
  LL[h]=llv[leng]
  MU[h]=mue[leng]
  SI[h]=sige[leng]
  NU[h]=nue[leng]
  LA[h]=lame[leng]
}
if(k==0){
  result=data.frame(MZTI=TI,MZLL=LL,MZLLF=LLF,MZMU=MU,MZSI=SI,MZNU=NU,MZLA=LA)
  save(result,file = "MMZ.RData")
}else if(k==1){
  result=data.frame(MOTI=TI,MOLL=LL,MOLLF=LLF,MOMU=MU,MOSI=SI,MONU=NU,MOLA=LA)
  save(result,file = "MMO.RData")
}else{
  result=data.frame(MMTI=TI,MMLL=LL,MMLLF=LLF,MMMU=MU,MMSI=SI,MMNU=NU,MMLA=LA)
  save(result,file = "MMM.RData")
}













# define the parameter

mu = 1; sig = 2; nu = 6; lam = 3; n = 200

# initialize

LL=0; MU=0; SI=0; NU=0; LA=0; TI=0; LLF=0

for (h in 1:100) {
  # generating sample of Y
  
  y = STN(n,mu,sig,nu,lam)
  
  LLF[h]=llf(n,y,mu,sig,nu,lam)
  
  # NEM algorithm
  
  ## initialize
  
  mue = mean(y); sige = sd(y); nue = 2; lame = 1; t=1; 
  
  llv = llf(n,y,mue,sige,nue,lame)
  
  #algorithm
  start_time = Sys.time()
  while (t==1 || (t < 10000 & abs(llv[t]-llv[t-1])>10^(-7))) {
    # initialization #
    
    # summation #
    Y_1R=0; R=0; TN=0; Y_2R=0; L=0; Y_1TN=0
    for (i in 1:n) {
      u=(y[i]-mue[t])/sige[t] 
      ust=(y[i]-mue[t])*lame[t]/sige[t]
      Y_1R=Y_1R+y[i]*nue[t]/(nue[t]+u^2)
      Y_2R=Y_2R+y[i]^2*nue[t]/(nue[t]+u^2)
      R=R+nue[t]/(nue[t]+u^2)
      TN=TN+dnorm(ust)/pnorm(ust)
      Y_1TN=Y_1TN+y[i]*dnorm(ust)/pnorm(ust)
      L=L+log(1+u^2/nue[t])
    }
    
    # mu #
    mue[t+1]=(1+1/nue[t])/sige[t]^2*Y_1R
    mue[t+1]=mue[t+1]+n*mue[t]/sige[t]^2*lame[t]^2
    mue[t+1]=mue[t+1]-TN/sige[t]*lame[t]
    mue[t+1]=mue[t+1]/((1+1/nue[t])/sige[t]^2*R+n*(lame[t]/sige[t])^2)
    
    # sigma #
    sige[t+1]=Y_2R-2*mue[t+1]*Y_1R+mue[t+1]^2*R
    sige[t+1]=sige[t+1]*(1+1/nue[t])/n
    sige[t+1]=sige[t+1]+sige[t]^2+lame[t]^2*(mue[t+1]-mue[t])^2
    sige[t+1]=sige[t+1]-sige[t]*lame[t]/n*(Y_1TN+(mue[t]-2*mue[t+1])*TN)
    sige[t+1]=sqrt(sige[t+1]/2)
    
    # lambda #
    lame[t+1]=-sige[t]/lame[t]/n*(Y_1TN+(mue[t]-2*mue[t+1])*TN)
    lame[t+1]=lame[t+1]+(sige[t]/lame[t])^2+(mue[t+1]-mue[t])^2
    lame[t+1]=1/sqrt(lame[t+1])*sige[t+1]
    
    # nu #
    nuc3=0
    nuc2=0
    for (j in 1:n) {
      u1=(y[j]-mue[t])/sige[t]
      u2=(y[j]-mue[t+1])/sige[t+1]
      nuc2=nuc2+u2^2/(1+u1^2/nue[t])
      nuc3=nuc3+(u1^2/(nue[t]+u1^2)-log(1+u1^2/nue[t]))/n
    }
    nuc3=nuc3-2/nue[t]+digamma((nue[t]+1)/2)-digamma(nue[t]/2)
    nuc2=nuc2/n
    nue[t+1]=(-1-sqrt(1-4*nuc3*nuc2))/2/nuc3
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
result=data.frame(NTI=TI,NLL=LL,NLLF=LLF,NMU=MU,NSI=SI,NNU=NU,NLA=LA)
save(result,file = "NEM.RData")















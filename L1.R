
seed <- as.double(1)

rand <- function(n=1) {
  if(n>1){
    res=integer(n)
    for(i in 1:n){
      seed <<- ((2^16 + 3) * seed) %% (2^31)
      res[i]=seed/(2^31)
    }
  }else{
    seed <<- ((2^16 + 3) * seed) %% (2^31)
    res=seed/(2^31)
  }
  res
}

Normal<-function(m=0,s=1,n=1){
  if(n>1){
    N=2*round(n/2+1)
    res=integer(N)
    pes=rand(N)
    res[1:(N/2)]=sqrt(-2*log(pes[1:(N/2)]))*sin(2*pi*pes[(N/2+1):N])
    res[(N/2+1):N]=sqrt(-2*log(pes[1:(N/2)]))*cos(2*pi*pes[(N/2+1):N])
  }else{
    res=sqrt(-2*log(rand()))*sin(2*pi*rand())
  }
  s*res[1:n]+m
}

hist(Normal(n=10000), freq = F)
curve(dnorm(x,mean=0,sd=1),col="red", add=TRUE, lwd=2)
library(normaltest)

shapiro.test(Normal(n=1000))
ks.test(Normal(n=10000),"pnorm", mean=0, sd=1)

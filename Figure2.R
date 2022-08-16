CorrEquiSpaced <- function(K,alpha=1e-15){
  cc <- function(dx){
    thresholds <- dx*((1:(K-1))-0.5*K)
    p     <- diff( pnorm(c(-Inf,thresholds,Inf)) )
    E_y   <- sum(p*(1:K))
    E_y2  <- sum(p*((1:K)^2))
    var_y <- E_y2 - E_y^2
    sd_y  <- sqrt(var_y)
    
    ## Contional means (truncated normal distribution theory)
    means_l    <- sapply(1:K, function(k){
      if(k==1){
        m_k <- (dnorm(-Inf)-dnorm(thresholds[1]))/(pnorm(thresholds[1])-pnorm(-Inf))
      }
      if(k==K){
        m_k <- (dnorm(thresholds[K-1])-dnorm(+Inf))/(pnorm(+Inf)-pnorm(thresholds[K-1]))
      }
      if(k>=2 & k<=K-1){
        m_k <- (dnorm(thresholds[k-1])-dnorm(thresholds[k]))/(pnorm(thresholds[k])-pnorm(thresholds[k-1]))
      }
      return(m_k)
    })
    Cov_yl <- sum(p * means_l * (1:K))
    corr   <- (Cov_yl / sd_y)
    #sum(dnorm(thresholds)) / sd_y
    return(corr)
  }
  values_obs <- 1:(K-1)
  if(K==2){
    dx      <- 0
    maxCorr <- 2*dnorm(0)
    Dx      <- 0
  }else{
    dxMax <- qnorm(1-alpha/2) / (0.5*K - 1)
    model <- optim(par=1e-5,fn=cc,
                   control = list(fnscale=-1),method = "Brent",
                   lower=0,upper=dxMax)
    dx      <- model$par
    maxCorr <- model$value
    Dx      <- model$par
  }
  thresholds <- dx*(values_obs-mean(values_obs))
  p <- diff( pnorm(c(-Inf,thresholds,Inf)) )
  names(p) <- paste0("prob_",1:K)
  return(list(maxCorr=maxCorr,
              Prob=p,Thresholds=thresholds,
              Dx=Dx))
}

## For the figure
Kmax <- 100
Ks   <- 2:Kmax
Cs   <- sapply(Ks,function(K) CorrEquiSpaced(K)$maxCorr)

png("Figure2.png",width=2000,height=1500,res=200)
par(mar=c(5,5,3,2))
plot(Ks,Cs,axes=FALSE,pch=19,type="l",
     xlim=c(1,Kmax),cex.lab=1.2,#log="x",
     xlab="Number of categories (k)",col=1,lwd=2,
     ylab=expression(paste("Correlation between continuous and categorical score: ",R[k])))
points(Ks,Cs,col="black",pch=19,type='b',cex=1)
#points(Ks,Cs,col="red",pch=1,type='b',cex=1.5)
axis(1,at=seq(0,Kmax,by=5))
axis(2)
abline(h=c(0.95,0.99,1),col=2:4,lty=1:3)
abline(v=c(5,10,15,20),lty=4,col="grey")

## Approximation
model  <- coef(lm(log(1-Cs[Ks>=3])~log(Ks[Ks>=3])))
predCs <- 1 - exp(model[1]) * (Ks**model[2])
points(Ks,predCs,col="red",pch=1,type='b',cex=1.5)

 legend(65,0.83,legend=expression(paste(R[k]%~~%"1 - 0.7"^(-1.7))),
        box.lty=0,pch=1,lty=1,col="red",cex=1.5,
        title="Approximation")
dev.off()


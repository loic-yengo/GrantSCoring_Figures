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
    dxMax <- qnorm(1-alpha) / (0.5*K - 1)
    model <- optim(par=1e-5,fn=cc,
                   control = list(fnscale=-1),method = "Brent",
                   lower=0,upper=dxMax)
    dx      <- model$par
    maxCorr <- (model$value)
    Dx      <- model$par
  }
  thresholds <- dx*(values_obs-mean(values_obs))
  p <- diff( pnorm(c(-Inf,thresholds,Inf)) )
  names(p) <- paste0("prob_",1:K)
  return(list(maxCorr=maxCorr,
              Prob=p,Thresholds=thresholds,
              Dx=Dx))
}

K <- 5
Model <- CorrEquiSpaced(K)
xmin <- -4
xmax <- +4
Xm   <- c(xmin,Model$Thresholds,xmax)
xm   <- sapply(1:(K+1), function(k) 0.5*(Xm[k+1]+Xm[k]))

png("Figure1.png",width=2000,height=2000,res=300)
ColDiscrete = 4
par(mar=c(5,5,5,3))
plot(dnorm,xlim=c(xmin,xmax),axes=FALSE,
     ylim=c(0,0.45),cex.lab=1.2,lwd=2,
     xlab="Unobserved Continuous Scale",
     ylab="Probability Density")
axis(1);axis(2)
abline(v=Model$Thresholds,col='coral1',lty=2)

mtext(0,side=1,text = "Observed Discrete Scale\n(Percentage of Scores in Category)",
      line = -25,font=2,col=ColDiscrete,cex=1.1)
for(k in 1:K){
  text(xm[k],0.45,k,cex=1,font=2,col=ColDiscrete)
  text(xm[k],0.43,paste0("(",round(100*Model$Prob[k]),"%)"),cex=0.85,font=2,col=ColDiscrete)
  arrows(Xm[k]+0.1,0.41,Xm[k+1]-0.1,0.41,len=0.05,col=ColDiscrete,code=3,lwd=2)
}
legend(xmin+0.1,0.4,legend="Thresholds",box.lty=0,col="coral1",lty=2)
dev.off()


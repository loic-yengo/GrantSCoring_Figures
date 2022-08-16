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
## Results of Eq. 3
## 4 panels: m=3, 4, 5 and 6
##
## s: proportion of variance in score explained by grant quality

L <- function(m,  # Number of grant assessors
              k,  # Number of score categories
              s){ # proportion of variance in score explained by grant quality
  lambda_u <- (1-s)/s
  R_k      <- CorrEquiSpaced(K=k)$maxCorr
  lambda_v <- (1-(R_k^2)*s)/((R_k^2)*s)
  return(sqrt( (m + lambda_u) / (m + lambda_v) ))
}

ss <- seq(0.1,0.9,by=0.1)
ms <- c(3,4,5,6)
ks <- c(5,7,10,13)

parameters <- expand.grid(ms,ks,ss)
colnames(parameters) <- c("m","k","s")
Lof_params <- sapply(1:nrow(parameters),function(i) L(m=parameters[i,"m"],k=parameters[i,"k"],s=parameters[i,"s"]))

panelID <- c("a","b","c","d")
names(panelID) <- ms
Cols <- c("coral1","dodgerblue","goldenrod","seagreen3")

png("Figure3.png",width=2200,height=2200,res=250)
op <- par(mfrow=c(2,2))
for(m in ms){
  Ls <- do.call("cbind",lapply(ks, function(k){
    Lof_params[which(parameters[,"m"]==m & parameters[,"k"]==k)]
  }))
  par(mar=c(5,6,3,2))
  matplot(ss,1-Ls,pch=19,col=Cols,axes=FALSE,type="l",lwd=3,lty=1:4,
          main=paste0("m = ",m," assessors"),
          xlab="Proportion (s) of variance in score\nexplained by grant quality",
          ylab="Loss of information in ranking grants\n( L(m,k,s) in Equation [3] )",
          ylim=c(0,0.04))
  axis(1,at=ss)
  axis(2,at=1-seq(0.96,1.00,by=0.01))
  legend(0.5,0.04,legend=paste("k=",ks),title="Number of categories",
         lty=1:4,col=Cols,box.lty=0,lwd=2)
  mtext(at=ss[1],panelID[as.character(m)],font=2,cex=1.2,line=0)
  abline(h=c(0,0.02),col="grey",lty=5)
}
par(op)
dev.off()


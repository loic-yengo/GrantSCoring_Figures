CorrEquiSpaced <- function(K,alpha=1e-15){
  cc <- function(dx){
    thresholds <- dx*((1:(K-1))-0.5*K)
    p     <- diff( pnorm(c(-Inf,thresholds,Inf)) )
    E_y   <- sum(p*(1:K))
    E_y2  <- sum(p*((1:K)^2))
    var_y <- E_y2 - E_y^2
    sd_y  <- sqrt(var_y)
    
    ## Conditional means (truncated normal distribution theory)
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
    maxCorr <- c(model$value)
    Dx      <- model$par
  }
  thresholds <- dx*(values_obs-mean(values_obs))
  p <- diff( pnorm(c(-Inf,thresholds,Inf)) )
  names(p) <- paste0("prob_",1:K)
  return(list(maxCorr=maxCorr,
              Prob=p,Thresholds=thresholds,
              Dx=Dx))
}
N <- 1e6
UniProb <- seq(1/N,1-1/N,by=1/N)
f <- function(k,v){
  s <- 1/sqrt(v)
  R <- CorrEquiSpaced(K=k)
  l <- qnorm(UniProb)#rnorm(N)
  g <- cut(l,breaks = c(-Inf,R$Thresholds,+Inf) * s,include.lowest = T)
  g <- as.numeric(g)
  r <- cor(l,g)
  p <- (mean(g==1) + mean(g==k))/2
  return(c(rObs=r,rExp=R$maxCorr,RObs=p,RExp=R$Prob[1]))
}

ks <- 2:15
f1 <- do.call("rbind",lapply(ks, function(k) f(k,v=0.75)))
f2 <- do.call("rbind",lapply(ks, function(k) f(k,v=0.50)))

Cols <- c("coral1","dodgerblue","goldenrod")

png("Figure4_v3.png",width=2000,height=1500,res=200)
par(mar=c(5,5,3,2))
matplot(ks,cbind(f1[,c("rExp","rObs")],f2[,"rObs"]),type="l",lwd=3,
        axes=FALSE,cex.lab=1.2,lty=1:3,
        xlab="Number of categories (k)",col=Cols,ylim=c(0.8,1),
        ylab=expression(paste("Correlation between continuous and categorical score (",R[k],")")))
axis(1,at=ks)
axis(2)
abline(h=c(0.95,0.99),col="grey")
legend(9,0.9,title="Variance of the distribution\nunderlying scores",
       legend=c(expression(sigma[s]^2==1.00),
                expression(sigma[s]^2==0.75),
                expression(sigma[s]^2==0.50)),
       box.lty=0,col=Cols,lty=1:3,cex=1.2)
dev.off()

## proportions
v <- 0.5
k <- 5
s <- 1/sqrt(v)
R <- CorrEquiSpaced(K=k)
l <- qnorm(UniProb)#rnorm(N)
g <- cut(l,breaks = c(-Inf,R$Thresholds,+Inf) * s,include.lowest = T)
g <- as.numeric(g)
tb<-table(g)
round(100*tb/N,1)

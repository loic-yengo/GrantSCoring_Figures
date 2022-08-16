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
N <- 1e5
K <- 5
R <- CorrEquiSpaced(K=K)
f <- function(s){
  l <- rnorm(N)
  g <- cut(l,breaks = c(-Inf,R$Thresholds,+Inf) * s,include.lowest = T)
  g <- as.numeric(g)
  r <- cor(l,g)
  p <- (mean(g==1) + mean(g==K))/2
  return(c(r=r,p=p))
}

us <- sqrt(seq(1,2,by=0.1)) #seq(0.1,10,by=0.1)
ss <- rep(us,each=100)
fs <- do.call("rbind",lapply(ss,f))
rs <- aggregate(fs[,"r"]~ss,FUN=mean)
ps <- aggregate(fs[,"p"]~ss,FUN=mean)

plot(rs[,1],rs[,2],pch=19,type='l',lwd=2)
abline(h=R$maxCorr,col=2,lty=2)
abline(v=1,col=4,lty=4)

## Loss of information
R_k      <- rs[,2]
png("Figure4.png",width=2000,height=1500,res=200)
par(mar=c(5,6,3,2))
plot(c(0.3,1),c(0,05),ylim=c(0,0.05),axes=FALSE,type="n",
     xlab="Ratio of Observed / Expected proportion of grants in extreme categories\n[Values towards the left mean that assessors less likely to rank in extreme categories ]",
     ylab="Loss of information in ranking grants\n( L(m,k,s) in Equation [3] )")
axis(1,at=seq(0.3,1,by=0.1))
axis(2,at=seq(0.,.05,by=0.01))
abline(h=0.02,col="grey",lty=5)
vs <- c(0.1,0.90)
Cols <- c("coral1","dodgerblue","goldenrod","seagreen3")
for(i in 1:length(vs)){
  v <- vs[i]
  lambda_u <- (1-v)/v
  lambda_v <- (1-(R_k^2)*v)/((R_k^2)*v)
  Loss     <- do.call("cbind",lapply(3:6,function(m) 1-sqrt( (m + lambda_u) / (m + lambda_v) ) ))
  matlines(ps[,2] / R$Prob[1],Loss,pch=19,type="l",lwd=2,col=Cols[i],lty=1:4)
}

legend(0.3,0.005,legend=paste0("m=",3:6),title="Number of assessors",
       lty=1:4,col=1,box.lty=0,lwd=2,horiz = TRUE)
legend(0.8,0.050,legend=paste0("s=",vs),
       title="Proportion (s) of variance in scores\nexplained by grant quality",
       fill=Cols[1:length(vs)],border=0,box.lty=0)
dev.off()



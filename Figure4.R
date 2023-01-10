source("CorrEquiSpaced.R")
N <- 1e6
# Generates equi-spaced values between 1/N and 1-1/N (N=1e6)
# as way to deterministically approximate a uniform distribution
UniProb <- seq(1/N,1-1/N,by=1/N)

f <- function(k,  ## number of grant categories
              v){ ## variance of the distribution underlying scores
  s <- 1/sqrt(v)
  R <- CorrEquiSpaced(K=k)
  l <- qnorm(UniProb)#rnorm(N)
  g <- cut(l,breaks = c(-Inf,R$Thresholds,+Inf) * s,include.lowest = T)
  g <- as.numeric(g)
  r <- cor(l,g)
  p <- (mean(g==1) + mean(g==k))/2
  return(c(rObs=r,rExp=R$maxCorr,RObs=p,RExp=R$Prob[1]))
}

ks <- 2:10 # range of values for the number categories
f1 <- do.call("rbind",lapply(ks, function(k) f(k,v=0.75)))
f2 <- do.call("rbind",lapply(ks, function(k) f(k,v=0.50)))

Cols <- c("coral1","dodgerblue","goldenrod")

png("Figure4.png",width=2000,height=1500,res=200)
par(mar=c(5,5,3,2))
matplot(ks,cbind(f1[,c("rExp","rObs")],f2[,"rObs"]),type="l",lwd=3,
        axes=FALSE,cex.lab=1.2,lty=1:3,
        xlab="Number of categories (k)",col=Cols,ylim=c(0.8,1),
        ylab=expression(paste("Correlation between continuous and categorical score (",R[k],")")))
axis(1,at=ks)
axis(2)
abline(h=c(0.95,0.99),col="grey")
legend(7.5,0.9,title="Variance of the distribution\nunderlying scores",
       legend=c(expression(sigma[s]^2==1.00),
                expression(sigma[s]^2==0.75),
                expression(sigma[s]^2==0.50)),
       box.lty=0,col=Cols,lty=1:3,cex=1.2)
dev.off()

## Application of the function "f" to specific examples
v <- 0.5
k <- 5
s <- 1/sqrt(v)
R <- CorrEquiSpaced(K=k)
l <- qnorm(UniProb)#rnorm(N)
g <- cut(l,breaks = c(-Inf,R$Thresholds,+Inf) * s,include.lowest = T)
g <- as.numeric(g)
tb<-table(g)
round(100*tb/N,1)

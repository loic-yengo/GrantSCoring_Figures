source("CorrEquiSpaced.R")
K     <- 5 # number of categories
Model <- CorrEquiSpaced(K)
xmin  <- -4
xmax  <- +4
Xm    <- c(xmin,Model$Thresholds,xmax)
xm    <- sapply(1:(K+1), function(k) 0.5*(Xm[k+1]+Xm[k]))

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


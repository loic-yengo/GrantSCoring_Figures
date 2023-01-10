source("CorrEquiSpaced.R")
Kmax <- 100    # maximum number of scoring categories
Ks   <- 2:Kmax # range for the number of scoring categories
Cs   <- sapply(Ks,function(K) CorrEquiSpaced(K)$maxCorr)

png("Figure2.png",width=2000,height=1500,res=200)
par(mar=c(5,5,3,2))
plot(Ks,Cs,axes=FALSE,pch=19,type="l",
     ylim=c(0.75,1),
     xlim=c(1,Kmax),cex.lab=1.2,#log="x",
     xlab="Number of categories (k)",col=1,lwd=2,
     ylab=expression(paste("Correlation between continuous and categorical score: ",R[k])))
points(Ks,Cs,col="black",pch=19,type='b',cex=1)
#points(Ks,Cs,col="red",pch=1,type='b',cex=1.5)
axis(1,at=seq(0,Kmax,by=5))
axis(2)
abline(h=c(0.95,0.99,1),col=2:4,lty=1:3)
abline(v=c(5,10,15,20),lty=4,col="grey")

## Approximation of the correlating as a function of number of categories
model  <- coef(lm(log(1-Cs[Ks>=3])~log(Ks[Ks>=3])))
predCs <- 1 - exp(model[1]) * (Ks**model[2])
points(Ks,predCs,col="red",pch=1,type='b',cex=1.5)

 legend(65,0.83,legend=expression(paste(R[k]%~~%"1 - 0.7"^(-1.7))),
        box.lty=0,pch=1,lty=1,col="red",cex=1.5,
        title="Approximation")
dev.off()


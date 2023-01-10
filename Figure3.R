source("CorrEquiSpaced.R")
## Results of Eq. 3
## 4 panels: m=3, 4, 5 and 6
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


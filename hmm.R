library(depmixS4)
set.seed(5)

y = data.frame(y=c(rnorm(50,.01,.01),rnorm(50,.03,.01)))
y.dm = depmix(y~1, data=y, family=gaussian(), nstates=2)
y.fit = fit(y.dm)
summary(y.fit)
y.h = y.fit@posterior

png(file="hmm_meanChange.png")
plot(y.h[,1], ylim=c(0,2))
lines(y.h[,2])
lines(y.h[,3], col="red")
lines(cumsum(y), col="green")
abline(v=51, lty=2)
dev.off()

readline()

y = data.frame(y=c(rnorm(10,.51,.02),rnorm(50,.51,.01),rnorm(50,.51,.02),rnorm(50,.51,.01)))
y.dm = depmix(y~1, data=y, family=gaussian(), nstates=2)
y.fit = fit(y.dm)
summary(y.fit)
y.h = y.fit@posterior

png(file="hmm_varChange.png")
plot(y.h[,1]-1, ylim=c(0,1), xlab="Time", ylab="")
#lines(y.h[,2])
#lines(y.h[,3], col="red")
lines((y), col="blue")
abline(v=11, lty=2)
abline(v=61, lty=2)
abline(v=111, lty=2)
legend(x=115,y=.25, legend=c("Data", "Change Point","Pred State"), col = c("blue", "black","black"), lty = c(1, 2, NA), pch = c(NA, NA, 21))
dev.off()

png(file="hmm_varChange_dat.png")
plot(y=y$y,x=(1:160), type="l", col="blue", ylim=c(0,1), xlab="Time", ylab="")
dev.off()

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

y = data.frame(y=c(rnorm(50,.01,.01),rnorm(50,.01,.03)))
y.dm = depmix(y~1, data=y, family=gaussian(), nstates=2)
y.fit = fit(y.dm)
summary(y.fit)
y.h = y.fit@posterior

png(file="hmm_varChange.png")
plot(y.h[,1], ylim=c(0,2))
lines(y.h[,2])
lines(y.h[,3], col="red")
lines(cumsum(y), col="green")
abline(v=51, lty=2)
dev.off()

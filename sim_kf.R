set.seed(5)

#R - obs noise cov
#Q - proc noise cov
#H - obs func 
H = matrix(c(1,0),nrow=1)
R = diag(10,1)
Q = matrix(c(.0000001,0,0,0.00001),2)
f = matrix(c(1,0,1,1), nrow=2)

prd = function(x_, p_){
    x = f%*%x_
    p = f%*%p_%*%t(f) + Q
    list(x=x,p=p)
}

upd = function(z, xh, p_){
    e = z - H%*%(xh)
    Se = R + H %*% p_ %*% t(H)
    K = p_ %*% t(H) %*% solve(Se)
    xp = xh + K%*%e
    pp = (diag(ncol(p_)) - K%*%H)%*%p_%*%t(diag(ncol(p_)) - K%*%H) + K%*%R%*%t(K)
    list(x=xp, p=pp)
}

y1 = (sin(2*pi*1:500/500)*2+rnorm(500,0,1))


y = data.frame(y=y1, yd=0)
y$yh = 0
y$se = 100

xh = c(0,0)
S = diag(100,2)
for(i in 1:nrow(y)){
    print(i)
    pd = prd(xh, S)
    xh = pd$x
    S = pd$p
    up = upd(y[i,"y"], xh, S)
    xh = up$x
    S = up$p
    y[i:nrow(y),"yh"] = xh[1,1]
    y[i:nrow(y),"se"] = S[1,1]
}

png(file="sin_kf.png")
plot(y$y)
lines(y$yh, col="red")
lines(y$yh+2*y$se, col="green")
lines(y$yh-2*y$se, col="blue")
lines(sin(2*pi*1:500/500)*2)
dev.off()

print("Enter for live filtering...")
readline()


y1 = (sin(2*pi*1:500/500)*2+rnorm(500,0,1))


y = data.frame(z=0, y=0, yh=0, se=100)
xh = c(0,0)
S = diag(100,2)
i = 0
while(T){
    i = i+1
    z = sin(2*pi*i/500)*2
    yn = z+rnorm(1,0,1)
    pd = prd(xh, S)
    xh = pd$x
    S = pd$p
    up = upd(yn, xh, S)
    xh = up$x
    S = up$p
    y = rbind(y, data.frame(z=z, y=yn, yh=xh[1,1], se=S[1,1]))
    #if(nrow(y)>500)y[-1,]
    
    plot(y$y, ylim=c(-3,3), xlim=c(i-500,i))
    lines(y$yh, col="red")
    lines(y$yh+2*y$se, col="green")
    lines(y$yh-2*y$se, col="blue")
    lines(y=sin(2*pi*((i-500):i)/500)*2,x=(i-500):i)
    Sys.sleep(.1)
}







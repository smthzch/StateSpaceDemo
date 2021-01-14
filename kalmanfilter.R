library(animation)
library(gganimate)
set.seed(5)

#R - obs noise cov
#Q - proc noise cov
#H - obs func 
H = matrix(c(1,0),nrow=1)
R = diag(10,1)
Q = matrix(c(.0000001,0,0,0.00001),2)
f = matrix(c(1,0,1,1), nrow=2)

#predict function
prd = function(x_, p_){
    x = f%*%x_
    p = f%*%p_%*%t(f) + Q
    list(x=x,p=p)
}

#update function
upd = function(z, xh, p_){
    e = z - H%*%(xh)
    Se = R + H %*% p_ %*% t(H)
    K = p_ %*% t(H) %*% solve(Se)
    xp = xh + K%*%e
    pp = (diag(ncol(p_)) - K%*%H)%*%p_%*%t(diag(ncol(p_)) - K%*%H) + K%*%R%*%t(K)
    list(x=xp, p=pp)
}

#gen data with noise
y1 = (sin(2*pi*1:500/500)*2+rnorm(500,0,1))


y = data.frame(y=y1, yd=0)
y$yh = 0 #initial mu
y$se = 100 #initial standard error

#run kalman filter
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

#save img
png(file="figs/sin_kf.png")
plot(y$y)
lines(y$yh, col="red")
lines(y$yh+2*y$se, col="green")
lines(y$yh-2*y$se, col="blue")
lines(sin(2*pi*1:500/500)*2)
dev.off()


#make animation
y1 = (sin(2*pi*1:500/500)*2+rnorm(500,0,1))
y = data.frame(z=0, y=0, yh=0, se=100, yh95=0, yh05=0)
xh = c(0,0)
S = diag(100,2)
for(i in 0:1000){
    i = i+1
    z = sin(2*pi*i/500)*2
    yn = z+rnorm(1,0,1)
    pd = prd(xh, S)
    xh = pd$x
    S = pd$p
    up = upd(yn, xh, S)
    xh = up$x
    S = up$p
    y = rbind(y, data.frame(z=z, y=yn, yh=xh[1,1], se=S[1,1], yh95=xh[1,1]+2*S[1,1], yh05=xh[1,1]-2*S[1,1]))
}
y$x=1:nrow(y)


ggplot(y) +    
    geom_point(aes(x=x,y=y, shape="1"), alpha=0.3, pch=19) +
    geom_line(aes(x=x, y=z, color="1"), alpha=0.5) +
    geom_line(aes(x=x, y=yh, color="2")) +
    geom_ribbon(aes(x=x, ymin=yh05, ymax=yh95, fill="3"), alpha=0.3)+
    scale_shape(name="Points", labels=c("Observations")) +
    scale_color_manual(
        name="Mu", 
        labels=c("True mu", "Estimated mu", "+/-95% CI"), 
        values=c("1"="black", "2"="darkred"), 
        breaks=1:3
    ) +
    scale_fill_manual(name="Confidence Interval", label=c("95% CI"), values=c("3"="darkred")) +
    ylim(c(-5,5)) + 
    transition_manual(x, cumulative=T) +
    view_follow(fixed_y=T)

anim_save("figs/sin_kf.gif")

## Sprial plot of COVID-19 cases in the US
## Sourya Shrestha
## data from nytimes github repository downloaded Jan 7, 2022
data <- read.csv('us-covid.csv')

scale <- 7
a <- 2
b <- .5

## generate daily cases counts from cumulative cases, and extend to start from Jan 1, 2020

cases.s <- c(rep(0,times=20),diff(data$cases)/max(diff(data$cases))*scale)

## angle
theta <- seq(2*pi/365, by=2*pi/365, length=length(cases.s))

## radii based on Archimedean spiral
rr <- a+b*theta
rr.c <-a+cases.s + b*theta
rr.1k <- a+100000/max(diff(data$cases))*scale + b*theta


## changing to cartesian coordinates
xx <- rr*sin(theta)
yy <- rr*cos(theta)

xx.c <- rr.c*sin(theta)
yy.c <- rr.c*cos(theta)

xx.1k <- rr.1k*sin(theta)
yy.1k <- rr.1k*cos(theta)
 
 
## plot 
 
pdf(width=9,height=12,file='covid_spiral.pdf') 
par(mar=c(0,0,0,0))
plot(range(-9,9), range(-9,15), type='n', axes=F, xlab='', ylab='')
abline(v=0,lwd=2,col='grey50',lty=2)
abline(h=0,lwd=2,col='grey50',lty=2)


points(xx.c[-(1:20)],yy.c[-(1:20)],pch=19, col=2, cex=0.5)

for(j in 21:length(theta)){
	segments(xx[j-1],yy[j-1],xx[j],yy[j],col=1,lwd=2)
	segments(xx[j],yy[j],xx.c[j],yy.c[j],col=2)
	segments(xx.1k[j-1],yy.1k[j-1],xx.1k[j],yy.1k[j],col="grey70",lty=2,lwd=2)	
}

text(0,15,"January", cex=1)
text(9,0,"April", cex=1)
text(0,-9,"July", cex=1)
text(-9,0,"October", cex=1)

text(0,a-.1,"2020", cex=1.5)
text(0,a-.1+ 0.5*2*pi,"2021", cex=1.5)
text(0,a-.1 + 0.5*4*pi,"2022", cex=1.5)

text(2,a+.2 + 0.5*4*pi + 0.55,"100K", cex=1,col="grey70")

dev.off()

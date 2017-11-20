# define wald density function

dwald <- function(x,alpha,gamma){
  return((alpha/(sqrt(2*pi*x^3)))*exp(-(alpha-gamma*x)^2/(2*x)))
}

#########################################
# plot multiple curves with same mean
#########################################

x<-seq(0,2,0.01)
alpha=2
# I want mu=750 for each curve.  Note that mu = alpha/gamma

y<-dwald(x,alpha=alpha, gamma=alpha/.75)

plot(x, y, type = "l", lwd = 2, main = "", ylab = "", 
     xlab = "", axes = FALSE, ylim = c(0,4), xlim = c(0, 2), cex.lab = 1.2, 
     font.lab = 2, cex.axis = 1.2, col = "black", bty = "n")

abline(v=0.75,col="red",lty=2,lwd=2)
text(0.75,3.5,"mean = 0.75 sec",pos=4,cex=1.2)
axis(1)
axis(2)
par(las = 0)
mtext("RT (sec)", side = 1, line = 2.5, cex = 1.5)
mtext("Density", side = 2, line = 2.8, cex = 1.5)

alpha=1.5
y<-dwald(x,gamma=alpha/.75,alpha=alpha)
lines(x,y,lwd=2)

alpha=3
y<-dwald(x,gamma=alpha/.75,alpha=alpha)
lines(x,y,lwd=2)

alpha=4.5
y<-dwald(x,gamma=alpha/.75,alpha=alpha)
lines(x,y,lwd=2)

# export as PDF with dimensions 8in (horizontal) by 6in (vertical)


##########################################################
# plot effects of varying location/scale/shape parameters
##########################################################

par(mfrow=c(1,3))
x=seq(0,2,0.01)
plot(x,dwald(x-0.2, alpha=1, gamma=3), 
     type="l", xlab="", ylab="", lwd=2, cex.axis=1.3, cex.main=1.5,
     main=expression(paste("Location ", psi)))     
lines(x, dwald(x-0.7, alpha=1, gamma=3), lwd=2, col="red")

plot(x,dwald(x-0.2, alpha=1, gamma=3), 
     type="l", xlab="", ylab="", lwd=2, cex.axis=1.3, cex.main=1.5,
     main=expression(paste("Scale ", alpha)))
lines(x, dwald(x-0.2, alpha=2, gamma=3), lwd=2, col="red")

plot(x,dwald(x-0.2, alpha=1, gamma=3), 
     type="l", xlab="", ylab="", lwd=2, cex.axis=1.3, cex.main=1.5,
     main=expression(paste("Shape ", gamma)))
lines(x, dwald(x-0.2, alpha=1, gamma=1), lwd=2, col="red")

# export as PDF with dimensions 8in (horizontal) by 3in (vertical)


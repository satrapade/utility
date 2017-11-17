
grid_plot<-function(data,show_grid=TRUE){
  n<-length(data)
  xw<-rep(1,n)
  yw<-rep(1,n)
  x<-rep(1:ceiling(sqrt(n)),times=ceiling(sqrt(n)))[1:n]
  y<-rep(1:ceiling(sqrt(n)),each=ceiling(sqrt(n)))[1:n]
  xplot<-mapply(function(d,xc,xw)rescale(seq_along(d),c(xc-xw*0.49,xc+xw*0.49)),d=data,xc=x,xw=xw,SIMPLIFY=FALSE)
  yplot<-mapply(function(d,yc,yw)rescale(d,c(yc-yw*0.49,yc+yw*0.49)),d=data,yc=y,yw=yw,SIMPLIFY=FALSE)
  plot(x=c(0,ceiling(sqrt(n))+1),y=c(0,ceiling(sqrt(n))+1),type="n",axes=FALSE,xlab="",ylab="",main="")
  matlines(x=do.call(cbind,xplot),y=do.call(cbind,yplot),col=rep("blue",n),lwd=rep(1,n),lty=1)
  if(show_grid)abline(h=1:(ceiling(sqrt(n))+1)-0.5,v=1:(ceiling(sqrt(n))+1)-0.5)
}






# map XY coordinates to fixed grid points
# for plotting and table layouts
force2grid<-function(
  df,
  col_count=ceiling(sqrt(nrow(df)))+col_slack,
  row_count=ceiling(nrow(df)/col_count)+row_slack,
  col_slack=1,
  row_slack=1,
  table_target=FALSE
){
  m<-matrix("",nrow=row_count,ncol=col_count)
  gx<-as.vector(col(m))
  gy<-as.vector(row(m))
  dfx<-rescale(df$x,range(gx)) #bin(df$x,max(gx))
  dfy<-rescale(df$y,range(gy)) #bin(df$y,max(gy))
  mx<-matrix(dfx,ncol=1)[,rep(1,length(gx)),drop=FALSE]
  my<-matrix(dfy,ncol=1)[,rep(1,length(gx)),drop=FALSE]
  gridx<-matrix(gx,nrow=1)[rep(1,length(dfx)),,drop=FALSE]
  gridy<-matrix(gy,nrow=1)[rep(1,length(dfy)),,drop=FALSE]
  dx<-(mx-gridx)^2
  dy<-(my-gridy)^2
  d<-dx+dy
  t2g <- solve_LSAP(d)
  data.table(
    x=gx[t2g],
    y=gy[t2g]
  )
}


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





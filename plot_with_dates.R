
plot_with_dates<-function(
  x,
  main="",
  ylab="m$",
  divisor=1e6,
  ylim=local({
    if(diff(range(c(min(x),max(x))))>0){
      c(min(x),max(x))
    } else {
      c(mean(x)-1,mean(x)+1)
    }
  }),
  col=rgb(0,0,1,0.25)
){
  plot(x,type="l",lwd=5,col=col,axes=FALSE,xlab="",ylab="",main=main,cex.main=2,ylim=ylim)
  dates<-as.character(as.Date(names(x),format="%Y-%m-%d"),format="%Y-%b-%d")
  at<-seq(1,length(x),length.out = 5)
  labels<-gsub("-","\n",dates)
  axis(1,at=at,labels=labels[at],line=NA,padj=0.75,xlab="",cex.axis=1.5)
  at<-axTicks(2)
  labels<-comma(round(at/divisor,digits=1),digits=1)
  axis(2,at=at,labels=labels,cex.axis=2)
}



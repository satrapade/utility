

vol_pa<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE()) {
  if(is.null(names(x))) 
    good_days <- TRUE 
  else 
    good_days <- !weekdays(as.Date(names(x),format="%Y-%m%-%d")) %in% c("Saturday","Sunday")
  sqrt(252)*sd(drop(x[exclude_zero & good_days]))
}

sharpe<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE() ) {
  if(is.null(names(x))) 
    good_days <- TRUE 
  else 
    good_days <- !weekdays(as.Date(names(x),format="%Y-%m%-%d")) %in% c("Saturday","Sunday")
  round(sqrt(252)*mean(drop(x[exclude_zero & good_days]))/(vol_pa(x)/sqrt(252)),digits=2)   # annualized
}
bps<-function(x,exclude_zero=(x!=0))round(10000*mean(drop(x[exclude_zero])),digits=2)
gain_ratio<-function(x)round(sum(x>0)/sum(x!=0),digits=2)
trade_ratio<-function(x)round(sum(x!=0)/length(x),digits=2)
drawdown<-function(x)round(100*max(cummax(cumsum(x))-cumsum(x)),digits=2)
pnl_stats<-function(x, show_tr = FALSE, show_gr = FALSE){
  if(class(x)=="matrix")if(ncol(x)>1)x<-x[,1]
  ret<-c(ret=round(100*365.25*mean(x),digits=3),vol=round(100*vol_pa(x),digits=2),sharpe=sharpe(x),dd=drawdown(x),mrd=round(meanrd(x),digits=2))
  if(show_tr) ret<-c(ret, trade_ratio=trade_ratio(x))
  if(show_gr) ret<-c(ret, gain_ratio=gain_ratio(x))
  ret
}
pnl_plot<-function(x,...){
  pargs<-as.list(match.call(expand.dots=TRUE))
  if(!"ylab" %in% names(pargs)) ylab<-deparse(substitute(x)) else ylab<-pargs$ylab
  if(!"main" %in% names(pargs)) main<-paste(names(pnl_stats(x)),pnl_stats(x),sep=":",collapse=" ") else main<-pargs$main
  z<-cumsum(x)
  ylim<-c(min(z),max(z))
  if("ylim" %in% names(pargs))ylim=eval(pargs$ylim,parent.frame())
  if("lwd" %in% names(pargs)){lwd<-eval(pargs$lwd,parent.frame())} else lwd<-1
  if("col" %in% names(pargs)){col<-eval(pargs$col,parent.frame())} else col<-"black"
  plot(cumsum(x),type="l",xlab="",ylab=ylab,main=main,axes=FALSE,ylim=ylim,lwd=lwd,col=col)
  if(!is.null(names(x))){
    axis(1,at=seq(1,length(x),length.out=5),labels=names(x)[seq(1,length(x),length.out=5)])
    axis(2)
  } else { axis(1); axis(2)}
}






#
# evaluate an expression over columns in a list of matrices
# and assemble the result into a matrix
#
# pair_pnl<-with_columns(expression({
#  mv*c(tret[-length(tret)],0)  
# }),mv=pair_mv,tret=pair_tret)
#

with_columns<-function(expr,...,MoreArgs=list()){
  mat<-list(...)
  if(length(mat)<1)stop("no inputs")
  if(length(unique(mapply(ncol,mat)))>1){
    print(mapply(ncol,mat))
    stop("unequal column counts")
   }
  if(is.null(MoreArgs$ndx))MoreArgs$ndx<-1:ncol(mat[[1]])
  res<-matrix(0,nrow=nrow(mat[[1]]),ncol=length(MoreArgs$ndx),dimnames=list(NULL,NULL))
  the_expressions<-setNames(lapply(seq_along(mat),function(v)bquote(mat[[.(v)]][,MoreArgs$ndx[i] ])),names(mat))
  the_environment<-c(the_expressions,MoreArgs)
  e1<-as.expression(do.call(substitute,list(expr[[1]],the_environment)))
  for(i in 1:length(MoreArgs$ndx))res[,i]<-eval(e1)
  dimnames(res)<-dimnames(mat[[1]][,MoreArgs$ndx])
  res
}

stopifnot(all(with_columns(expr=expression(x+y),x=matrix(1:100,ncol=10),y=diag(10))==matrix(1:100,ncol=10)+diag(10)))




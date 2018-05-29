
# add arguments with default values to function formals
with_formals<-function(fun,form){
  for(i in setdiff(names(form),"")){
    formals(fun)[[i]]<-form[[i]]
  }
  fun
}

# perform calculation on matrix columns
with_columns<-function(expr,...,MoreArgs=NULL){
  mat<-list(...)
  n<-unique(mapply(ncol,mat))
  if(length(n)>1)stop("unequal column counts")
  df<-lapply(mat,data.table)
  the_fun<-with_formals(function(){},c(structure(rep(list(numeric(0)),length(mat)),.Names=names(mat)),MoreArgs))
  body(the_fun)<-expr
  args<-c(list(FUN=the_fun,SIMPLIFY=FALSE,MoreArgs=MoreArgs),df)
  res0<-do.call(mapply,args)
  res1<-do.call(cbind,res0)
  rownames(res1)<-rownames(mat[[1]])
  res1
}

stopifnot(all(with_columns(expr=expression(x+y),x=matrix(1:100,ncol=10),y=diag(10))==matrix(1:100,ncol=10)+diag(10)))



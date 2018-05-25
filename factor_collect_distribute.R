

# factor 2 permutation matrix
#
# inputs:
#   x vector of factor assigments
#
# output:
#   permutation matrix that "collects" values by factor
#
fac2perm<-function(x)diag(length(unique(x)))[match(x,unique(x)),,drop=FALSE]

# collect factors 
factor_collect_distribute<-function(
  x, # matrix of values
  f, # factor assigment to each column
  fun=identity
){
  pm<-fac2perm(f)
  collect<-x%*%pm
  xform<-apply(collect,2,fun)
  ndx<-match(f,unique(f))
  if(is.null(dim(xform))){
    distribute<-xform[ndx]
  }else{
    distribute<-xform[,ndx]
  }
  return(distribute)
}

stopifnot(all(
  factor_collect_distribute(matrix(1:100,ncol=10),rep(c(0,1),each=5),fun=identity)==
  cbind((1:10)*5+100,(1:10)*5+350)[,rep(c(1,2),each=5)]
))


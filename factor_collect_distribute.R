

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
    distribute<-xform[ndx]
  }
  return(distribute)
}




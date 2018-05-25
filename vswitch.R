
#
# vectorized switch
#

vswitch<-function(values=sin(seq(0,pi,length.out = 100)),categories=sign(seq_along(x)-which.max(x)),...){
  map<-c(...)
  categories<-as.character(categories)
  ndx<-match(categories,names(map))
  structure(
    ifelse(is.na(ndx),get(class(map))(1),map[ndx]),
    .Names=ifelse(is.na(ndx),"missing",names(map)[ndx])
  )
}

stopifnot(all(vswitch(values=1:5,categories=(1:5)%%3,"0"=4,"1"=10)==c(10,0,4,10,0)))

#
# cast data frame to matrix
#
require(data.table)

scrub <- function(x, default = 0)
{
  if(length(x) == 0) return(default)
  x[which(!is.finite(x))] <- default
  return(x)
}


NNcast<-function(
  data,
  i_name="date",
  j_name="id",
  v_name="value",
  fun=sum,
  scrub_fun=function(x)scrub(x,default=0)
){
  i_expr<-parse(text=as.character(i_name))
  j_expr<-parse(text=as.character(j_name))
  v_expr<-parse(text=as.character(v_name))
  i<-as.character(eval(i_expr,envir=data))
  j<-as.character(eval(j_expr,envir=data))
  x<-eval(v_expr,envir=data)
  df<-data.table(i=i,j=j,x=x)[,.(x=fun(x)),keyby="i,j"]
  is<-sort(unique(df$i))
  js<-sort(unique(df$j))
  res<-matrix(
    0,
    nrow=length(is),
    ncol=length(js),
    dimnames = list(is,js)
  )
  i<-match(df$i,rownames(res))
  j<-match(df$j,colnames(res))
  res[cbind(i,j)[!is.na(df$x),]]<-df$x[!is.na(df$x)]
  scrub_fun(res)
}


stopifnot(all(
  NNcast(
    data.table(x=1:10,y=1:10,v=1:10,w=10:1),
    i_name="sprintf('%08d', x)",
    j_name="sprintf('%08d', y)",
    v_name="v"
  )==diag(1:10)
))


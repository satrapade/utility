
require(brotli)
require(base64enc)


compress<-function(x){
  if(all(x=="")&length(x)==1)return("")
  x0<-serialize(object=x,connection=NULL,version=2)
  x1<-brotli_compress(x0)
  x2<-base64encode(x1)
  x2
}

#
decompress<-function(x){
  if(x=="")return("")
  x0<-base64decode(x)
  x1<-brotli_decompress(x0)
  x2<-unserialize(x1)
  x2
}





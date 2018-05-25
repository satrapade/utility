
# append some stuff to a function                      
append2fun<-function(fun,stub){
  e<-body(fun)
  i<-length(e)
  e[[i+1]]<-substitute(stub)
  body(fun)<-e
  fun
}




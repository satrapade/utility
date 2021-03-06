require(gsubfn)
require(stringi)

query<-function(statement,db=get("db",parent.frame())){
  q<-try(dbSendQuery(conn=db,statement),silent = TRUE)
  if(any(class(q)=="try-error")){
    cat(as.character(attributes(q)$condition))
    stop(attributes(q)$condition)
  }
  r<-try(dbFetch(q,n=-1),silent=TRUE)
  if(any(class(r)=="try-error")){
    cat(as.character(attributes(r)$condition))
    stop(attributes(r)$condition)
  }
  dbClearResult(q)
  data.table(r)
}

# 
# file include and macro expansion
# 
make_query<-function(
  ..., 
  query_string=paste(readLines(file),collapse="\n"),
  file="bucket_pnl.sql"
){
  no_text<-function(x){
    eval(substitute(x),parent.frame())
    return("")
  }
  R_regex<-"--R{[\\\\ \\n \\t \\r  \\* \\. \\$ \\[ \\] \\( \\) \\: <> \\, \\_ '!\"a-zA-Z0-9=\\-+/% ]*}--"
  error_regex<-"--error{[\\\\ \\n \\t \\r  \\* \\. \\$ \\[ \\] \\( \\) \\: <> \\, \\_ '!\"a-zA-Z0-9=\\-+/% ]*}--"
  include_regex<-"--include{[:/ a-zA-Z0-9\\.\\_]+}--"
  e<-list2env(list(...))
  eval_fun<-function(x){
    r_expr_txt<-stri_sub(x,5,-4)
    r_expr<-parse(text=r_expr_txt)
    res<-try(eval(r_expr,envir=e),silent=TRUE)
    if(any(class(res)=="try-error"))return(paste0("--error{",gsub("(\n)|(\")","",as.character(res)),"}--"))
    paste(as.character(res),sep="",collapse="")
  }
  include_fun<-function(x){
    include_file_name<-stri_sub(x,11,-4)
    res<-try(paste(readLines(include_file_name),collapse="\n"),silent=TRUE)
    if(any(class(res)=="try-error"))return(paste0("--error{",gsub("(\n)|(\")","",as.character(res)),"}--"))
    paste(as.character(res),sep="",collapse="")
  }
  res0<-gsubfn(include_regex,include_fun,query_string)
  res1<-gsubfn(R_regex,eval_fun,res0)
  error_env<-new.env()
  error_env$errors<-""
  gsubfn(error_regex,function(x){
    error_env$errors<-c(error_env$errors,x)
  },res1)
  attributes(res1)$values<-as.list(e)
  attributes(res1)$errors<-error_env$errors
  res1
}

stopifnot(make_query(x=1,y=2,query_string = "--R{x+y}--")=="3")
stopifnot(make_query(query_string = "--R{if(!exists('product_id')) product_id<-'8' else product_id}--")=="8")



#
# bloomberg bunctions
#
#
require(stringi)
require(data.table)
require(Matrix)
library(Matrix.utils)
require(R.cache)
require(Rblpapi)
con<-blpConnect()
#dir.create(path="~/.Rcache", showWarnings=F)
#setCacheRootPath(path="./.Rcache")


#
# fetch futures historical tickers
#
futures_historical_tickers<-structure(function(futures_tickers){
  fmc<-c("F"="Jan","G"="Feb","H"="Mar","J"="Apr","K"="May","M"="Jun","N"="Jul","Q"="Aug","U"="Sep","V"="Oct","X"="Nov","Z"="Dec")
  futures<-sort(unique(futures_tickers))  
  f2d<-function(f){
    date_string<-paste0("01-",fmc[stri_sub(f,3,3)],"-201",stri_sub(f,4,4))
    as.character(as.Date(date_string,format="%d-%b-%Y"),format="%Y-%m-%d")
  }
  lookup_table<-do.call(rbind,mapply(function(f){
    ffc<-paste0(stri_sub(f,1,2),"1 Index")
    res<-bds(ffc,"FUT_CHAIN", override=c(CHAIN_DATE=gsub("-","",f2d(f))))
    data.frame(contract=f,historical=res[1,1],row.names=NULL,stringsAsFactors=FALSE)
  },futures,SIMPLIFY=FALSE))
  lookup_table[futures_tickers,"historical"]
},location="eq_ptf_bbg_functionst.R")

memo_calc_futures_static<-structure(function(
  futures
){
  futures_static<-loadCache(key=list(futures))
  if(is.null(futures_static)){
    futures_bbg<-futures_historical_tickers(futures)
    res<-bdp(
      futures_bbg,
      c(
        "SECURITY_TYP",
        "NAME",
        "CRNCY",
        "UNDL_SPOT_TICKER",
        "FUT_CONT_SIZE"
      )
    )[futures_bbg,]
    futures_static<-data.table(
      id=securities$id[match(futures,securities$ticker)],
      ticker=futures,
      bbg=futures_bbg,
      res
    )
    saveCache(futures_static,key=list(futures))
  }
  return(futures_static)
},location="eq_ptf_bbg_functionst.R")


#
# fill a date by ticker matrix with valued from a BDH field
#
populate_history_matrix<-function(
  tickers,
  field,
  start,
  end,
  overrides=NULL
){
  all_dates<-as.character(seq(
    min(as.Date(start),as.Date(end)),
    max(as.Date(start),as.Date(end)),
    by=1
  ),format="%Y-%m-%d")
  bbg_res<-Rblpapi::bdh(
    tickers,
    field,
    as.Date(min(all_dates)),
    as.Date(max(all_dates)),
    overrides=overrides
  )
  df<-data.frame(
    ticker=do.call(c,mapply(rep,names(bbg_res),mapply(nrow,bbg_res),SIMPLIFY=FALSE)),
    date=as.character(do.call(c,mapply("[[",bbg_res,MoreArgs=list("date"),SIMPLIFY=FALSE)),format="%Y-%m-%d"),
    value=do.call(c,mapply("[[",bbg_res,MoreArgs=list(field),SIMPLIFY=FALSE)),
    row.names=NULL,
    stringsAsFactors=FALSE
  )
  #return(df)
  m<-sparseMatrix(#
    i=match(df$date,all_dates),
    j=match(df$ticker,tickers),   
    x=df$value,
    dims=c(length(all_dates),length(tickers)),
    dimnames=list(all_dates,tickers)
  )
  return(m)
}

memo_populate_history_matrix<-function(
  ref_matrix=local({
    warning("memo_populate_history_matrix: \"ref_matrix\" argument not supplied, using default",call.=FALSE)
    dMcast(data=calc_bucket_trades(securities=securities),formula=date~constituent,value.var="amount")
  }),
  bbg_field="PX_LAST",
  bbg_overrides=NULL,
  post_fetch_fun=replace_zero_with_last,
  force=FALSE,
  securities=if(exists("securities",parent.frame()))local({
    warning("memo_populate_history_matrix: \"securities\" not supplied, found in parent frame",call.=FALSE)
    get("securities",parent.frame())
  }) else local({
    warning("memo_populate_history_matrix: \"securities\" not found, reading default",call.=FALSE)
    read_securities()
  }),
  verbose=FALSE,
  from_file=NULL,
  to_file=NULL
){
  if(!is.null(from_file)){
    x<-load_matrix(from_file)
    if(!all(grepl("^buid",colnames(x)))){
      colnames(x)<-paste0("buid",gsub("^buid","",colnames(x)))
    }
    return(x)
  }
  if(!all(grepl("^buid",colnames(ref_matrix)))){
    warning("memo_populate_history_matrix: invalid \"ref_matrix\" argument",call.=FALSE)
    return(NULL)
  }
  the_key<-list(dimnames(ref_matrix),bbg_field,bbg_overrides,"memo_populate_history_matrix")
  cached_value<-loadCache(key=the_key)
  if(!is.null(cached_value))if(!force){
    if(verbose)warning("memo_populate_history_matrix: using cached value for ",paste(c(bbg_field,bbg_overrides),collapse=", "),".",call.=FALSE)
    if(!is.null(to_file))write.csv(as.matrix(cached_value),to_file)
    return(cached_value)
  }
  if(verbose)warning("memo_populate_history_matrix: accessing bloomberg for ",paste(c(bbg_field,bbg_overrides),collapse=", "),".",call.=FALSE)
  tickers<-gsub("^buid","/buid/ ",colnames(ref_matrix))
  res<-populate_history_matrix(
    tickers,
    bbg_field,
    min(rownames(ref_matrix)),
    max(rownames(ref_matrix)),
    overrides=bbg_overrides
  )
  cached_value<-apply(res,2,post_fetch_fun)
  dimnames(cached_value)<-dimnames(ref_matrix)
  saveCache(cached_value,key=the_key)
  if(!is.null(to_file))write.csv(as.matrix(cached_value),to_file)
  cached_value
}


memo_populate_sheet_history_matrix<-function(
  ref_matrix=local({
    warning("memo_populate_sheet_history_matrix: \"ref_matrix\" argument not supplied, stopping",call.=FALSE)
    stop
  }),
  bbg_field="PX_LAST",
  bbg_overrides=NULL,
  post_fetch_fun=replace_zero_with_last,
  force=FALSE,
  verbose=FALSE
){
  if(any(ticker_class(colnames(ref_matrix))=="nomatch")){
    warning("memo_populate_sheet_history_matrix: invalid \"ref_matrix\" argument",call.=FALSE)
    return(NULL)
  }
  the_key<-list(dimnames(ref_matrix),bbg_field,bbg_overrides,"memo_populate_sheet_history_matrix")
  cached_value<-loadCache(key=the_key)
  if(!is.null(cached_value))if(!force){
    if(verbose)warning(
      "memo_populate_sheet_history_matrix: using cached value for ",paste(c(bbg_field,bbg_overrides),collapse=", "),".",call.=FALSE
    )
    return(cached_value)
  }
  if(verbose)warning(
    "memo_populate_sheet_history_matrix: accessing bloomberg for ",paste(c(bbg_field,bbg_overrides),collapse=", "),".",call.=FALSE
  )
  tickers<-colnames(ref_matrix)
  res<-populate_history_matrix(
    tickers,
    bbg_field,
    min(rownames(ref_matrix)),
    max(rownames(ref_matrix)),
    overrides=bbg_overrides
  )
  cached_value<-apply(res,2,post_fetch_fun)
  dimnames(cached_value)<-dimnames(ref_matrix)
  attributes(cached_value)$bbg_overrides=bbg_overrides
  attributes(cached_value)$bbg_field=bbg_field
  attributes(cached_value)$post_fetch_fun=post_fetch_fun
  saveCache(cached_value,key=the_key)
  cached_value
}




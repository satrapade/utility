require(stringi)
require(readxl)
require(clue)

#
scrub<-function(x,default=0){
  if(length(x)==0)return(default)
  x[which(!is.finite(x))]<-default
  x
}
#
nz<-function(x,tol=1e-12){
  if(all(abs(x)<tol))return(0)
  x[abs(x)>tol]
}

#
replace_zero_with_last<-function(x,a=x!=0,tol=1e-10){
  if(all(abs(x)<tol))return(x)
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

# dMcast makes ugly colnames
rename_colnames<-function(x,pattern,replacement){
  colnames(x)<-gsub(pattern,replacement,colnames(x))
  x
}

# size matrix to cover a universe
row_size2universe<-function(x,u){
  m<-matrix(0,nrow=length(u),ncol=ncol(x),dimnames=list(u,colnames(x)))
  m[rownames(x),]<-x
  m
}

#
pair2pm<-function(o){
  x<-NULL
  if(is.character(o))x<-o
  if(is.null(x)&!is.null(names(o)))x<-names(o)
  if(is.null(x)&!is.null(colnames(o)))x<-colnames(o)
  if(is.null(x))x<-as.character(o)
  gsub("[0-9]{1,3}$","",x)
}

#
weights_for_target_vol<-function(pnl_matrix,target_vol=4){
  strat_sd<-apply(pnl_matrix,2,sd)
  rp_weights<-structure(
    diag(ifelse(strat_sd>0,mean(strat_sd)/strat_sd,0)),
    dimnames=list(colnames(pnl_matrix),colnames(pnl_matrix))
  )
  rp_pnl_matrix<-pnl_matrix%*%rp_weights
  ratio<-target_vol/pnl_stats(rowSums(rp_pnl_matrix))["vol"]
  res<-diag(ratio*rp_weights)
  attributes(res)$pnl<-ratio*rp_pnl_matrix
  res
}


# df2matrix<-function(
#   df
# ){
#   k<-sqrt(floor(sqrt(nrow(df))+1)^2)
#   m<-matrix("",nrow=k,ncol=k)
#   gx<-as.vector(col(m))
#   gy<-as.vector(row(m))
#   dfx<-rescale(df$x,range(gx))
#   dfy<-rescale(df$y,range(gy))
#   mx<-matrix(dfx,ncol=1)[,rep(1,length(gx))]
#   my<-matrix(dfy,ncol=1)[,rep(1,length(gx))]
#   gridx<-matrix(gx,nrow=1)[rep(1,length(dfx)),]
#   gridy<-matrix(gy,nrow=1)[rep(1,length(dfy)),]
#   dx<-(mx-gridx)^2
#   dy<-(my-gridy)^2
#   d<-dx+dy
#   t2g <- solve_LSAP(d)
#   m[cbind(nrow(m)-gy[t2g]+1,gx[t2g])]<-df$text
#   m
# }

# fast exponential moving average
calc_ama<-function(x,n){
  x0<-c(sum(x[1:n]),x[(n+1):length(x)])/n
  res<-filter(x0,(n-1)/n,method="recursive",sides=1)
  as.numeric(res)
}

# fast rsi
rsi<-function(x,n){
  dimnames(x)<-NULL
  abs_x<-abs(x)
  xup<-(abs_x+x)/2 # same as pmax(x,0) but faster, no dimnames
  xdn<-(abs_x-x)/2 # same as pmax(-x,0) but faster
  up<-apply(xup,2,calc_ama,n)
  dn<-apply(xdn,2,calc_ama,n)
  rsi<-100-100/(1+up/dn)
  rsi[which(!is.finite(rsi),arr.ind=TRUE)]<-100
  rsi
}

#
make_date_range<-function(
  start="2017-06-01",
  end="2017-06-30",
  leading_days=0,
  trailing_days=0
){
  fmt="%Y-%m-%d"
  date_seq<-seq(from=as.Date(start,format=fmt)-leading_days,to=as.Date(end,format=fmt)+trailing_days,by=1)
  as.character(date_seq,format=fmt)
}

# pattern-matched ticker classification
ticker_class<-function(x){
  
  x_trim<-stri_trim(x)
  x_upper<-toupper(x_trim)
  x_lower<-tolower(x_trim)
  
  all_matches<-list(
    list(class="date",match=grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",x_trim)),
    list(class="equity",match=grepl("^[A-Z0-9/]+\\s+[A-Z]{2,2}$",x_trim)),
    list(class="equity",match=grepl("^[A-Z0-9/]+\\s+[A-Z]{2,2}\\sEquity$",x_trim)),
    list(class="equity",match=grepl("^[a-z0-9/]+\\s+[a-z]{2,2}\\sequity$",x_lower)),
    list(class="index",match=grepl("^[A-Z0-9]{3,20}$",x_trim)),
    list(class="index",match=grepl("^[A-Z0-9]{3,20}\\sIndex$",x_trim)),
    list(class="cix",match=grepl("^\\.[A-Z0-9]{3,20}\\sIndex$",x_trim)),
    list(class="index",match=grepl("^[a-z0-9]{3,20}\\sindex$",x_lower)),
    list(class="index",match=grepl("^[A-Z0-9]+\\s+[A-Z]{2}\\s+Index$",x_trim)),
    list(class="future",match=grepl("^([A-Z]{2,4}|[A-Z]\\s)([FGHJKMNQUVXZ]\\d{1,2})$",x_trim)),
    list(class="future",match=grepl("^([A-Z]{2,4}|[A-Z]\\s)([FGHJKMNQUVXZ]\\d{1,2}) Index$",x_trim)),
    list(class="future",match=grepl("^([A-Z]{2,4}|[A-Z]\\s)([FGHJKMNQUVXZ]\\d{1,2})$",x_upper)),
    list(class="future",match=grepl("^([A-Z]{2,4}|[A-Z]\\s)([FGHJKMNQUVXZ]\\d{1,2}) INDEX$",x_upper)),
    list(class="bbgisin",match=grepl("^/isin/[A-Z]{2}[A-Z0-9]{10}$",x_trim)),
    list(class="isin",match=grepl("^[A-Z]{2}[A-Z0-9]{10}$",x_trim)),
    list(class="equity_option",match=grepl("^[A-Z0-9]{1,10} [A-Z0-9]{2} [0-9]{2}/[0-9]{2}/[0-9]{2} [CP]{1}[\\.0-9]{1,5}$",x_trim)),
    list(class="otc_equity_option",match=grepl("^OTC-[A-Z0-9]{1,10} [A-Z0-9]{2} [0-9]{2}/[0-9]{2}/[0-9]{2} [CP]{1}[\\.0-9]{1,5}$",x_trim)),
    list(class="index_option",match=grepl("^[A-Z0-9]{1,10} [0-9]{2}/[0-9]{2}/[0-9]{2} [CP]{1}[\\.0-9]{1,5}$",x_trim)),
    list(class="index_option",match=grepl("^[A-Z0-9]{1,10} [0-9]{1,2} [CP]{1}[\\.0-9]{1,5}$",x_trim)),
    list(class="otc_index_option",match=grepl("^OTC-[A-Z0-9]{1,10} [0-9]{2}/[0-9]{2}/[0-9]{2} [CP]{1}[\\.0-9]{1,5}$",x_trim))
  )
  all_classes<-do.call(cbind,mapply(function(m)ifelse(m$match,m$class,"nomatch"),all_matches,SIMPLIFY=FALSE))
  ticker_patterns<-apply(all_classes,1,function(a)paste(sort(unique(a)),collapse="|"))
  
  ticker_patterns
}

#fn<-"duke_exposure.csv"
load_matrix<-function(fn,row_names=TRUE){
  x<-fread(fn)
  if(!row_names)return(as.matrix(x))
  m<-as.matrix(x[,-1])
  rownames(m)<-x[[1]]
  return(m)
}




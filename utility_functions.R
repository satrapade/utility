#
#
#

.satrapade_functions_version="Version 1.00"
require(RcppRoll)
require(stringi)
require(data.table)
require(Matrix)
require(Matrix.utils)
require(scales)
options(stringsAsFactors=FALSE)
require(R.cache)
require(digest)
require(timeDate)
require(brotli)
require(base64enc)
require(RSQLite)

# fast data dump/pump functions, using binary little-endian connections
fdump<-function(object,filename){
  conn<-file(filename,open="wb")
  serialize(object,conn,ascii=FALSE,xdr=FALSE)
  close(conn)
}
fpump<-function(filename){
  unserialize(readBin(filename,"raw",file.info(filename)[["size"]]))
}

#
load_matrix<-function(fn,row_names=TRUE){
  x<-fread(fn)
  if(!row_names)return(as.matrix(x))
  m<-as.matrix(x[,-1])
  rownames(m)<-x[[1]]
  return(m)
}

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

#
query<-function(statement,db=get("db",parent.frame())){
  q<-dbSendQuery(conn=db,statement)
  r<-dbFetch(q,n=-1)
  dbClearResult(q)
  r
}




# dMcast makes ugly colnames
rename_colnames<-function(x,pattern,replacement){
  colnames(x)<-gsub(pattern,replacement,colnames(x))
  x
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
stopifnot(all(make_date_range("2017-01-01","2017-01-03")==c("2017-01-01","2017-01-02","2017-01-03")))

#
nz<-function(x,tol=1e-12){
  if(all(abs(x)<tol))return(0)
  x[abs(x)>tol]
}
stopifnot(nz(c(0,1,0))==1)

#
replace_zero_with_last<-function(x,a=x!=0){
  if(all(abs(x)<1e-10))return(x)
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}
stopifnot(all(replace_zero_with_last(c(0,0,1,2,3,0,0,4,5,6,0,0))==c(1,1,1,2,3,3,3,4,5,6,6,6)))

replace_blank_with_last<-function(x){
  x_upper<-toupper(x)
  x_edges<-c(
    x_upper[1]!="",
    head(x_upper,-1)!=tail(x_upper,-1) & nchar(tail(x_upper,-1))>0
  )
  x_all<-x_upper[x_edges]
  x_i<-findInterval(seq_along(x_edges),which(x_edges))
  x_all[pmax(x_i,1)]
}

#
bin<-function(x,n=10)findInterval(x,quantile(x,seq(0,1,length.out=n+1)),rightmost.closed=TRUE)

rec<-function(x)seq_along(x)-which(c(TRUE,tail(x,-1)>0))[cumsum(c(TRUE,tail(x,-1)>0))]
meanrd<-function(x)mean(rec(cummax(cumsum(x))==cumsum(x)))
maxrd<-function(x)max(rec(cummax(cumsum(x))==cumsum(x)))

# map XY coordinates to fixed grid points
# for plotting and table layouts
force2grid<-function(
  df,
  col_count=ceiling(sqrt(nrow(df)))+col_slack,
  row_count=ceiling(nrow(df)/col_count)+row_slack,
  col_slack=1,
  row_slack=1,
  table_target=FALSE
){
  m<-matrix("",nrow=row_count,ncol=col_count)
  gx<-as.vector(col(m))
  gy<-as.vector(row(m))
  dfx<-rescale(df$x,range(gx)) #bin(df$x,max(gx))
  dfy<-rescale(df$y,range(gy)) #bin(df$y,max(gy))
  mx<-matrix(dfx,ncol=1)[,rep(1,length(gx)),drop=FALSE]
  my<-matrix(dfy,ncol=1)[,rep(1,length(gx)),drop=FALSE]
  gridx<-matrix(gx,nrow=1)[rep(1,length(dfx)),,drop=FALSE]
  gridy<-matrix(gy,nrow=1)[rep(1,length(dfy)),,drop=FALSE]
  dx<-(mx-gridx)^2
  dy<-(my-gridy)^2
  d<-dx+dy
  t2g <- solve_LSAP(d)
  data.table(
    x=gx[t2g],
    y=gy[t2g]
  )
}

safe_cor<-function(strat,factor){
  if(any(is.na(c(strat,factor))))return(0)
  if(any(is.na(c(sd(strat),sd(factor)))))return(0)
  if(any(c(sd(strat),sd(factor))==0))return(0)
  cor(strat,factor)
}

safe_beta<-function(strat,factor){
  if(any(is.na(c(strat,factor))))return(0)
  if(any(is.na(c(sd(strat),sd(factor)))))return(0)
  if(any(c(sd(strat),sd(factor))==0))return(0)
  cov(strat,factor)/var(factor)
}

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


vol_pa<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE()) {
  if(is.null(names(x))) 
    good_days <- TRUE 
  else 
    good_days <- !weekdays(as.Date(names(x),format="%Y-%m%-%d")) %in% c("Saturday","Sunday")
  sqrt(252)*sd(drop(x[exclude_zero & good_days]))
}

sharpe<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE() ) {
  if(is.null(names(x))) 
    good_days <- TRUE 
  else 
    good_days <- !weekdays(as.Date(names(x),format="%Y-%m%-%d")) %in% c("Saturday","Sunday")
  round(sqrt(252)*mean(drop(x[exclude_zero & good_days]))/(vol_pa(x)/sqrt(252)),digits=2)   # annualized
}
bps<-function(x,exclude_zero=(x!=0))round(10000*mean(drop(x[exclude_zero])),digits=2)
gain_ratio<-function(x)round(sum(x>0)/sum(x!=0),digits=2)
trade_ratio<-function(x)round(sum(x!=0)/length(x),digits=2)
drawdown<-function(x)round(100*max(cummax(cumsum(x))-cumsum(x)),digits=2)
pnl_stats<-function(x, show_tr = FALSE, show_gr = FALSE){
  if(class(x)=="matrix")if(ncol(x)>1)x<-x[,1]
  ret<-c(ret=round(100*365.25*mean(x),digits=3),vol=round(100*vol_pa(x),digits=2),sharpe=sharpe(x),dd=drawdown(x),mrd=round(meanrd(x),digits=2))
  if(show_tr) ret<-c(ret, trade_ratio=trade_ratio(x))
  if(show_gr) ret<-c(ret, gain_ratio=gain_ratio(x))
  ret
}
pnl_plot<-function(x,...){
  pargs<-as.list(match.call(expand.dots=TRUE))
  if(!"ylab" %in% names(pargs)) ylab<-deparse(substitute(x)) else ylab<-pargs$ylab
  if(!"main" %in% names(pargs)) main<-paste(names(pnl_stats(x)),pnl_stats(x),sep=":",collapse=" ") else main<-pargs$main
  z<-cumsum(x)
  ylim<-c(min(z),max(z))
  if("ylim" %in% names(pargs))ylim=eval(pargs$ylim,parent.frame())
  if("lwd" %in% names(pargs)){lwd<-eval(pargs$lwd,parent.frame())} else lwd<-1
  if("col" %in% names(pargs)){col<-eval(pargs$col,parent.frame())} else col<-"black"
  plot(cumsum(x),type="l",xlab="",ylab=ylab,main=main,axes=FALSE,ylim=ylim,lwd=lwd,col=col)
  if(!is.null(names(x))){
    axis(1,at=seq(1,length(x),length.out=5),labels=names(x)[seq(1,length(x),length.out=5)])
    axis(2)
  } else { axis(1); axis(2)}
}

plot_with_dates<-function(
  x,
  main="",
  ylab="m$",
  divisor=1e6,
  ylim=local({
    if(diff(range(c(min(x),max(x))))>0){
      c(min(x),max(x))
    } else {
      c(mean(x)-1,mean(x)+1)
    }
  }),
  col=rgb(0,0,1,0.25)
){
  plot(x,type="l",lwd=5,col=col,axes=FALSE,xlab="",ylab="",main=main,cex.main=2,ylim=ylim)
  dates<-as.character(as.Date(names(x),format="%Y-%m-%d"),format="%Y-%b-%d")
  at<-seq(1,length(x),length.out = 5)
  labels<-gsub("-","\n",dates)
  axis(1,at=at,labels=labels[at],line=NA,padj=0.75,xlab="",cex.axis=1.5)
  at<-axTicks(2)
  labels<-comma(round(at/divisor,digits=1),digits=1)
  axis(2,at=at,labels=labels,cex.axis=2)
}

# moving average of previous n elements : 0 for first n-1 elements
ma<-function(x,n,f=identity){
  res<-as.numeric(filter(f(x),rep(1/n,n),method="convolution",sides=1,circular=FALSE)); ifelse(is.na(res),0,res)
}

rolling_beta<-function(strat,hedge,w) {
  var_hedge<-roll_var(hedge,w)
  cov_strat_hedge<-(w/(w-1))*(roll_mean(strat*hedge,w)-roll_mean(strat,w)*roll_mean(hedge,w))
  beta<-ifelse(var_hedge>0,cov_strat_hedge/var_hedge,0)
  beta
}


# shift forward if n +ve , backward if n -ve.
shift<-function(a,n=1,filler=0){
  x<-switch(class(a),matrix=a,matrix(a,ncol=1,dimnames=list(names(a),NULL)))
  if(n==0)return(x)
  if(n>0){
    rbind(matrix(filler,ncol=ncol(x),nrow=n),head(x,-n)) 
  } else {
    rbind(tail(x,n),matrix(filler,ncol=ncol(x),nrow=abs(n)))
  }
}


# replace non-finite elements in x with zeroes
scrub<-function(x){
  if(length(x)==0)return(0)
  x[which(!is.finite(x))]<-0
  x
}

pnl_matrix<-function(perf, digits = 2){
  month_map<-c("01"="Jan","02"="Feb","03"="Mar","04"="Apr","05"="May","06"="Jun","07"="Jul","08"="Aug","09"="Sep","10"="Oct","11"="Nov","12"="Dec")
  perf_dates<-structure(do.call(rbind,strsplit(names(perf),"-")),dimnames=list(names(perf),c("Year","Month","Day")))
  perf_dates[,"Month"]<-month_map[perf_dates[,"Month"]]
  perf_years  <- sort(unique(perf_dates[,"Year"]))
  perf_months <- month_map
  res<-structure(
    outer(perf_years,perf_months,function(i_vec,j_vec)mapply(function(i,j){
      perf_ndx <- perf_dates[,"Year"]==i & perf_dates[,"Month"]==j
      if(sum(perf_ndx)==0)return(NA)
      prod(perf[perf_ndx]+1)-1
    },i_vec,j_vec)),
    dimnames=list(perf_years,perf_months)
  )
  round(cbind(res,Year=apply(res,1,function(r)prod(r[!is.na(r)]+1)-1))*100,digits=2)
}



make_matrix<-function(x,fun=function(x)x,empty_value=NA){
  month_map<-c(
    "01"="Jan","02"="Feb","03"="Mar","04"="Apr","05"="May","06"="Jun",
    "07"="Jul","08"="Aug","09"="Sep","10"="Oct","11"="Nov","12"="Dec"
  )
  x_dates<-structure(do.call(rbind,strsplit(names(x),"-")),dimnames=list(names(x),c("Year","Month","Day")))
  x_dates[,"Month"]<-month_map[x_dates[,"Month"]]
  x_years  <- sort(unique(x_dates[,"Year"]))
  x_months <- month_map
  res<-structure(
    outer(x_years,x_months,function(i_vec,j_vec)mapply(function(i,j){
      x_ndx <- x_dates[,"Year"]==i & x_dates[,"Month"]==j
      if(sum(x_ndx)==0)return(empty_value)
      fun(x[x_ndx])
    },i_vec,j_vec)),
    dimnames=list(x_years,x_months)
  )
  cbind(res,Year=apply(res,1,function(r)fun(r[!is.na(r)])))
}

user_id<-paste(system("whoami",TRUE),"_",system("hostname",TRUE),sep="")

# find which environment contains a name
where<-function (name, env = parent.frame()) {
  if(!is.character(name))return(NULL)
  if(length(name)!=1)return(NULL)
  if(class(env)!="environment")return(NULL)
  if (identical(env, emptyenv())) return(NULL)
  if (exists(name, env, inherits = FALSE)) return(env)
  where(name, parent.env(env))
}

# append some stuff to a function                      
append2fun<-function(fun,stub){
  e<-body(fun)
  i<-length(e)
  e[[i+1]]<-substitute(stub)
  body(fun)<-e
  fun
}


########
#
#
list2data.frame<-function(x){
  l2e<-expression({data.frame(
    names=ls(sorted=FALSE),
    values=mapply(get,ls(sorted=FALSE),MoreArgs=list(envir=environment())),
    stringsAsFactors=FALSE,
    row.names=NULL
  )})
  eval(l2e,as.list(x))
}

###################
#
# repeat rows, columns
# to make a matrix
#
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

apply_column<-function(x,f){
  y<-apply(x,2,f)
  rownames(y)<-tail(rownames(x),nrow(y))
  y
}

apply_row<-function(x,f){
  y<-t(apply(x,1,f))
  rownames(y)<-tail(rownames(x),nrow(y))
  colnames(y)<-colnames(x)
  y
}

#
shift_matrix<-function(m,n,filler=0){
  if(n==0)return(m)
  if(n>0){
    sm<-rbind(
      matrix(filler,ncol=ncol(m),nrow=n),
      m[head(1:nrow(m),-n),,drop=FALSE]
    )
    dimnames(sm)<-dimnames(m)
    return(sm)
  }
  if(n<0){
    sm<-rbind(
      m[tail(1:nrow(m),n),,drop=FALSE],
      matrix(filler,ncol=ncol(m),nrow=abs(n))
    )
    dimnames(sm)<-dimnames(m)
    return(sm)
  }
  stop("invalid shift")
}

#
matrix_indices<-function(x)cbind(
  rep(1:nrow(x),times=ncol(x)),
  rep(1:ncol(x),each=nrow(x))
)

fast_col_apply<-function(x,fun)structure(
  apply(structure(as.matrix(x),dimnames=NULL),2,fun),
  dimnames=dimnames(x)
)


# 
plot2matrix<-function(
  y,
  x=seq_along(y),
  col="red",
  cex=0.25,
  type="p",
  lwd=1,
  width=50,
  height=50
){
  fig <- image_graph(width=width, height=height, res = 96)
  par(mai=c(0,0,0,0))
  plot(
    rescale(x),
    rescale(y),
    pch=19,
    cex=cex,
    lwd=lwd,
    col=col,
    type=type,
    axes=FALSE,main="",xlab="",ylab=""
  )
  dev.off()
  as.matrix(as.raster(tail(fig,1)))
}
                       
                       
#
# image
#
make_image<-function(code,size=256){
  fig <- image_graph(width=size, height=size, res = 96)
  par(mai=c(0,0,0,0))
  try_code <- try(eval(code,envir = parent.frame()))
  dev.off()
  if(class(try_code)=="try-error")stop("plot error")
  fig
}

pack_image <- function(fig,data,pword="password"){
  nonce=random(24)
  payload=brotli_compress(serialize(list(
    timestamp=as.character(Sys.time()),
    sysinfo=Sys.info(),
    sessioninfo=sessionInfo(),
    wd=getwd(),
    md5=digest(data),
    size=as.integer(object.size(data)),
    data=data
  ),NULL))
  raw_data<-serialize(list(
    nonce=nonce,
    payload=data_encrypt(serialize(payload,NULL),hash(charToRaw(pword)),nonce)
  ),NULL)
  bits<-c(
    as.integer(intToBits(19680805L)), # magic number
    as.integer(intToBits(length(raw_data))), # length of data
    as.integer(rawToBits(raw_data)) # the data
  )
  k<-round(max((floor(sqrt(length(bits)+1024))+1),128),digits=0)
  if (k>4096)stop("large data")
  img<-image_scale(image_read(fig[[1]]),paste0(k,"x",k))[[1]]
  i <- seq_along(bits)
  r <- as.raw(img)
  r[i] <- as.raw(bitops::bitOr(bitops::bitAnd(r[i], 254L),bits))
  img[,,]<-r
  new_img<-image_convert(image_read(img),"png")
  return(new_img)
}

unpack_image<-function(img,pword="password"){
  r<-as.raw(img[[1]])
  if(packBits(as.integer(bitAnd(r[1:32],1L)),type="integer")!=19680805L)stop("no data")
  len<-packBits(as.integer(bitAnd(r[33:64],1L)),type="integer")
  if(len<0|len>(4096*4096))stop("bad length")
  b<-packBits(as.integer(bitAnd(r[seq_len(8*len)+64],1L)),type="raw")
  w<-unserialize(as.raw(b))
  p<-unserialize(data_decrypt(w[[2]],hash(charToRaw(pword)),w[[1]]))
  res<-unserialize(brotli_decompress(p))
  c(res,list(packed_length=length(b),unpacked_length=as.integer(object.size(res))))
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
    list(class="otc_index_option",match=grepl("^OTC-[A-Z0-9]{1,10} [0-9]{2}/[0-9]{2}/[0-9]{2} [CP]{1}[\\.0-9]{1,5}$",x_trim)),
    list(class="forex",match=grepl("^[A-Z]{3,20}\\sCurncy$",x_trim))
  )
  all_classes<-do.call(cbind,mapply(function(m)ifelse(m$match,m$class,"nomatch"),all_matches,SIMPLIFY=FALSE))
  ticker_patterns<-apply(all_classes,1,function(a)paste(sort(unique(a)),collapse="|"))
  
  ticker_patterns
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
assign_color<-function(x,prefix="",col_scheme=rainbow,col_alpha=0.33){
  items<-sort(unique(x))
  col<-col_scheme(length(items),alpha=col_alpha)
  res<-data.table(
    item=x,
    name=paste0(prefix,gsub("[\\.]+","",make.names(toupper(x)))),
    r_color=col[match(x,items)]
  )[,
    c("latex_col","latex_col_def"):=list(
      latex_col=stri_sub(gsub("^#","",r_color),1,6),
      latex_col_def=paste0("\\definecolor{",name,"}{HTML}{",stri_sub(gsub("^#","",r_color),1,6),"}\n")
    )
  ]
  res
}
                        

                         
                         

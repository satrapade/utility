require(stringi)
require(readxl)

#
scrub<-structure(function(x,default=0){
  if(length(x)==0)return(default)
  x[which(!is.finite(x))]<-default
  x
},location="eq_ptf_sheet_functions.R")

#
nz<-structure(function(x,tol=1e-12){
  if(all(abs(x)<tol))return(0)
  x[abs(x)>tol]
},location="eq_ptf_sheet_functions.R")

#
replace_zero_with_last<-structure(function(x,a=x!=0,tol=1e-10){
  if(all(abs(x)<tol))return(x)
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
},location="eq_ptf_sheet_functions.R")

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
},location="eq_ptf_sheet_functions.R")

# pattern-matched ticker classification
ticker_class<-structure(function(x){
  
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

#
# fetch futures historical tickers
#
futures_historical_tickers<-function(futures_tickers){
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
}
memo_calc_sheet_futures_static<-function(
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
      ticker=futures,
      bbg=futures_bbg,
      res
    )
    saveCache(futures_static,key=list(futures))
  }
  return(futures_static)
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


get_year_sheet_folders<-function(
  year="2017"
){
  location<-paste0(
    "N:/Depts/Global/Absolute Insight/UK Equity/Daily Snap Shot UK Fund/",
    year
  )
  dirs<-list.dirs(location)
  month_folders<-gsub(paste0("^",location,"/"),"",setdiff(dirs,location))
  month_folders
},location="eq_ptf_sheet_functions.R")

make_sheet_name<-structure(function(
 date="2016-06-29"
){
  month_templates=data.table(
    template=c("^JAN","^FEB","^MAR","^APR","^MAY","^JUN","^JUL","^AUG","^SEP","^OCT","^NOV","^DEC"),
    number=sprintf("%02d",1:12)
  )
  date_components<-strsplit(date,"-")[[1]]
  year=date_components[1]
  day=date_components[3]
  month_folders<-get_year_sheet_folders(year)
  month_template<-month_templates[number==date_components[2],template]
  month_match<-grepl(month_template,toupper(month_folders))
  month<-month_folders[month_match]
  location<-paste0(
    "N:/Depts/Global/Absolute Insight/UK Equity/Daily Snap Shot UK Fund/",
    year,
    "/",
    month
  )
  candidate_files<-list.files(location)
  file_template<-paste0("^[A-Za-z ]+",date_components[3],"[ \\.]",date_components[2],"\\.(xls|xlsm)")
  candidate_match<-grepl(file_template,candidate_files)
  if(sum(candidate_match)<1)return(NULL)
  selected_file<-head(candidate_files[candidate_match],1)
  return(paste0(location,"/",selected_file))
}


determine_excel_filetype<-function(fn){
  o<-capture.output({
    e_xls<-try(
      read_xls(path=fn,sheet=1,range="A1",col_names=FALSE,col_types="text"),
      silent=TRUE)
    e_xlsx<-try(
      read_xlsx(path=fn,sheet=1,range="A1",col_names=FALSE,col_types="text"),
      silent=TRUE)
  })
  if(all(class(e_xls)!="try-error"))return("xls")
  if(all(class(e_xlsx)!="try-error"))return("xlsx")
  return("unknown")
}

try_read_excel<-function(
  fn,
  sheet=1,
  range="A1",
  col_names=FALSE,
  col_types="text",
  file_type=determine_excel_filetype(fn)
){
  res<-NULL
  if(file_type=="xls"){
    res<-read_xls(path=fn,sheet=sheet,range=range,col_names=col_names,col_types = col_types)
  }
  if(file_type=="xlsx"){
    res<-read_xlsx(path=fn,sheet=sheet,range=range,col_names=col_names,col_types=col_types)
  }
  return(res)
}

range_rows<-function(cell_range)eval(parse(text=gsub("[A-Z]+","",cell_range)))
range_col<-function(cell_range)gsub("[0-9]+:[A-Z]+[0-9]+$","",cell_range)
range_names<-function(cell_range)paste0(range_col(cell_range),range_rows(cell_range))
change_range_col<-function(
  cell_range="G10:G2000",
  start_col="B",
  end_col=start_col
){
  x0<-gsub("^[A-Z]+",start_col,cell_range)
  x1<-gsub(":[A-Z]+",paste0(":",end_col),x0)
  x1
}

#
get_sheet_range<-function(
  fn=make_sheet_name(date="2017-06-30"),
  cell_range=change_range_col(get_sheet_extent(fn),"B"),
  sheet="AIL Open Trades",
  file_type=determine_excel_filetype(fn)
){
  if(all(grepl("^[A-Z]+[0-9]$",cell_range)))cell_range<-paste0(cell_range,":",cell_range)
  excel_cols<-c(LETTERS,apply(expand.grid(LETTERS,LETTERS,stringsAsFactors=FALSE)[,c(2,1)],1,paste0,collapse=""))
  start_col<-match(gsub("[0-9]+:[A-Z]+[0-9]+$","",cell_range),excel_cols)
  end_col<-match(gsub("[0-9]+$","",gsub("[A-Z]+[0-9]+:","",cell_range)),excel_cols)
  raw_cells<-try_read_excel(
    fn, 
    sheet = sheet,
    range=cell_range,
    col_names=excel_cols[start_col:end_col],
    col_types = rep("text",length(excel_cols[start_col:end_col])),
    file_type=file_type
  )
  x0<-as.matrix(raw_cells,dimnames=list(range_rows(cell_range),excel_cols[start_col:end_col]))
  x1<-apply(x0,2,function(x)ifelse(is.na(x),"",x))
  dimnames(x1)<-list(range_rows(cell_range),excel_cols[start_col:end_col])
  x1
}

#
get_sheet_position_range<-structure(function(
  date="2017-04-18",
  fn=make_sheet_name(date),
  file_type=determine_excel_filetype(fn),
  sheet="AIL Open Trades",
  summary_sheet="AIL Summary"
){
  if(is.null(fn))return(NULL)
  if(!file.exists(fn))return(NULL)
  if(file_type=="unknown")return(NULL)
  s1<-get_sheet_range(
    fn, 
    sheet = summary_sheet,
    cell_range="A4:B11",
    file_type=file_type
  )
  AUM<-scrub(as.numeric(s1["5","B"]))
  r1<-get_sheet_range(
    fn, 
    sheet = sheet,
    cell_range="B10:P2000",
    file_type=file_type
  )
  i<-which(
    grepl("^[A-Z]{2,4}[0-9]{1,3}$",stri_trim(toupper(r1[,"B"]))) 
  )
  r2<-r1[i,]
  attributes(r2)$date<-date
  attributes(r2)$filename<-fn
  attributes(r2)$filetype<-file_type
  attributes(r2)$AUM<-AUM
  r2
},location="eq_ptf_sheet_functions.R")


# 
get_sheet_unwind_range<-structure(function(
  date="2017-04-18",
  fn=make_sheet_name(date),
  file_type=determine_excel_filetype(fn),
  sheet="AIL Closed Trades",
  summary_sheet="AIL Summary"
){
  if(is.null(fn))return(NULL)
  if(!file.exists(fn))return(NULL)
  if(file_type=="unknown")return(NULL)
  s1<-get_sheet_range(
    fn, 
    sheet = summary_sheet,
    cell_range="A4:B11",
    file_type=file_type
  )
  AUM<-scrub(as.numeric(s1["5","B"]))
  r1<-get_sheet_range(
    fn, 
    sheet = sheet,
    cell_range="A10:O5000",
    file_type=file_type
  )
  i<-which(
    grepl("^[A-Z]{2,4}[0-9]+$",stri_trim(toupper(r1[,"A"]))) 
  )
  if(length(i)==0)return(NULL)
  r2<-r1[i,]
  attributes(r2)$date<-date
  attributes(r2)$filename<-fn
  attributes(r2)$filetype<-file_type
  attributes(r2)$AUM<-AUM
  r2
},location="eq_ptf_sheet_functions.R")

#
get_sheet_positions<-structure(function(
  date="2017-04-18",
  fn=make_sheet_name(date),
  file_type=determine_excel_filetype(fn),
  position_range=get_sheet_position_range(date=date,fn=fn,file_type=file_type)
){
  if(is.null(position_range))return(NULL)
  AUM<-attributes(position_range)$AUM
  r2<-position_range
  res0<-data.table(
    date=rep(attributes(r2)$date,nrow(r2)),
    sheet_row=rownames(r2),
    manager=toupper(r2[,"C"]),
    pair=toupper(r2[r2[,"B"]!="","B"][findInterval(1:nrow(r2),which(r2[,"B"]!=""),all.inside = TRUE,rightmost.closed = TRUE)]),
    direction=toupper(r2[,"D"]),
    ticker=gsub("INDEX$","Index",gsub("EQUITY$","Equity",toupper(r2[,"F"]))),
    units=scrub(as.integer(r2[,"I"])),
    multiplier=scrub(as.integer(r2[,"H"])),
    quantity=scrub(as.integer(r2[,"H"]))*scrub(as.integer(r2[,"I"])),
    price=scrub(as.numeric(r2[,"G"])),
    cash=(-1)*scrub(as.numeric(r2[,"J"])),
    asset_value=scrub(as.numeric(r2[,"N"])),
    pnl=scrub(as.numeric(r2[,"O"])),
    bps=scrub(as.numeric(r2[,"P"])),
    fx=scrub(as.numeric(r2[,"M"])),
    local_asset_value=scrub(as.numeric(r2[,"L"])),
    class=ticker_class(gsub("INDEX$","Index",gsub("EQUITY$","Equity",toupper(r2[,"F"]))))
  )[,c(.SD,.(
    digest=local({
      key=paste0(manager,pair,direction,ticker,units,multiplier,price)
      mapply(function(x)digest(object=x,algo="md5"),key)
    })
  ))]
  attributes(res0)$filename<-attributes(r2)$filename
  attributes(res0)$filetype<-attributes(r2)$filetype
  attributes(res0)$cellrange<-r2
  res0
},version="1.0",location="eq_ptf_sheet_functions.R")


# 
# digest calculation
# 
dcalc<-function(x,rounding_factor=100,rounding_shift=0)with(x,sapply(paste0(
  manager,
  "|",
  pair,
  "|",
  direction,
  "|",
  ticker,
  "|quantity:",
  as.character(as.integer(round(rounding_shift+units*multiplier/rounding_factor,digits=0))),
  "|entry:",
  as.character(as.integer(round(rounding_shift+entry_cash/rounding_factor,digits=0))),
  "|exit:",
  as.character(as.integer(round(rounding_shift+exit_cash/rounding_factor,digits=0))),
  "|pnl:",
  as.character(as.integer(round(rounding_shift+pnl/rounding_factor,digits=0)))
),function(x)paste0(x,":",digest::digest(x))))

#
get_sheet_unwinds<-function(
  date="2017-04-18",
  fn=make_sheet_name(date),
  file_type=determine_excel_filetype(fn),
  unwind_range=get_sheet_unwind_range(date=date,fn=fn,file_type=file_type)
){
  if(is.null(unwind_range))return(NULL)
  r2<-unwind_range
  AUM<-attributes(r2)$AUM
  res0<-data.table(
    date=rep(attributes(r2)$date,nrow(r2)),
    sheet_row=rownames(r2),
    manager=stri_trim(toupper(r2[,"B"])),
    pair=stri_trim(toupper(r2[,"A"])),
    direction=toupper(r2[,"C"]),
    ticker=gsub("INDEX$","Index",gsub("EQUITY$","Equity",toupper(r2[,"E"]))),
    units=scrub(as.integer(r2[,"H"])),
    multiplier=scrub(as.integer(r2[,"G"])),
    quantity=scrub(as.integer(r2[,"G"]))*scrub(as.integer(r2[,"H"])),
    entry_price=scrub(as.numeric(r2[,"F"])),
    entry_cash=(-1)*scrub(as.numeric(r2[,"I"])),
    exit_price=scrub(as.numeric(r2[,"J"])),
    exit_cash=scrub(as.numeric(r2[,"M"])),
    pnl=scrub(as.numeric(r2[,"N"])),
    bps=scrub(as.numeric(r2[,"O"])),
    class=ticker_class(gsub("INDEX$","Index",gsub("EQUITY$","Equity",toupper(r2[,"E"]))))
  )[,c(.SD,.(
    calc_pnl=entry_cash+exit_cash,
    calc_bps=if(AUM>0){10000*(entry_cash+exit_cash)/AUM}else{0},
    digest=dcalc(.SD,rounding_factor = 100,rounding_shift=0),
    digest1=dcalc(.SD,rounding_factor = 100,rounding_shift = 0.5)
  ))][,.(
    date=date[1],
    sheet_row=paste0(sheet_row,collapse=","),
    manager=manager[1],
    pair=pair[1],
    direction=direction[1],
    ticker=ticker[1],
    units=sum(units),
    multiplier=max(multiplier),
    quantity=sum(quantity),
    entry_price=sum(entry_price),
    entry_cash=sum(entry_cash),
    exit_price=sum(exit_price),
    exit_cash=sum(exit_cash),
    pnl=sum(pnl),
    bps=sum(bps),
    calc_pnl=sum(calc_pnl),
    calc_bps=sum(calc_bps)
  ),keyby="digest,digest1"]

  res0$digest=dcalc(res0)
  res0$digest1=dcalc(res0,rounding_factor = 100,rounding_shift = 0.5)
  
  attributes(res0)$filename<-attributes(r2)$filename
  attributes(res0)$filetype<-attributes(r2)$filetype
  attributes(res0)$cellrange<-r2
  res0
}


disaggregate_unwinds<-function(
  unwind_list
){ 
  all_unwinds<-do.call(rbind,unwind_list)
  
  x1<-all_unwinds[,.(
    digest=sapply(paste0(
      manager,
      pair,
      direction,
      ticker,
      as.character(units*multiplier),
      as.character(entry_cash),
      as.character(exit_cash),
      as.character(pnl)
    ),fastdigest)
  )]
  all_unwinds$digest<-transaction_digest(df=y,fields=list(
    manager=identity,
    pair=identity,
    direction=identity,
    ticker=identity,
    units=identity,
    multiplier=identity,
    quantity=identity,
    entry_price=identity,
    entry_cash=identity,
    exit_price=identity,
    exit_cash=identity
  ))
  au0<-y[,.SD[which(date==min(date)),],by=digest]
  au1<-au0[date>start]
  au2<-au1[,.(
    units=sum(units),
    multiplier=max(multiplier),
    quantity=sum(quantity),
    entry_price=sum(quantity*entry_price)/sum(quantity),
    entry_cash=sum(entry_cash),
    exit_price=sum(quantity*exit_price)/sum(quantity),
    exit_cash=sum(exit_cash),
    pnl=sum(pnl),
    bps=sum(bps),
    class=class[1]
  ),keyby="date,manager,pair,ticker,direction"]
  au2
}

#
#
#
aggregate_sheet_positions<-function(
  date="2017-07-07",
  fn=make_sheet_name(date),
  file_type=determine_excel_filetype(fn),
  sheet_positions=get_sheet_positions(date=date,fn=fn,file_type=file_type)
){
  if(!"data.table" %in% class(sheet_positions))return(NULL)
  only_has_pnl<-which(with(sheet_positions,abs(asset_value)<1e-12 & abs(cash)<1e-12 & abs(pnl)>1e-11))
  pnl_sign_reversed<-which(with(sheet_positions,abs(sign(asset_value+cash)*sign(pnl)+1)<1e-3))
  pnl_is_ok<-which(with(sheet_positions,abs(asset_value+cash-pnl)<100))
  other<-setdiff(1:nrow(sheet_positions),unique(c(only_has_pnl,pnl_sign_reversed,pnl_is_ok)))
  
  spa1<-sheet_positions[only_has_pnl,.(
    long_inventory=0,
    long_value=0,
    short_cash=sum(pmin(pnl,0)),
    short_inventory=round(sum(quantity[quantity<0]),digits=0),
    short_value=0,
    long_cash=sum(pmax(pnl,0)),
    multiplier=max(multiplier),
    type="open"
  ),by="date,manager,pair,ticker"]
  
  spa2<-sheet_positions[pnl_is_ok,.(
    long_inventory=sum(quantity[quantity>0]),
    long_value=sum(asset_value[quantity>0]),
    short_cash=sum(cash[cash<0]),
    short_inventory=sum(quantity[quantity<0]),
    short_value=sum(asset_value[quantity<0]),
    long_cash=sum(cash[cash>0]),
    multiplier=max(multiplier),
    type="open"
  ),by="date,manager,pair,ticker"]
  
  spa3<-sheet_positions[pnl_sign_reversed,.(
    long_inventory=sum(quantity[quantity>0]),
    long_value=(-1)*sum(asset_value[quantity>0]),
    short_cash=(-1)*sum(cash[cash<0]),
    short_inventory=sum(quantity[quantity<0]),
    short_value=(-1)*sum(asset_value[quantity<0]),
    long_cash=(-1)*sum(cash[cash>0]),
    multiplier=max(multiplier),
    type="open"
  ),by="date,manager,pair,ticker"]
  
  res0<-rbind(spa1,spa2,spa3)
  res1<-res0[,.(
    long_inventory=sum(long_inventory),
    long_value=sum(long_value),
    short_cash=sum(short_cash),
    short_inventory=sum(short_inventory),
    short_value=sum(short_value),
    long_cash=sum(long_cash),
    multiplier=max(multiplier),
    type="open"
  ),by="date,manager,pair,ticker"]
  attributes(res1)$other<-other
  res1
}


aggregate_sheet_unwinds<-function(
  date="2017-07-07",
  fn=make_sheet_name(date),
  file_type=determine_excel_filetype(fn),
  sheet_unwinds=get_sheet_unwinds(date=date,fn=fn,file_type=file_type)
){
  if(!"data.table" %in% class(sheet_unwinds))return(NULL)
  res0<-sheet_unwinds[,.(
    long_inventory=0,
    long_value=0,
    long_cash=sum(pnl[pnl>0]),
    short_inventory=0,
    short_value=0,
    short_cash=sum(pnl[pnl<0]),
    multiplier=1,
    type="closed"
  ),by="date,manager,pair,ticker"]
  res0
}

aggregate_sheet<-function(
  date="2017-07-07",
  fn=make_sheet_name(date),
  file_type=determine_excel_filetype(fn),
  sheet_positions=get_sheet_positions(date=date,fn=fn,file_type=file_type),
  sheet_unwinds=get_sheet_unwinds(date=date,fn=fn,file_type=file_type)
){
  if(is.null(fn))return(NULL)
  if(is.null(file_type))return(NULL)
  if(any(file_type=="unknown"))return(NULL)
  if(is.null(sheet_positions)&is.null(sheet_unwinds))return(data.table(
    date=date,
    manager="AC",
    pair="AC0",
    ticker="",
    type="closed",
    long_incentory=0,
    long_value=0,
    long_cash=0,
    short_inventory=0,
    short_value=0,
    short_cash=0,
    pnl=0,
    multiplier=0
  ))
  x1<-aggregate_sheet_positions(date=date,fn=fn,file_type=file_type,sheet_positions=sheet_positions)
  x2<-aggregate_sheet_unwinds(date=date,fn=fn,file_type=file_type,sheet_unwinds=sheet_unwinds)
  x3<-rbind(x1,x2)
  x4<-x3[,.(
    long_inventory=sum(long_inventory),
    long_value=sum(long_value),
    long_cash=sum(long_cash),
    short_inventory=sum(short_inventory),
    short_value=sum(short_value),
    short_cash=sum(short_cash),
    pnl=sum(long_value)+sum(long_cash)+sum(short_value)+sum(short_cash),
    multiplier=max(multiplier)
  ),keyby="date,manager,pair,ticker,type"]
  x4
}

unwind_diff<-function(final,initial){
  new_entry_digest<-setdiff(unique(final$digest),unique(initial$digest))
  i<-which(final$digest %in% new_entry_digest)
  new_entries<-final[i,]
  new_entries
}

update_sheet_range_db<-function(
  start="2017-06-01",
  end="2017-06-10",
  force=FALSE,
  verbose=TRUE
){
  sheet_range_db<-data.table(
    date=character(0),
    aum=numeric(0),
    fn=character(0),
    type=character(0),
    position_range=list(),
    unwind_range=list()
  )
  if(file.exists("sheet_range_db.tbl"))sheet_range_db<-fpump("sheet_range_db.tbl")
  all_dates<-make_date_range(start,end) 
  new_dates<-setdiff(all_dates,sheet_range_db$date)
  if(length(new_dates)==0)return(sheet_range_db[,.SD,keyby=date][all_dates])
  #
  res_fn<-local({
    fns<-mapply(function(x){
      if(verbose)message(paste0("sheet name:",x))
      make_sheet_name(x)
    },new_dates,SIMPLIFY=FALSE)
    #i<-which(!mapply(is.null,fns))
    #fns[i]
    fns
  })
  if(length(res_fn)==0)return(sheet_range_db[,.SD,keyby=date][setdiff(all_dates,new_dates)])
  new_dates<-names(res_fn)
  #
  res_ft<-mapply(function(x){
    if(verbose)message(paste0("sheet type:",x))
    determine_excel_filetype(x)
  },res_fn,SIMPLIFY=FALSE)
  #
  res_filename<-mapply(function(x)if(length(x)==0){""}else{x},res_fn)
  #
  res_filetype<-mapply(identity,res_ft)
  #
  res_position_range<-mapply(function(date,fn,file_type){
    if(verbose)message(paste0("position range:",date))
    if(file_type=="unknown")return(NULL)
    res<-get_sheet_position_range(date=date,fn=fn,file_type = file_type)
    res
  },new_dates,res_filename,res_filetype,SIMPLIFY=FALSE)
  #
  res_aum<-replace_zero_with_last(mapply(
    function(x)if(!is.null(x)){attributes(x)$AUM}else{0},
    res_position_range
  ))
  #
  res_unwind_range<-mapply(function(date,fn,file_type){
    if(verbose)message(paste0("unwind range:",date))
    if(file_type=="unknown")return(NULL)
    res<-get_sheet_unwind_range(date=date,fn=fn,file_type = file_type)
    res
  },new_dates,res_filename,res_filetype,SIMPLIFY=FALSE)
  #
 
  #
  new_sheet_range_db<-data.table(
    date=new_dates,
    aum=res_aum,
    fn=res_filename,
    type=res_filetype,
    position_range=res_position_range,
    unwind_range=res_unwind_range
  )
  combined_sheet_range_db<-rbind(sheet_range_db,new_sheet_range_db)
  fdump(combined_sheet_range_db,"sheet_range_db.tbl")
  combined_sheet_range_db
}
# 
# calc added unwinds 
# 
calc_added_unwinds<-function(final,initial){
  final_digests<-unique(c(final$digest,final$digest1))
  initial_digests<-unique(c(initial$digest,initial$digest1))
  i<-which(
    (!final$digest %in% initial_digests) &  
    (!final$digest1 %in% initial_digests)
  )
  new_entries<-final[i,]
  new_entries
}
calc_dropped_unwinds<-function(final,initial,reverse_transaction=TRUE){
  final_digests<-unique(c(final$digest,final$digest1))
  initial_digests<-unique(c(initial$digest,initial$digest1))
  i0<-which(!initial$digest %in% final_digests)
  i1<-which(!initial$digest1 %in% final_digests)
  i<-sort(intersect(i0,i1))
  missing_entries<-initial[i,]
  if(reverse_transaction){
    missing_entries$date        <- unique(final$date)[1]
    missing_entries$direction   <- local({
      direction<-missing_entries$direction
      ifelse(grepl("^(S|B)$",direction),c(S="B",B="S")[direction],direction)
    })
    missing_entries$units       <- (-1) * missing_entries$units
    missing_entries$quantity    <- (-1) * missing_entries$quantity
    missing_entries$entry_price <- (-1) * missing_entries$entry_price
    missing_entries$entry_cash  <- (-1) * missing_entries$entry_cash
    missing_entries$exit_price  <- (-1) * missing_entries$exit_price
    missing_entries$exit_cash   <- (-1) * missing_entries$exit_cash
    missing_entries$pnl         <- (-1) * missing_entries$pnl
    missing_entries$bps         <- (-1) * missing_entries$bps
    missing_entries$calc_pnl    <- (-1) * missing_entries$calc_pnl
    missing_entries$calc_bps    <- (-1) * missing_entries$calc_bps
  }
  missing_entries
}
#
#
#
update_sheet_db<-structure(function(
  start="2017-06-01",
  end="2017-06-10",
  force=FALSE,
  verbose=TRUE
){
  sheet_db<-data.table(
    date=character(0),
    aum=numeric(0),
    fn=character(0),
    type=character(0),
    position_range=list(),
    unwind_range=list(),
    positions=list(),
    unwinds_all=list(),
    unwinds=list(),
    aggregates=list()
  )
  if(file.exists("sheet_db.tbl"))sheet_db<-fpump("sheet_db.tbl")
  all_dates<-make_date_range(start,end) 
  new_dates<-setdiff(all_dates,sheet_db$date)
  if(length(new_dates)==0)return(sheet_db[,.SD,keyby=date][all_dates])
  #
  sheet_ranges<-update_sheet_range_db(start=start,end=end)[,.SD,keyby=date]
  i<-which(sheet_ranges[new_dates,fn]!="")
  if(length(i)==0)return(sheet_db[,.SD,keyby=date][setdiff(all_dates,new_dates)])
  res_filename<-sheet_ranges[new_dates[i],fn]
  res_filetype<-sheet_ranges[new_dates[i],type]
  res_position_range<-sheet_ranges[new_dates[i],position_range]
  res_aum<-sheet_ranges[new_dates[i],aum]
  res_unwind_range<-sheet_ranges[new_dates[i],unwind_range]
  
  #
  res_position<-mapply(function(sp){
    if(is.null(sp))return(NULL)
    if(verbose)message(paste0("position:",attributes(sp)$date))
    get_sheet_positions(
      date=attributes(sp)$date,
      fn=attributes(sp)$filename,
      file_type=attributes(sp)$filetype,
      position_range=sp
    )
  },res_position_range,SIMPLIFY=FALSE)
  #
  res_unwind<-mapply(function(su){
    if(is.null(su))return(NULL)
    if(verbose)message(paste0("unwind:",attributes(su)$date))
    get_sheet_unwinds(
      date=attributes(su)$date,
      fn=attributes(su)$filename,
      file_type=attributes(su)$filetype,
      unwind_range=su
    )
  },res_unwind_range,SIMPLIFY=FALSE)
  #
  res_unwind_diff<-c(
    res_unwind[1],
    mapply(function(final,initial){
      dropped<-calc_dropped_unwinds(final,initial,reverse_transaction=TRUE)
      added<-calc_added_unwinds(final,initial)
      if(verbose)message(paste0("unwind diff:",final$date[1]))
      if(nrow(dropped)<100)return(rbind(added,dropped))
      return(added)
    },tail(res_unwind,-1),head(res_unwind,-1),SIMPLIFY=FALSE)
  )
  
  #
  res_aggregate<-mapply(function(sp,ui,fn,file_type,d,udif){
      if(nchar(fn)==0)return(NULL)
      if(file_type=="unknown")return(NULL)
      if(is.null(sp))return(NULL)
      su<-do.call(rbind,udif[1:ui])
      the_date<-unique(sp$date)[1]
      if(verbose)message(paste0("aggregate:",the_date))
      su$date<-rep(the_date,nrow(su))
      aggregate_sheet(date=d,fn=fn,file_type=file_type,sheet_positions=sp,sheet_unwinds=su)
    },
    res_position,
    seq_along(res_unwind_diff),
    res_filename,
    res_filetype,
    new_dates[i],
    MoreArgs=list(udif=res_unwind_diff),
    SIMPLIFY=FALSE
  )
  #
  new_sheet_db<-data.table(
    date=new_dates[i],
    aum=res_aum,
    fn=res_filename,
    type=res_filetype,
    position_range=res_position_range,
    unwind_range=res_unwind_range,
    positions=res_position,
    unwinds_all=res_unwind,
    unwinds=res_unwind_diff,
    aggregates=res_aggregate
  )
  combined_sheet_db<-rbind(sheet_db,new_sheet_db)
  fdump(combined_sheet_db,"sheet_db.tbl")
  combined_sheet_db
},location="eq_ptf_sheet_functions.R")



memo_get_sheet_positions<-structure(function(
  date,
  force=FALSE
){
  key<-list(date,"memo_get_sheet_positions",as.character(attributes(get_sheet_positions)$srcref))
  sheet_positions<-loadCache(key=key)
  if(is.null(sheet_positions)|force){
    sheet_positions<-get_sheet_positions(date)
    saveCache(sheet_positions,key=key)
  }
  return(sheet_positions)
},location="eq_ptf_sheet_functions.R")


memo_get_sheet_unwinds<-structure(function(
  date,
  force=FALSE
){
  key<-list(date,"memo_get_sheet_unwinds",as.character(attributes(get_sheet_unwinds)$srcref))
  sheet_unwinds<-loadCache(key=key)
  if(is.null(sheet_unwinds)|force){
    sheet_unwinds<-get_sheet_unwinds(date)
    saveCache(sheet_unwinds,key=key)
  }
  return(sheet_unwinds)
},location="eq_ptf_sheet_functions.R")






transaction_digest<-structure(function(
  df,
  fields=list("date"=identity,"manager"=identity,"ticker"=identity,"units"=identity,"multiplier"=identity)
){
  values<-mapply(function(fn,ff,df){ff(df[[fn]])},names(fields),fields,MoreArgs = list(df=df),SIMPLIFY = FALSE)
  key<-do.call(paste0,values)
  mapply(function(x)digest(object=x,algo="md5"),key)
},location="eq_ptf_sheet_functions.R")

get_period_sheet_unwinds<-structure(function(
  start="2017-06-01",
  end="2017-06-30",
  verbose=TRUE,
  sheet_force=FALSE,
  raw=FALSE
){ 
  date_range<-make_date_range(start,end)
  x<-mapply(function(d){
    if(verbose)cat(d,"\n")
    unwinds<-memo_get_sheet_unwinds(d,sheet_force)
    unwinds
  },date_range,SIMPLIFY=FALSE)
  y<-do.call(rbind,x)[ticker!=""]
  if(raw)return(y)
  y$digest<-transaction_digest(df=y,fields=list(
    manager=identity,
    pair=identity,
    direction=identity,
    ticker=identity,
    units=identity,
    multiplier=identity,
    quantity=identity,
    entry_price=identity,
    entry_cash=identity,
    exit_price=identity,
    exit_cash=identity
  ))
  au0<-y[,.SD[which(date==min(date)),],by=digest]
  au1<-au0[date>start]
  au2<-au1[,.(
        units=sum(units),
        multiplier=max(multiplier),
        quantity=sum(quantity),
        entry_price=sum(quantity*entry_price)/sum(quantity),
        entry_cash=sum(entry_cash),
        exit_price=sum(quantity*exit_price)/sum(quantity),
        exit_cash=sum(exit_cash),
        pnl=sum(pnl),
        bps=sum(bps),
        class=class[1]
  ),keyby="date,manager,pair,ticker,direction"]
  au2
},location="eq_ptf_sheet_functions.R")

calc_pair_beta<-function(
  exposure=get("constituent_exposure",parent.frame()),
  beta_matrix=get("all_sxxp_beta",parent.frame()),
  tret=get("all_tret",parent.frame())
){
  hedge<-attributes(beta_matrix)$bbg_overrides["BETA_OVERRIDE_REL_INDEX"]
  map_matrix<-rename_colnames(dMcast(
    data=data.table(
      constituent=colnames(exposure),
      pair=gsub("_[A-Z]+[0-9A-Z/ ]*\\s(Index|Equity)$","",colnames(exposure)),
      value=1
    ),
    formula=constituent~pair,
    fun.aggregate = "sum",
    value.var="value"
  ),"^pair","")[colnames(exposure),]
  pair_beta<-get("*")(
    exposure,
    beta_matrix[rownames(exposure),gsub("[A-Z]+[0-9]+_","",colnames(exposure))]
  )%*%map_matrix
  pair_hedge_pnl<-get("*")(
    pair_beta,
    shift_matrix(tret[rownames(pair_beta),rep(hedge,ncol(pair_beta))],1)
  )
  list(
    pnl=pair_hedge_pnl,
    beta=pair_beta,
    hedge=hedge
  )
}


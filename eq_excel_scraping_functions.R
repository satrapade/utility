#
# locate sheets saved for a specific date
# scrape positions and unwinds off the sheet
#
require(stringi)
require(readxl)

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
}

make_sheet_name<-function(
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
get_sheet_position_range<-function(
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
  attributes(r2)$sheet<-sheet
  attributes(r2)$summary_sheet<-summary_sheet
  attributes(r2)$AUM<-AUM
  r2
}


# 
get_sheet_unwind_range<-function(
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
  attributes(r2)$sheet<-sheet
  attributes(r2)$summary_sheet<-summary_sheet
  attributes(r2)$AUM<-AUM
  r2
}

#
get_sheet_positions<-function(
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
}

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


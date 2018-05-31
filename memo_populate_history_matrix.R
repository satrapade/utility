
make_date_range<-function(start, end)
{
  from <- as.Date(start, format = "%Y-%m-%d")
  to <- as.Date(end, format = "%Y-%m-%d")
  date_seq <- seq(from = from, to = to, by = 1)
  res  <- as.character(date_seq, format = "%Y-%m-%d" )
  return(res)
}

populate_history_matrix <- function(tickers, field, start, end, overrides)
{
  all.dates <- make_date_range(start, end)
  
  res <- bdh(
    tickers, 
    field, 
    as.Date(min(all.dates),format="%Y-%m-%d"), 
    as.Date(max(all.dates),format="%Y-%m-%d"), 
    overrides=overrides,
    include.non.trading.days=TRUE
  )
  
  df <- data.frame(
    ticker = do.call(c, mapply(rep, names(res), mapply(nrow, res), SIMPLIFY = FALSE)),
    date = as.character(do.call(
      c, 
      mapply("[[", res, MoreArgs = list("date"), SIMPLIFY = FALSE)
    ), format = "%Y-%m-%d"),
    value = do.call(c, mapply("[[", res, MoreArgs = list(field), SIMPLIFY = FALSE)),
    row.names = NULL,
    stringsAsFactors = FALSE)
  
  sm <- sparseMatrix(
    i = match(df$date, all.dates),
    j = match(df$ticker, tickers),
    x = df$value,
    dims = c(length(all.dates), length(tickers)),
    dimnames = list(all.dates, tickers))
  
  return(sm)
}

memo_populate_history_matrix <- function(
  ref.matrix, 
  post.function = replace_zero_with_last, 
  field = "PX_LAST", 
  overrides = NULL, 
  force = FALSE, 
  verbose = FALSE,
  ticker_xlat_fun=function(ticker)gsub(
    pattern=ticker_modifier$pattern,
    ticker_modifier$replacement,
    ticker
  ),
  ticker_modifier=list(pattern="",replacement="")
){
  key <- list(dimnames(ref.matrix), field, overrides, "memo_populate_history_matrix")
  
  cached.value <- loadCache(key = key)
  
  if(!is.null(cached.value) & !force)
  {
    if(verbose)warning(
      "memo_populate_history_matrix: using cached value for ", 
      paste(c(field, overrides), collapse = ", "), 
      ".", 
      call. = FALSE
    )
    return(cached.value)
  }
  if(verbose)warning(
    "memo_populate_history_matrix: accessing bloomberg for ", 
    paste(c(field, overrides), collapse = ", "), 
    ".", 
    call. = FALSE
  )
  
  tickers <- ticker_xlat_fun(colnames(ref.matrix))
  
  res <- populate_history_matrix(
    tickers, 
    field, 
    min(rownames(ref.matrix)), 
    max(rownames(ref.matrix)), 
    overrides
  )
  
  cached.value <- apply(res, 2, post.function)
  
  dimnames(cached.value) <- dimnames(ref.matrix)
  
  saveCache(cached.value, key = key)
  
  return(cached.value)
}







make_date_range<-function(start, end)
{
  from <- as.Date(gsub("[\"\'a-z]","",start), format = "%Y-%m-%d")
  to <- as.Date(gsub("[\"\'a-z]","",end), format = "%Y-%m-%d")
  date_seq <- seq(from = from, to = to, by = 1)
  res  <- as.character(date_seq, format = "%Y-%m-%d" )
  return(res)
}




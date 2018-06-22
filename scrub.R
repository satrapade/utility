

scrub <- function(x, default = 0)
{
  if(length(x) == 0) return(default)
  x[which(!is.finite(x))] <- default
  return(x)
}


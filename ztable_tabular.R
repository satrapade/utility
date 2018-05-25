
require(ztable)

ztabular<-function(x,colors=TRUE,center=TRUE){
  if(class(x)!="ztable")return("")
  x0<-capture.output(print(x,type="latex"))
  start<-head(which(grepl("(^\\\\begin\\{tabular\\})|(^\\\\begin\\{longtable\\})",x0),1))
  end<-head(which(grepl("(^\\\\end\\{tabular\\})|(^\\\\end\\{longtable\\})",x0),1))
  cols<-which(grepl("^\\\\definecolor\\{[a-z0-9\\-]+\\}",x0),1)
  x1<-paste0(c(
    if(colors)x0[cols]else"",
    if(center)"\\begin{center}\n"else"",
    x0[start:end],
    if(center)"\\end{center}\n"else""
  ),collapse="\n")
  x1
}














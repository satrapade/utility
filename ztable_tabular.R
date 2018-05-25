
require(ztable)

ztabular<-function(x){
  if(class(x)!="ztable")return("")
  x0<-capture.output(print(x,type="latex"))
  start<-head(which(grepl("(^\\\\begin\\{tabular\\})|(^\\\\begin\\{longtable\\})",x0),1))
  end<-head(which(grepl("(^\\\\end\\{tabular\\})|(^\\\\end\\{longtable\\})",x0),1))
  cols<-which(grepl("^\\\\definecolor\\{[a-z0-9\\-]+\\}",x0),1)
  x1<-paste0(c(
    "\\begin{center}\n",
    x0[cols],
    x0[start:end],
    "\\end{center}\n"
  ),collapse="\n")
  x1
}














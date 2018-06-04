#
# tikz shapes
#

tikz_rect<-function(col,xsize="1.0ex",ysize="0.5ex",raise="0.2ex"){
  paste0(
    "\\raisebox{",raise,"}{\\tikz \\fill[",col,"] (0,0) rectangle (",xsize,",",ysize,");}"
  )
}




#
# image_write(
#    pack_image(
#     image_read("dbk.png"),
#     list(pdf=readBin("quest_performance_analysis.pdf","raw",n=file.size("quest_performance_analysis.pdf")))
#    ),
#    "dbk1.png"
# )
# writeBin(unpack_image(image_read("dbk1.png"))$data$pdf,"test1.pdf")
#

require(brotli)
require(base64enc)
require(digest)
require(magick)
require(pdftools)
require(sodium)
require(bitops)

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



















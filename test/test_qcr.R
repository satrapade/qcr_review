
require(stringi)
require(scales)
require(digest)
require(clue)
require(FRAPO)
require(data.table)
require(Matrix)
require(Matrix.utils)
require(magrittr)
require(gsubfn)
require(Hmisc)
require(ggplot2)
require(magick)
require(DBI)
require(readxl)
require(Rtsne)
require(knitr)
require(stringi)
require(RcppRoll)
require(magick)
require(corrplot)

make_date_range<-function(start, end){
  from <- as.Date(gsub("[\"\'a-z]","",start), format = "%Y-%m-%d")
  to <- as.Date(gsub("[\"\'a-z]","",end), format = "%Y-%m-%d")
  date_seq <- seq(from = from, to = to, by = 1)
  res  <- as.character(date_seq, format = "%Y-%m-%d" )
  return(res)
}

n<-fread("ticker_desc.csv")

x<-fread("EURUSD.csv") %>% 
  {structure(do.call(cbind,.[,-1]),dimnames=list(.[[1]],names(.)[-1]))}

y<-apply(x,2,function(a){
  if(min(a)>0){a0<-c(0,diff(a))}else{a0<-a}
  rescale(a0,from=max(abs(a0))*c(-1,1),to=c(-1,1))
})

y0<-cbind(sign(y[,1]))[,rep(1,ncol(y)-1)]
y1<-sign(y[,2:ncol(y)])
y2<-apply(y0*y1,2,roll_meanr,180,fill=0)

y3<-y2[,order(colSums(y2))]
        
df <-  y3  %>%
  {data.table(ndx=1:nrow(.),.)} %>% 
  {melt(.,id.vars="ndx",measure.vars=names(.)[-1])}
    
gg1 <-  df %>% 
    ggplot +
    geom_raster(aes(x=variable,y=ndx,fill = value), interpolate = FALSE) + 
    scale_fill_gradient2( low="red", mid="white", high="blue", midpoint = 0 ,guide=FALSE) + 
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) 





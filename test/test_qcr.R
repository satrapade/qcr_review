
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


x<-fread("EURUSD.csv") %>% 
  {do.call(cbind,.[,-1])}

x %>% 
  ggplot() +
  geom_line(aes(x=as.Date(date),y=value,col=variable))




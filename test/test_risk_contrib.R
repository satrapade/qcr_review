#
# testing extended dataframe with rendering
#

require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)

tri<-function(n,d=1,s=1)(s*row(diag(n))<s*col(diag(n)))+d*diag(n)

tret<- 50 %>% {rnorm(10000*.)} %>% {matrix(.,ncol=length(.)/10000)}

ptfs<- tri(ncol(tret))%*%diag(1/seq(ncol(tret)))

vars<-setNames(apply(tret%*%ptfs,2,var),seq(ncol(tret)))
vols<-setNames(apply(tret%*%ptfs,2,sd),seq(ncol(tret)))



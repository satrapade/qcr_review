#
# test simple backtest
#

require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)
require(FactoMineR)
require(RcppRoll)

# ccy total returns matrix
ccy_tr <- fread("ccy_tr.csv") %>% # ccy returns
  {as.matrix(.[,-1],rownames=.$rn)} %>%
  {.[,-which(apply(.,2,sd)<1e-10)]} %>%
  {apply(log(.),2,diff)} %>%
  {structure(.%*%diag((0.05/16)/apply(.,2,sd)),dimnames=dimnames(.))} %>%
  {cbind(.,USD=rep(0,nrow(.)))}
  
# subsets
g10<-c("USD","EUR","JPY","GBP","AUD","CAD","CHF","SEK","NZD", "NOK")
g3<-head(g10,3)
g10xg3<-tail(g10,-3)

# make cross returns from subsets
universe2matrix<- . %>% 
  {outer(.,.,paste0)} %>% 
  {.[upper.tri(.)]} %>%
  {structure(ccy_tr[,stri_sub(.,1,3)]-ccy_tr[,stri_sub(.,-3,-1)],dimnames=list(rownames(ccy_tr),.))}

# matrices of cross returns
g3tr     <- universe2matrix(g3)
g10tr    <- universe2matrix(g10)
g10xg3tr <- universe2matrix(g10xg3)

# make matrix of signals: 3m total returns
tret2sig <- . %>% 
  {structure(apply(.,2,function(x)c(rep(0,90),roll_sum(x,n=91))),dimnames=dimnames(.))} %>%
  {sign(.)}

# matrixes of signals
g3sig <- tret2sig(g3tr)
g10sig <- tret2sig(g10tr)
g10xg3sig <- tret2sig(g10xg3tr)

g3perf<-rowMeans(head(g3sig,-1)*tail(g3tr,-1))
g10perf<-rowMeans(head(g10sig,-1)*tail(g10tr,-1))
g10xg3perf<-rowMeans(head(g10xg3sig,-1)*tail(g10xg3tr,-1))






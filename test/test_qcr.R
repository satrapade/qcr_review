
g10<-c("USD","EUR","JPY","GBP","AUD","CAD","CHF","SEK","NZD", "NOK")

ccy2pair <- . %>% 
  head(g10,.) %>% 
  {paste0(.,collapse="|")} %>%
  {paste0("^(",.,")(",.,")$")}

g3regexp<- ccy2pair(3) 

# make matrix of signals: sign of rolling total returns
tret2sig <- function(x,w=91,thresh=0.05,type="rangeloc"){
  if(type=="sign"){
    tret<-apply(x,2,function(p)c(rep(0,w-1),roll_sum(p,n=w)))
    attributes(tret)$dimnames<-dimnames(x)
    return(sign(tret))
  }
  if(type=="rangeloc"){
    rng<-structure(apply(x,2,function(p){
      px<-cumsum(p)
      pxstart<-head(px,-(w-1))
      pxend<-tail(px,-(w-1))
      pxmax<-roll_max(px,n=w)
      pxmin<-roll_min(px,n=w)
      rng<-2*((pxend-pxmin)/(pxmax-pxmin)-0.5)
      c(rep(0,w-1),rng)
    }),dimnames=dimnames(x))
    return(rng*(abs(rng)>thresh))
  }
}

# ccy total returns matrix
ccy_tr <- fread("ccy_tr.csv") %>% # ccy returns
{as.matrix(.[,-1],rownames=.$rn)} %>%
{.[,-which(apply(.,2,sd)<1e-10)]} %>%
{apply(log(.),2,diff)} %>%
{structure(.%*%diag((0.05/16)/apply(.,2,sd)),dimnames=dimnames(.))} %>%
{cbind(.,USD=rep(0,nrow(.)))}

qcr_tr<-fread("currency_total_returns.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=.$V1)}
qcr_sd<-apply(qcr_tr,2,sd)
qcr_rp<-qcr_tr%*%diag(0.05/qcr_sd)

qcr_msig<-fread("momentum_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,1,10))} %>%
{head(.,-1)[,colnames(qcr_tr)]}

ndx<-intersect(rownames(ccy_tr),rownames(qcr_msig))

sig<-qcr_msig[ndx,]
tret<-structure(
  ccy_tr[ndx,stri_sub(colnames(qcr_msig),1,3)]-ccy_tr[ndx,stri_sub(colnames(qcr_msig),-3,-1)],
    dimnames=list(ndx,colnames(qcr_msig))
)
sig1<-tret2sig(tret,w=180,type="rangeloc")

perf<-tail(head(sig,-1)*tail(tret,-1),-180)
perf1<-tail(head(sig1,-1)*tail(tret,-1),-180)

data.table(
  qcr_g3=cumsum(rowMeans(perf[,grepl(ccy2pair(3),colnames(perf))])),
  qcr_g10xg3=cumsum(rowMeans(perf[,!grepl(ccy2pair(3),colnames(perf))])),
  sig_g3=cumsum(rowMeans(perf1[,grepl(ccy2pair(3),colnames(perf1))])),
  sig_g10xg3=cumsum(rowMeans(perf1[,!grepl(ccy2pair(3),colnames(perf1))])),
  date=as.Date(rownames(perf),format="%Y-%m-%d")
) %>%
  {melt(.,id.vars="date",measure.vars=setdiff(names(.),"date"))} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable))









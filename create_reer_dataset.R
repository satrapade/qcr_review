#
#
#
#
require(RcppRoll)
require(data.table)
require(clue)
require(gsubfn)
require(magrittr)
require(R.cache)

require(FRAPO)
require(RSQLite)
require(DBI)
require(Matrix)
require(Matrix.utils)
require(Rblpapi)

off_site<-if(Sys.info()["sysname"]=="Windows"){FALSE}else{TRUE}

source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/nn_cast.R")

rgxccy<- .%>% {paste0(.,collapse="|")} %>% {paste0("^(",.,")(",.,")$")}
sharpe<-function(strat)sqrt(356)*mean(strat)/sd(strat)

make_date_range<-function(start, end){
  from <- as.Date(gsub("[\"\'a-z]","",start), format = "%Y-%m-%d")
  to <- as.Date(gsub("[\"\'a-z]","",end), format = "%Y-%m-%d")
  date_seq <- seq(from = from, to = to, by = 1)
  res  <- as.character(date_seq, format = "%Y-%m-%d" )
  return(res)
}

img_sig <- function(x){
  res<- x %>% 
    {.[rowSums(abs(.))>1e-10,] } %>%
    {
      all_dates <- rownames(.) %>% {make_date_range(start=min(.),end=max(.))}
      ndx<-findInterval(as.Date(all_dates),as.Date(rownames(.)))
      structure(.[ndx,],dimnames=list(all_dates,colnames(.)))
    } %>%
    {data.table(date=as.Date(rownames(.)),.)} %>% 
    {melt(.,id.vars="date",measure.vars=names(.)[-1])}
  gg1 <-  ggplot(res) +
    geom_raster(aes(x=variable,y=date,fill = value), interpolate = FALSE) + 
    scale_fill_gradient2( low="red", mid="white", high="blue", midpoint = 0 ,guide=FALSE) + 
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    ggtitle(deparse(substitute(x)))
  gg1
}

g10<-c("USD","EUR","JPY","GBP","AUD","CAD","CHF","SEK","NZD", "NOK")

reersn<-c(
  USD="BISNUSR",EUR="BISNEUR",JPY="BISNJPR",GBP="BISNGBR",AUD="BISNAUR",
  CAD="BISNCAR",CHF="BISNCHR",SEK="BISNSER",NZD="BISNNZR",NOK="BISNNOR",
  DKK="BISNDKR"
)

reersb<-c(
  USD="BISBUSR",EUR="BISBEUR",JPY="BISBJPR",GBP="BISBGBR",AUD="BISBAUR",
  CAD="BISBCAR",CHF="BISBCHR",SEK="BISBSER",NZD="BISBNZR",NOK="BISBNOR",
  DKK="BISBDKR"
)

ppp<-c(
  USD="BPPPCPUS",EUR="BPPPCPEU",JPY="BPPPCPJP",GBP="BPPPCPGB",AUD="BPPPCPAU",
  CAD="BPPPCPCA",CHF="BPPPCPCH",SEK="BPPPCPSE",NZD="BPPPCPNZ",NOK="BPPPCPNO",
  DKK="BISBDKR"
)

qcr_tr<-fread("currency_total_returns.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=.$V1)}
qcr_sd<-apply(qcr_tr,2,sd)
qcr_rp<-qcr_tr%*%diag(0.05/qcr_sd)

qcr_ltvf<-fread("ltfv_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,1,10))} %>%
{head(.,-1)[,colnames(qcr_tr)]}

qcr_stvf<-fread("stfv_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,1,10))} %>%
{head(.,-1)[,colnames(qcr_tr)]}

qcr_carry_level<-fread("carry_level_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,1,10))} %>%
{head(.,-1)[,colnames(qcr_tr)]}

qcr_carry_momentum<-fread("carry_momentum_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,1,10))} %>%
{head(.,-1)[,colnames(qcr_tr)]}

qcr_msig<-fread("momentum_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,1,10))} %>%
{head(.,-1)[,colnames(qcr_tr)]}

start<-"2004-12-31"
end<-max(rownames(qcr_tr))

if(!off_site)rconn<-Rblpapi::blpConnect()

if(!off_site)reersn_bdh<-memoizedCall(
  Rblpapi::bdh,
  securities=paste(reersn,"Index"),
  fields="PX_LAST",
  start.date=as.Date(start),
  end.date=as.Date(end)
)

if(!off_site)reersn_df<- reersn_bdh %>% 
  {mapply(
    function(n,df){
      data.table(ticker=n,ccy=names(reersn)[grepl(gsub(" Index$","",n),reersn)],df)
    },
    n=names(.),
    df=.,SIMPLIFY=FALSE
  )} %>% 
  {do.call(rbind,.)}

if(off_site)reersn_df<-fread("reersn_df.csv")

reersn_m<-NNcast(reersn_df,i_name="date",j_name="ccy",v_name="PX_LAST")

reersn_em<- reersn_m %>% 
  {
    all_dates <- rownames(.) %>% {make_date_range(start=min(.),end=max(.))}
    ndx<-findInterval(as.Date(all_dates),as.Date(rownames(.)))
    structure(.[ndx,],dimnames=list(all_dates,colnames(.)))
  } 

reersn_rv<-structure(
  log(reersn_em[,stri_sub(colnames(qcr_stvf),1,3)])-log(reersn_em[,stri_sub(colnames(qcr_stvf),-3,-1)]),
  dimnames=list(rownames(reersn_em),colnames(qcr_stvf))
)

reersn_rvchg<- reersn_rv - apply(reersn_rv,2,roll_meanr,n=250,fill=0)
ndx<-intersect(rownames(reersn_rvchg),rownames(qcr_stvf%>%{.[which(rowSums(abs(.))>1e-3),]}))
mean(sign(reersn_rvchg[ndx,])*sign(qcr_stvf[ndx,]))

if(!off_site)reersb_bdh<-memoizedCall(
  Rblpapi::bdh,
  securities=paste(reersb,"Index"),
  fields="PX_LAST",
  start.date=as.Date(start),
  end.date=as.Date(end)
)

if(!off_site)reersb_df<- reersb_bdh %>% 
  {mapply(
    function(n,df){
      data.table(ticker=n,ccy=names(reersn)[grepl(gsub(" Index$","",n),reersb)],df)
    },
    n=names(.),
    df=.,SIMPLIFY=FALSE
  )} %>% 
  {do.call(rbind,.)}

if(off_site)reersb_df<-fread("reersb_df.csv")

reersb_m<-NNcast(reersb_df,i_name="date",j_name="ccy",v_name="PX_LAST")

reersb_em<- reersb_m %>% 
  {
    all_dates <- rownames(.) %>% {make_date_range(start=min(.),end=max(.))}
    ndx<-findInterval(as.Date(all_dates),as.Date(rownames(.)))
    structure(.[ndx,],dimnames=list(all_dates,colnames(.)))
  } 

reersb_rv<-structure(
  log(reersb_em[,stri_sub(colnames(qcr_stvf),1,3)])-log(reersb_em[,stri_sub(colnames(qcr_stvf),-3,-1)]),
  dimnames=list(rownames(reersn_em),colnames(qcr_stvf))
)

reersb_rvchg<- reersb_rv - apply(reersb_rv,2,roll_meanr,n=250,fill=0)
ndx<-intersect(rownames(reersb_rvchg),rownames(qcr_stvf%>%{.[which(rowSums(abs(.))>1e-3),]}))

mean(sign(reersn_rvchg[ndx,])*sign(qcr_stvf[ndx,]))
mean(sign(reersb_rvchg[ndx,])*sign(qcr_stvf[ndx,]))

mean(sign(
  sign(qcr_carry_level[ndx,])+
    sign(qcr_msig[ndx,])+
    sign(reersb_rvchg[ndx,])+
    sign(reersn_rvchg[ndx,])
)*sign(qcr_stvf[ndx,]))



sign(reersn_rv) %>% 
  {apply(.,2,cumsum)} %>%
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("REER NARROW")

sign(reersb_rv) %>% 
  {apply(.,2,cumsum)} %>%
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("REER BROAD")

sign(qcr_stvf) %>%
  {apply(.,2,cumsum)[seq(nrow(.))>head(which(rowSums(abs(.))>1e-3),1),]} %>% 
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("STVF")


sign(qcr_ltvf) %>%
  {apply(.,2,cumsum)[seq(nrow(.))>head(which(rowSums(abs(.))>1e-3),1),]} %>% 
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("LTVF")

sign(qcr_carry_level) %>%
  {apply(.,2,cumsum)[seq(nrow(.))>head(which(rowSums(abs(.))>1e-3),1),]} %>% 
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("CARRY LEVEL")

sign(qcr_carry_momentum) %>%
  {apply(.,2,cumsum)[seq(nrow(.))>head(which(rowSums(abs(.))>1e-3),1),]} %>% 
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("CARRY MOMENTUM")


sign(qcr_msig) %>%
  {apply(.,2,cumsum)[seq(nrow(.))>head(which(rowSums(abs(.))>1e-3),1),]} %>% 
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("PRICE MOMENTUM")


combined<-( 
  sign(qcr_msig%*%diag(ifelse(grepl(rgxccy(head(g10,3)),colnames(qcr_msig)),1,0))) +
  sign(qcr_msig%*%diag(ifelse(grepl(rgxccy(tail(g10,7)),colnames(qcr_msig)),-1,0))) +
  sign(qcr_ltvf) + 
  sign(qcr_stvf) + 
  sign(qcr_carry_level) + 
  sign(qcr_carry_momentum)
) 

combined %>%
  {apply(.,2,cumsum)[seq(nrow(.))>head(which(rowSums(abs(.))>1e-3),1),]} %>% 
  {data.table(date=as.Date(rownames(.)),.)} %>% 
  {melt(.,id.vars="date",measure.vars=names(.)[-1])} %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable,group=variable),show.legend=FALSE)+
  ggtitle("combined")

combined_pnl<-head(combined,-1)*tail(qcr_tr,-1)
combined_xcarry_pnl<-head(combined-sign(qcr_carry_level),-1)*tail(qcr_tr,-1)
carry_pnl<-head(sign(qcr_carry_level),-1)*tail(qcr_tr,-1)

img_sig(combined[,order(apply(qcr_carry_level,2,sum))])



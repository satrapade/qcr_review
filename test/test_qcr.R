

qcr_tr<-fread("currency_total_returns.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=.$V1)}


qcr_msig<-fread("momentum_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,1,10))}

x<-head(qcr_msig[,colnames(qcr_tr)],-2)*tail(qcr_tr,-1)




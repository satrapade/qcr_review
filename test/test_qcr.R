

qcr_tr<-fread("currency_total_returns.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=.$rn)}


qcr_msig<-fread("momentum_signal.csv") %>% 
{as.matrix(.[,2:(ncol(.)-1)],rownames=stri_sub(.$V1,10))}






qcr_sv<-(head(apply(qcr_stvf,2,roll_mean,n=30,fill=0,align="r"),-1)*tail(qcr_tr,-1)) %>% 
{setNames(.,tail(rownames(qcr_tr),-1))} 

plot(cumsum(rowSums(qcr_sv[-(1:ss),])),type="l")


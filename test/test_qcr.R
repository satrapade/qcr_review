

x1<-apply(qcr_tr,2,roll_sumr,n=30,fill=0)
y1<-apply(qcr_tr,2,roll_sumr,n=120,fill=0)

x2<-apply(qcr_tr^2,2,roll_meanr,n=90,fill=0)
y2<-apply(qcr_tr^2,2,roll_meanr,n=250,fill=0)

z<-apply(qcr_tr,2,cumsum)

scor<-function(i=120,n=3000){
  res<-sign(tail(z,-i)-head(z,-i))
  data.table(
    pm=mean(tail(sign(tail(qcr_msig,-i))*res,n)),
    cm=mean(tail(sign(tail(qcr_carry_momentum,-i))*res,n)),
    cl=mean(tail(sign(tail(qcr_carry_level,-i))*res,n)),
    lv=mean(tail(sign(tail(qcr_ltvf,-i))*res,n)),
    sv=mean(tail(sign(tail(qcr_stvf,-i))*res,n))
  )
}

res<-seq(10,500,length.out=100) %>% as.integer %>% {data.table(
  i=.,
  do.call(rbind,mapply(scor,.,SIMPLIFY=FALSE))
)}

res %>%
{ melt(.,id.vars="i",measure.vars=names(.)[-1]) } %>%
 ggplot()+geom_point(aes(x=i,y=value,col=variable,group=variable))

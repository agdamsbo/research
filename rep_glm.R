rep_glm<-function(x,y){
## x is data.frame, y is vector
  require(broom)
  df<-data.frame(matrix(ncol = 3))
  names(df)<-c("pred","b","pv")
  
  for(i in 1:ncol(x)){
     m<-glm(y~x[,i],family = binomial())
     
     b<-round(coef(m)[-1],3)
     
     pv<-round(tidy(m)$p.value[-1],3)
    pv<-ifelse(pv<0.001,"<0.001",pv)
    pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),pv)
     
     pred<-names(x)[i]
     
     df<-rbind(df,cbind(pred,b,pv))
     }
  return(df)
}
rep_lm<-function(rep_glm<-function(y,v1,string,ci=FALSE,data,v2=NULL,v3=NULL){
## x is data.frame of predictors, y is vector of an aoutcome as a factor
## output is returned as coefficient, or if ci=TRUE as coefficient with 95 % CI.
## The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
  
  require(broom)
  
  d<-data
  x<-select(d,one_of(c(string)))
  
if (is.factor(y)){stop("Some kind of error message would be nice, but y should not be a factor!")}
  
    if (ci==TRUE){
      
             df<-data.frame(matrix(ncol = 3))
  names(df)<-c("pred","co_ci","pv")
      
      for(i in 1:ncol(x)){
     m<-lm(y~v1+x[,i])
     
     l<-suppressMessages(round(confint(m)[-1,1],2))
     u<-suppressMessages(round(confint(m)[-1,2],2))
     co<-round(coef(m)[-1],2)
     
     co_ci<-paste0(co," (",l," to ",u,")")
     
     pv<-round(tidy(m)$p.value[-1],3)
     pv<-ifelse(pv<0.001,"<0.001",pv)
     pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),
                  ifelse(pv>0.05&pv<=0.1,paste0(".",pv),pv))

     
    v<-x[,i]
     
    if (is.factor(v)){
       pred<-paste(names(x)[i],levels(v)[-1],sep = "_")
    }
       
       else {pred<-names(x)[i]}
      
      df<-rbind(df,cbind(pred,co_ci,pv))
      
    }}
    
  if (ci==FALSE){
     
       df<-data.frame(matrix(ncol = 3))
  names(df)<-c("pred","b","pv")
     
  for(i in 1:ncol(x)){
     m<-lm(y~v1+x[,i])
     
     b<-round(coef(m)[-1],3)
     
     pv<-round(tidy(m)$p.value[-1],3)
     pv<-ifelse(pv<0.001,"<0.001",pv)
     pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),
                  ifelse(pv>0.05&pv<=0.1,paste0(".",pv),pv))
     
     v<-x[,i]
     
     if (is.factor(v)){
       pred<-paste(names(x)[i],levels(v)[-1],sep = "_")
     }
       
       else {pred<-names(x)[i]}
     
     df<-rbind(df,cbind(pred,b,pv))
  }}
  
  return(df)
}

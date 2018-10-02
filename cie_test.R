cie_test<-function(v1,v2=NULL,v3=NULL,y,string,data){
## Calculating variables, that should be included for a change in estimate analysis. v1-3 are possible locked variables, y is the outcome vector. String defines variables to test, and is provided as vector of variable names. Use dput().
  
  require(broom)
  
  if (is.factor(y)){stop("Some kind of error message would be nice, but y should not be a factor!")}
  
  d<-data
  x<-select(d,one_of(c(string)))
  
if (is.null(v2)&is.null(v3)){     
  
  e<-as.numeric(round(coef(lm(y~v1)),3))[1]
     
       df<-data.frame(pred="base",b=e)
  
  for(i in 1:ncol(x)){

     m<-lm(y~v1+x[,i])
     
     b<-as.numeric(round(coef(m),3))[1]
  
     v<-x[,i]
     
   pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))
  }
       t<-c(NA,ifelse(abs(e-as.numeric(df[-1,2]))>=(e*.1),"include","drop"))
df<-cbind(df,t)  
       }

       if (!is.null(v2)&is.null(v3)){   
         
           e<-as.numeric(round(coef(lm(y~v1+v2)),3))[1]
     
       df<-data.frame(pred="base",b=e)
       
  for(i in 1:ncol(x)){

     m<-lm(y~v1+v2+x[,i])
     
     b<-as.numeric(round(coef(m),3))[1]
  
     v<-x[,i]

       pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))
  }
       t<-c(NA,NA,ifelse(abs(e-as.numeric(df[3:nrow(df),2]))>=(e*.1),"include","drop"))
df<-cbind(df,t)  
       }
       
       if (!is.null(v2)&!is.null(v3)){   
         
           e<-as.numeric(round(coef(lm(y~v1+v2+v3)),3))[1]
     
       df<-data.frame(pred="base",b=e)
       
  for(i in 1:ncol(x)){

     m<-lm(y~v1+v2+v3+x[,i])
     
     b<-as.numeric(round(coef(m),3))[1]
     
     v<-x[,i]
    
       pred<-paste(names(x)[i])
     
     df<-rbind(df,cbind(pred,b))
     
  }
       t<-c(NA,NA,NA,ifelse(abs(e-as.numeric(df[4:nrow(df),2]))>=(e*.1),"include","drop"))
df<-cbind(df,t)  
}

    return(df)
}

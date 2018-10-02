rep_biv<-function(y,v1,string,data,method="pval",logistic=FALSE,ci=FALSE,cut=0.1,v2=NULL,v3=NULL){
##
##
##
##
##
##

source("https://raw.githubusercontent.com/agdamsbo/research/master/cie_test.R")
source("https://raw.githubusercontent.com/agdamsbo/research/master/rep_glm.R")
source("https://raw.githubusercontent.com/agdamsbo/research/master/rep_lm.R")

if (method=="pval"&logistic==FALSE){
rep_lm(y=y,v1=v1,string=string,data=data,ci=ci)
}
if (method=="pval"&logistic==TRUE){
rep_lm(y=y,v1=v1,string=string,data=data,ci=ci)
}
if (method=="cie"){
cie_test(y=y,v1=v1,string=string,data=data,logistic=logistic,cut=cut,v2=v2,v3=v3)
}
return(df)
}

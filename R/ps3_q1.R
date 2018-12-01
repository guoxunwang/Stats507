##Stats 506, F18, Problem Set 3, Question 1
##
##data analysis using data.table package of Residential Energy Consumption 
##Survey (RECS 2015) balanced repeated replication (BRR) method of estimating 
##standard error
##
##Author: Xun Wang, xunwang@umich.edu
##Updated: November 2,2018- Lasted modified date

#80:--------------------------------------------------------------------------

#Libraries:-------------------------------------------------------------------
library(data.table)
library(magrittr)
library(ggplot2)

#Directories or website read from:--------------------------------------------
file='recs2015_public_v3.csv'
if(file.exists(file)){
  recs_dt=data.table::fread(file)}else{
    recs_dt=data.table::fread("https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv")
  }


##Question1_a:----------------------------------------------------------------
names_brrwt=paste("BRRWT",1:96,sep="")
recs_1=recs_dt[,.SD,.SDcols=c("DIVISION","WALLTYPE","NWEIGHT",names_brrwt)]
para=4/96
m=qnorm(.975)

###Decode functions-div-------------------------------------------------------
decode_div=function(x){
  switch(x,"New England","Middle Atlantic","East North Central",
         "West North Central","South Atlantic","East South Central",
         "West South Central","Mountain North","Mountain South","Pacific")}
decode_all_div=function(x){
  sapply(x,decode_div)}

###Decode functions-major outside wall material
decode_mowm=function(x){
  switch(x,"Brick","Wood","Siding","Stucco","Shingle",
         "Stone","Block","BLANK","Other")}
decode_all_mowm=function(x){
  sapply(x,decode_mowm)}

###Decode functions-urban and rural
###see both urban cluster and urban area as urban
decode_ur=function(x){
  y=which(c("U","C","R")==x)
  switch(y,"Urban","Urban","Rural")}
decode_all_ur=function(x){
  sapply(x,decode_ur)}

###define a function to calculate percent
pct=function(x){
  if(is.numeric(x)){
    x/sum(x)
  }else{
    x
  }
}
###major outside wall material in each division and BRR standard error--------
recs_plot_1=recs_1[,`:=`(div=decode_all_div(DIVISION),
                     mowm=decode_all_mowm(WALLTYPE))]%>%
  .[,lapply(.SD,sum),by=.(div,mowm),.SDcols=NWEIGHT:BRRWT96]%>%
  .[,lapply(.SD,pct),by=.(div),.SDcols=mowm:BRRWT96]%>%
  .[mowm=="Stucco",]%>%
  .[,lapply(.SD,function(x){(x-NWEIGHT)^2}),by=.(div,NWEIGHT),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,pct=100*NWEIGHT,se=100*sqrt(para*rowSums(.SD))),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,lwr:=pmax(pct-m*se,0)]%>%
  .[,upr:=pct+m*se]%>%
  .[order(-pct)]%>%
  .[,-c("se")]
recs_table_1=recs_plot_1[,.(div,ci=sprintf("%5.3f(%5.3f,%5.3f)",pct,lwr,upr))]

##Question_b:----------------------------------------------------------------
###average stratified by urban and rural status------------------------------
recs_2=recs_dt[,.SD,.SDcols=c("DIVISION","UATYP10","KWH","NWEIGHT",names_brrwt)]
recs_plot_2=recs_2[,`:=`(div=decode_all_div(DIVISION),
                     ur=decode_all_ur(UATYP10))]%>%
  .[,lapply(.SD,function(x){sum(KWH*x)/sum(x)}),by=.(div,ur),
    .SDcols=NWEIGHT:BRRWT96]%>%
  .[,lapply(.SD,function(x){(x-NWEIGHT)^2}),by=.(div,ur,NWEIGHT),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,ur,average_ur=NWEIGHT,se=sqrt(para*rowSums(.SD))),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,ur,average_ur,lwr=average_ur-m*se,upr=average_ur+m*se)]
recs_table_2=recs_plot_2[,.(div,ur,average_ur,ci=sprintf("%8.3f(%8.3f,%8.3f)",
                                              average_ur,lwr,upr))]%>%
  dcast(div~ur,value.var=c("average_ur","ci"))%>%
  .[order(-average_ur_Rural)]%>%
  .[,-c("average_ur_Rural","average_ur_Urban")]

###total average-------------------------------------------------------------
recs_ave_plot=recs_2[,`:=`(div=decode_all_div(DIVISION))]%>%
  .[,lapply(.SD,function(x){sum(KWH*x)/sum(x)}),by=.(div),
    .SDcols=NWEIGHT:BRRWT96]%>%
  .[,lapply(.SD,function(x){(x-NWEIGHT)^2}),by=.(div,NWEIGHT),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,average=NWEIGHT,se=sqrt(para*rowSums(.SD))),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,`:=`(lwr=average-m*se,upr=average+m*se)]%>%
  .[order(-average)]%>%
  .[,-c("se")]
recs_ave_table=recs_ave_plot[,.(div,ci=sprintf("%8.3f(%8.3f,%8.3f)",
                                               average,lwr,upr))]

##Question1_c:---------------------------------------------------------------
###proportion of homes with internet access and BRR standard error-----------
recs_3=recs_dt[,.SD,.SDcols=c("DIVISION","UATYP10","INTERNET","NWEIGHT",names_brrwt)]
recs_plot_3=recs_3[,`:=`(div=decode_all_div(DIVISION),
                     ur=decode_all_ur(UATYP10))]%>%
  .[,lapply(.SD,function(x){sum(INTERNET*x)/sum(x)}),by=.(div,ur),
    .SDcols=NWEIGHT:BRRWT96]%>%
  .[,lapply(.SD,function(x){(x-NWEIGHT)^2}),by=.(div,ur,NWEIGHT),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,ur,pct=100*NWEIGHT,se=100*sqrt(para*rowSums(.SD))),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,ur,pct,lwr=pmax(pct-m*se,0),upr=pct+m*se)]
recs_table_3=recs_plot_3[,.(div,ur,ci=sprintf("%5.3f(%5.3f,%5.3f)",
                                              pct,lwr,upr))]%>%
  dcast(div~ur,value.var="ci")

###disparity between urban and rural areas------------------------------------
recs_diff=recs_3[,`:=`(div=decode_all_div(DIVISION),
                       ur=decode_all_ur(UATYP10))]%>%
  .[,lapply(.SD,function(x){sum(INTERNET*x)/sum(x)}),by=.(div,ur),
    .SDcols=NWEIGHT:BRRWT96]%>%
  .[,lapply(.SD,diff),by="div",.SDcols=NWEIGHT:BRRWT96]%>%
  .[,lapply(.SD,function(x){(x-NWEIGHT)^2}),by=.(div,NWEIGHT),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,diff=-100*NWEIGHT,se=100*sqrt(para*rowSums(.SD))),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,difference:=sprintf("%5.3f(%5.3f,%5.3f)",diff,diff-m*se,diff+m*se)]%>%
  .[,-c("se")]

###division with largest disparity--------------------------------------------
recs_disp=merge(recs_table_3,recs_diff,by="div",all=TRUE)%>%
  .[order(-diff)]%>%
  .[,-c("diff")]

##Question1_d:---------------------------------------------------------------
recs_4=recs_dt[,.SD,.SDcols=c("DIVISION","UATYP10","SOLAR","NWEIGHT",names_brrwt)]
recs_plot_4=recs_4[,`:=`(div=decode_all_div(DIVISION),
                         ur=decode_all_ur(UATYP10))]%>%
  .[!SOLAR==-2,]%>%
  .[,lapply(.SD,function(x){sum(SOLAR*x)/sum(x)}),by=.(div,ur),
    .SDcols=NWEIGHT:BRRWT96]%>%
  .[,lapply(.SD,function(x){(x-NWEIGHT)^2}),by=.(div,ur,NWEIGHT),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,ur,pct=100*NWEIGHT,se=100*sqrt(para*rowSums(.SD))),
    .SDcols=BRRWT1:BRRWT96]%>%
  .[,.(div,ur,pct,lwr=pmax(pct-m*se,0),upr=pct+m*se)]
recs_table_4=recs_plot_4[,.(div,ur,pct,ci=sprintf("%5.3f(%5.3f,%5.3f)",
                                              pct,lwr,upr))]%>%
  dcast(div~ur,value.var=c("ci","pct"))%>%
  .[order(-pct_Rural,-pct_Urban)]%>%
  .[,-c("pct_Rural","pct_Urban")]

#80:--------------------------------------------------------------------------
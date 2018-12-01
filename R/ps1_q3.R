##Stats 506, F18, Problem Set 1, Question 3
##
##data analysis of Residential Energy Consumption Survey (RECS 2015)
##balanced repeated replication (BRR) method of estimating standard error
##
##Author: Xun Wang, xunwang@umich.edu
##Updated: October 1,2018- Lasted modified date

#80:--------------------------------------------------------------------------

#Libraries:-------------------------------------------------------------------
library(dplyr)
library(tibble)
library(tidyr)
library(magrittr)

#Directories or website read from:--------------------------------------------
file='./recs2015_public_v3.csv'
if(file.exists(file)){
  recs_tib=readr::read_delim(file, delim=',')}else{
  recs_tib=readr::read_delim("https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv",delim=',')
  }

##Question3_a:----------------------------------------------------------------
recs_2015=select(recs_tib,div=DIVISION,mowm=WALLTYPE,ur=UATYP10,
                 ele=KWH,internet=INTERNET,wei=NWEIGHT,BRRWT1:BRRWT96)
para=1/(96*(0.5)^2)

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

###major outside wall material in each division and BRR standard error--------
recs_1=select(recs_2015,div,mowm,wei,BRRWT1:BRRWT96)
recs_decode_1=mutate(recs_1,div=decode_all_div(div),
                     mowm=decode_all_mowm(mowm))%>%
  group_by(div,mowm)%>%
  summarize_at(.vars=vars(wei:BRRWT96),
               .funs=sum)%>%
  mutate_at(.vars=vars(wei:BRRWT96),
            .funs=funs(./sum(.)))%>%
  filter(mowm=="Stucco")%>%
  mutate_at(.vars=vars(BRRWT1:BRRWT96),
            .funs=funs((.-wei)^2))%>%
  ungroup()%>%
  mutate(var_1=para*rowSums(.[4:99]))%>%
  group_by(div)%>%
  mutate(se_1=sqrt(var_1)*100,rse_1=sqrt(var_1)*100/wei)%>%
  mutate(pct_stucco=100*wei)%>%
  select(div,pct_stucco,se_1,rse_1)%>%
  arrange(desc(pct_stucco))

###the highest Proportion of stucco as the major outside wall material
high_stucco=recs_decode_1$div[1]

###the lowest Proportion of stucco as the major outside wall material
low_stucco=recs_decode_1$div[length(recs_decode_1$div)]

##Question_b:----------------------------------------------------------------
###average stratified by urban and rural status------------------------------
recs2=select(recs_2015,div,ur,ele,wei,BRRWT1:BRRWT96)
recs_decode_2=mutate(recs2,div=decode_all_div(div),
                     ur=decode_all_ur(ur))%>%
  group_by(div,ur)%>%
  summarize_at(.vars=vars(wei:BRRWT96),
               .funs=funs(sum(.*ele)/sum(.)))%>%
  mutate_at(.vars=vars(BRRWT1:BRRWT96),
            .funs=funs((.-wei)^2))%>%
  ungroup()%>%
  mutate(var_2=para*rowSums(.[4:99]))%>%
  group_by(div)%>%
  mutate(se_2=sqrt(var_2),rse_2=100*sqrt(var_2)/wei)%>%
  rename(ave_ur=wei)%>%
  select(div,ur,ave_ur,se_2,rse_2)

###total average-------------------------------------------------------------
recs_ave_total=mutate(recs2,div=decode_all_div(div),
                      ur=decode_all_ur(ur))%>%
  select(-ur)%>%
  group_by(div)%>%
  summarize_at(.vars=vars(wei:BRRWT96),
               .funs=funs(sum(.*ele)/sum(.)))%>%
  mutate_at(.vars=vars(BRRWT1:BRRWT96),
            .funs=funs((.-wei)^2))%>%
  ungroup()%>%
  mutate(var_t=rowSums(.[3:98])*para)%>%
  group_by(div)%>%
  mutate(se_t=sqrt(var_t),rse_t=100*sqrt(var_t)/wei)%>%
  rename(ave=wei)%>%
  select(div,ave,se_t,rse_t)

##Question3_c:---------------------------------------------------------------
###proportion of homes with internet access and BRR standard error-----------
recs_3=select(recs_2015,div,ur,internet,wei,BRRWT1:BRRWT96)
recs_decode_3=mutate(recs_3,div=decode_all_div(div),
                     ur=decode_all_ur(ur))%>%
  group_by(div,ur)%>%
  summarize_at(.vars=vars(wei:BRRWT96),
               .funs=funs(sum(.*internet)/sum(.)))%>%
  mutate_at(.vars=vars(BRRWT1:BRRWT96),
            .funs=funs((.-wei)^2))%>%
  ungroup()%>%
  mutate(var_3=rowSums(.[4:99])*para)%>%
  group_by(div)%>%
  mutate(se_3=100*sqrt(var_3),rse_3=100*sqrt(var_3)/wei)%>%
  mutate(prop=100*wei)%>%
  select(div,ur,prop,se_3,rse_3)

###disparity between urban and rural areas------------------------------------
recs_disp=select(recs_decode_3,div,ur,prop)%>%
  spread(ur,prop)%>%
  mutate(disp=abs(Rural-Urban))%>%
  arrange(desc(disp))

###division with largest disparity--------------------------------------------
prop_disp=recs_disp[1,]$div

#80:--------------------------------------------------------------------------

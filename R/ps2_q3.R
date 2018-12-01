##Stats 506, F18, Problem Set 2, Question 3
##
##logistic regression of NHANES
##using predict,margins
##
##Author: Xun Wang, xunwang@umich.edu
##Updated: October 12,2018- Lasted modified date

#80:--------------------------------------------------------------------------

#Libraries:-------------------------------------------------------------------
library(dplyr)
library(tibble)
library(tidyr)
library(magrittr)
library(SASxport)

#Read data:-------------------------------------------------------------------
oral=read.xport("OHX_D.XPT")
demo=read.xport('DEMO_D.XPT')

##Part A:---------------------------------------------------------------------
nhanes=merge(oral,demo,by="SEQN")

##Part B:---------------------------------------------------------------------
decode_ohx04htc=function(rvec){
  sapply(rvec,function(r) switch(r,0,1,"Blank",1,1))
}
model_1_data=filter(nhanes, !is.na(RIDAGEMN))%>%
  filter(!is.na(OHX04HTC))%>%
  filter(OHX04HTC!=9)%>%
  mutate(tooth=decode_ohx04htc(OHX04HTC))%>%
  mutate(tooth=factor(tooth))
model_1=glm(tooth~RIDAGEMN,data=model_1_data,family=binomial(link='logit'))
age_25=round((log(.25/.75)-coef(model_1)[1])/coef(model_1)[2])
age_50=round((log(.5/.5)-coef(model_1)[1])/coef(model_1)[2])
age_75=round((log(.75/.25)-coef(model_1)[1])/coef(model_1)[2])
age_floor=floor(age_25/12)
age_ceiling=ceiling(age_75/12)
##representative age range is 8,9,10,11,12

##Part C:---------------------------------------------------------------------
###gender
bic_1=BIC(model_1)
model_2_data=filter(model_1_data, !is.na(RIAGENDR))%>%
  mutate(gender=factor(RIAGENDR))
model_2=glm(tooth~RIDAGEMN+gender,data=model_2_data,family=binomial(link='logit'))
bic_2=BIC(model_2)
##the bic is bigger, do not retain gender in model

###race
decode_mexico=function(rvec){
  sapply(rvec,function(r) switch(r,1,0,0,0,0))
}
decode_other=function(rvec){
  sapply(rvec,function(r) switch(r,0,1,0,0,1))
}
decode_white=function(rvec){
  sapply(rvec,function(r) switch(r,0,0,1,0,0))
}
decode_black=function(rvec){
  sapply(rvec,function(r) switch(r,0,0,0,1,0))
}
model_3_data=filter(model_2_data,!is.na(RIDRETH1))%>%
  mutate(mexico=decode_mexico(RIDRETH1))%>%
  mutate(other=decode_other(RIDRETH1))%>%
  mutate(white=decode_white(RIDRETH1))%>%
  mutate(black=decode_black(RIDRETH1))
mexico_pop=sum(model_3_data$mexico)
other_pop=sum(model_3_data$other)
white_pop=sum(model_3_data$white)
black_pop=sum(model_3_data$black)
##white poplulation as the reference
model_3=glm(tooth~RIDAGEMN+mexico,data=model_3_data,family=binomial(link='logit'))
bic_3=BIC(model_3)
##bic is bigger, do not add mexico to the model
model_4=glm(tooth~RIDAGEMN+other,data=model_3_data,family=binomial(link='logit'))
bic_4=BIC(model_4) 
##bic is bigger, do not add other to the model
model_5=glm(tooth~RIDAGEMN+black,data=model_3_data,family=binomial(link='logit'))
bic_5=BIC(model_5)
##bic is smaller, retain black in the model

###poverty income ratio
model_6_data=filter(model_3_data,!is.na(INDFMPIR))
model_6=glm(tooth~RIDAGEMN+black+INDFMPIR,data=model_3_data,
            family=binomial(link='logit'))
bic_6=BIC(model_6)
##bic is lower strongly, add poverty income ratio to the model

##Part D:---------------------------------------------------------------------
###Part D_1
atmean_repre_age=mutate(model_6_data, black=mean(black),
                        INDFMPIR=mean(INDFMPIR))%>%
  select(RIDAGEMN,black,INDFMPIR)%>%
  unique()
p8=predict(model_6,atmean_repre_age%>%mutate(RIDAGEMN=8*12),type='link')
p9=predict(model_6,atmean_repre_age%>%mutate(RIDAGEMN=9*12),type='link')
p10=predict(model_6,atmean_repre_age%>%mutate(RIDAGEMN=10*12),type='link')
p11=predict(model_6,atmean_repre_age%>%mutate(RIDAGEMN=11*12),type='link')
p12=predict(model_6,atmean_repre_age%>%mutate(RIDAGEMN=12*12),type='link')
pred=sapply(list(p8,p9,p10,p11,p12),function(x) exp(mean(x))/{1+exp(mean(x))})

###Part_D_2
atmean_ind_white=mutate(model_6_data,INDFMPIR=mean(INDFMPIR))%>%
  filter(black==0)%>%
  select(RIDAGEMN,black,INDFMPIR)%>%
  unique()
p8_white=predict(model_6,atmean_ind_white%>%mutate(RIDAGEMN=8*12),type='link')
p9_white=predict(model_6,atmean_ind_white%>%mutate(RIDAGEMN=9*12),type='link')
p10_white=predict(model_6,atmean_ind_white%>%mutate(RIDAGEMN=10*12),type='link')
p11_white=predict(model_6,atmean_ind_white%>%mutate(RIDAGEMN=11*12),type='link')
p12_white=predict(model_6,atmean_ind_white%>%mutate(RIDAGEMN=12*12),type='link')
pred_white=sapply(list(p8_white,p9_white,p10_white,p11_white,p12_white),
                  function(x) exp(mean(x))/{1+exp(mean(x))})
atmean_ind_black=mutate(model_6_data,INDFMPIR=mean(INDFMPIR))%>%
  filter(black==1)%>%
  select(RIDAGEMN,black,INDFMPIR)%>%
  unique()
p8_black=predict(model_6,atmean_ind_black%>%mutate(RIDAGEMN=8*12),type='link')
p9_black=predict(model_6,atmean_ind_black%>%mutate(RIDAGEMN=9*12),type='link')
p10_black=predict(model_6,atmean_ind_black%>%mutate(RIDAGEMN=10*12),type='link')
p11_black=predict(model_6,atmean_ind_black%>%mutate(RIDAGEMN=11*12),type='link')
p12_black=predict(model_6,atmean_ind_black%>%mutate(RIDAGEMN=12*12),type='link')
pred_black=sapply(list(p8_black,p9_black,p10_black,p11_black,p12_black),
                  function(x) exp(mean(x))/{1+exp(mean(x))})
margin_atmean=pred_black-pred_white

###Part_D_3
average_white=mutate(model_6_data,black=0)%>%
  select(RIDAGEMN,black,INDFMPIR)
p8_ave_w=predict(model_6,average_white%>%mutate(RIDAGEMN=8*12),type='link')
p9_ave_w=predict(model_6,average_white%>%mutate(RIDAGEMN=9*12),type='link')
p10_ave_w=predict(model_6,average_white%>%mutate(RIDAGEMN=10*12),type='link')
p11_ave_w=predict(model_6,average_white%>%mutate(RIDAGEMN=11*12),type='link')
p12_ave_w=predict(model_6,average_white%>%mutate(RIDAGEMN=12*12),type='link')
pred_ave_w=sapply(list(p8_ave_w,p9_ave_w,p10_ave_w,p11_ave_w,p12_ave_w),
                  function(x) mean(exp(x)/{1+exp(x)}))
average_black=mutate(model_6_data,black=1)%>%
  select(RIDAGEMN,black,INDFMPIR)
p8_ave_b=predict(model_6,average_black%>%mutate(RIDAGEMN=8*12),type='link')
p9_ave_b=predict(model_6,average_black%>%mutate(RIDAGEMN=9*12),type='link')
p10_ave_b=predict(model_6,average_black%>%mutate(RIDAGEMN=10*12),type='link')
p11_ave_b=predict(model_6,average_black%>%mutate(RIDAGEMN=11*12),type='link')
p12_ave_b=predict(model_6,average_black%>%mutate(RIDAGEMN=12*12),type='link')
pred_ave_b=sapply(list(p8_ave_b,p9_ave_b,p10_ave_b,p11_ave_b,p12_ave_b),
                  function(x) mean(exp(x)/{1+exp(x)}))
pred_average=pred_ave_b-pred_ave_w

#80:--------------------------------------------------------------------------

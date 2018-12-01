##Stats 506, F18, Problem Set 1, Question 2
##
##data analysis of flights originating from NY through 2013 and Oct,2014
##using nycflights2013 R package and data from websites
##
##Author: Xun Wang, xunwang@umich.edu
##Updated: October 1,2018- Lasted modified date

#80:--------------------------------------------------------------------------

#Libraries:-------------------------------------------------------------------
library(dplyr)
library(tibble)
library(tidyr)
library(magrittr)
library(nycflights13)

#Read data:-------------------------------------------------------------------
nycflight14=readr::read_delim(
  "https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv",delim = ',' )

##Question2_a:----------------------------------------------------------------
Oct31=select(nycflights13::flights,month,carrier)%>%
  filter(month<=10)
carrier_code=group_by(Oct31,carrier)%>%
  summarize(times=n())%>%
  mutate(pct=100*times/nrow(Oct31))%>%
  filter(pct>=1)
carrier_name=filter(nycflights13::airlines,carrier%in%carrier_code$carrier)
carrier_name_list=left_join(carrier_name,carrier_code)%>%
  select(-times,-carrier)
  
##Question2_b:----------------------------------------------------------------
###number, percent and CI of 2014---------------------------------------------
m=qnorm(1-{1-0.95}/2)
tib_Oct_2014=select(nycflight14,year,month,carrier)%>%
  filter(month<=10)
tib_2014=filter(tib_Oct_2014,carrier%in%carrier_code$carrier)%>%
  group_by(carrier)%>%
  summarise(times_2014=n(),pct_2014=100*times_2014/nrow(tib_Oct_2014))%>%
  mutate(se_2014=sqrt(pct_2014*(100-pct_2014)/nrow(tib_Oct_2014)),
         lwr_2014=pct_2014-se_2014*m,upr_2014=pct_2014+se_2014*m)%>%
  left_join(carrier_name)%>%
  select(-carrier,-se_2014)%>%
  select(name,times_2014,pct_2014,lwr_2014,upr_2014)

###number, percent and CI of 2013---------------------------------------------
tib_Oct_2013=select(nycflights13::flights,year,month,carrier)%>%
  filter(month<=10)
tib_2013=filter(tib_Oct_2013,carrier%in%carrier_code$carrier)%>%
  group_by(carrier)%>%
  summarize(times_2013=n(),pct_2013=100*times_2013/nrow(tib_Oct_2013))%>%
  mutate(se_2013=sqrt(pct_2013*(100-pct_2013)/nrow(tib_Oct_2013)),
         lwr_2013=pct_2013-se_2013*m,upr_2013=pct_2013+se_2013*m)%>%
  left_join(carrier_name)%>%
  select(-carrier,-se_2013)%>%
  select(name,times_2013,pct_2013,lwr_2013,upr_2013)

###change in percent and its CI-----------------------------------------------
tib=left_join(tib_2013,tib_2014)%>%
  rowwise()%>%
  mutate(times_diff=times_2014-times_2013,
         pct_diff=pct_2014-pct_2013,
         se_diff=sqrt(pct_2013*(100-pct_2013)/nrow(tib_Oct_2013)+
                      pct_2014*(100-pct_2014)/nrow(tib_Oct_2014)),
         lwr_diff=pct_diff-se_diff*m,upr_diff=pct_diff+se_diff*m)%>%
  select(-se_diff)%>%
  arrange(desc(pct_diff))
tib_diff=select(tib,name,times_diff,pct_diff,lwr_diff,upr_diff)

###airlines with largest increase
in_name=tib[1,]$name

###airlines with largest decrease
de_name=tib[length(tib$pct_diff)-1,]$name

###increase in the percent of flights but a decrease in the number of flights
tib_in_de=filter(tib,pct_diff>=0,times_diff<=0)%>%
  select(name,times_2013,pct_2013,times_2014,pct_2014,times_diff,pct_diff)
num_2013=sum(tib$times_2013)
num_2014=sum(tib$times_2014,na.rm=TRUE)

##Question1_c:----------------------------------------------------------------
###2013 year, annual percent and CI-------------------------------------------
nyc2013=select(nycflights13::flights,year,origin,carrier)%>%
  group_by(year,origin,carrier)%>%
  summarize(times_2013=n())%>%
  mutate(pct_2013=100*times_2013/sum(times_2013),
         se_2013=sqrt(pct_2013*(100-pct_2013)/sum(times_2013)),
         lwr_2013=pct_2013-se_2013*m,upr_2013=pct_2013+se_2013*m)%>%
  filter(carrier%in%carrier_code$carrier)%>%
  left_join(carrier_name)%>%
  select(-times_2013,-se_2013,-carrier)%>%
  select(year,origin,name,pct_2013,lwr_2013,upr_2013)

###largest carrier for airport EWR in 2013
ewr_2013=filter(nyc2013,origin=="EWR")%>%
  arrange(desc(pct_2013))
ewr_name_2013=(ewr_2013[1,])$name

###largest carrier for airport JFK in 2013
jfk_2013=filter(nyc2013,origin=="JFK")%>%
  arrange(desc(pct_2013))
jfk_name_2013=(jfk_2013[1,])$name

###largest carrier for airport LGA in 2013
lga_2013=filter(nyc2013,origin=="LGA")%>%
  arrange(desc(pct_2013))
lga_name_2013=(lga_2013[1,])$name

###2014 year,percent of data through Oct. and CI-------------------------------
nyc2014=select(nycflight14,year,origin,carrier)%>%
  group_by(year,origin,carrier)%>%
  summarize(times_2014=n())%>%
  mutate(pct_2014=100*times_2014/sum(times_2014),
         se_2014=sqrt(pct_2014*(100-pct_2014)/sum(times_2014)),
         lwr_2014=pct_2014-se_2014*m,upr_2014=pct_2014+se_2014*m)%>%
  filter(carrier%in%carrier_code$carrier)%>%
  left_join(carrier_name)%>%
  select(-times_2014,-se_2014,-carrier)%>%
  select(year,origin,name,pct_2014,lwr_2014,upr_2014)

###largest carrier for airport EWR in 2014
ewr_2014=filter(nyc2014,origin=="EWR")%>%
  arrange(desc(pct_2014))
ewr_name_2014=(ewr_2014[1,])$name

###largest carrier for airport JFK in 2014
jfk_2014=filter(nyc2014,origin=="JFK")%>%
  arrange(desc(pct_2014))
jfk_name_2014=(jfk_2014[1,])$name

###largest carrier for airport LGA in 2014
lga_2014=filter(nyc2014,origin=="LGA")%>%
  arrange(desc(pct_2014))
lga_name_2014=(lga_2014[1,])$name

#80:--------------------------------------------------------------------------

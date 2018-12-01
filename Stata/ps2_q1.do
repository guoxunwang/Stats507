* ------------------------------------------------------------------------------
* Stats 506, F18, Problem Set 2, Question 1	  	          
* 
* Using reshape command to calculate the standard error 
* of survey sample with weights
* 
* Author: xunwang@umich.edu
* Date: Oct 8, 2018	 
* ------------------------------------------------------------------------------

* Files: recs2015_public_v3.csv
* imported from the address below:
* https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv 

version 14.2

import delimited recs2015_public_v3.csv, clear
keep kwh cufeetng gallonlp gallonfo nweight brrwt*

*reshape is fail in the first time because some observations are duplicate
rename nweight brrwt0
generate case_number=_n
reshape long brrwt, i(kwh cufeetng gallonlp gallonfo case_number) j(brrwt_number)
drop case_number

replace kwh=brrwt*kwh
replace cufeetng=brrwt*cufeetng
replace gallonlp=brrwt*gallonlp
replace gallonfo=brrwt*gallonfo
drop brrwt

collapse (sum) kwh cufeetng gallonlp gallonfo, by(brrwt_number)

generate kwh_se=sqrt(sum((kwh-kwh[1])^2)/(96*0.25))
generate cufeetng_se=sqrt(sum((cufeetng-cufeetng[1])^2)/(96*0.25))
generate gallonlp_se=sqrt(sum((gallonlp-gallonlp[1])^2)/(96*0.25))
generate gallonfo_se=sqrt(sum((gallonfo-gallonfo[1])^2)/(96*0.25))
replace kwh_se=kwh if brrwt_number==0
replace cufeetng_se=cufeetng if brrwt_number==0
replace gallonlp_se=gallonlp if brrwt_number==0
replace gallonfo_se=gallonfo if brrwt_number==0
drop kwh cufeetng gallonlp gallonfo
keep if brrwt_number==0|brrwt_number==96

export delimited recs2015_usage.csv, replace






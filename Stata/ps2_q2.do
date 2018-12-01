* ------------------------------------------------------------------------------
* Stats 506, F18, Problem Set 2, Question 2	  	          
* 
* NHANES Oral Health and demographic data
* 
* Author: xunwang@umich.edu
* Date: Oct 10, 2018
* 	 
* ------------------------------------------------------------------------------
*
* Files: OHX_D.XPT DEMO_D.XPT
* imported from the address below:
* 

version 14.2
log using ps2_q2.log, text replace

*Question a:--------------------------------------------------------------------
fdause OHX_D.XPT, clear
sort seqn
save OHX_D.dta,replace
fdause DEMO_D.XPT, clear
sort seqn
merge 1:1 seqn using "OHX_D.dta"
keep if _merge==3

*Question b:--------------------------------------------------------------------
replace ohx04htc=2 if ohx04htc==5
drop if ohx04htc==9
drop if ohx04htc==.
drop if ridagemn==.
*see Perment as 0
replace ohx04htc=0 if ohx04htc==2
replace ohx04htc=0 if ohx04htc==4
replace ohx04htc=7 if ohx04htc==0
replace ohx04htc=0 if ohx04htc==1
replace ohx04htc=1 if ohx04htc==7

//Table 2. regression table of age and probability of losing primary------------
logit ohx04htc ridagemn, nolog
ssc install outreg2
outreg2 using model_1.txt, stats(coef se tstat pval ci) auto(5) noparen replace ctitle(Logit Coeff)

*25% individuals lose primary tooth
matrix b1=e(b)
local age_25=(log(.25/.75)-b1[1,2])/b1[1,1]
display round(`age_25')
*the age for 25% is 104 month
local age_50=(log(.5/.5)-b1[1,2])/b1[1,1]
display round(`age_50')
*the age for 50% is 120 month
local age_75=(log(.75/.25)-b1[1,2])/b1[1,1]
display round(`age_75')
*the age for 75% is 136 month

*take the floor of representative ages
display floor(`age_25'/12)
display ceil(`age_75'/12)
*representative ages are from 8 to 12 ages
estat ic
*bic=1533.407

*Question c:--------------------------------------------------------------------
*gender
drop if riagendr==.
//Regression table of age, gender and probability of losing primary-------------
logit ohx04htc ridagemn i.riagendr,nolog
estat ic
*bic is larger, do not retain gender in model

*race
drop if ridreth1==.
replace ridreth1=2 if ridreth1==5
generate mexico=1 if ridreth1==1
replace mexico=0 if mexico==.
generate other=1 if ridreth1==2
replace other=0 if other==.
generate white=1 if ridreth1==3
replace white=0 if white==.
generate black=1 if ridreth1==4
replace black=0 if black==.
generate sum_mexico=sum(mexico)
generate sum_other=sum(other)
generate sum_white=sum(white)
generate sum_black=sum(black)
*white has the largest population, see white as reference

//Regression table of age, mexico race and probability of losing primary--------
logit ohx04htc ridagemn i.mexico,nolog
estat ic
*bic is larger, do not add mexico to the model

//Regression table of age, other races and probability of losing primary--------
logit ohx04htc ridagemn i.other,nolog
estat ic
*bic is larger, do not add other race to the model

//Regression table of age, black race and probability of losing primary---------
logit ohx04htc ridagemn i.black,nolog
estat ic
*bic=1529.281 is smaller, add black into model

* poverty income ratio
drop if indfmpir==.
//Table 3. regression table of age, black race, poverty income ratio------------
//and probability of losing primary, which is final model
logit ohx04htc ridagemn i.black indfmpir,nolog
estat ic
*bic is smaller, add poverty income ratio into model
outreg2 using final_model.txt, stats(coef se tstat pval ci) auto(5) noparen replace ctitle(Logit Coeff)

*Question d:--------------------------------------------------------------------
//Table 4. Adjusted predictions at the mean-------------------------------------
margins, at(ridagemn=(96 108 120 132 144)) atmeans post
outreg2 using margin_1.txt, stats(coef se tstat pval ci) auto(5) noparen replace ctitle(Adjusted Predictions)

//Table 5. Marginal effects at the mean-----------------------------------------
quietly logit ohx04htc ridagemn i.black indfmpir,nolog
margins, dydx(i.black) at(ridagemn=(96 108 120 132 144)) atmeans post
outreg2 using margin_2.txt, stats(coef se tstat pval ci) auto(5) noparen replace ctitle(Margin)

//Table 6. Average marginal effects---------------------------------------------
quietly logit ohx04htc ridagemn i.black indfmpir,nolog
margins, dydx(i.black) at(ridagemn=(96 108 120 132 144)) post
outreg2 using margin_3.txt, stats(coef se tstat pval ci) auto(5) noparen replace ctitle(Margin)

*Question e:--------------------------------------------------------------------
//Table 7. regression tabel of the final model with svy prefix------------------
svyset sdmvpsu [pweight=wtmec2yr], strata(sdmvstra) vce(linearized)
svy:logit ohx04htc ridagemn i.black indfmpir,nolog
outreg2 using svy_final_model.txt, stats(coef se tstat pval ci) auto(5) noparen replace ctitle(Margin)


log close
* ------------------------------------------------------------------------------

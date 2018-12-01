*install ridgereg package
ssc install ridgereg

*import data and data description
import delimited meatspec.csv,clear
summarize v1 v2 v3 v4 v5 fat

*standardize independent variables and centered dependent variable
summarize fat
local mean_fat=r(mean)
replace fat=fat-r(mean) 
foreach i of numlist 1/100 {
    summarize v`i'
	replace v`i'=(v`i'-r(mean))/r(sd)
}

*cross-validation assign group
generate n=_n

*graph of coefficients: converge to 0
preserve
ridgereg fat v* if mod(n,5)!=0,model(orr) kr(0) coll diag
matrix coeff=e(b)
local index=1
generate lambda=0
foreach i of numlist 1e-9(1e-9)5e-8{
    local index=`index'+1
    ridgereg fat v* if mod(n,5)!=0,model(orr) kr(`i') coll diag
	replace lambda=`i' in `index'
	matrix b=e(b)
	matrix coeff=(coeff\b)
}
svmat coeff,names(coeff)
line coeff1-coeff99 lambda
restore

*return GCV(Generalized Cross-Validation) value
generate gcv=10
generate lamda=0
local index=0
foreach i of numlist 0(1e-11)1e-9{
    local index=`index'+1
    ridgereg fat v* if mod(n,5)!=0,model(orr) kr(`i') coll diag
	replace lamda=`i' in `index'
	replace gcv=e(gcv) in `index'
}

*graph of GCV values on lamda
lowess gcv lamda if n<=101

summarize gcv if n<=101
summarize lamda if gcv==r(min)

*test performances of model, lamda=5e-10
ridgereg fat v* if mod(n,5)!=0,model(orr) kr(5e-10) coll predict(pred)
matrix coeff=e(b)'
generate constant=1
mkmat v* constant, matrix(predictors)
matrix pred_fat=predictors*coeff
svmat pred_fat,names(pred_fat)

**compute MSE
generate err_fat=(pred_fat-fat)^2
summarize err_fat if mod(n,5)==0
display r(mean)
**exclude 727 abnormal observation
summarize err_fat if mod(n,5)==0&n!=185
display r(mean)


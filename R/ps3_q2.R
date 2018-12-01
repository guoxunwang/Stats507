##Stats 506, F18, Problem Set 3, Question 2
##
##design a Monte Carlo study in R to compare the performance of different 
##methods that adjust for multiple comparisons
##
##Author: Xun Wang, xunwang@umich.edu
##Updated: November 7,2018- Lasted modified date

#80:--------------------------------------------------------------------------

#Libraries:-------------------------------------------------------------------
library(data.table)
library(magrittr)
library(tidyr)

##Question2_a:----------------------------------------------------------------
n=1000;p=100
beta=c(rep(1,10),rep(0,90))
p_value=function(X,beta,sigma,mc_rep){
  
  ##because epsilon are independent
  set.seed(77)
  epsilon=rnorm(n*mc_rep,0,sigma)
  Y=epsilon+as.vector(X%*%beta)
  dim(Y)=c(n,mc_rep)
  QR=qr(t(X)%*%X)
  
  ##i.estimate of beta
  beta_est=solve(qr.R(QR),t(qr.Q(QR))%*%t(X)%*%Y)
  
  ##ii.estimate of the error variance
  resids=Y-X%*%beta_est
  sigma_square_est=(1/(n-p))*diag(t(resids)%*%(resids))
  
  ##iii.the variance of the estimate of beta
  v=diag(chol2inv(chol(t(X)%*%X)))
  dim(v)=c(p,1)
  dim(sigma_square_est)=c(1,mc_rep)
  var_beta=v%*%sigma_square_est
  
  ##iv.compute p-values
  Z=beta_est/sqrt(var_beta)
  p=2*(1-pnorm(abs(Z)))
  p
}

###test the function
###use the Cholesky factorization to generate X=M
sigma_x=diag(p)
sigma_x[77,7]=0.7
sigma_x[7,77]=0.7
R=chol(sigma_x)
x=rnorm(n*p)
dim(x)=c(n,p)
M=x%*%R
###generate the same y
sigma_y=7
set.seed(77)
epsil=rnorm(n,0,sigma_y)
y=epsil+as.vector(M%*%beta)
###use lm function to do linear regression
test=lm(y~0+M)
###use function above
test_p=p_value(M,beta,sigma_y,1)

##Question2_b:----------------------------------------------------------------
pvalues=p_value(M,beta,sigma_y,1000)

##Question2_c:----------------------------------------------------------------
evaluate=function(pvalues,non_zero_index){
  
  ###the family wise error rate is type 1 error of H0:beta_i=0
  zero_index=c(1:length(beta))[-non_zero_index]
  V=colSums(pvalues[zero_index,]<=0.05)
  fwer=sum(V>=1)/dim(pvalues)[2]
  
  ###the false discovery rate is the expectation of the proportion of false 
  ###discoveries among the discoveries
  S=colSums(pvalues[non_zero_index,]<=0.05)
  fdr=sum(V/(V+S))/dim(pvalues)[2]
  
  ###the sensitivity is N(true positive)/(N(true positive)+N(false negatives))
  fn=colSums(pvalues[non_zero_index,]>0.05)
  sensitivity=sum(S/(S+fn))/dim(pvalues)[2]
  
  ###the specificity is N(true negatives)/(N(true negatives)+N(false positives))
  U=colSums(pvalues[zero_index,]>0.05)
  specificity=sum(U/(U+V))/dim(pvalues)[2]
  
  v=c(fwer,fdr,sensitivity,specificity)
  names(v)=c("family wise error rate","false discovery rate",
             "sensitivity","specificity")
  v
}

##Question2_d:----------------------------------------------------------------
non_zero_beta=which(beta!=0)
before_adjust=evaluate(pvalues,non_zero_beta)
###adjust p-values using four methods
bonferroni_adjust=apply(pvalues,2,p.adjust,method="bonferroni")
holm_adjust=apply(pvalues,2,p.adjust,method="holm")
BH_adjust=apply(pvalues,2,p.adjust,method="BH")
BY_adjust=apply(pvalues,2,p.adjust,method="BY")
###use evaluate() function to adjusted p-values again
bonferroni_evaluate=evaluate(bonferroni_adjust,non_zero_beta)
holm_evaluate=evaluate(holm_adjust,non_zero_beta)
BH_evaluate=evaluate(BH_adjust,non_zero_beta)
BY_evaluate=evaluate(BY_adjust,non_zero_beta)


#80:--------------------------------------------------------------------------

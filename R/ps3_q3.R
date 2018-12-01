##Stats 506, F18, Problem Set 3, Question 3
##
##Compute (n-1) times univariate regression coefficients for mpg vs
##other continuous variables by cylinder (cyl) groups.
##
##Author: Xun Wang, xunwang@umich.edu
##Updated: November 7,2018- Lasted modified date

#80:--------------------------------------------------------------------------

#Libraries:-------------------------------------------------------------------
library(data.table)
library(magrittr)
library(dplyr)

##Question3_a:----------------------------------------------------------------
dt=as.data.table(mtcars)
dt=dt%>%
  .[,.(mpg,cyl,disp,hp,wt)]
beta_cyl=dt%>%
  
  ##compute centered variables, cross product with mpg, and squares.
  .[,`:=`(disp_gc=disp-mean(disp),hp_gc=hp-mean(hp),wt_gc=wt-mean(wt)),
    by=.(cyl)]%>%
  .[,`:=`(dispXmpg=mpg*disp_gc,hpXmpg=mpg*hp_gc,wtXmpg=mpg*wt_gc,
    disp_sq=disp_gc*disp_gc,hp_sq=hp_gc*hp_gc,wt_sq=wt_gc*wt_gc),
    by=.(cyl)]%>%
  
  ##Compute the cross products, sum of squares, and regression coefficients
  .[,.(dispXmpg=sum(dispXmpg),disp_sq=sum(disp_sq),
       hpXmpg=sum(hpXmpg),hp_sq=sum(hp_sq),
       wtXmpg=sum(wtXmpg),wt_sq=sum(wt_sq)),
    by=.(cyl)]%>%
  
  ##Compute betas
  .[,`:=`(beta_cyl_disp=dispXmpg/disp_sq,
          beta_cyl_hp=hpXmpg/hp_sq,
          beta_cyl_wt=wtXmpg/wt_sq),
    by=.(cyl)]%>%
  .[,-c("dispXmpg","disp_sq","hpXmpg","hp_sq","wtXmpg","wt_sq")]
##save as new file
fwrite(beta_cyl,file='mpg_cor_by_cyl.csv')

##Question3_b:----------------------------------------------------------------
uni_reg=function(y,x,group){
  beta_group=as.data.table(cbind(y,x,group))%>%
    .[,x_gc:=x-mean(x),group]%>%
    .[,`:=`(xXy=y*x_gc,x_sq=x_gc*x_gc),group]%>%
    .[,.(xXy=sum(xXy),x_sq=sum(x_sq)),group]%>%
    .[,beta_group_x:=xXy/x_sq,group]%>%
    .[,-c("xXy","x_sq")]
  beta_group
}

##Question3_c:----------------------------------------------------------------
df=as_tibble(mtcars)
df=df%>%select(mpg, cyl, disp, hp, wt)
beta_cyl_tibble=df%>%
  group_by(cyl)%>%
  mutate_at(.vars=vars(disp:wt),
            .funs=funs(.-mean(.)))%>%
  summarize_at(.vars=vars(disp:wt),
               .funs=funs(sum(.*mpg),var,n()))%>%
  mutate(beta_cyl_disp=disp_sum/{disp_var*{disp_n-1}},
         beta_cyl_hp=hp_sum/{hp_var*{hp_n-1}},
         beta_cyl_wt=wt_sum/{wt_var*{wt_n-1}})%>%
  rename(n=disp_n)%>%
  select(cyl,beta_cyl_disp,beta_cyl_hp,beta_cyl_wt)

##Question3_d:----------------------------------------------------------------
uni_reg_tibble=function(y,x,group){
  group=enquo(group)
  x=enquo(x)
  y=enquo(y)
  x_gc=paste0(quo_name(x),"_gc")
  xXy=paste0(quo_name(x),"X",quo_name(y))
  x_sq=paste0(quo_name(x),"_sq")
  beta_group_tibble=tibble(!!y,!!x,!!group)%>%
    group_by(!!group)%>%
    mutate(!!x_gc:=!!x-mean(!!x),
           !!xXy:=!!y*(!!x-mean(!!x)),
           !!x_sq:=(!!x-mean(!!x))*(!!x-mean(!!x)))%>%
    summarize(!!xXy:=sum(!!y*(!!x-mean(!!x))),
              !!x_sq:=sum((!!x-mean(!!x))*(!!x-mean(!!x))))
  beta_group_x=beta_group_tibble[,xXy]/beta_group_tibble[,x_sq]
  as_tibble(cbind(beta_group_tibble[,quo_name(group)],beta_group_x))
}

#80:--------------------------------------------------------------------------

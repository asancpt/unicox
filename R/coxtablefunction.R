#' cox propotional hazard ratio
#'
#' This function allows you to calculate HR, CI, p-value by Cox
#' @param yr Survival duration
#' @param outcome Survival outcome
#' @param group Interested variable
#' @keywords coxtable
#' @export
#' @examples
#' cox_table(survival duration, survival outcome, variable intrested, data)

cox_table<-function(yr,outcome,group,dat,digit=3){
  library(survival)
  attach(dat)
  digits=3
  f1<-coxph(Surv(yr,outcome)~group,data=dat)
  sf1<-summary(f1)
  hr<-data.frame(sf1$conf.int[,1]);
  lb<-data.frame(sf1$conf.int[,3])
  ub<-data.frame(sf1$conf.int[,4]);
  pv<-data.frame(sf1$coefficients[,5])
  result<-round(cbind(hr,lb,ub,pv),4)
  names(result)<-c("Hazard Ratio","Lower 95% CI","Upper 95% CI","p value")
  result
  detach(dat)
}



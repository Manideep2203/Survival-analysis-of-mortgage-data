library(survival)
library(readxl)
library(dplyr)
setwd("/Users/manideep_sharma/Documents/Purdue documents/2. Spring module/Spring Mod 1/MGMT 672 - Advanced BA/Project")
mortgage <- read_excel("MortgageData.xlsx")

mortgage["duration"]=(mortgage["end_date"]-mortgage["start_date"])/30
#mortgage["vintage_year"] <- as.Date(as.character(mortgage["vintage"]),          # as.Date & as.character functions
                   #format = "%Y")
#mortgage["vintage_duration"]=mortgage['start_date']-mortgage['vintage']
str(mortgage)
head(mortgage)

#  KM survival curve for all data. 
fit.all<-survfit(Surv(duration,event==1)~1,data=mortgage) # analyze w/ all data
plot(fit.all,xlab="Duration of loan (in months)",ylab="Proportion of Non-Defaults", main="Overall Default Rate",col=3) #x-axis scale
#plot(fit.all,ylim=c(0.5, 1), col=3, main='Overall Active Rate') # make plot more readable

fit.prepay<-survfit(Surv(duration,event==1)~Prepayment,data=mortgage)
plot(fit.prepay,col=2:4, lty=1:3, ylim=c(0,1), main="Overall Default Rate with respect to Prepayment")
llabel<-gsub("x=","",names(fit.prepay$strata))
legend("top",legend=llabel,col=2:4,lty=1:3,bty='n')

summary(mortgage)
str(mortgage)

mortgage1=mortgage %>% mutate(cred_score_category= case_when(
  cred_score <= 579 ~ "Poor",
  cred_score >=580 & cred_score <=669 ~ "Fair",
  cred_score >=670 & cred_score <=850 ~ "Good",
  #cred_score >=740 & cred_score <=799 ~ "Very Good",
  #cred_score >=800 & cred_score <=850 ~ "Exceptional",
  #TRUE ~ " "
))

str(mortgage1)

mortgage1["cred_score_category"]

mortgage2=mortgage1 %>% mutate(dbt_ratio_category= case_when(
  DBT_RATIO >=0 & DBT_RATIO <=0.25 ~ "Low",
 # DBT_RATIO >=0.25 & DBT_RATIO <=0.50 ~ "Medium",
  DBT_RATIO >=0.26 & DBT_RATIO <=0.50 ~ "Medium",
  DBT_RATIO >=0.51 ~ "High",
 # TRUE ~ " "
))

str(mortgage2)
mortgage2["dbt_ratio_category"]

fit.cred<-survfit(Surv(duration,event==1)~cred_score_category,data=mortgage2)
plot(fit.cred,col=2:4, lty=1:3, ylim=c(0,1),xlab="Loan Duration (in months)", ylab="Proportion of Non-Default Customers", main="Overall Default Rate by Credit Score")
llabel<-gsub("x=","",names(fit.cred$strata))
legend("topright",legend=llabel,col=2:4,lty=1:3,bty='n',cex=0.6)

fit.cred

fit.debt<-survfit(Surv(duration,event==1)~dbt_ratio_category,data=mortgage2)
plot(fit.debt,col=2:4, lty=1:3, ylim=c(0,1), xlab="Loan Duration (in months)", ylab="Proportion of Non-Default Customers", main="Overall Default Rate by Debt Ratio")
llabel<-gsub("x=","",names(fit.debt$strata))
legend("topright",legend=llabel,col=2:4,lty=1:3,bty='n',cex=0.6)

fit.debt

mortgage2["vintage_var"]=ifelse(mortgage2["vintage_duration"]>0,"Refinanced","Non Refinanced")
str(mortgage2)

fit.vintage<-survfit(Surv(duration,event==1)~vintage_var,data=mortgage2)
plot(fit.vintage,col=2:4, lty=1:3, ylim=c(0,1), xlab="Loan Duration (in months)", ylab="Proportion of Non-Default Customers", main="Overall Default Rate by Vintage")
llabel<-gsub("x=","",names(fit.vintage$strata))
legend("topright",legend=llabel,col=2:4,lty=1:3,bty='n',cex=0.6)

fit.vintage

#Cox Model
fit2.cox<-coxph(Surv(duration,event==1)~cred_score_category+dbt_ratio_category+vintage_var,data=mortgage2)
fit2.cox

fit3.cox<-coxph(Surv(duration,event==1)~Prepayment+vintage+cred_score+DBT_RATIO,data=mortgage2)
fit3.cox

fit4.cox<-coxph(Surv(duration,event==1)~DBT_RATIO,data=mortgage2)
fit4.cox


#  KM survival curve for prepayment. 
fit.all_2<-survfit(Surv(duration,Prepayment==0)~1,data=mortgage2) # analyze w/ all data
plot(fit.all_2,xlab="Duration of loan months",ylab="Proportion of Prepayment", main="Overall Prepayment Rate",col=3) #x-axis scale
#plot(fit.all,ylim=c(0.5, 1), col=3, main='Overall Active Rate') # make plot more readable

fit.prepay<-survfit(Surv(duration,event==1)~Prepayment,data=mortgage)
plot(fit.prepay,col=2:4, lty=1:3, ylim=c(0,1), main="Overall Default Rate")
llabel<-gsub("x=","",names(fit.prepay$strata))
legend("top",legend=llabel,col=2:4,lty=1:3,bty='n')


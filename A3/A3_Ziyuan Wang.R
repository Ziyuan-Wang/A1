setwd("/Users/yuan/Dropbox/Econ 613/A3")
library(ggplot2)
library(tidyr)
library(reshape2)
library(moments)
library(tidyverse)
library(nnet)
library(texreg)
#========================================
# Exercise 1 
#========================================
dat_pop<-read.csv(file="population.csv",header = T,sep=",")
dat_crime<-read.csv(file="crime_long.csv",header = T,sep=",")
dat_officers<-read.csv(file="officers.csv",header = T,sep=",")

#========================================
# Exercise 2 
#========================================
#subtotal
CrimePerMonth<-data.frame(CrimeNum=tapply(dat_crime$crimes, INDEX=dat_crime$crime_month, FUN=sum))
CrimeDate<-as.Date(rownames(CrimePerMonth),format="%Y-%m-%d")
rownames(CrimePerMonth)=NULL
CrimePerMonth$CrimeDate=CrimeDate
#plot time series graph
ggplot(data=CrimePerMonth,aes(x=CrimeDate,y=CrimeNum,group = 1))+
  geom_line(colour="orange")+scale_x_date(date_labels = "%m-%Y")
#reshape the dat_crime
dat_crime_wide=dcast(dat_crime,crime_month+district~crime_type,sum,value.var = "crimes")
#merge two datasets
fulldata=merge(dat_pop, dat_crime_wide, by.x = c("month", "district"),by.y = c("crime_month","district"), all = T)
#correct the period
fulldata$period=as.numeric(factor(fulldata$month))
#generate panel data
fulldata$tot_crime=rowSums(fulldata[,9:12])
fulldata$p_black =fulldata[,"tot_black"]/fulldata[,"tot_pop"]
fulldata$p_hisp =fulldata[,"tot_hisp"]/fulldata[,"tot_pop"]
fulldata$p_white =fulldata[,"tot_white"]/fulldata[,"tot_pop"]
paneldata=select(fulldata,"month","district","period","tot_crime","violent","property","p50_inc","p_black","p_hisp","p_white")
paneldata[1:10,]
#========================================
# Exercise 3 
#========================================
paneldata=merge(paneldata,dat_officers,by.x=c("month","district"),by.y = c("month","unit"))
model1=lm(arrest~tenure+tot_crime+p50_inc+p_black+p_hisp+p_white ,data=paneldata)
summary(model1)   #beta=-4.161e-06 

#========================================
# Exercise 4
#========================================
model2=lm(arrest~tenure+tot_crime+p50_inc+p_black+p_hisp+p_white+factor(district)+factor(period) ,data=paneldata)
summary(model2)  #beta=-3.810e-06

#========================================
# Exercise 5
#========================================
#-------------------------------
# 5-1: Within Estimator           
#-------------------------------

# t average: A_ij_bar            
WE_data_ij<-dcast(paneldata,NUID~district,mean,value.var="arrest")
WE_data_ij=gather(WE_data_ij,district, arrest,-NUID)
WE_data_ij=WE_data_ij[-which(is.na(WE_data_ij[,"arrest"])),]
colnames(WE_data_ij)[3]="A_ij_bar"

# t average: tau_i_bar           
WE_data_i<-data.frame(tau_i_bar=tapply(paneldata$tenure, INDEX=paneldata$NUID, FUN=mean))
WE_data_i$NUID<-rownames(WE_data_i)
rownames(WE_data_i)=NULL

# i average: A_jt_bar            
WE_data_jt<-dcast(paneldata,district~period,mean,value.var="arrest")
WE_data_jt=gather(WE_data_jt,period, arrest,-district)
WE_data_jt=WE_data_jt[-which(is.na(WE_data_jt[,"arrest"])),]
colnames(WE_data_jt)[3]="A_jt_bar"

# i average: tau_t_bar           
WE_data_t<-data.frame(tau_t_bar=tapply(paneldata$tenure, INDEX=paneldata$period, FUN=mean))
WE_data_t$period<-rownames(WE_data_t)
rownames(WE_data_t)=NULL

# t&i average: A_j_bar           
WE_data_j<-data.frame(A_j_bar=tapply(paneldata$arrest, INDEX=paneldata$district, FUN=mean))
WE_data_j$district<-rownames(WE_data_j)
rownames(WE_data_j)=NULL

# t&i average: tau_bar           
WE_data_cons<-mean(paneldata$arrest)

# merge dataset                  
WE_data<-select(paneldata,"period","NUID","arrest","tenure","district")
WE_data<-merge(WE_data,WE_data_t,by="period")
WE_data<-merge(WE_data,WE_data_i,by="NUID")
WE_data<-merge(WE_data,WE_data_j,by="district")
WE_data<-merge(WE_data,WE_data_ij,by=c("NUID","district"))
WE_data<-merge(WE_data,WE_data_jt,by=c("district","period"))
WE_data$tau_bar=WE_data_cons


## A_ijt - t average - (i average - t and i average)           
WE_data$WE_A<-WE_data$arrest-WE_data$A_ij_bar-(WE_data$A_jt_bar-WE_data$A_j_bar)
WE_data$WE_tau<-WE_data$tenure-WE_data$tau_i_bar-(WE_data$tau_t_bar-WE_data$tau_bar)

#regression
model3<-lm(WE_A~WE_tau,data=WE_data)
summary(model3)  #beta=1.490e-05           

#-------------------------------
# 5-2: Between Estimator
#-------------------------------
# t average: A_ij_bar
BE_data_ij<-dcast(paneldata,NUID~district,mean,value.var="arrest")
BE_data_ij=gather(BE_data_ij,district, arrest,-NUID)
BE_data_ij=BE_data_ij[-which(is.na(BE_data_ij[,"arrest"])),]
colnames(BE_data_ij)[3]="A_ij_bar"

# t average: tau_i_bar
BE_data_i<-data.frame(tau_i_bar=tapply(paneldata$tenure, INDEX=paneldata$NUID, FUN=mean))
BE_data_i$NUID<-rownames(BE_data_i)
rownames(BE_data_i)=NULL

# t&j average= A_i_bar
BE_data_i$A_i_bar<-tapply(paneldata$arrest, INDEX=paneldata$NUID, FUN=mean)

#merge dataset
BE_data<-select(paneldata,"period","NUID","arrest","tenure","district")
BE_data<-merge(BE_data,BE_data_ij,by=c("NUID","district"))
BE_data<-merge(BE_data,BE_data_i,by="NUID")

#regression
model4<-lm(A_i_bar~tau_i_bar,data=BE_data)
summary(model4)  #beta=-7.792e-06  

#-------------------------------
#5-3: First Difference Estimator  
#-------------------------------
FD_data_ijt<-select(paneldata,"NUID","district","period","arrest","tenure")
FD_data_ijt<-FD_data_ijt[order(FD_data_ijt$NUID,FD_data_ijt$district),]

# a=dcast(paneldata,district+period~NUID,mean,value.var="arrest")
FD_data_ijt$IJ=paste0("I",FD_data_ijt$NUID,"J",FD_data_ijt$district)
a=FD_data_ijt
#period difference
a1=a[-1,]
a2=a[-dim(a)[1],]
a1$sameIJ=ifelse(a1$IJ==a2$IJ,1,NA)
a1$Tdiff_arrest=(a1$arrest-a2$arrest)*a1$sameIJ
a1$Tdiff_tenure=(a1$tenure-a2$tenure)*a1$sameIJ
#NUID difference
a1<-a1[order(a1$district,a1$period),]
a1$JT=paste0("J",a1$district,"T",a1$period)
a3=a1[-1,]
a4=a1[-dim(a1)[1],]
a3$sameJT=ifelse(a3$JT==a4$JT,1,NA)
a3$ITdiff_arrest=(a3$Tdiff_arrest-a4$Tdiff_arrest)*a3$sameJT
a3$ITdiff_tenure=(a3$Tdiff_tenure-a4$Tdiff_tenure)*a3$sameJT
FD_data<-select(a3,NUID,district,period,ITdiff_arrest,ITdiff_tenure)

#regression
model5<-lm(ITdiff_arrest~ITdiff_tenure,data = FD_data)
summary(model5)   # beta=-8.613e-05


#-------------------------------
#5-4: GMM approach
#-------------------------------
#####################
# direct calculation
#####################
dist_dummy=class.ind(paneldata$district)
colnames(dist_dummy)=paste0("J",1:25)
period_dummy<-class.ind(paneldata$period)
colnames(period_dummy)=paste0("T",1:length(colnames(period_dummy)))
paneldata<-cbind(paneldata,dist_dummy,period_dummy)
X=cbind(rep(1,1077909),paneldata$tenure,paneldata$tot_crime,paneldata$p50_inc,paneldata$p_black,paneldata$p_hisp,paneldata$p_white,paneldata[,15:38],paneldata[,40:170])

X=as.matrix(X)
na_row=which(is.na(X[,3]))
Y=paneldata$arrest
Y=Y[-na_row]
X=X[-na_row,]
param = solve(t(X) %*% X ) %*% t(X) %*% Y
rownames(param)[1:7]=c("cons","tenure","tot_crime","p50_inc","p_black","p_hisp","p_white")



#####################
# simulation method (takes too long)
#####################
#transform categorical to dummy variables
dist_dummy=class.ind(paneldata$district)
colnames(dist_dummy)=paste0("J",1:25)
period_dummy<-class.ind(paneldata$period)
colnames(period_dummy)=paste0("T",1:length(colnames(period_dummy)))
paneldata<-cbind(paneldata,dist_dummy,period_dummy)

#reference category: col24 and col39 
mm_sim = function(param,paneldata)
{ X=cbind(rep(1,1077909),paneldata$tenure,paneldata$tot_crime,paneldata$p50_inc,paneldata$p_black,paneldata$p_hisp,paneldata$p_white,paneldata[,15:38],paneldata[,40:170])
sim_arrest<-as.matrix(X)%*%param
error=paneldata$arrest - sim_arrest
na_col=which(is.na(error))
error=error[-na_col]
moment = mean(t(X)[,-na_col]%*%error,na.rm = T)
return(moment);
}
start = runif(162,0,1)
res  = optim(start,fn=mm_sim,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),paneldata=paneldata)
param    = res$par













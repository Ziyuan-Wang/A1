#==============================================================
## Part 1
#==============================================================

setwd("/Users/yuan/Dropbox/Econ 613")
datstu <- read.csv("datstu.csv") 
datjss <- read.csv("datjss.csv")
datsss <- read.csv("datsss.csv")
library(tidyverse)
library(data.table)
library(stringr)
library(plyr)
library(dplyr)

#--------------------------------------------------------------
## Exercise 1 Missing data
#--------------------------------------------------------------
numStu <- length(datstu$X)  #number of students: 340823
numSch <- length(unique(datsss$schoolname)) #number of schools: 842
programs <- unique(c(unique(datstu$choicepgm1),unique(datstu$choicepgm2),unique(datstu$choicepgm3),unique(datstu$choicepgm4),unique(datstu$choicepgm5),unique(datstu$choicepgm6)))
numProg <- length(programs)   #number of programs:33

schprgm1=paste(datstu$schoolcode1,datstu$choicepgm1)
schprgm2=paste(datstu$schoolcode2,datstu$choicepgm2)
schprgm3=paste(datstu$schoolcode3,datstu$choicepgm3)
schprgm4=paste(datstu$schoolcode4,datstu$choicepgm4)
schprgm5=paste(datstu$schoolcode5,datstu$choicepgm5)
schprgm6=paste(datstu$schoolcode6,datstu$choicepgm6)
schprgm=unique(c(unique(schprgm1),unique(schprgm2),unique(schprgm3),unique(schprgm4),unique(schprgm5),unique(schprgm6)))
numCho <- length(schprgm) #number of choices: 3086  

numMisc <- length(datstu$score[is.na(datstu$score)]) #number of missing score: 179887

SchList <- rbind(datstu$schoolcode1,datstu$schoolcode2,datstu$schoolcode3,datstu$schoolcode4,datstu$schoolcode5,datstu$schoolcode6)
uniSch=function(x){
  n=dim(x)[2] #nrow
  numUniSch=c()
  for(i in 1:n){
    numUniSch[i]=length(unique(x[,i]))
  }
  return(numUniSch)
}
numSame=sum(uniSch(SchList)[uniSch(SchList)<6]) #number of students who apply to same school: 608970

stuLess6 <- datstu$choicepgm6[datstu$choicepgm6==""]
numstuLess6 <- length(stuLess6) #number of students who apply to less than 6 choices: 18954

exercise1 <- t(data.frame("number of students"=numStu,"number of schools"=numSch,"number of programs"=numProg,"number of choices"=numCho,"number of missing test score"=numMisc,"apply to the same school"=numSame,"apply to less than 6 choices"=numstuLess6))
exercise1

#--------------------------------------------------------------
## Exercise 2 Data
#--------------------------------------------------------------

#choice
datstu = datstu %>% mutate(choice1 = paste0(schoolcode1,choicepgm1),
                           choice2 = paste0(schoolcode2,choicepgm2),
                           choice3 = paste0(schoolcode3,choicepgm3),
                           choice4 = paste0(schoolcode4,choicepgm4),
                           choice5 = paste0(schoolcode5,choicepgm5),
                           choice6 = paste0(schoolcode6,choicepgm6))

#rank dummy
datstu$rank1 = as.numeric(datstu$rankplace==1)
datstu$rank2 = as.numeric(datstu$rankplace==2)
datstu$rank3 = as.numeric(datstu$rankplace==3)
datstu$rank4 = as.numeric(datstu$rankplace==4)
datstu$rank5 = as.numeric(datstu$rankplace==5)
datstu$rank6 = as.numeric(datstu$rankplace==6)

#admitted school
datstu$admittedschool = datstu$schoolcode1*datstu$rank1 + 
  datstu$schoolcode2*datstu$rank2 + 
  datstu$schoolcode3*datstu$rank3 + 
  datstu$schoolcode4*datstu$rank4 + 
  datstu$schoolcode5*datstu$rank5 + 
  datstu$schoolcode6*datstu$rank6  

#admitted choice
datstu$admittedchoice=c(rep("---",length(datstu$X)))
datstu$admittedchoice[which(datstu$rank1==1)] = datstu$choice1[which(datstu$rank1==1)]
datstu$admittedchoice[which(datstu$rank2==1)] = datstu$choice2[which(datstu$rank2==1)]
datstu$admittedchoice[which(datstu$rank3==1)] = datstu$choice3[which(datstu$rank3==1)]
datstu$admittedchoice[which(datstu$rank4==1)] = datstu$choice4[which(datstu$rank4==1)]
datstu$admittedchoice[which(datstu$rank5==1)] = datstu$choice5[which(datstu$rank5==1)]
datstu$admittedchoice[which(datstu$rank6==1)] = datstu$choice6[which(datstu$rank6==1)]


#SchPrgmInfo
dat_choice <- gather(select(datstu,choice1,choice2,choice3,choice4,choice5,choice6),'choice','SchPrgm')
dat_schools <- datstu %>%
  select(schoolcode1:schoolcode6) %>%
  gather(key = "schoolchoice", value = "schoolcode",schoolcode1:schoolcode6)
dat_long=cbind(dat_choice,dat_schools)
SchPrgm=dat_long[!duplicated(dat_long$SchPrgm), ]  #clean school programs
Schools=datsss[!duplicated(datsss$schoolcode),]  #clean schools
SchPrgm=merge(SchPrgm,Schools,by="schoolcode")

Info=select(datstu,admittedchoice,score)
Quality=aggregate(Info[,2],list(Info[,1]),mean)
Cutoff=aggregate(Info[,2],list(Info[,1]),min)
Size=data.frame(table(Info$admittedchoice))
colnames(Size) = c("SchPrgm","Size")
Info=cbind(Size,"Quality"=Quality[,2],"Cutoff"=Cutoff[,2])

SchPrgm=merge(SchPrgm,Info,by = "SchPrgm")
exercise2=select(SchPrgm,SchPrgm,sssdistrict,ssslong,ssslat,Cutoff,Quality,Size)
exercise2[1:20,]


#--------------------------------------------------------------
## Exercise 3 Distance
#--------------------------------------------------------------
datstu_1=select(datstu,X,admittedschool,jssdistrict)

datstu_1=merge(datstu_1,select(datjss,-X),by="jssdistrict",all = T)
datstu_1=merge(datstu_1,select(Schools,schoolcode,ssslong,ssslat),by.x="admittedschool", by.y="schoolcode",all.x = T)
colnames(datstu_1)=c("HighSchool","SeniorSchool","StuId","jsslong","jsslat","ssslong","ssslat")

datstu_1$JSdist=sqrt((69.172*(datstu_1$ssslong-datstu_1$jsslong)*cos(datstu_1$jsslat/57.3))^2+(69.172*(datstu_1$ssslat - datstu_1$jsslat))^2)
datstu_1=arrange(datstu_1,StuId)
datstu_1[1:20,]



#--------------------------------------------------------------
## Exercise 4 Descriptive Characteristics
#--------------------------------------------------------------
# 1: By ranked choice
datstu_2=merge(datstu,exercise2,by.x="admittedchoice",by.y = "SchPrgm")
datstu_2=merge(datstu_1,datstu_2,by.x="StuId",by.y="X",all.x = T)
exercise4_1 <- matrix(,6,7)
for (i in 1:6) {
  exercise4_1[i,1]=i
  exercise4_1[i,2]=mean(as.numeric(datstu_2$Cutoff[which(datstu_2$rankplace==i)]),na.rm = T)
  exercise4_1[i,3]=sd(as.numeric(datstu_2$Cutoff[which(datstu_2$rankplace==i)]),na.rm = T)
  exercise4_1[i,4]=mean(as.numeric(datstu_2$Quality[which(datstu_2$rankplace==i)]),na.rm = T)
  exercise4_1[i,5]=sd(as.numeric(datstu_2$Quality[which(datstu_2$rankplace==i)]),na.rm = T)
  exercise4_1[i,6]=mean(as.numeric(datstu_2$JSdist[which(datstu_2$rankplace==i)]),na.rm = T)
  exercise4_1[i,7]=sd(as.numeric(datstu_2$JSdist[which(datstu_2$rankplace==i)]),na.rm = T)
} 
exercise4_1 <- data.frame(exercise4_1)
names(exercise4_1) <- c("Rank","Cutoff_mean","Cutoff_sd","Quality_mean","Quality_sd","Distance_mean","Distance_sd")
exercise4_1

# 2: By test score quartile

quarData<-function(standard,target){  #standard: use quartile of which variable; target: describe which variable
  quarInterval1=as.numeric(target[which(target>=quantile(standard,na.rm = T)[[1]]&target<=quantile(standard,na.rm = T)[[2]])])
  quarInterval2=as.numeric(target[which(target> quantile(standard,na.rm = T)[[2]]&target<=quantile(standard,na.rm = T)[[3]])])
  quarInterval3=as.numeric(target[which(target> quantile(standard,na.rm = T)[[3]]&target<=quantile(standard,na.rm = T)[[4]])])
  quarInterval4=as.numeric(target[which(target> quantile(standard,na.rm = T)[[4]]&target<=quantile(standard,na.rm = T)[[5]])])
  quarInterval=rbind.fill(data.frame(quarInterval1),data.frame(quarInterval2),data.frame(quarInterval3),data.frame(quarInterval4))
  return(quarInterval)
}

exercise4_2 <- matrix(,4,7)

for (i in 1:4) {
  exercise4_2[i,1]=c("0-25%","25-50%","50-75%","75-100%")[i]
  exercise4_2[i,2]=round(mean(quarData(datstu_2$score,datstu_2$Cutoff)[,i],na.rm = T),2)
  exercise4_2[i,3]=round(sd(quarData(datstu_2$score,datstu_2$Cutoff)[,i],na.rm = T),2)
  exercise4_2[i,4]=round(mean(quarData(datstu_2$score,datstu_2$Quality)[,i],na.rm = T),2)
  exercise4_2[i,5]=round(sd(quarData(datstu_2$score,datstu_2$Quality)[,i],na.rm = T),2)
  exercise4_2[i,6]=round(mean(quarData(datstu_2$score,datstu_2$JSdist)[,i],na.rm = T),2)
  exercise4_2[i,7]=round(sd(quarData(datstu_2$score,datstu_2$JSdist)[,i],na.rm = T),2)
}

exercise4_2 <- data.frame(exercise4_2)
names(exercise4_2) <- c("Interval","Cutoff_mean","Cutoff_sd","Quality_mean","Quality_sd","Distance_mean","Distance_sd")

exercise4_2


#==============================================================
## Part 2
#==============================================================

#--------------------------------------------------------------
## Exercise 5 Data creation
#--------------------------------------------------------------
set.seed(123)
X1 <- runif(10000,1,3)
X2 <- rgamma(10000,shape = 3,scale = 2)
X3 <- rbinom(10000,1,0.3)
epsilo <- rnorm(10000,2,1)

Y <- 0.5 + 1.2*X1 - 0.9*X2 + 0.1*X3 + epsilo    # n x 1
ybar <- mean(Y)
ydum <- c(10000)
ydum[which(Y>ybar)]=1
ydum[which(Y<=ybar)]=0

table(ydum)

#--------------------------------------------------------------
## Exercise 6 OLS
#--------------------------------------------------------------
#correlation
r <- function(x,y){
  corr=cov(x,y) / sqrt(var(x)* var(y))
  return(corr)
}

print(paste("corr(Y,X1) =",r(Y,X1)))

#standardized variables: Y_1,X1_1,X2_1,X3_1
Y_1=Y/sd(Y)
X1_1=X1/sd(X1)
X2_1=X2/sd(X2)
X3_1=X3/sd(X3)
X_1=cbind(rep(1,10000),X1_1,X2_1,X3_1)  # n x k
beta_1= solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y_1

print(paste("after standardization: coeff(X1) =",beta_1[2]))

#estimate the coefficients
X=cbind(rep(1,10000),X1,X2,X3)  # n x k
beta = solve(t(X)%*%X)%*%t(X)%*%Y
print("coefficients:")
beta

#estimate the standard errors
e = Y-X%*%beta                  # n x 1
n = 10000
k = 3
s_2=(1/(n-k))*t(e)%*%e          # 1 x 1
se = s_2[1]*solve(t(X)%*%X)     # k x k
print("standard errors:")
se 


#--------------------------------------------------------------
## Exercise 7 Discrete choice
#--------------------------------------------------------------
#probit
flikep <- function(par,x1,x2,x3,yvar){
  xbeta = par[1]+par[2]*x1+par[3]*x2+par[4]*x3
  pr = pnorm(xbeta)
  pr[pr>0.999999]=0.999999
  pr[pr<0.000001]=0.000001
  like = yvar * log(pr) + (1-yvar) * log(1-pr)
  return(-sum(like))
}
#logit
flikel <- function(par,x1,x2,x3,yvar){
  xbeta = par[1]+par[2]*x1+par[3]*x2+par[4]*x3
  pr = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999]=0.999999
  pr[pr<0.000001]=0.000001
  like = yvar * log(pr) + (1-yvar) * log(1-pr)
  return(-sum(like))
}

## optimization
start = runif(4)
#probit
res_p = optim(start,fn=flikep,method = "BFGS",control=list(trace=6,REPORT = 1,maxit=1000), x1=X1,x2=X2,x3=X3, yvar=ydum, hessian=TRUE)
fisher_info_p = solve(res_p$hessian)
prop_sigma_p = sqrt(diag(fisher_info_p))

#logit
res_l = optim(start,fn=flikel,method = "BFGS",control=list(trace=6,REPORT = 1, maxit=1000), x1=X1,x2=X2,x3=X3, yvar=ydum, hessian=TRUE)
fisher_info_l = solve(res_l$hessian)
prop_sigma_l = sqrt(diag(fisher_info_l))

## significance
#probit
t.value.pro = res_p$par / prop_sigma_p
p.value.pro = 2*pt(-abs(t.value.pro), df=10000-1)
#logit
t.value.logit = res_l$par / prop_sigma_l
p.value.logit = 2*pt(-abs(t.value.logit), df=10000-1)

## output
est = cbind(res_p$par,prop_sigma_p,t.value.pro,p.value.pro,res_l$par,prop_sigma_l,t.value.logit,p.value.logit)
colnames(est) = c("Probit:est","Probit:se","Probit:t-value","Probit:p-value","Logit:est","Logit:se","Logit:t-value","Logit:p-value")
rownames(est) = c("cons","X1","X2","X3")
est


#--------------------------------------------------------------
## Exercise 8 Marginal Effects
#--------------------------------------------------------------
#probit
margin.probit.avg  = mean(pnorm(X%*%res_p$par))*res_p$par
margin.probit.mean = pnorm(colMeans(X)%*%res_p$par)[1]*res_p$par
#logit
margin.logit.avg   = mean(exp(X%*%res_l$par)/(1+exp(X%*%res_l$par)))*res_l$par
margin.logit.mean = (exp(colMeans(X)%*%res_l$par)/(1+exp(colMeans(X)%*%res_l$par)))[1]*res_l$par

ME = cbind(margin.probit.avg,margin.probit.mean,margin.logit.avg,margin.logit.mean)
rownames(ME) = c("cons","X1","X2","X3")
ME
## Exercise 1 Data Description

setwd("/Users/yuan/Dropbox/Econ 613/A2")
library(bayesm)
library(tidyverse)
data("margarine")  #import data

A=margarine$choicePrice
B=margarine$demos

avg=apply(A[,3:12],MARGIN = 2,FUN=mean)
sd=apply(A[,3:12],MARGIN = 2,FUN=sd)
des=rbind(avg,sd)

## market share
byChoice=table(A$choice)

## market share by product characteristics
byChar=matrix(0,2,10)      
for (i in 1:10){
  byChar[1,i]=length(A$choice[A[,i+2]>avg[i]]) #above avg
  byChar[2,i]=length(A$choice[A[,i+2]<avg[i]]) #below avg
}
rownames(byChar)=c("above avg","below avg")
colnames(byChar)=colnames(A)[3:12]

## mapping attr and choices
# merge data by hhid
dat=merge(A,B,by="hhid",all=T)
# assign chosen price
dat = dat %>% mutate(Price = ifelse(choice==1,A[,3], ifelse(choice==2,A[,4], ifelse(choice==3,A[,5], ifelse(choice==4,A[,6], ifelse(choice==5,A[,7], ifelse(choice==6,A[,8], ifelse(choice==7,A[,9], ifelse(choice==8,A[,10], ifelse(choice==9,A[,11], A[,12]))))))))))
# average demographic char by choice
ChobyDemo=matrix(0,10,7)
for (i in 1:10){
  ChobyDemo[i,]=round(colMeans(dat[which(dat$choice==i),13:19]) ,3)
}
colnames(ChobyDemo)=colnames(dat)[13:19]
rownames(ChobyDemo)=colnames(A)[3:12]

DemobyCho=matrix(0,3,10)
for (i in 1:10){
  DemobyCho[1,i]=round(length(which(dat$choice[dat$college==1]==i))/length(which(dat$college==1)),3)
  DemobyCho[2,i]=round(length(which(dat$choice[dat$whtcollar==1]==i))/length(which(dat$whtcollar==1)),3)
  DemobyCho[3,i]=round(length(which(dat$choice[dat$retired==1]==i))/length(which(dat$retired==1)),3)
}
colnames(DemobyCho)=colnames(A)[3:12]
rownames(DemobyCho)=c("college","whtcollar","retired")

print("Average and dispersion in product characteristics (price):")
summary(A)
print(des) # Average and dispersion in product characteristics
print("Market share (choice frequency):")
print(byChoice) #market share (choice frequency)
print("Market share by product characteristics:")
print(byChar) #market share by product characteristics
print("Mapping between observed attributes and choices:")
print(ChobyDemo)
print(DemobyCho)


## Exercise 2 First Model

library(nloptr)
brandlist=c("PPK","PBB","PFI","PHse","PGen","Plmp","PSS")
dat$brand=0
dat$brand[which(dat$choice%in%c(1,8))]=brandlist[1]
dat$brand[which(dat$choice==2)]=brandlist[2]
dat$brand[which(dat$choice%in%c(3,9))]=brandlist[3]
dat$brand[which(dat$choice%in%c(4,10))]=brandlist[4]
dat$brand[which(dat$choice==5)]=brandlist[5]
dat$brand[which(dat$choice==6)]=brandlist[6]
dat$brand[which(dat$choice==7)]=brandlist[7]

typelist=c("Stick","Tub") 
dat$type=0
dat$type[which(dat$choice%in%c(1:6))]=typelist[1]
dat$type[which(dat$choice%in%c(7:10))]=typelist[2]

ni=nrow(dat)
like_fun=function(param){
  #xbeta (or ut)
  ni=nrow(dat)
  nj=10 #number of choices
  ut=mat.or.vec(ni,nj)
  for (j in 1:nj){
    ut[,j]=param[1]+param[2]*dat[,j+2]   #dat[,j+2]: choice j's price
  }
  
  prob=exp(ut)
  prob=sweep(prob,MARGIN = 2,FUN = "/",STATS=rowSums(prob))
  probc=NULL
  for (i in 1:ni){
    probc[i]=prob[i,dat$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like=sum(log(probc))
  return(-like)
}
#  ut_store=ut
npar=2
param=runif(npar)

res_condlogit = optim(param,fn=like_fun,method = "BFGS",control=list(trace=6,maxit=100))
model1=matrix(res_condlogit$par,1,2)
colnames(model1)=c("cons","price")
print("The coefficients for the first model:")
model1



## Exercise 3 Second Model

like_fun2=function(coef){
  ni=nrow(dat)
  nj=10
  pn1=coef[1:nj]
  pn2=coef[(nj+1):(2*nj)]
  pn3=coef[(2*nj+1):(3*nj)]
  pn4=coef[(3*nj+1):(4*nj)]
  pn5=coef[(4*nj+1):(5*nj)]
  pn6=coef[(5*nj+1):(6*nj)]
  
  ut2=mat.or.vec(ni,nj)
  for (j in 1:nj) {
    ut2[,j]=pn1[j]+pn2[j]*dat$Income+pn3[j]*dat$Fam_Size+pn4[j]*dat$college+pn5[j]*dat$whtcollar+pn6[j]*dat$retired
  }
  prob2=exp(ut2)
  prob2=sweep(prob2,MARGIN = 1,FUN = "/",STATS=rowSums(prob2))
  probc2=NULL
  for (i in 1:ni){
    probc2[i]=prob2[i,dat$choice[i]]
  }
  probc2[probc2>0.999999] = 0.999999
  probc2[probc2<0.000001] = 0.000001
  like2=sum(log(probc2))
  return(-like2)
}
nj=10
ncoef=6
coef=runif(ncoef*nj)
res_mlnlogit = optim(coef,fn=like_fun2,method = "BFGS",control=list(trace=6, maxit=100))
model2=matrix(res_mlnlogit$par,10,6)
colnames(model2)[2:6]=colnames(dat)[c(13,16:19)]
colnames(model2)[1]="cons"
rownames(model2)=colnames(dat)[3:12]
print("The coefficients for the second model:")
model2


## Exercise 4 Marginal Effects

## model 1

ut=mat.or.vec(ni,nj)
for (j in 1:nj){
  ut[,j]=param[1]+param[2]*dat[,j+2]   #dat[,j+2]: choice j's price
}
pr=dlogis(colMeans(ut))   #1x10
#delta matrix: if j=k equals to one.
delta=diag(1,10,10)
#p_ik matrix
Pik=matrix(rep(pr,10),10,10)
#p_ij matrix
Pij=diag(pr)
#for parameter2
margeff2.m1=(delta-Pik)%*%Pij*res_condlogit$par[2]   #10x10
rownames(margeff2.m1)=colnames(dat)[3:12]
colnames(margeff2.m1)=colnames(dat)[3:12]
#for parameter1
margeff1.m1=(delta-Pik)%*%Pij*res_condlogit$par[1]   #10x10
rownames(margeff1.m1)=colnames(dat)[3:12]
colnames(margeff1.m1)=colnames(dat)[3:12]

## model 2
#P_ij matrix
pn1=model2[1:nj]
pn2=model2[(nj+1):(2*nj)]
pn3=model2[(2*nj+1):(3*nj)]
pn4=model2[(3*nj+1):(4*nj)]
pn5=model2[(4*nj+1):(5*nj)]
pn6=model2[(5*nj+1):(6*nj)]
ut2=mat.or.vec(ni,nj)
for (j in 1:nj) {
  ut2[,j]=pn1[j]+pn2[j]*dat$Income+pn3[j]*dat$Fam_Size+pn4[j]*dat$college+pn5[j]*dat$whtcollar+pn6[j]*dat$retired
}
prob2=dlogis(colMeans(ut2)) #p_ij  1x10

#betabar matrix
betabar=prob2%*%model2   #1x6
betabar_mat=matrix(rep(betabar,10),10,6,byrow = T)
#p_ij matrix
Pij2=diag(prob2)   #10x10

margeff.m2=Pij2%*%(model2-betabar_mat)   #10x6
rownames(margeff.m2)=colnames(dat)[3:12]

##outcome reports
print("marginal effect of model1")
print("parameter 1")
margeff1.m1
print("parameter 2")
margeff2.m1

print("marginal effect of model2")
margeff.m2


## Exercise 5 IIA

## mixed logit model
like_fun3=function(coef3){
  ni=nrow(dat)
  nj=10
  pn_1=coef3[1:nj]
  pn_2=coef3[(nj+1):(2*nj)]
  pn_3=coef3[(2*nj+1):(3*nj)]
  pn_4=coef3[(3*nj+1):(4*nj)]
  pn_5=coef3[(4*nj+1):(5*nj)]
  pn_6=coef3[(5*nj+1):(6*nj)]
  pn_7=coef3[(6*nj+1)]
  
  ut3=mat.or.vec(ni,nj)
  for (j in 1:nj) {
    ut3[,j]=pn_1[j]+pn_2[j]*dat$Income+pn_3[j]*dat$Fam_Size+pn_4[j]*dat$college+pn_5[j]*dat$whtcollar+pn_6[j]*dat$retired+pn_7*dat[,j+2]
  }
  prob3=exp(ut3)
  prob3=sweep(prob3,MARGIN = 1,FUN = "/",STATS=rowSums(prob3))
  probc3=NULL
  for (i in 1:ni){
    probc3[i]=prob3[i,dat$choice[i]]
  }
  probc3[probc3>0.999999] = 0.999999
  probc3[probc3<0.000001] = 0.000001
  like3=sum(log(probc3))
  return(-like3)
}

ncoef3=6
coef3=runif(ncoef3*nj+1)
res_mixlogit = optim(coef3,fn=like_fun3,method = "BFGS",control=list(maxit=100))
model3=matrix(cbind(res_mixlogit$par[1:60],rep(res_mixlogit$par[61],10)),10,7)
nameofcol=c(1:7)
nameofcol[1]="cons"
nameofcol[2:6]=c(colnames(dat)[c(13,16:19)])
nameofcol[7]="price"
colnames(model3)=nameofcol
rownames(model3)=colnames(dat)[3:12]

print("Estimated coefficients for mixed logit model")
model3




## remove choice 10: PHse_Tub
# mixed logit model
nj=9
like_fun4=function(coef4){
  ni=nrow(dat)
  nj=9
  pn__1=coef4[1:nj]
  pn__2=coef4[(nj+1):(2*nj)]
  pn__3=coef4[(2*nj+1):(3*nj)]
  pn__4=coef4[(3*nj+1):(4*nj)]
  pn__5=coef4[(4*nj+1):(5*nj)]
  pn__6=coef4[(5*nj+1):(6*nj)]
  pn__7=coef4[(6*nj+1)]
  
  ut4=mat.or.vec(ni,nj)
  for (j in 1:nj) {
    ut4[,j]=pn__1[j]+pn__2[j]*dat$Income+pn__3[j]*dat$Fam_Size+pn__4[j]*dat$college+pn__5[j]*dat$whtcollar+pn__6[j]*dat$retired+pn__7*dat[,j+2]
  }
  prob4=exp(ut4)
  prob4=sweep(prob4,MARGIN = 1,FUN = "/",STATS=rowSums(prob4))
  probc4=NULL
  for (i in 1:ni){
    probc4[i]=ifelse(dat$choice[i]<=9,prob4[i,dat$choice[i]],NA)
  }
  probc4[probc4>0.999999] = 0.999999
  probc4[probc4<0.000001] = 0.000001
  probc4=probc4[!is.na(probc4)]
  like4=sum(log(probc4))
  return(-like4)
}

ncoef4=6
coef4=runif(ncoef4*nj+1)
res_mixlogit_2 = optim(coef4,fn=like_fun4,method = "BFGS",control=list(maxit=100))
model4=matrix(cbind(res_mixlogit_2$par[-length(res_mixlogit_2$par)],rep(res_mixlogit_2$par[length(res_mixlogit_2$par)],nj)),nj,7)
colnames(model4)=nameofcol
rownames(model4)=colnames(dat)[3:11]

print("Estimated coefficients for the second mixed logit model")
model4

MTT=(-2)*(like_fun3(coef3)-like_fun4(coef4))
critical_value=qchisq(p=0.05,df=length(coef4))
print(paste("At 95% statistical level, the critical value is",critical_value,"and the MTT is ", MTT,"."))
print("Therefore, the model satisfies IIA assumption.")

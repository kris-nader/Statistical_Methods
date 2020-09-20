#LAB chapter 5

## VALIDATION SET APPROACH

library(ISLR)
set.seed(1)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)

# get the test MSE
y_estimate_1=predict(lm.fit,Auto)
mean((mpg-y_estimate_1)[-train]^2)
# test MSe = 23.266

# use different fits using poly
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
y_estimate_2=predict(lm.fit2,Auto)
mean((mpg-y_estimate_2)[-train]^2)
# 18.71

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
y_estimate_3=predict(lm.fit3,Auto)
mean((mpg-y_estimate_3)[-train]^2)
# 18.79

# However, if you were to choose a different subset
# you would get differnt results for the MSE
# repeat everything with the new samples

set.seed(2)
train2=sample(392,196)

lm.fit_1=lm(mpg~horsepower,data=Auto,subset=train2)
y_estimate1=predict(lm.fit_1,Auto)
mean((mpg-y_estimate)[-train2]^2)
# test MSE sample 1 = 23.266
# test MSE sample 2 = 25.29277

# use different fits using poly
lm.fit_2=lm(mpg~poly(horsepower,2),data=Auto,subset=train2)
y_estimate2=predict(lm.fit2,Auto)
mean((mpg-y_estimate2)[-train2]^2)
# test MSE sample 1 = 18.71
# test MSE sample 2 = 19.95408

lm.fit_3=lm(mpg~poly(horsepower,3),data=Auto,subset=train2)
y_estimate3=predict(lm.fit3,Auto)
mean((mpg-y_estimate3)[-train2]^2)
# test MSE sample 1 = 18.79
# test MSE sample 2 = 19.97979



## LEAVE ONE OUT CROSS VALIDATION (LOOCV)
glm.fit1=glm(mpg~horsepower,data=Auto)
coef(glm.fit1)

# we can use cv.glm  part of boot library
library(boot)
cv.err=cv.glm(Auto,glm.fit1)
cv.err$delta
# the cross validation error for the test error is 24.23

cv.error_full=rep(0,5)
for (i in 1:5){
  glm.fit_full=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error_full[i]=cv.glm(Auto,glm.fit_full)$delta[1]
}

cv.error_full


## K-FOLD CROSS VALIDATION

set.seed(17)
cv.error.10=rep(0,10)

for (i in 1:10){
  glm.fit_k_full=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit_full,K=10)$delta[1]
}

cv.error_full
# cv.error$delta gives 2 values 
# the 1st is the CV estimate so 1/k sum MSE
# the second is the bias corrected version



## THE BOOTSTRAP
## needs the boot library and 2 steps 
library(boot)

### Estimate accuracy of stat of interest
# create function of statistic of interest

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)
# here im estimating alpha based on 100 datapoints

# the next step uses sample replace=T  and recompute alpha based on new dataset

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

# record all estimates for alpha and get SD with
# the number of bootstrap estimates of alpha is 1000(R)
boot(Portfolio,alpha.fn,R=1000)
# so we know that BS can give an estimate on the standard errors of the coefficients
# so SE(alpha estimate=0.0905)

### Estimate accuracy of a linear regression model
boot.fn=function(data,index){
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))


### compute standard error of 1000 BS for intercept and slope
boot(Auto,boot.fn,R=1000)
# BS estimate for SE(B0)=0.85
# BS estimate for SE(B1)=0.007
## you can get same with summary
summary(lm(mpg~horsepower,data=Auto))
## bootstrap does not rely on assumptions so it gives accrate estimate of SE

## now BS for poly(horsepower,2)
boot.fn=function(data,index){
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
}
set.seed(1)
boot(Auto,boot.fn,R=1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef




## Solve problems 
# problem 5

set.seed(1)
log.reg1=glm(default~income+balance,data=Default,family = "binomial")
summary(log.reg1)
## uisng validation set approach

train=sample(dim(Default)[1],dim(Default)[1]/2)

log.reg2=glm(default~income+balance,data=Default,family = "binomial",subset=train)
summary(log.reg2)

probs=predict(log.reg2,newdata=Default[-train,],type="response")
pred_glm_2=rep("No",length(probs))
pred_glm_2[probs>0.5]="Yes"


mean(pred_glm_2 != Default[-train,]$default)
# This is the test error rate MSE 0.0274


# second test error rate when rerunning is 0.0244
# third test error rate when rerunning is 0.027

train=sample(dim(Default)[1],dim(Default)[1]/2)
glm_fit3=glm(default~income+balance+student,data=Default,subset=train,family="binomial")

probs=predict(glm_fit3,newdata=Default[-train,],type="response")
pred_a=rep("No",length(probs))
pred_a[probs>0.5]="Yes"

mean(pred_a!=Default[-train,]$default)
# The MSE for the validation set is 0.0264 so adding student variable does not seem to improve the validation MSe

# problem 6
set.seed(1)
attach(Default)

fit.glm=glm(default~income+balance,data=Default,family="binomial")
summary(fit.glm)

# the standard errors for the:
# intercept: B0: 4.348e-01
# income : B1 : 4.985e-06
# balance : B2 : 2.274e-04

boot.fn= function(data,index){
  fit<-glm(default~income+balance,data=data,subset=index,family="binomial")
  return(coef(fit))
  
}

boot(Default,boot.fn,R=1000)
# the standard errors by bootstrap for the:
# intercept: B0: 4.348e-01 : 4.347974e-01
# income : B1 : 4.985e-06  : 4.847106e-06
# balance : B2 : 2.274e-04 : 2.300532e-04

# Question 7

set.seed(1)
attach(Weekly)
fit.glm=glm(Direction~Lag1+Lag2,data=Weekly,family = "binomial")
cv.out=cv.glm(Weekly,fit.glm)
cv.out$delta




# Chpater 5_exercises

# Exercise 5

library(ISLR)
attach(Default)
log.fit=glm(default~income+balance,data=Default,family=binomial(link="logit"))

# use validation set approach
train=sample(1:nrow(Default),nrow(Default)/2)
default.train=Default[train,]
default.test=Default[-train,]


log.fit.train=glm(default~income+balance,data=default.train,family=binomial(link="logit"))

probs=predict(log.fit.train,newdata=default.test,type="response")
pred_glm_2=rep("No",length(probs))
pred_glm_2[probs>0.5]="Yes"

mean(pred_glm_2 != Default[-train,]$default)

#using different split
train2=sample(1:nrow(Default),nrow(Default)*0.8)
default.train2=Default[train2,]
default.test2=Default[-train2,]


log.fit.train2=glm(default~income+balance,data=default.train2,family=binomial(link="logit"))

probs2=predict(log.fit.train2,newdata=default.test2,type="response")
pred_glm_22=rep("No",length(probs2))
pred_glm_22[probs2>0.5]="Yes"

mean(pred_glm_22 != default.test2$default)


detach(Default)
##Question 7
# A
attach(Weekly)
log_1=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial(link="logit"))
# B
log_2=glm(Direction~Lag1+Lag2,data=Weekly[-1,],family=binomial(link="logit"))
# C
probs=predict(log_2,newdata=Weekly[1,],type="response")
pred_glm_22=rep("Down",length(probs))
pred_glm_22[probs>0.5]="Up"

###
#predictweekly<-predict(weekly,Weekly[1,],type = "response")
#predictweekly.class<-ifelse(predictweekly>0.5,"Up","Down")
#predictweekly.class

table(pred_glm_22, Direction[1])
# did not classify correctly

# write a loop from 1 to n 
error=rep(0,nrow(Weekly))
for ( i in 1:nrow(Weekly)){
  temp=glm(Direction~Lag1+Lag2,data=Weekly[-i,],family=binomial(link="logit"))
  probs=predict(log_2,newdata=Weekly[1,],type="response")
  pred_glm_22=rep("Down",length(probs))
  pred_glm_22[probs>0.5]="Up"
  if(pred_glm_22!=Direction[i]){
    error[i]=1
  }
}
head(error)
loocv=(1/nrow(Weekly))*sum(error)
# this is your test error


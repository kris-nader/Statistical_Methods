library(ISLR)
fix(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
####################################
#       best subset                #
####################################

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
reg.summary=summary(regfit.full)

# 2p possible variables, can overfit(high var) large search space
names(reg.summary)
reg.summary$rsq
# rsq will increase monotonically as more variables are added

## R2 and ADJ2
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",
     type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="adjr2",
     type="l")
which.max(reg.summary$adjr2)
points(8,reg.summary$adjr2[8],col="red",cex=2,pch=20)

## CP
plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
     type="l")
min_cp=which.min (reg.summary$cp )
points (min_cp, reg.summary$cp [min_cp], col ="red",cex =2, pch =20)
## BIC
plot(reg.summary$bic ,xlab =" Number of Variables ",ylab="Bic",
     type="l")
min_bic=which.min (reg.summary$bic )
points (min_bic, reg.summary$bic [min_bic], col ="red",cex =2, pch =20)


# can be used to display variables for best model with a given # of ored
plot(regfit.full,scale = "r2")
plot(regfit.full,scale = "adjr2")
plot(regfit.full,scale = "Cp")
plot(regfit.full,scale = "bic")

# depending on which you use
coef(regfit.full,min_bic)

####################################
#       forward subset             #
####################################
regfit.fwd=regsubsets(Salary~.,data=Hitters,method="forward")
regfit.fwd_sum=summary(regfit.fwd)

par(mfrow=c(1,1))
## BIC
plot(regfit.fwd_sum$bic ,xlab =" Number of Variables ",ylab="Bic",
     type="l")
min_bic_fwd=which.min (regfit.fwd_sum$bic )
points (min_bic_fwd, regfit.fwd_sum$bic [min_bic], col ="red",cex =2, pch =20)

coef(regfit.fwd,min_bic_fwd)
####################################
#       backward subset             #
####################################
regfit.bck=regsubsets(Salary~.,data=Hitters,method="backward")
regfit.bck_sum=summary(regfit.bck)

par(mfrow=c(1,1))
## BIC
plot(regfit.bck_sum$bic ,xlab =" Number of Variables ",ylab="Bic",
     type="l")
min_bic_bck=which.min (regfit.bck_sum$bic )
points (min_bic_bck, regfit.bck_sum$bic [min_bic], col ="red",cex =2, pch =20)

coef(regfit.fwd,min_bic_bck)
####################################
#       step F/B subset            #
####################################
attach(Hitters)
# step_forward
install.packages("leaps")
library(leaps)
lin_lm=lm(Salary~1,data=Hitters)
slm.forward=step(lin_lm,direction="forward",scope=~AtBat+NewLeague+CWalks+Walks)


# step_back
reg.lm1 <- lm(Salary ~AtBat+NewLeague+CWalks+Walks,data=Hitters) 
slm.backward <- step(reg.lm1, direction="backward")



####################################
#       Choosing model             #
####################################


# CV + BEST SUBSET

set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])

val.errors=rep(NA,19)
for( i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean(Hitters$Salary[test]-pred)^2
}

val.errors
which_min_errors=which.min(val.errors)
coef(regfit.best,which_min_errors)

# we can write that in a function
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax = 19)
coef(regfit.best,10)


# K-FOLD+ BEST SUBSET
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))


for(j in 1:k){
  best.fit =regsubsets (Salary???.,data=Hitters [folds !=j,],
                        nvmax =19)
  for(i in 1:19) {
    pred=predict (best.fit ,Hitters [folds ==j,], id=i)
    cv.errors [j,i]=mean( (Hitters$Salary[folds ==j]-pred)^2)
  }
}

# get the average MSE
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
plot(mean.cv.errors,type="b")

which_min_errors=which.min(mean.cv.errors)
coef(regfit.best,which_min_errors)

reg.best=regsubsets (Salary???.,data=Hitters , nvmax =19)
coef(reg.best,10)

####################################
#       RIDGE REGRESSIOn           #
####################################
library(glmnet)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
grid=10^seq(10,-2,length=100)

ridge.mod=glmnet(x,y,alpha=0,lambda = grid)
# dimensions are 20 rows for every pred+intercept
# and 100 for every value of lambda
## 20 100
dim(coef(ridge.mod))
# when lambda is at index 50 the coef:
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
# this is the l2 norm
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# when lambda is at index 60--l=705
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# predict the coef for a specific lambda
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# split into test and train to determine performance of lasso and ridge
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
# if we wanted 80 20
# train=sample(1:nrow(x),nrow(x)*0.80)
test=(-train)
Hitters.train=Hitters[train,]
Hitters.test=Hitters[test]
y.test=y[test]

# use lambda of 4
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,
                 thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
# the MSE is 105670.4

# fitting an intercept only model woudl have predicted eahc oversvation using the mean of the training data
mean((mean(y[train])-y.test)^2)
#204464.6


# using lambda of 10^10
ridge.pred=predict(ridge.mod,s=10,newx=x[test,])
mean((ridge.pred-y.test)^2)
#105670.4

# to fit a normal OLS we use lambda=0
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)

##### CV TO FIND LAMBDA
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam.cv=cv.out$lambda.min
bestlam.cv
# TEST ERROR
ridge.pred=predict(ridge.mod,s=bestlam.cv,newx = x[test,])
mean((ridge.pred-y.test)^2)
#108858.8

# fit the final model
out=glmnet(x,y,alpha=0)
predict(out,type="coefficient",s=bestlam.cv)[1:20,]


####################################
#       LASSO REGRESSIOn           #
####################################

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)

lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  legend('topleft', legend=labs, col = 1:20, lty=1)
}

lbs_fun1 <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  legend('topright', legend=labs, col = 1:20, lty=1)
}


plot(lasso.mod,"norm",col = 1:20)
lbs_fun(lasso.mod)

plot(lasso.mod,label=TRUE,"lambda",col = 1:20)
lbs_fun1(lasso.mod)

set.seed(1)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
bestlam

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
#108771.9

# fit the final lasso model
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

####################################
#       PCR REGRESSIOn             #
####################################
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,
            validation="CV")
# by default its a 10-fold CV
summary(pcr.fit)
# theres 19 possible principle components
# PCR gives the root mean squared error so
# you need to square this quantity to get MSE

# visualize the CV-MSE 
validationplot(pcr.fit,val.type = "MSEP")

cverr <- RMSEP(pcr.fit)$val[1,,]
imin <- which.min(cverr) - 1

# Use training and testing dataset
set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,
            validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
summary(pcr.fit)

RMSEP(pcr.fit)
# get the minimum 
cverr <- RMSEP(pcr.fit)$val[1,,]
imin <- which.min(cverr) - 1

# compute test MSE
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
#114160.2
# PCR is much more difficult to interpret and does not do any feature selection

# so final fit with optimal number of components
pcr.fit=pcr(y~x,scale=TRUE,ncomp=imin)
summary(pcr.fit)

####################################
#       PLS REGRESSIOn             #
####################################

set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters,subset=train, scale=TRUE,
             validation="CV")
summmary(pls.fit)

# pls uses directions not componets
cverr <- RMSEP(pls.fit)$val[1,,]
imin <- which.min(cverr) - 1
validationplot(pls.fit,val.type = "MSEP")

# check on testing data
pls.pred=predict(pls.fit,x[test,],ncom=imin)
mean((pls.pred-y.test)^2)
#114772.6
# PLS tries to look for directions that will explain vairblity in both the response and the predictors

detach(Hitters)

####################################
#           EXERCISES              #
####################################

# exericse 9
attach(College)
# A
train=sample(1:nrow(College),nrow(College)/2)
College.train=College[train,]
College.test=College[test,]

# B
fit.lm =lm(Apps~., data = College.train)
lm.pred=predict(fit.lm, College.test)
lm_mse=mean((lm.pred - College.test$Apps)^2)
# TEST MSE: 1120923

# C
x=model.matrix(Apps~.,College)[,-2]
y=College$Apps
grid=10^seq(10,-2,length=100)
y.test=y[test]

set.seed(1)
ridge.mod=glmnet(x,y,alpha=0,lambda = grid)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam.cv=cv.out$lambda.min

ridge.pred=predict(ridge.mod,s=bestlam.cv,newx = x[test,])
ridge_mse=mean((ridge.pred-y.test)^2)
#TEST MSE: 1408176

# fit the final model
out=glmnet(x,y,alpha=0)
predict(out,type="coefficient",s=bestlam.cv)[1:18,]


# D
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
set.seed(1)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
bestlam

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
lasso_mse=mean((lasso.pred-y.test)^2)
# TEST MSE : 1120783

# fit the final lasso model
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

# E

set.seed(1)
pcr.fit=pcr(Apps~.,data=College,subset=train,scale=TRUE,
            validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
summary(pcr.fit)

RMSEP(pcr.fit)
# get the minimum 
cverr <- RMSEP(pcr.fit)$val[1,,]
imin <- which.min(cverr) - 1

# compute test MSE
pcr.pred=predict(pcr.fit,x[test,],ncomp=imin)
pcr_mse=mean((pcr.pred-y.test)^2)
# TEST MSE: 1225525
# PCR is much more difficult to interpret and does not do any feature selection

# so final fit with optimal number of components
pcr.fit=pcr(y~x,scale=TRUE,ncomp=imin)
summary(pcr.fit)

# F

set.seed(1)
pls.fit=plsr(Apps~.,data=College,subset=train, scale=TRUE,
             validation="CV")
summmary(pls.fit)

# pls uses directions not componets
cverr <- RMSEP(pls.fit)$val[1,,]
imin <- which.min(cverr) - 1
validationplot(pls.fit,val.type = "MSEP")

# check on testing data
pls.pred=predict(pls.fit,x[test,],ncom=imin)
pls_mse=mean((pls.pred-y.test)^2)
# TEST MSE: 1222861

message("MSE for *test data is ", round(lm_mse,3)," for linear fit")
message("MSE for *test data is ", round(ridge_mse,3)," for ridge fit")
message("MSE for *test data is ", round(lasso_mse,3)," for lasso fit")
message("MSE for *test data is ", round(pcr_mse,3)," for pcr fit")
message("MSE for *test data is ", round(pls_mse,3)," for pls fit")

total_mse_names=c("linear"," ridge","lasso", "pcr", "pls")
total_mse=c(lm_mse,ridge_mse,lasso_mse,pcr_mse,pls_mse)
min_error=which.min(total_mse)
total_mse_names[min_error]


# calculate R2
test.avg <- mean(College.test$Apps)
lm.r2 <- 1 - mean((pred.lm - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
ridge.r2 <- 1 - mean((ridge.pred - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
lasso.r2 <- 1 - mean((lasso.pred - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pcr.r2 <- 1 - mean((pcr.pred - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pls.r2 <- 1 - mean((pls.pred - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)

lm.r2
ridge.r2
lasso.r2
pcr.r2
pls.r2








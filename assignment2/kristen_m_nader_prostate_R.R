#########################################################################
## Kristen Michelle Nader                                               ##    
## Statistial Methods for Bioinformatics                                ##
## r0771801                                                             ##    
## 5/19/2020                                                            ##
#########################################################################

#setwd("/Users/user/Desktop/Assignment_2/")
# fix data
fix(prostate)
# install packages
install.packages("gam")
install.packages("corrplot")
install.packages("glmnet")
install.packages("viridis")

# call libraries for use
library(viridis)
library(gam)
library(glmnet)
library(corrplot)

#Exploration of data
prostate_data=prostate
attach(prostate_data)
# 1. Statistical Modeling : survey the data
# what kind of respnse and explanatory variables do you have?
summary(prostate_data)
# we see that svi takes on values of 0 and 1 so it should be classified as a factor
sapply(prostate_data,class)
# what is the shape of the distribution(histograms) and check linearity assumptions 
lm_t1=lm(Cscore~.,data=prostate_data)
# check linearity assumtions
fit<-fitted(lm_t1)
rs<-rstandard(lm_t1)
plot(rs~fit)
# the ad random pattern seems to be violated(non linear model?)
# test the normality of residuals
shapiro.test(rs)
# H0: normally distributed H1: not normally distributed
# p-value = 3.305e-11 < 0.05 reject null but also check histogram bc shapiro is often too strict

# Figure 1a
jpeg('Figure-1.jpg',width = 700,height = 500)
par(mfrow=c(1,2))
hist(Cscore,main="Histogram for Cscore",xlab="Cscore",col="pink",xlim=c(-50,300),las=2,breaks=50,probability = TRUE)
lines(density(Cscore),col="purple")

# Figure 1b
d<-density(Cscore)
plot(d, main="Kernel Density of Cscore")
polygon(d, col="pink", border="purple")
# we can see again from the histograms that its not very normal dis 
# try log transformed--> but we have negative values and would run into trouble 
dev.off()
# Figure 1c-supplementary
jpeg('Figure-1x.jpg',width = 700,height = 500)
par(mfrow=c(1,1))
boxplot(Cscore,col="pink",horizontal = TRUE)
title("Boxplot of a Cscore distribution")
dev.off()

#Figure 2
# do you see any associations woth other variables(scatter)
# an interesting thing to look at would be the pairs plot to see correlation
jpeg('Figure-2.jpg',width = 700,height = 500)
par(mfrow=c(1,1))
pairs(prostate_data)
dev.off()
# we see that there a curve in the plots corresponnding to Cscore and lcavol and lpsa
# leads us to believe that we have non-linearity and should try non-linear models 

# plots for exploratory analysis
#Figure 3
jpeg('Figure-3.jpg',width = 700,height = 500)
par(mfrow=c(1,1))
plot(as.factor(svi),Cscore, main="Cscore versus svi",xlab="SVI (0=no, 1=yes)", ylab="Cscore")
dev.off()

#Figure 4a
jpeg('Figure-4_mod.jpg',width = 700,height = 500)
par(mfrow=c(1,2))
temp_csore_lcavol=lm(Cscore~lcavol)
plot(lcavol,Cscore,type="p", pch=21,col="black" ,main="Cscore versus lcavol",xlab="lcavol", ylab="Cscore")
abline(lm(Cscore~lcavol),lwd=2,col="pink")
legend("topleft",c("Linear Fit"),fill=c("pink"))

#Figure 4b       
plot(lpsa,Cscore, main="Cscore versus lpsa",xlab="lpsa",ylab="Cscore")
abline(lm(Cscore~lpsa),lwd=2,col="purple")
legend("topleft",c("Linear Fit"),fill=c("purple"))

dev.off()


#Figure 5
jpeg('Figure-5.jpg',width = 700,height = 500)
par(mfrow=c(1,1))
C=cor(prostate)
corrplot(C,method="square",main="Correlation plot",col=rev(plasma(11)))
dev.off()

# 2. Statistical Modeling: Choose the model


# try using a linear model and lasso
x=model.matrix(Cscore~.,prostate_data)[,-1]
y=prostate_data$Cscore
set.seed(1)
grid=10^seq(10,-2,length=100)

train=sample(1:nrow(x),nrow(x)/2)
test=-train
y.test=y[test]

prostate.train <- prostate_data[train, ]
prostate.test <- prostate_data[test, ]

# Question 1 : LASSO
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)

##https://stackoverflow.com/questions/30560689/adding-labels-on-curves-in-glmnet-plot-in-r
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  legend('topleft', legend=labs, col = 1:7, lty=1)
}


lbs_fun1 <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  legend('topright', legend=labs, col = 1:7, lty=1)
}



cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min

#Figure 6
jpeg('Figure-6.jpg',width = 700,height = 500)
plot(lasso.mod,label=TRUE,"norm",col = 1:7)
lbs_fun(lasso.mod)
dev.off()

#Figure 7
jpeg('Figure-7.jpg',width = 700,height = 500)
plot(lasso.mod,label=TRUE,"lambda",col = 1:7)
lbs_fun1(lasso.mod)
abline(v=log(bestlam))
dev.off()

par(mfrow=c(1,2))
#Figure 8
jpeg('Figure-8.jpg',width = 700,height = 500)
par(mfrow=c(1,1))
plot(cv.out)
dev.off()

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
lasso_mse=mean((lasso.pred-prostate.test$Cscore)^2)
# MSE of the test set: 717.7531

out_lasso=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out_lasso,type="coefficients",s=bestlam)[1:8,]
lasso.coef
lasso.coef[lasso.coef!=0]
# so according to lasso
# a unit increase lcavol will lead to a 3.02 decreae in C_score
# in addition lcavol does stay in the model and lasso is a variable 
# detection model and therefore must have imprtnt predictive abilities



# fit a linear model using the features selected using LASSO

ls.fit = lm(Cscore~lcavol+lweight+svi+lcp+lpsa,data=prostate.train)
ls.fit.pred=predict(ls.fit, prostate.test)
MSE.test = mean((ls.fit.pred-prostate.test$Cscore)^2)

message("MSE for *test data is ", round(MSE.test,3)," for Cscore ~ lcavol + lweight + svi + lcp + lpsa")
message("MSE for *test data is ", round(lasso_mse,3)," for Cscore ~ lcavol + lweight + svi + lcp + lpsa *****LASSO")



# use a non-linear model
# need to use a generalized additive model

# try fitting a polynomial model for lpsa and lcavol

# find the optimal polynomial that explains the relationship using test MSE
test_mse_lcavol <- rep(NA, 10)
for (i in 1:10){
  temp=lm(Cscore~poly(lcavol,i),data=prostate.train)
  pred_temp=predict(temp, prostate.test)
  test_mse_lcavol[i]=mean((pred_temp-prostate.test$Cscore)^2)
}

test_mse_lpsa <- rep(NA, 10)
for (i in 1:10){
  temp=lm(Cscore~poly(lpsa,i),data=prostate.train)
  pred_temp=predict(temp, prostate.test)
  test_mse_lpsa[i]=mean((pred_temp-prostate.test$Cscore)^2)
}

#Figure 8a
jpeg('Figure-8.jpg',width = 700,height = 500)
par(mfrow=c(1,2))
plot(1:10, test_mse_lpsa, xlab = "Degree", ylab = "Test MSE", type = "l",lwd=2,main="MSE-lpsa")
d.min <- which.min(test_mse_lpsa)
points(which.min(test_mse_lpsa), test_mse_lpsa[which.min(test_mse_lpsa)], col = "pink", cex = 2, pch = 20)

#Figure 8b
plot(1:10, test_mse_lcavol, xlab = "Degree", ylab = "Test MSE", type = "l",lwd=2,main="MSE-lcavol")
d.min <- which.min(test_mse_lcavol)
points(which.min(test_mse_lcavol), test_mse_lcavol[which.min(test_mse_lcavol)], col = "pink", cex = 2, pch = 20)

dev.off()



#Figure 9a
jpeg('Figure-9.jpg',width = 700,height = 500)
par(mfrow=c(1,2))
lm_poly3_lcavol=lm(Cscore~poly(lcavol,3),data=prostate)
plot(Cscore~lcavol,main="Cscore and lcavol fitted with a poly-3 and Smoothing Spline")

myPredict <- predict( lm_poly3_lcavol ) 
ix <- sort(lcavol,index.return=T)$ix
lines(lcavol[ix], myPredict[ix], col="purple", lwd=2  )  

lcavol_range=range(lcavol)
lcavol.grid=seq(from=lcavol_range[1],to=lcavol_range[2])
fit_lcavol=smooth.spline(lcavol,Cscore,cv=T)
lcavol_f_df=as.integer(fit_lcavol$df)

lines(fit_lcavol,col="yellow",lwd=2)
legend("topleft",c("polynomial-Fit-3","Smoothing spline-df-3"),fill=c("purple","yellow"))

#Figure 9b
lm_poly3_lpsa=lm(Cscore~poly(lpsa,3),data=prostate)
plot(Cscore~lpsa,main="Cscore and lpsa fitted with a poly-3 and Smoothing Spline")

myPredict <- predict( lm_poly3_lpsa ) 
ix <- sort(lpsa,index.return=T)$ix
lines(lpsa[ix], myPredict[ix], col="purple", lwd=2)  

lpsa_range=range(lpsa)
lpsa.grid=seq(from=lpsa_range[1],to=lpsa_range[2])
fit_lpsa=smooth.spline(lpsa,Cscore,cv=T)
lpsa_f_df=as.integer(fit_lpsa$df)

lines(fit_lpsa,col="yellow",lwd=2)
legend("topleft",c("polynomial-Fit-3","Smooting spline-df-4"),fill=c("purple","yellow"))


dev.off()



#full model
gam.m=gam(Cscore~s(lcavol,df=3.842969)+s(lpsa,df=4.004397)+svi+lweight+lcp+age+lbph,data=prostate.train)
#Figure 10
par(mfrow=c(1,7))
plot(gam.m,se=TRUE,col="purple")
preds <- predict(gam.m, prostate.test)
err <- mean((preds-prostate.test$Cscore)^2)
coef(gam.m)

#model without lbph
gam.m1=gam(Cscore~s(lcavol,df=3.842969)+s(lpsa,df=4.004397)+svi+lweight+lcp+age,data=prostate.train)
preds1 <- predict(gam.m1, prostate.test)
err1 <- mean((preds1-prostate.test$Cscore)^2)

#model without age
gam.m2=gam(Cscore~s(lcavol,df=3.842969)+s(lpsa,df=4.004397)+svi+lweight+lcp+lbph,data=prostate.train)
preds2 <- predict(gam.m2, prostate.test)
err2 <- mean((preds2-prostate.test$Cscore)^2)

#model without both
gam.m3=gam(Cscore~s(lcavol,df=3.842969)+s(lpsa,df=4.004397)+svi+lweight+lcp,data=prostate.train)
preds3 <- predict(gam.m3, prostate.test)
err3 <- mean((preds3-prostate.test$Cscore)^2)

anova(gam.m,gam.m1)

anova(gam.m,gam.m2)

anova(gam.m,gam.m3)


# compare the 2 models


AIC(lm_fitted_lasso)
AIC(ls.fit)
AIC(gam.m)
AIC(gam.m1)
AIC(gam.m2)
AIC(gam.m3)


## Finished script

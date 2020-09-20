##CHAPTER 8_NON linear modeling

#NON-LINEAR MODELING

library(ISLR)
attach(Wage)

#7.8.1. polynomrial regression and step functions
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$ft-2*preds$se.fit)

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# use anova
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# try to predict
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

# plot
plot(age,I(wage>250),xlims=agelims,type="n",ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex=0.5,pch="|",
       col="darkgrey")
matlines(age.grid,se.bands,lwd=1,
         col="blue",lty=3)


####################################
#      STEP FUNCTION               #
####################################

table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

####################################
#      SPLINES FUNCTION            #
####################################
install.packages("splines")
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
dim(bs(age,knot=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

####################################
#      NATURAL FUNCTION            #
####################################

fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata = list(age=age.grid),
              se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

####################################
#      SMOOTHING FUNCTION          #
####################################
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Smoothing splines")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend = c("16DF","6.79DF"),
       col=c("red","blue"),lty=1,lwd=2,cex=0.8)
####################################
#      LOESS FUNCTION              #
####################################
plot(age,wage,xlims=agelims,cex=0.5,
     col="darkgrey")
title("local regression")
fit=loess(wage~age,span=0.2,data=Wage)
fit2=loess(wage~age,span=0.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),
      col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),
      col="blue",lwd=2)
legend("topright",legend=c("span=0.2","span=0.5"),
       col=c("red","blue"),lty=1,lwd=2,cex=0.8)
####################################
#      GAM FUNCTION              #
####################################
gam1=lm(wage~ns(year,4)+ns(age,5),education,data=Wage)
plot.Gam(gam1,se=TRUE,col="red")

library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")

# use anova to test
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)

anova(gam.m1,gam.m2,gam.m3,test="F")

preds=predict(gam.m2,newdata=Wage)
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,
           data=Wage)
plot.Gam(gam.lo,se=TRUE,col="green")

# can make interactions
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,
             data=Wage)

# plot akima
install.packages("akima")
library(akima)
par(mfrow=c(1,1))
plot(gam.lo.i)

# logistic regression using GAM
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,
           data=Wage,family=binomial)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")


####################################
#      EXERCISES                   #
####################################
# exercise 1

par(mfrow=c(1,1))
library(boot)
library(ISLR)

set.seed(2)
poly.mse=c()
for(degree in 1:7){
  poly.fit=glm(wage~poly(age,degree,raw=T),data=Wage)
  mse=cv.glm(poly.fit,data = Wage,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}

plot(poly.mse,xlab='Degree of Polynomial',ylab='Cross Validation Error',type='l')
x=which.min(poly.mse)
points(x,poly.mse[x],pch=20,cex=2,col='red')




set.seed(42)
step.mse=c()
for(br in 2:10){
  Wage.model=model.frame(wage~cut(age,br),data=Wage)
  names(Wage.model)=c('wage','age')
  
  step.fit=glm(wage~cut(age,br),data=Wage)
  mse=cv.glm(step.fit,data = Wage.model,K=10)$delta[1]
  step.mse=c(step.mse,mse)
}

plot(step.mse,xlab='Number of cuts',ylab='Cross Validation Error',type='l')
x=which.min(step.mse)
points(x,step.mse[x],pch=20,cex=2,col='red')


# write the 7cut step function
table(cut(age,7))
fit=glm(wage~cut(age,7),data=Wage)
coef(summary(fit))
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
title("7-cut step function")

# question 7
set.seed(1)

step.mse_year_lo=c()
for(i in seq(0.1,1,by=0.1)){
  print(i)
  step.fit_lo=glm(wage~lo(year,span=i),data=Wage)
  mse_lo=cv.glm(step.fit_lo,data = Wage,K=10)$delta[1]
  step.mse_year_lo=c(step.mse_year_lo,mse_lo)
}

plot(step.mse_year_lo,xlab='Span',ylab='Cross Validation Error',type='l')
x=which.min(step.mse_year_lo)
points(x,step.mse_year_lo[x],pch=20,cex=2,col='red')

# the best span is 0.9

fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df

# fit the gam
fit1<- gam(wage ~ lo(year, span = 0.9) + s(age, 7) + education ,data=Wage)
fit2 <- gam(wage ~ lo(year, span = 0.9) + s(age, 7) + education + jobclass, data = Wage)
fit3 <- gam(wage ~ lo(year, span = 0.9) + s(age, 7) + education + maritl, data = Wage)
fit4 <- gam(wage ~ lo(year, span = 0.9) + s(age, 7) + education + jobclass + maritl, data = Wage)
anova(fit1,fit2,fit3,fit4)

# exercise 8

pairs(Auto)
plot(mpg~displacement,data=Auto)
plot(mpg~horsepower,data=Auto)
plot(mpg~weight,data=Auto)
plot(mpg~acceleration,data=Auto)

set.seed(1)
poly.mse=c()
for(degree in 1:4){
  poly.fit=glm(mpg~poly(displacement,degree,raw=T),data=Auto)
  mse=cv.glm(poly.fit,data = Auto,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}

plot(poly.mse,xlab='Degree of Polynomial',ylab='Cross Validation Error',type='l')
title("Displacement")
x=which.min(poly.mse)
points(x,poly.mse[x],pch=20,cex=2,col='red')
# best is 3

set.seed(1)
poly.mse=c()
for(degree in 1:4){
  poly.fit=glm(mpg~poly(horsepower,degree,raw=T),data=Auto)
  mse=cv.glm(poly.fit,data = Auto,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}

plot(poly.mse,xlab='Degree of Polynomial',ylab='Cross Validation Error',type='l')
title("horsepower")
x=which.min(poly.mse)
points(x,poly.mse[x],pch=20,cex=2,col='red')
# best is 3

set.seed(1)
poly.mse=c()
for(degree in 1:4){
  poly.fit=glm(mpg~poly(weight,degree,raw=T),data=Auto)
  mse=cv.glm(poly.fit,data = Auto,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}

plot(poly.mse,xlab='Degree of Polynomial',ylab='Cross Validation Error',type='l')
title("weight")
x=which.min(poly.mse)
points(x,poly.mse[x],pch=20,cex=2,col='red')
# best is 2

set.seed(1)
poly.mse=c()
for(degree in 1:4){
  poly.fit=glm(mpg~poly(acceleration,degree,raw=T),data=Auto)
  mse=cv.glm(poly.fit,data = Auto,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}

plot(poly.mse,xlab='Degree of Polynomial',ylab='Cross Validation Error',type='l')
title("acceleration")
x=which.min(poly.mse)
points(x,poly.mse[x],pch=20,cex=2,col='red')
# best is 2

lm.1=lm(mpg~poly(displacement,3)+poly(weight,2)+poly(horsepower,3)+poly(acceleration,2),data=Auto)
summary(lm.1)


fit_displacement=smooth.spline(Auto$displacement,Auto$mpg,cv=TRUE)
fit_displacement$df
fit_weight=smooth.spline(Auto$weight,Auto$mpg,cv=TRUE)
fit_weight$df
fit_acceleration=smooth.spline(Auto$acceleration,Auto$mpg,cv=TRUE)
fit_acceleration$df
fit_horsepower=smooth.spline(Auto$horsepower,Auto$mpg,cv=TRUE)
fit_horsepower$df
attach(Auto)
fit.2=gam(mpg~s(displacement,20)+
            s(weight,10)+
            s(acceleration,5)+
            s(horsepower,6))
par(mfrow=c(1,4))
plot(fit.2,se=TRUE,col="blue")

lm1 <- glm(mpg ~ displacement + horsepower + weight, data = Auto)
lm2 <- glm(mpg ~ poly(displacement, 3) + poly(horsepower, 3) + weight, data = Auto)
lm3 <- glm(mpg ~ poly(displacement, 5) + poly(horsepower, 5) + poly(weight, 5), data=Auto)

           
cv.glm(Auto, lm1, K = 10)$delta[1]          
#18.37
cv.glm(Auto, lm2, K = 10)$delta[1]     
# 15.67752
cv.glm(Auto, lm3, K = 10)$delta[1]     
#15.23363   
cv.glm(Auto, fit.2, K = 10)$delta[1]    
#15.06259
detach(Auto)
detach(Wage)
# Exercise 9
par(mfrow=c(1,1))
library(MASS)
pairs(Boston)
attach(Boston)
plot(nox~dis)

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
Boston=na.omit(Boston)
Boston.train=Boston[train,]
Boston.test=Boston[-train,]

test_mse<- rep(NA, 5)

for (i in 1:5){
  temp=lm(nox~poly(dis,i),data=Boston.train)
  pred_temp=predict(temp, Boston.test)
  test_mse[i]=mean((pred_temp-Boston.test$nox)^2)
}
plot(1:5, test_mse, xlab = "Degree", ylab = "Test MSE", type = "l",lwd=2,main="MSE")
d.min <- which.min(test_mse)
points(which.min(test_mse), test_mse[which.min(test_mse)], col = "pink", cex = 2, pch = 20)


fit.1=lm(nox~poly(dis,4))
summary(fit.1)
dislims=range(dis)

dis.grid=seq(from=dislims[1],to=dislims[2])
preds=predict(fit.1,newdata=list(dis=dis.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$ft-2*preds$se.fit)

#par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(dis,nox,xlim=dislims,cex=0.5,col="darkgrey")
title("degree-4 Polynomial",outer=T)
lines(dis.grid,preds$fit,lwd=2,col="blue")
matlines(dis.grid, se.bands, lwd = 1, col = "red", lty = 3)

library(splines)
fit=lm(nox~bs(dis,df=4),data=Boston)
attr(bs(Boston$dis,df=4),"knots")

pred=predict(fit,newdata=list(dis=dis.grid),se=T)
plot(dis,nox,col="gray")
lines(dis.grid,pred$fit,lwd=2)
lines(dis.grid,pred$fit+2*pred$se,lty="dashed",col="red")
lines(dis.grid,pred$fit-2*pred$se,lty="dashed",col="red")

test_mse=c()
rss = c()

for (i in 1:16){
  fit=lm(nox~bs(dis,df=i),data=Boston.train)
  pred=predict(fit,newdata=Boston.test)
  test_mse[i]=mean((pred-Boston.test$nox)^2)
  rss=c(rss,sum(fit$residuals^2))
}

plot(1:16, test_mse, xlab = "Degree", ylab = "Test MSE", type = "l",lwd=2,main="MSE")
d.min <- which.min(test_mse)
points(which.min(test_mse), test_mse[which.min(test_mse)], col = "pink", cex = 2, pch = 20)

detach(Boston)
#Exercise 10
library(ISLR)
attach(College)

pairs(College)

train=sample(1:nrow(College),nrow(College)/2)
College.train=College[train,]
College.test=College[-train,]
library(leaps)
forward=regsubsets(Outstate~.,data=College,method = 'forward')
reg.summary=summary(forward)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")

plot(summary(forward)$bic,type="b")


points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="blue",cex=2,pch=20)
coef(forward, id = 6)


library(splines)
# private room board, phd perc alumni expend grad rate


plot(Outstate~Room.Board) #  linear
plot(Outstate~PhD) # non linerar
plot(Outstate~perc.alumni) # linear
plot(Outstate~Expend) # non
plot(Outstate~Grad.Rate) # linear

fit1=smooth.spline(PhD,Outstate,cv=T) #5.436682
fit2=smooth.spline(Expend,Outstate,cv=T) # 5


gam1 = gam(Outstate~Private +s(Room.Board,4) +s(PhD,4)+ s(perc.alumni,4)+s(Expend,4)+s(Grad.Rate,4) ,data=College,subset=train) 
predgam = predict(gam1, newdata=College.test) 
msegam1 = mean((predgam-College.test$Outstate)^2)
msegam1 # 3687478



gam2 = gam(Outstate~Private+Room.Board+s(PhD,5.436682)+ perc.alumni+s(Expend,5)+Grad.Rate,data=College,subset=train)
predgam2 = predict(gam2, newdata=College.test) 
msegam2 = mean((predgam2-College.test$Outstate)^2)
msegam2 # 3669790






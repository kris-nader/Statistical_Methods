## Chapter 7: moving beyond linearity

###Non linear modeling 

library(ISLR)
attach(Wage)


##Polynomial regression and step functions
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se)

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlims=agelims,cex=0.5,col="darkgrey")
title("degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="purple")

matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


# how to decide on the degree of the polynomial
# we can use hypothesis testing and anova



fit.1=lm(wage~poly(age,1),data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)

anova(fit.1,fit.2,fit.3,fit.4,fit.5)
# from this we see that either a cubic or 4th degree is the best choice

## TASK: predict if an indivdual earns more than 250.000

fit=glm(I(wage>250)~poly(age,4),data=Wage,family="binomial")
preds=predict(fit,newdata=list(age=age.grid),se=T)

#### rug plots
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex=0.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="purple")

matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)



## step functions
# cut function will randomly find cut points
# step functions return ordered categorical variables
table(cut(age,4))
dim(Wage)
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

## splines
install.packages("splines")
library(splines)

fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata = list(age=age.grid),se=T)

plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed",col="purple")
lines(age.grid,pred$fit-2*pred$se,lty="dashed",col="purple")

# we have K+4 degrees of freedom
# then we have 3 knots==> 7 df

# natural spline--> constraint so that teh function is linear after the bounds of the knots
# g"(xi)=g"(xn)=0

fit2=lm(wage~ns(age,df=4),dat=Wage)
pred2=predict(fit2,newdata = list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("smooting splines")

fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
#6.794596
lines(fit,col="yellow",lwd=2)
lines(fit2,col="green",lwd=2)



plot(age,wage,xlmin=agelims,cex=0.5,col="darkgrey")
title("Local regression")
fit=loess(wage~age,span=0.2,data=Wage)
fit2=loess(wage~age,span=0.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)

lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)

##GAM
install.packages("gam")
library(gam)
#using natural splines
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

#using smoothing splines
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")

plot.gam(gam1,se=TRUE,col="red")


#use anova to see which modle is bettwe
gam.m1=gam(wage~s(age,5)+education,data = Wage)
gam.m2=gam(wage~year+s(age,5)+education,data = Wage)

summary(gam.m3)
# null hypothesis: linear relationship
# alternative hypothesis: non-linear relatonship


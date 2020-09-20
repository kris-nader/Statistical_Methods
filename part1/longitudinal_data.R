##LONGITUDINAL DATA
# install packages
install.packages("lattice")
install.packages("lme4")
install.packages("arm")
install.packages("pbkrtest")

library(lme4)
library(arm)
library(pbkrtest)

library(lattice)
early.int1=read.table(file=file.choose(),header=TRUE,sep=",")

attach(early.int1)
head(early.int1)
fix(early.int1)

## Spaghetti plots
n=length((unique(id)))
interaction.plot(age,id,cog,xlab="Age in years", ylab="IQ",legend=F)

# Descriptive tables
# Mean:
early.mean=tapply(cog,list(age,program),mean)
# Standard Deviations:
early.sd=tapply(cog,list(age,program),sd)
# Variance
early.var=tapply(cog,list(age,program),var)

#Frequency
early.n=table(age,program)

early.mean
early.var
early.sd
early.n

## Boxplots
# IQ vs age
boxplot(cog~as.factor(age),xlab="Age in years", ylab="IQ",col="pink")
# IQ vs age and program
boxplot(cog~as.factor(age)*program)
# create subsets 
program1=subset(early.int1,early.int1$program==1)
program0=subset(early.int1,early.int1$program==0)

# for program 1
boxplot(cog~age,data=program1,xlab="Age in years", ylab="IQ",main="program is 1",col="pink")
# for program 0
boxplot(cog~age,data=program0,xlab="Age in years", ylab="IQ",main="program is 0",col="yellow")

boxplot(cog[program==0]~age[program==0],main="no Intervention",xlab="Age in years",ylab="IQ",col="yellow")
boxplot(cog[program==1]~age[program==1],main="Intervention",xlab="Age in years",ylab="IQ",col="pink")

# mean evolution and error bars
errbar=function(x,y,height,width,lty=1,col="black"){
  arrows(x,y,x,y+height,angle=90,lenfth=width,lty=lty,col=col)
  arrows(x,y,x,y-height,angle=90,length=width,lty=lty,col=col)
}
# plotting mean evolutions
plot(age[id==1],early.mean[,1],type="b",xlim=c(1,2),ylim=c(40,160),xlav="Age in years",ylab="IQ",axes=F,main="mean evolution with 1SE intervals")
axis(side=1,at=c(1,1.5,2),labels=c(1,1.5,2))
axis(side=2,at=seq(40,160,20))

box()
points(age[id==1],early.mean[,2],type="b",col="red")
errbar(age[id==1]-.005,early.mean[,1],early.sd[,1],.1)
errbar(age[id==1]+.005,early.mean[,2],early.sd[,2],.1,col="red")

## reshaping data into a wide form
early.int2=reshape(early.int1,timevar="age",idvar=c("id","program"),direction="wide")
early.int2
head(early.int2)

# find correlation between IQ scores at differnt ages
cor(early.int2[,3:5])

# Linear regression per person--LEVEL 1 how the person varies within himslef 
# Trellis plot
early.int1$age0<-early.int1$age-1

inbed=function(x){
  mod1=lm(cog~age0,data=subset(early.int1,id==x))
  coef(mod1)
}

cf<-sapply(early.int1$id, inbed)

Sx=reorder(early.int1$id, cf[1,])

xyplot(cog~age0|Sx,groups=program,data=early.int1,
       type=c('p','r'),auto.key=T,aspect="xy",
       par.settings=list(axis.text=list(cex=0.6),
                         fontsize=list(text=8,points=10)),
       scales=list(x=list(at=c(0,0.5,1),labels=c("0","0.5","1"))))


# linear regression per participant of cof on age

## coefficients
lin.reg.coef=by(early.int1,early.int1$id, function(data) coef(lm(cog~age0, data=data)))
lin.reg.coef1<-unlist(lin.ref.coef)
names(lin.reg.coef1)<-NULL
lin.reg.coef2=matrix(lin.reg.coef1,length(lin.reg.coef1)/2,2,byrow=TRUE)

# R squared
lin.reg.r.squared=by(early.int1,early.int1$id,function(data) summary(lm(cog~age, data=data))$r.squared)
lin.reg.r.squared1=as.vector(unlist(lin.reg.r.squared))


# Histograms-- between subject variablity second level
par(mfrow=c(3,1))
hist(lin.reg.coef2[,1],xlab="intercept",col="lightblue",main="Histogram of individual intercepts")
hist(lin.reg.coef2[,2],xlab="slope",col="lightpink",main="Histogram of individual slopes")
hist(lin.reg.r.squared1,xlab="Rsquared",col="lightgreen",main="Histogram of individual R squared")

# why do wee have a large range for intercept and slope?

reg.coef=cbind(lin.reg.coef2,early.int1[early.int1$age==1,]$program)
mean.int=tapply(reg.coef[,1],reg.coef[,3],mean)
mean.slope=tapply(reg.coef[,2],reg.coef[,3],mean)





par(mfrow=c(1,2))
plot(age,cog,type="n",xlim=c(1,2),ylim=c(40,160),main="No intervention",
     xlab="Age-1 in years", ylab="IQ",axes=F)
axis(side=1,at=c(1,1.5,2),labels=c(1,1.5,2))
axis(side=2,at=seq(40,160,20))
box()
for (i in 1:103){
  if(reg.coef[i,3]==0)
    curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")
}
curve(cbind(1,x)%*%c(mean.int[1],mean.slope[1]),add=T,col="black")


plot(age,cog,type="n",xlim=c(1,2),ylim=c(40,160),main="Intervention",
     xlab="Age-1 in years", ylab="IQ",axes=F)
axis(side=1,at=c(1,1.5,2),labels=c(1,1.5,2))
axis(side=2,at=seq(40,160,20))
box()
for (i in 1:103){
  if(reg.coef[i,3]==0)
    curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")
}
curve(cbind(1,x)%*%c(mean.int[2],mean.slope[1]),add=T,col="red")


## FITTING THE MODEL

early.int1$age0=early.int1$age-1

# fitting the model using ml
early.lmer1=lmer(cog~1+age0*program+(1+age0|id),REML=FALSE,data=early.int1)

summary(early.lmer1)
anova(early.lmer1)

# estimating the fixed efffects using boostrap
fixed.boot=bootMer(early.lmer1,FUN = fixef,use.u=TRUE,nsim=250)
fixed.boot
summary(fixed.boot)

# calculating confidence intervals for
# 1. WALDS test 
confint(early.lmer1,par=5:8,method="Wald",oldNames=FALSE)
# the confidence interval will be not significant if it contains 0 program CI not sig

# 2. Bootstrap
confint(early.lmer1,method="boot",boot.type="perc",oldNames=FALSE)
# once again if theres the 0 it is not sig so here  program CI not sig 

# 3. profile likelihood
confint(early.lmer1,level=0.95,method="profile",oldNames=FALSE)
# if the 0 is in the CI it is not significant  so program again nos significant 

## LIKELIHOOD RATIO TEST
early.lmer1.noprog=lmer(cog~1+age0+(1+age0|id),REML=FALSE,data=early.int1)
early.lmer1.intprog=lmer(cog~1+age0+program+(1+age0|id),REML=FALSE,data=early.int1)
early.lmer1=lmer(cog~1+age0*program+(1+age0|id),REML=FALSE,data=early.int1)

# estimating effects of the random effects
summary(early.lmer1)
anova(early.lmer1.noprog,early.lmer1.intprog,early.lmer1)

# get the random effects covariance matrix D
D.early=unclass(VarCorr(early.lmer1))$id
D.early

# predict the random effects
early.lmer1.re=ranef(early.lmer1)$id
head(early.lmer1.re,10)

# create a plot
par(mfrow=c(1,1))
plot(early.lmer1.re,main="random intercept boi vs random slop b1i")
# we can see a drecreasing and negative tremd

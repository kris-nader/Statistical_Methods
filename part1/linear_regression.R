##Linear regression

##library
install.packages("pastecs")
library(pastecs)
kalama=read.table(file=file.choose(),header=T)
kalama
# Descriptive Statistics
options(digits=2)
descrip.kalama<-stat.desc(kalama[,c("age","height")],basic=TRUE,desc=TRUE)
descrip.kalama

# calculate the covaraince and correlation
cov.age.height=cov(kalama$age,kalama$height)
corr.age.height=cor(kalama$age,kalama$height)

cov.age.height
corr.age.height

# use pearsons correltaion
corr.age.height.test=cor.test(kalama$age,kalama$height,alternative = "two.sided",method="pearson")

plot(age,height)
lm.fit=lm(height~age)

## Fitting the model

res=lm(height~age,data=kalama)
kalama.anova=anova(res)
kalama.summary=summary(res)
kalama.anova

kalama.summary

detach(kalama)

## PATIENT SATIFACTION MODEL

satistfaction=read.table(file=file.choose(),header=T)
head(satistfaction,10)

cor(satistfaction)
descript.sat<-stat.desc(satistfaction,basic=TRUE,desc=TRUE)
descript.sat
# nbr.val- number of values
# nbr.null- number of null values
# nbr.na-number of nas

plot(satistfaction)

# FITTING THE MODEL

attach(satistfaction)

satistfaction.lm=lm(satis~age+severity+anxiety,data=satistfaction)
satistfaction.summary=summary(satistfaction.lm)
satistfaction.summary


##Likelihood ration test null model vs full model
#Null model
satistfaction.lm.int=lm(satis~1,data=satistfaction)
anova(satistfaction.lm.int,satistfaction.lm)
# we will get the same p value with: and the H0:b1=b2=b3
summary(satistfaction.lm)


# now we compare the anova(sat.lm) to summary(lm)

# changing the order in lm will change anova reults of P
anova(satistfaction.lm)
summary(satistfaction.lm)

lm.fit2=lm(satis~age+anxiety+severity,data=satistfaction)
summary(lm.fit2)

# make the final models
satistfaction.lm.final=lm(satis~age+anxiety,data = satistfaction)
summary(satistfaction.lm.final)

# Making predictions
newdata=data.frame(age=c(43),anxiety=c(2.7))
pred.w.plim=predict(satistfaction.lm.final,newdata, interval = "predict")
pred.w.clim=predict(satistfaction.lm.final,newdata, interval = "confidence")





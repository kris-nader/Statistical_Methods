# longitudinal data analysis- for dental dataset
dental = read.table(file=file.choose(),header=T)
head(dental)
dental$sex_new=ifelse(dental$SEX=="2","F","M")
# is dental growth related to gender

# install packages
install.packages("lattice")
install.packages("lme4")
install.packages("arm")
install.packages("pbkrtest")
install.packages("ggplot2")

# load library
library(lme4)
library(arm)
library(pbkrtest)
library(lattice)
library(ggplot2)


# attach data
attach(dental)

# Question
#Is dental growth related to gender?

# Spaghetti lpot
n=length((unique(IDNR)))
interaction.plot(AGE,IDNR,MEASURE,xlab="Age in years", ylab="MEASURE",legend=F)


# Descriptive Statistics
dental.mean=tapply(MEASURE,list(SEX,AGE),mean)
# Standard Deviations:
dental.sd=tapply(MEASURE,list(SEX,AGE),sd)
# Variance
dental.var=tapply(MEASURE,list(SEX,AGE),var)
# Frequency
dental.n=table(SEX,AGE)


# Boxplots
boxplot(MEASURE~sex_new,xlab="SEX", ylab="MEASURE",col="pink")
boxplot(MEASURE~as.factor(AGE)*sex_new)

# create subsets 
Sex_f=subset(dental,dental$SEX==2)
Sex_m=subset(dental,dental$SEX==1)

# for program FEMALE
boxplot(MEASURE~AGE,data=Sex_f,xlab="Age in years", ylab="MEASURE",main="FEMALE",col="pink")
boxplot(MEASURE[SEX==2]~AGE[SEX==2],main="FEMALE",xlab="Age in years",ylab="MEASURE",col="yellow")

# for program MALE
boxplot(MEASURE~AGE,data=Sex_m,xlab="Age in years", ylab="MEASURE",main="MALE",col="pink")
boxplot(MEASURE[SEX==1]~AGE[SEX==1],main="MALE",xlab="Age in years",ylab="MEASURE",col="yellow")

# plot the trellis graph-LEVEL 1
dental$AGE_new<-dental$AGE-8
attach(dental)
inbed=function(x){
  mod1=lm(MEASURE~AGE_new,data=subset(dental,IDNR==x))
  coef(mod1)
}

cf<-sapply(dental$IDNR, inbed)

Sx=reorder(dental$IDNR, cf[1,])

xyplot(MEASURE~AGE_new|Sx,groups=SEX,data=dental,
       type=c('p','r'),auto.key=T,aspect="xy",
       par.settings=list(axis.text=list(cex=0.6),
                         fontsize=list(text=8,points=10)),
       scales=list(x=list(at=c(0,0.5,1),labels=c("0","0.5","1"))))
# FITTING THE MODEL
dental$AGE_new<-dental$AGE-8
attach(dental)


dental.lmer1=lmer(MEASURE~1+AGE+SEX+AGE:SEX+(1+AGE|IDNR),REML=FALSE,data=dental)
summary(dental.lmer1)

# test the effects
# 1. WALD TEST
confint(dental.lmer1,method="Wald",oldNames=FALSE)
# sex and age:sex
# 2. BOOTSTRAP
confint(dental.lmer1,method="boot",boot.type="perc",oldNames=FALSE)
# sex and age:sex
# 3. PROFILE LIKELIHOOD
confint(dental.lmer1,level=0.95,method="profile",oldNames=FALSE)
# if they contain 0 then not significant--> sex may not be significant and the interction may not be sig


# test the effects of sex on intercept and slope
dental.lmer1.nosex=lmer(MEASURE~1+AGE+(1+AGE|IDNR),REML=FALSE,data=dental)
dental.lmer1.interceptsex=lmer(MEASURE~1+AGE+SEX+(1+AGE|IDNR),REML=FALSE,data=dental)

anova(dental.lmer1.nosex,dental.lmer1,dental.lmer1.interceptsex)

# assess random effects
D.dent=unclass(VarCorr(dental.lmer1))$IDNR
D.dent

# predicting random effects-- getting all the boi and b1i
dental.lmer.re=ranef(dental.lmer1)$IDNR
head(dental.lmer.re)
# plot the random intercept and random slope
plot(dental.lmer.re,main="Random Intecept boi vs random slope b1i")

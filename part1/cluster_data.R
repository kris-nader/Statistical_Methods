##CLUSTER data
# load packages
install.packages("dplyr")
install.packages("lattice")
install.packages("grid")
install.packages("nlme")

library(dplyr)
library(lattice)
library(grid)
library(nlme)
# reading the data
ratpup=read.table(file=file.choose(),header=T)
fix(ratpup)
ratpup$sex1[ratpup$sex=="Female"]=1
ratpup$sex1[ratpup$sex=="Male"]=0
attach(ratpup)

# table descirbing the data
g=function(x)c(N=length(x),Mean=mean(x,na.rm=TRUE),
               SD=sd(x,na.rm=TRUE),Min=min(x,na.rm=TRUE),
               Max=max(x,na.rm = TRUE))
summarize(weight,by=list(treatment,sex),g)
summary(ratpup)

#COmparing the distributions of birth weights
# for each treatment by sex combination

# changing aspect to 1 will give u horizontal plots
bwplot(weight~sex|treatment,data = ratpup,aspect=2,
       ylab="Birth Weights", xlab="sex",col="lightpink",
       main="Boxplots of birth weights for levels of treatment by sex")

# Comparing the distributions of birth weight for each treatment
dotplot(litterid~weight,group=treatment,data=ratpup,
        xlab="Weight",ylab="Litter",
        auto.key=list(space="top",column=3, cex=0.8,title="",
                      cex.title=1,lines=FALSE,points=TRUE))
with(ratpup, interaction.plot(treatment,sex,weight))


# fitting the homocedastic model
meanfull.hom=lme(weight~treatment+sex1+litsize+treatment:sex1,
                 random=~1| litterid, ratpup,method="REML")

# it will choose the lowest level alphabetically or numerically as the lowest
# so we have high, low, control so control is smallest alphabetically
# if you want ot change this
# treatment=relevel(treatment,ref="High")

# random=~1| litterid is the intercept/ radnom effect for each level of the litter model

summary(meanfull.hom)
anova(meanfull.hom)

# test for equal residual varince beween the 2 treatment groups
anova(meanfull.het,meanfull.hilo)
# display random effects of the model
random.effects(meanfull.hom)


# fitting the hetercedastic model
meanfull.het=lme(weight~treatment+sex1+litsize+treatment:sex1,
                 random=~1| litterid, ratpup,method="REML",
                 weights=varIdent(form=~1| treatment))
summary(meanfull.het)

# compare hetero and homo model
anova(meanfull.hom,meanfull.het)
# compare high and low dose for an equal residual variance

ratpup$trtgroup[treatment=="Control"]=1
ratpup$trtgroup[treatment=="Low"| treatment=="High"]=2

meanfull.hilo=lme(weight~treatment+sex1+litsize+treatment:sex1,
                  random=~1|litterid,ratpup,method="REML",
                  weights=varIdent(form=~1| trtgroup))
summary(meanfull.hilo)
anova(meanfull.hilo)
anova(meanfull.het,meanfull.hilo)

# is there a litter effect?
meanfull.hilo.nolitter=gls(weight~treatment+sex1+litsize+treatment:sex1,
                           data=ratpup, weights = varIdent(form=~1 | trtgroup))
summary(meanfull.hilo.nolitter)
summary(meanfull.hilo)
anova(meanfull.hilo.nolitter,meanfull.hilo)

## MODEL THE MEAN STrUCTURE AND THE FINAL MODEL USING ML
meanfull.hilo.ml=lme(weight~treatment+sex1+litsize+treatment:sex1,
                     random=~1| litterid, ratpup, method="ML",
                     weights=varIdent(form=~1| trtgroup))
summary(meanfull.hilo.ml)
anova(meanfull.hilo.ml)

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

## STEP 1: Fit the model using the loaded mean structure

# model 3.1
model3.1.fit=lme(weight~treatment+sex1+litsize+treatment:sex1,
                 random=~1|litterid, data=ratpup, method="REML")
summary(model3.1.fit)
anova(model3.1.fit)
    ## display random effects of the model
random.effects(model3.1.fit)

## STEP 2: select a structure for the random effects 
## test to see if the random effects associated w intercept can be removed

# model 3.1.A
model3.1a.fit=gls(weight~treatment+sex1+litsize+treatment:sex1,data=ratpup)
anova(model3.1.fit,model3.1a.fit)
# we keep random effects but we need to divide p value by 2
# bc h0: random litter effetcs=0 on the boundary of parameter space
# then p value is actually 0.001/2

## STEP 3: select covariance structure for residueals
## testing if varhigh=vvarlow=varcontrol
## should a hetero model(not all equal) or homo model(all equal)

# model 3.2.A
model3.2.a.fit=lme(weight~treatment+sex1+litsize+treatment:sex1,
                   random=~1| litterid, method="REML",
                   weights=varIdent(form=~1 | treatment))
      ## we look at summary
summary(model3.2.a.fit)
anova(model3.1.fit,model3.2.a.fit)
# we choose the model3.2.afit

# can we pool the residual var for high and low

ratpup$trtgroup[treatment=="Control"]=1
ratpup$trtgroup[treatment=="Low"| treatment=="High"]=2

model3.2.b.fit=lme(weight~treatment+sex1+litsize+treatment:sex1,
                   random=~1| litterid, method="REML",
                   weights=varIdent(form=~1 | trtgroup))
anova(model3.2.a.fit,model3.2.b.fit)
# the p value is 0.27 so var are equal and can be pooled


## STEP 4: REDUCE THE MODEL BY REMOVING NON SIG FIXED EFFECTS
anova(model3.2.b.fit)
# we see here that the p value for treatment:sex1 is 0.7 so we can can remove it from the model
# because it is not sig

# now we want to test the fixed effects so we use ML

# model 3.3
model3.3.ml.fit=lme(weight~treatment+sex1+litsize,
                    random=~1 | litterid,method = "ML",
                    weights=varIdent(form=~1 |trtgroup))

model3.3a.ml.fit=lme(weight~sex1+litsize,
                    random=~1 | litterid,method = "ML",
                    weights=varIdent(form=~1 |trtgroup))
anova(model3.3.ml.fit,model3.3a.ml.fit)

# refit using REML to get unbiased estimate for the variance 

# model3.3
model3.3.reml.fit=lme(weight~sex1+litsize+treatment,
                      random=~1 | litterid,method = "REML",
                      weights=varIdent(form=~1 |trtgroup))
summary(model3.3.reml.fit)

anova(model3.3.reml.fit)











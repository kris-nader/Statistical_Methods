##Logistic regression
donner=read.table(file=file.choose(),header=T)
head(donner)


# subset the data
# choose only the ones with complete entries
donner.na<-na.omit(donner,select=c('Age','Outcome','Sex'))
donner.na$fem=as.numeric(donner.na$Sex=="Female")
head(donner.na)


#Fitting a Logistic Regression
donner.log=glm(Outcome~Age+fem,data=donner.na, family=binomial(link="logit"))
summary(donner.log)
# from here we see that age: -0.03 so higher age corresponds to a lower probability of survival
# also fem coef is 1.06 so they have a higher prob of survival

## Odds Ratio this wil be eB0, eB1, eB2
# Odds ratio so: odds of survical knowing female over odds of survival knowing man 
# this is exp(coef of fem)=2.990
exp(donner.log$coefficients)

#confidence interval for parameters
confint(donner.log)

# find the confidnce intervals at the 95% for odds ratio
exp(confint(donner.log))

# combine the true OR to the confidence interval
exp(cbind(OR=donner.log$coefficients,confint(donner.log)))



# estimating the odds ratio for survival after 10 years
# keeping fem constant
exp(donner.log$coefficients*10)

# get confidence interval of this odds ratio
exp(c(OR=donner.log$coefficients[2]*10,confint(donner.log)[2,]*10))

# Plotting the survival probabilities--> logit curve
logit=function(x)log(x/1-x)
ilogit=function(x,a,b)exp(a+b*x)/(1+exp(a+b*x))

# plotting survival for men vs women
cl=coef(donner.log)
cols=c("red","black")
plot(donner.na$Age,jitter(donner.na$Outcome,.2),pch=20,col=cols,
     cex=1.2,xlab="Age",ylab="Status-jittered")


curve(ilogit(cl[1]+cl[2]*x+cl[3]*0,0,1),add=T)
curve(ilogit(cl[1]+cl[2]*x+cl[3]*1,0,1),add=T,col="red")
legend("topright",pch=20,lty="solid",col=c("red","black"),c("women","men"))

#predict the outcome
# for a woman of average age of 20.22
newdata2<-data.frame(fem=1,Age=mean(donner.na$Age))
newdata2$greP<-predict(donner.log,newdata=newdata2,type="response")
newdata2

# for a man of average age of 20.22
newdata3=data.frame(fem=0,Age=mean(donner.na$Age))
newdata3$greP=predict(donner.log,newdata=newdata3,type="response")
newdata3

# for both in the same argument
newdata4=data.frame(fem=c(0,1),Age=mean(donner.na$Age))
newdata4$greP=predict(donner.log,newdata = newdata4,type="response")
newdata4

#### FORMING THE INTERACTION MODEL

m4<-glm(Outcome~Age*fem,data=donner.na,family=binomial(link = "logit"))
summary(m4)

## finding aikaiki weights and comparing models

donner.list=list()
donner.list[[1]]=glm(Outcome~Age,data=donner.na,family = binomial(link="logit"))
donner.list[[2]]=glm(Outcome~fem,data=donner.na,family = binomial(link="logit"))
donner.list[[3]]=glm(Outcome~Age+fem,data=donner.na,family = binomial(link="logit"))
donner.list[[4]]=glm(Outcome~Age*fem,data=donner.na,family = binomial(link="logit"))

donner.modnames=c("Age","sex","Age+sex","age+sex+age*sex")


## AIKIAKI 

install.packages("AICcmodavg")
library(AICcmodavg)

donner.aictab=aictab(cand.set=donner.list,modnames=donner.modnames)
donner.aictab
########################################
#                model average         #
########################################
# for age
modavg(cand.set=donner.list,parm="Age",second.ord=TRUE,
        modnames=donner.modnames,uncond.se="revised",
        exclude=list("Age:fem"),conf.level=0.95,warn=TRUE)
#average effect of age across differnt models is -0.04

# for fem
modavg(cand.set=donner.list,parm="fem",second.ord=TRUE,
       modnames=donner.modnames,uncond.se="revised",
       exclude=list("Age:fem"),conf.level=0.95,warn=TRUE)
# average effect of gender across different models is 1.07

modavg(cand.set=donner.list,parm="Age:fem",second.ord=TRUE,
       modnames=donner.modnames,uncond.se="revised",
       conf.level=0.95,warn=TRUE)

#https://danstich.github.io/stich/classes/BIOL678/06_modelSelection.html



## plottinf for interaction model
x=seq(1,70,0.01)
y=exp(coef(m4)[3]+coef(m4)[4]*x)

plot(x,y,type="n",ylim=c(0.7,4.5),xlab="Age",ylab="Odds ratio",main="odds ratio  for gender")
lines(x,y,lty=1,col="red")
abline(h=1)

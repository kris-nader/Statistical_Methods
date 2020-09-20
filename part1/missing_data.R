##Missing data

#installing packages
install.packages(c("mice","lattic","VIM","aod","BaM"))

#load packages
library(mice)
library(lattic)
library(VIM)
library(aod)
library(BaM)

# read the data
titanic.missing=read.table(file=file.choose(),header=T,sep=",")
head(titanic.missing,10)
titanic.subset=titanic.missing[,c(1,2,3,4,5,11)]
titanic.subset$survived=as.factor(titanic.subset$survived)
titanic.subset$pclass=as.factor(titanic.subset$pclass)

summary(titanic.subset)
# exploring the missingness -VIM
titanic.subset.aggr=aggr(titanic.subset,numbers=TRUE,prop=FALSE,
                          ylab=c("Histogram of the missing data","Pattern"))
titanic.subset.aggr

aggr(titanic.subset, combined=TRUE, numbers=TRUE,prop=TRUE, 
     cex.numbers=0.87, varheight=FALSE)

# amount of missingness in age for eacch survived group
barMiss(titanic.subset[,c("survived","age")])

# amount of missingness in age for each sex group
barMiss(titanic.subset[,c("sex","age")])
histMiss(titanic.subset$pclass,titanic.subset$age)


#fit the logistic model for complete cases
titanic.logistic.omit=glm(survived~pclass+sex+age,family=binomial(link="logit"),data=titanic.subset)
summary(titanic.logistic.omit)

# global effect of class
wald.test(b=coef(titanic.logistic.omit),Sigma=vcov(titanic.logistic.omit),Terms=2:3)

# odds ratio
exp(cbind(OR=titanic.logistic.omit$coefficients,confint(titanic.logistic.omit)))

#######################################
#       multiple imputation           #
#######################################
# studying the patterns of missingness
pattern=md.pattern(titanic.subset)
pattern
# this command will tell you how the variables are missing together
# rr: both observed
# rm: 1st is observed and 2nd is missing
# mr: 1st is missing and 2nd is observed
# mm: 1st and 2nd are missing 
pairs=md.pairs(titanic.subset)    
pairs

# imputing the missing values
imp=mice(titanic.subset,m=5)
#imp

# need to check that the impuations actually make sense and rspect the relation between variables
# we use diagnostic checks
# each row is a missing entry in age
# each column is the multiple imputations
imp$imp$age[1:10,1:5]
head(imp$imp$age)
# then the 1st completed data set 
complete(imp,1)[1:10,]

# do diagnostic check to inpect the distibutions of the original andimputed data
# blue is original and red is imputed
com=complete(imp,"long",inc=T)
col=rep(c("blue","red")[1+as.numeric(is.na(imp$data$age))],101)
stripplot(age~.imp,data=com,jit=TRUE,fac=0.8,col=col,pch=20,cex=1.4,
          xlab="Imputation number")

# columns that dont need to be imputed have ""
# imp= mice(titanic.missing,metho=c("","","logreg","pmm"),m=100)
## analyze the imputed data

fit=with(data=imp,exp=glm(survived~pclass+sex+age,family=binomial))

# create a data set witht the results of all the analysis
MI.matrix=matrix(0,10,5)
for (k in 1:10)
  MI.matrix[k,]=coefficients(fit$analyses[[k]])

MI.results=data.frame(Intercept=MI.matrix[,1],pclass2=MI.matrix[,2],
                     pclass3=MI.matrix[,3],sex=MI.matrix[,4],age=MI.matrix[,5])
MI.results[1:10,]


# combining the results using rubin rules
est=pool(fit)
summary(est)

#the column fmi contains the fraction of missing imfo
#--> the proportion os variabilitt attributed to the uncertainty caused by the missing data





# using inverse probability weighting

# create the missing data indicator r
titanic.missing$r=as.numeric(!is.na(titanic.missing$age))*as.numeric(!is.na(titanic.missing$sex))
head(titanic.missing$r,10)
fix(titanic.missing)


# fit the logistic regression model to calculate the prob
# of being complete

titanic.ipw.glm=glm(r~pclass+survived,family=binomial,data=titanic.missing)
summary(titanic.ipw.glm)

# calculate the weights: inverse probabilities
titanic.missing$w=1/fitted(titanic.ipw.glm)
head(titanic.missing,10)

# now model the full using Y=survived
titanic.results.ipw=glm(survived~pclass+age+sex,data=titanic.missing,weight=titanic.missing$w,
                        family=binomial)
summary(titanic.results.ipw)






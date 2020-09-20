boston<-read.table(file=file.choose(),header=TRUE)
#Exercise 1
names(boston)
t_district<-as.factor(boston$internatfact)
reg1<-lm(rm~t_district,data=boston,contrasts = list(t_district="contr.sum"))
summary(reg1)
#check within group variance
library(car)
#h0 equal var h1: not equal var
leveneTest(boston$rm~t_district)
#pvalue<0.05 then we reject h0 in favor of h1
#rule of the thumb
tapply(boston$rm,t_district,var)
0.52798253/0.07994635
boxplot(boston$rm~t_district)
# less than 5 so we cannot assume homogenity of variance
aov1=oneway.test(boston$rm~ t_district,var.equal = FALSE)
# check the normality of residuals
shapiro.test(reg1$residuals)
# pvalue<0.05 reject null not normally dis
hist(reg1$residuals) # assume normal distrubution
# influential points
plot(cooks.distance(reg1))
# not influential points
#multiple comparison of districts
polly.aov1<-aov(boston$rm~t_district)
diffs<-TukeyHSD(polly.aov1,which="t_district")
diffs
install.packages("agricolae")
library(agricolae)
d<-scheffe.test(polly.aov1,"t_district",group=TRUE)
plot(d)
kruskal.test(boston$rm~t_district)
library("pgirmess")
kruskalmc(boston$rm,t_district)


#Exercise 2
attach(boston)
k<-log(dis)
reg3=lm(log(crim)~dis+k)
summary(reg3)
newdata=data.frame(dis=c(3.123),k=c(1.138794))
res.pred1<-predict(reg3,newdata,interval="confidence")
res.pred2<-predict(reg3,newdata,interval="prediction")


#Exercise 4
passing<-subset(boston,boston$chas=="1")
not_passing<-subset(boston,boston$chas=="0")
NROW(passing)
NROW(not_passing)
var.test(passing$crim,not_passing$crim)
t.test(passing$crim,not_passing$crim,var.equal = FALSE)

fix(passing)

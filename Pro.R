#******install packages dplyr and ggplot2********

library(ggplot2)
library(dplyr)
set.seed(123)


rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }

r <- rnorm2(200,20,1)
x<- rnorm2(200,21,1.58)
mice<-data.frame("Before"=r,
              "After"=x)
mice
str(mice)
rats<-data.frame("Before"=rweibull(200, shape=10,scale=20),
                 "After"= rweibull(200, shape=9,scale=21))
rats
str(rats)

qplot(Before,data=mice,geom="density",main=" Qplot mice  ")
qplot(After,data=mice,geom="density",main=" Qplot mice ")
qplot(Before,data=rats,geom="density",main="Qplot rats")
qplot(After,data=rats,geom="density",main="Qplot rats")
qplot(x=Before,y=After,data=mice,geom="boxplot",main="MICE DATA")   
qplot(x=Before,y=After,data=rats,geom="boxplot",main="RATS DATA")  

####TASK 2
a<-mice$Before
b<-mice$After
c<-rats$Before
d<-rats$After
qqplot(mice$Before, mice$After, xlab = "before", ylab = "after", main = "mice Q-Q Plot")
abline(lm(sort(b) ~ sort(a)), col = "steelblue", lwd = 2)

qqplot(rats$Before, rats$After, xlab = "before", ylab = "after", main = "rats Q-Q Plot")
abline(lm(sort(d) ~ sort(c)), col = "steelblue", lwd = 2)
dplyr::sample_n(mice, 10)
shapiro.test(mice$Before)
shapiro.test(mice$After)
dplyr::sample_n(rats, 10)
shapiro.test(rats$Before)
shapiro.test(rats$After)

#Task 3

res<-t.test(mice$Before,mice$After,paired=TRUE)
res

res$statistic
res$p.value
res$conf.int
res$estimate
res$parameter


res1 <- wilcox.test(rats$Before, rats$After, paired = TRUE)
res1
res1$statistic
res1$p.value





####Task 4
# ***** install package fitdistrplus"
library(fitdistrplus)
f11<-fitdist(rats$Before,"lnorm")
f11
f12<-fitdist(rats$After,"lnorm")
f12
f21<-fitdist(rats$Before,"gamma")
f21
f22<-fitdist(rats$After,"gamma")
f22
f31<-fitdist(rats$Before,"weibull")
f31
f32<-fitdist(rats$After,"weibull")
f32
summary(f32)
summary(f12)
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(f31, f11, f21), legendtext = plot.legend)
qqcomp(list(f31, f11, f21), legendtext = plot.legend)
cdfcomp(list(f31, f11, f21), legendtext = plot.legend)
ppcomp(list(f31, f11, f21), legendtext = plot.legend)


denscomp(list(f32, f12, f22), legendtext = plot.legend)
qqcomp(list(f32, f12, f22), legendtext = plot.legend)
cdfcomp(list(f32, f12, f22), legendtext = plot.legend)
ppcomp(list(f32, f12, f22), legendtext = plot.legend)





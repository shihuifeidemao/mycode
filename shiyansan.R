install.packages('BSDA')
install.packages('lattice')
install.packages('UsingR')
library(BSDA)
library(UsingR)
x <-rnorm(10000,12,4.1)
z.test(x,sigma.x=4.1)
simple.z.test(x, 4.1, conf.level=0.95)
#########################################
prop.test(9,30)
#########################################
two.sample.ci<-function(x,y,conf.level=0.95){
  options(digits=4) 
  m = length(x); n = length(y)
  xbar=mean(x)-mean(y); alpha = 1 - conf.level
  vp = ((m-1)*var(x) + (n-1)*var(y)) / (m+n-2)
  tstar= qt(alpha/2,m+n-2) * (vp/m+vp/n)^(1/2)
  xbar +c(-tstar, +tstar)
}
x1=c(628,583,510,554,612,523,530,615)
x2=c(535,433,398,470,567,480,498,560,503,426)
two.sample.ci(x1,x2)

t.test(x1,x2,var.equal = T)
#########################################
x=c(20.5,19.8,19.7,20.4,20.1,20.0,19.0,19.9)
y=c(20.7,19.8,19.5,20.8,20.4,19.6,20.2)
two.sample.var<-function(x,y,conf.level=0.95){
  options(digits=4)
  m = length(x); n = length(y)
  s12=(sum(x^2)-sum(x)^2/length(x))/(length(x)-1)
  s22=(sum(x^2)-sum(x)^2/length(x))/(length(x)-1)
  p1 <- 0.95/2
  F1 <- qf(p1, 7, 6, lower.tail = F)
  p2 <- 0.05/2
  F2<- qf(p2, 7, 6, lower.tail = F)
  c(s12/s22/F1,s12/s22/F2)
}
two.sample.var(x,y)

var.test(x,y)
########################################
X<-c(20.5,19.8,19.7,20.4,20.1,20.0,19.0,19.9)
Y<-c(20.7,19.8,19.5,20.8,20.4,19.6,20.2)
t.test(X,Y,var.equal = TRUE,alternative = "less")
##################################################
x1=c(20.5,19.8,19.7,20.4,20.1,20.0,19.0,19.9)
x2=c(20.7,19.8,19.5,20.8,20.4,19.6,20.2)
x = c(x1,x2)
account = data.frame(x,A=factor(rep(1:2,c(8,7))))
a.aov=aov(x~A,data=account)
summary(a.aov)



data<- read.xlsx("fruit",1);head(data)[,1:6]
str(data)

# 输入数据
x_bar <- 12    # 样本均值
s <- 4.1      # 样本标准差
n <- 16        # 样本大小
conf_level <- 0.95    # 置信水平

# 计算置信区间
z.test(x = x_bar, sigma = s, n = n, conf.level = conf_level)


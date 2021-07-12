####### 9주차 #######
# 다섯수치요약
setwd('C:\\Users\\cwryu\\OneDrive\\Desktop\\대학\\응통\\탐색적자료분석\\실습데이터\\')
exam1 = read.table('exam1.txt', header=T)
attach(exam1)
summary(score)
quantile(score, c(1/4, 1/2, 3/4), type=8)
skew = ((58-48)-(48-32))/((58-48)+(48-32)); skew
# 분위수
score.sort = sort(score)
q.5 = score.sort[33]
q.25 = (1/3) * score.sort[16] + (2/3)*score.sort[17]
q.125 = (1/2) * score.sort[8] + (1/2)*score.sort[9]
q.0625 = (7/12) * score.sort[4] + (5/12)*score.sort[5]
c(q.5, q.25, q.125, q.0625)
quantile(score, c(1/2, 1/4, 1/8, 1/16), type=8)

qL = quantile(score, c(1/2, 1/4, 1/8, 1/16), type=8);qL
qU = quantile(score, c(1/2, 3/4, 7/8, 15/16), type=8);qU
end = quantile(score, c(0,1), type=8); end
mid = (qL + qU) / 2 ; mid
spr = qU - qL; spr
endmid = (end[2]+end[1]) / 2; endmid
endspr = end[2] - end[1]; endspr

sn = rnorm(10000, 0, 1)
qL = quantile(sn, c(1/2, 1/4, 1/8, 1/16), type=8); qL
qU = quantile(sn, c(1/2, 3/4, 7/8, 15/16), type=8); qU
end = quantile(sn, c(0,1), type=8); end
mid = (qL + qU) / 2 ; mid
spr = qU - qL; spr
ku1 = spr[3]/spr[2] - 1.704; ku1
ku2 = spr[4]/spr[2] - 2.274; ku2
# 여러 묶음 수치자료의 비교
구인구 = read.csv('광역시-구 인구.csv', header=T)
str(구인구)
attach(구인구)
boxplot(인구 ~ 지역명)
시 = reorder(지역명, 지역코드)
x11()
boxplot(인구 ~ 시)

install.packages('UsingR')
library(UsingR)
str(Cars93)
attach(Cars93)
boxplot(Max.Price ~ Origin)
x11()
boxplot(Max.Price ~ AirBags)

####### 10주차 #######
# 멱승, 로그, 지수 변환에 의한 재표현
income = c(880, 1511, 1944, 2350, 2738, 3135, 3609, 4170, 5068, 7695)
par(mfrow=c(1, 2))
plot(income); plot(log(income))
hist(income); hist(log(income))

x = rnorm(1000, 1, 0.15)
par(mfrow=c(1, 2))
hist(x); hist(2*x+100)
par(mfrow=c(1, 2))
hist(x^2); hist(x^4)
par(mfrow=c(1, 2))
hist(sqrt(x)); hist(x^(1/4))

par(mfrow=c(1, 2))
hist(log(x)); hist(exp(x))
# 재표현의 목적
x1 = rgamma(100, 4); g1=rep('A',100)
x2 = rgamma(100, 5); g2=rep('B',100)
x3 = rgamma(100, 9); g3=rep('C',100)
clame = c(x1, x2, x3)
group = factor(c(g1, g2, g3))
par(mfrow=c(1,1))
boxplot(clame ~ group)

par(mfrow=c(1,2))
boxplot(sqrt(clame)~group, ylab='sqrt')
boxplot(log(clame)~group, ylab='log')

par(mfrow=c(1,2))
x=rnorm(100, 5, 1.5)
y1 = 0.1*x^2 + 0.2*rnorm(100,0,1)
plot(y1~x, xlim=c(0,10), ylim=c(0,10))
y2 = 10*sqrt(x/10) + 0.2*rnorm(100,0,1)
plot(y2~x, xlim=c(0,10), ylim=c(0,10))

par(mfrow=c(1,2))
plot(sqrt(y1)~x, xlim=c(0,10))
plot((y2)^2~x, xlim=c(0,10))

par(mfrow=c(1,2))
x1 = x^2
plot(y1~x1)
x2 = sqrt(x)
plot(y2~x2)
# 재표현의 활용사례
library(MASS)
data("Animals"); str(Animals)
attach(Animals)
par(mfrow=c(1,2))
plot(brain~body, xlim=c(0,100000))
plot(log(brain)~log(body))

m0 = lm(log(brain)~log(body));m0
plot(log(brain)~log(body))
abline(m0$coef, lty='dotted')
m1 = rlm(log(brain)~log(body));m1
r = m1$resid
plot(log(brain)~log(body))
abline(m1$coef, lty='dotted')

names = rownames(Animals)
names1 = names[order(r, decreasing = T)]
r1 = r[order(r, decreasing = T)]
data.frame(names1, round(r1,2))
par(mfrow=c(1,1))
plot(28:1~r1, xlim=c(-5, 5))
text(y=28:1, x=r1, label=names1, pos=4,
     adj=0.2, cex=0.9)
# 표준화 변환
x = rnorm(1000, 40, 10)
z.x = (x-mean(x))/sd(x)
zz.x = (x-median(x))/(IQR(x)/1.35)
par(mfrow=c(1,2))
hist(z.x); hist(zz.x)

####### 11주차 #######
# 이산형 확률분포
rbinom(25, 1, 0.5); rbinom(25, 10, 0.5)
mean(rbinom(25, 10, 0.5))
dbinom(2,4,0.2); pbinom(2,4,0.2)
dgeom(3,0.2)
dnbinom(2,2,0.2)
dpois(5,6)
dhyper(2,4,3,3)
dnbinom()

####### 12주차 #######
# 정규확률 플롯
x = seq(-3, 3, 0.01)
y = dnorm(x)
par(mfrow=c(1,3))
plot(y~x, type='l', ylim=c(0,0.5),
     ylab='density')
darwin=c(49,-67,8,16,6,23,28,41,14,
         29,56,24,75,60,-48)
p = (1:length(darwin)-0.5)/length(darwin); p
z = qnorm(p); z
plot(sort(darwin)~z, ylim=c(-75,75),
     xlim=c(-2,2), main='Darwin')
qqnorm(darwin, ylim=c(-75,75), xlim=c(-2,2))
# 각종 임의의 수와 모의생성 방법
rnorm(10)
rnorm(10, 5, 2.5)
pnorm(-1.96)
pnorm(1.96)
qnorm(0.025)
x=rexp(10000)
x=rexp(10000, 2)
mean(x)
x=rexp(10000)
x.p=x[x>1]-1
mean(x.p)
# 정규확률 플롯의 여러 패턴
par(mfrow=c(1,2))
qqnorm(rnorm(40,100,15))
qqnorm(c(rnorm(20,70,15), rnorm(20,130,15))) # 혼합
qqnorm(c(25,175,rnorm(38,100,15))) # 특이값
qqnorm(runif(40, 80, 120))
qqnorm(c(rexp(20), -rexp(20)))
qqnorm(exp(rnorm(40,5,1)))
qqnorm(1500-exp(rnorm(40,5,1)))
# 지수분포와 와이블분포로부터의 확률 플롯
par(mfrow=c(1,1))
leukemia = c(1,1,2,2,3,4,4,5,5,8,8,8,8,
             11,11,12,12,15,17,22,23)
n = length(leukemia); n
p = seq(1:n)/n - 0.5/n
x = -log(1-p)
y = leukemia
plot(y~x, main='Q-Q Plot for exponential dist')
library(lattice)
qqmath(~leukemia, distribution = function(p)qexp(p,1))
qqmath(~leukemia, distribution = function(p)qweibull(p,1.5,1))
qqmath(~leukemia, distribution = function(p)qnorm(p,mean(leukemia), sd(leukemia)))
library(MASS)
fitdistr(leukemia, 'weibull')
fitdistr(leukemia, 'exponential')
fitdistr(rnorm(40,100,15), 'normal')

####### 13주차 #######
# 2원 빈도표 분석
install.packages('vcd'); library(vcd)
attach(Arthritis); head(Arthritis)
str(Arthritis)
my_table0 = table(Improved); my_table0
summary(Improved)
my_table1 = with(Arthritis, table(Improved))
my_table1
xtabs(~Improved, data=Arthritis)
prop.table(my_table1)
options('digits'); options('digits'=2)
prop.table(my_table1)
# 교차표
table(Treatment, Improved)
my_table2 = with(Arthritis, table(Treatment, Improved))
my_table2
my_table3 = xtabs(~Treatment+Improved, data=Arthritis)
my_table4 = xtabs(~Improved+Treatment, data=Arthritis)
my_table3; my_table4
margin.table(my_table3,1)
margin.table(my_table3,2)
margin.table(my_table3)
prop.table(margin.table(my_table3,1))
prop.table(margin.table(my_table3,2))
prop.table(my_table3,1)
prop.table(my_table3,2)

addmargins(my_table3)
addmargins(xtabs(~Treatment+Improved, data=Arthritis))
addmargins(prop.table(my_table3))
addmargins(prop.table(my_table3),1)
addmargins(prop.table(my_table3),2)
install.packages('gmodels')
library(gmodels)
CrossTable(Treatment, Improved)
# 교차표 그림
barplot(my_table3)
barplot(my_table3, legend=rownames(my_table3))
x11()
barplot(my_table4, legend=rownames(my_table4))
mosaic(~Treatment+Improved, data=Arthritis, color=T)
x11()
mosaic(~Treatment+Improved, data=Arthritis, gp=gpar(fill=c('green', 'red')))
mosaicplot(~Treatment+Improved, data=Arthritis, color=T)
x11()
mosaicplot(~Improved+Treatment, data=Arthritis, color=T)
mosaicplot(~Improved+Treatment+Sex, data=Arthritis, color=T)

####### 14주차 #######
# 2원 자료 표의 분해
twoway=read.table('twoway.txt', header=T)
twoway
medpolish(twoway)
world.temp = read.table('WorldTemperature_Mean.txt', header=T)
twoway.w = medpolish(world.temp); twoway.w
attach(twoway.w)
comparison = matrix(row, ncol=1)%*%matrix(col, nrow=1)/overall
plot(residuals~comparison, xlim=c(-15,15), ylim=c(-15,15))
boxplot(residuals, ylab='residuals')
round(residuals[order(row),],1)
# 가구 소비지출 사례
consumption = read.table('household.txt', header=T)
twoway.out = medpolish(consumption)
attach(twoway.out)
comparison = matrix(row, ncol=1)%*%matrix(col, nrow=1)/overall
plot(residuals~comparison)
twoway.log.out = medpolish(log(consumption))
attach(twoway.log.out)
comparison = matrix(row, ncol=1)%*%matrix(col, nrow=1)/overall
plot(residuals~comparison)

####### 15주차 #######
# 시계열 평활
t = seq(1:60)
s = sin(2*pi*t/12) + sin(2*pi*t/30)
a = rnorm(60,0,1)
x = s + a
plot(x, type='l', ylab='', ylim=c(-3,3),
     lty='dotted');
plot(s, type='l', ylim=c(-3,3),col='blue')
par(new=T)
plot(x,type='l',ylab='',ylim=c(-3,3),
     lty='dotted')
smooth.f = smooth(x);
plot(smooth.f, type='l', ylab='',
     ylim=c(-3,3), col='blue')
par(new=T)
plot(x, type='l', ylim=c(-3,3), lty='dotted')
# 평활방법
x=c(1.5,19.3,-0.8,27.7,-4.3,-11.2,-
      13.6,-13.5,-25.2,-17.7,-27.2,-18.3)
x3 = smooth(x, kind='3');x3
x3r = smooth(x, kind='3R');x3r
x=c(2.1,9.8,19.5,22.5,16.6,16.1,18.5,
    -3.4,8.9,-25.2,-14.0,-0.4)
x3=smooth(x, kind="3"); x3
x3r=smooth(x3, kind="3R"); x3r
xs=smooth(x3r, kind="S"); xs
xxx=smooth(x, kind="3RS3R");xxx
x55=runmed(x,5); x55














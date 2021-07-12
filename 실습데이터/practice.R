############# 7주차 복습 ##############
getwd()
setwd("C:\\Users\\cwryu\\Downloads\\실습데이터\\")
getwd()

# 패키지
library()
install.packages("readxl")
library(readxl)

# 외부data파일 읽기
geyser = read.table("geyser299.txt", header=T)
iris = read.csv("iris.csv", header=T)
world95 = readxl::read_excel("World95.xlsx") # readxl:: 안쳐도 됨

# 확인하자
str(geyser)
str(iris)
str(world95)
summary(geyser)

## 외부출력
# data 외부로
write.table(iris, "iris.txt")
write.table(iris, "iris2.txt", sep="\t")
write.csv(iris, "iris22.csv")

# R 결과물을 txt로 저장
attach(geyser)
sink("output7.txt")
    summary(waiting)
    cor(waiting, duration)
sink()

# 작업공간 저장
save.image()
q()
load(".RData")

# 기타, 기술통계함수
plot(waiting, duration)
geyser[1:5,]
geyser[,2] # duration
geyser[1:5, 2]
summary(iris)
mean(); median; sd(); sum();
table(); plot; cov(); cor();
quantile()

## 자료형

# 벡터
1:3
x = c(1,2,3)
seq(1,3)
seq(1,10,by=2)
rep(c(1,2,3), times=2)
rep(c(1,2,3), each=2)
is.vector(x)
as.vector(x)

# 행렬
x = 1:12
matrix(x, nrow=3)
matrix(x, nrow=3, byrow=T)

# 요인
str(iris)
table(iris$Species)
iris$Species.f = factor(iris$Species, 
                        level=c("setosa", "versicolor", "virginica")) 
str(iris)

# dataframe
name = c('철수', '영희', '길동')
age = c(21, 20, 31)
gender = factor(c('M', "F", 'M'))
ch = data.frame(name, age, gender)
str(ch)

# data 변환 - 변수계산, 코딩변경
# 새로운 변수 생성
# 변수계산
air = airquality
attach(air)
air$Tot = Wind + Temp
sqrt() ; log(); 1/Temp

# 코딩변경
air$type[Temp>=mean(Temp)] = 'high'
air$type[Temp<mean(Temp)] = 'low'
# chr는 factor로 바꾸는게 좋음
air$type = factor(air$type, level=c('high', 'low')) 
str(air)

# 정렬
air[order(Temp),]
air2 = air[order(Temp, -Wind),] # Temp는 오름차순, Wind는 내림차순

# 데이터선택
air_Temp_high = air[which(Temp >= mean(Temp)),]  # Temp high인거만
subset(air, select=type, subset=(Temp>=mean(Temp))) #high만 
air[sample(1:nrow(air), 10),]

# 연산자
# 산술 > 비교 > 논리 > 배정
3**2+2

# 반복문 조건문
#while(); for()
fac.x = 1; i = 1
while(i<=5){
  fac.x <- fac.x * i
  cat(i, '!=', fac.x, '\n', sep='')
  i = i + 1
}

for(j in 2:9){
  cat('===', j, '단===\n', sep=' ')
  for (i in 1:9){
    y = j * i
    cat(j, '*', i, '=', y, "\n", sep=' ')
  }
}

# 사용자 정의 함수
sk_ku = function(){
   
}
sk_ku(air$Temp)

rt = function(a, b, c){
  D = b^2 - 4*a*c
  if (D>0 & a!=0){
    roots = c((-b+sqrt(D))/(2*a), (-b-sqrt(D))/(2*a))
  }
  else if (D==0){
    roots = -b/(2*a)
  }
  else if (D>0 & a==0){
    roots = -c/b
  }
  else{
    roots = c('No Root')
  }
  return(roots)
}

rt(1,2,3); rt(1,4,4);
rt(1,4,3); rt(0,1,2)

# 병합
# cbind, rbind, data.frame

# 줄기와 잎
attach(air)
stem(Temp)
stem(Temp, scale=0.5)
stem(Temp, scale=2)
hist(Temp, nclass = 9, right=T)

# 다섯 수치 요약
quantile(Temp, c(1/4, 1/2, 3/4), type=8)
quantile(Temp, c(1/8, 7/8), type=8)
boxplot(Temp)



























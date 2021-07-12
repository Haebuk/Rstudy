setwd('C:\\Users\\cwryu\\Downloads\\실습데이터')
install.packages('fBasics')
library(fBasics)
sk_ku = function(x){
  n = length(x)
  mean = mean(x)
  std = sd(x)
  
  sk = n/((n-1)*(n-2))*sum(((x-mean)/std)^3)
  ku = (n*(n+1))/((n-1)*(n-2)*(n-3))*sum(((x-mean)/std)^4) - 3*(n-1)^2/((n-2)*(n-3))
  
  return(c(sk, ku))
}

sk_ku(airquality$Temp)
skewness(airquality$Temp)
kurtosis(airquality$Temp)

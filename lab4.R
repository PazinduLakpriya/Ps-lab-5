#1
attendance<-scan()
salary<-scan()
years<-scan()
data<-data.frame(attendance,salary,years)
attach(data)
##this is un useful fix(data)
#2.a
boxplot(attendance,main="Boxplot for attendance",ylab="Attendance")
boxplot(salary,main="boxplot for salary",ylab="salary")
boxplot(years,main="boxplot for years",ylab="years",outpch=8)
hist(attendance,main="Boxplot for attendance",ylab="Attendance")
hist(salary,main="boxplot for salary",ylab="salary")
hist(years,main="boxplot for years",ylab="years")
stem(attendance)
stem(salary)
stem(years)
#2.b
mean(attendance)
summary(attendance)
#2.c
sapply(data,sd)
#2.d
sapply(data,IQR)
#3.
#function
get.mode<-function(x){
  counts<-table(x)
  names(counts[counts==max(counts)])
}
get.mode(years)

table(years)

find.outliers<-function(x){
  q1<-quantile(x)[2]
  q3<-quantile(x)[4]
  
  iqr<-q3-q1
   
  ub<-q3+1.5*iqr
  lb<-q1-1.5*iqr
  
  print(paste("upper bound=",ub))
  print(paste("lower bound=",lb))
  
  print(paste("outliers: ",paste(sort(x[x<lb|x>ub]),collapse=",")))
}
find.outliers(years)
quantile(years)
quantile(years)[2]
quantile(years)[4]

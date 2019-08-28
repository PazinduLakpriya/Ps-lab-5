#import data table
data<-read.table("C:/Users/IT18194654/Downloads/Data.txt",header=TRUE,sep=",")
#view darta set
fix(data)
#"c" means we are going to pass more value
names(data)<-c("x1","x2")
attach(data)
fix(data)
#draw histogram
hist(x2,main="histogram for number of shareholders")
#how to find class gap (270-130)/7=20
#130-150,150-170,150-170,170-190,190-210,210-230,230-250
#length = num of classes+1(7+1)=8
histogram = hist(x2,main="Histogram of shareholders",breaks = seq(130,270,length=8),right = FALSE)
x2
#right = false mean we are not going to add margin numbers to x2
#Whenever you have a limited number of different values in R,
#you can get a quick summary of the data by calculating a frequency table.
#A frequency table is a table that represents the number of occurrences of 
#every unique value in the variable

#Q3
breaks<-round(histogram$breaks)
freq<-histogram$counts
mids<-histogram$mids

classes <- c()
for(i in 1:length(breaks)-1){
  classes[i] <- paste("[",breaks[i],",",breaks[i+1],")")
}
#cbind = combind values
cbind(clasess=classes,frequency=freq)

#Q4
#add freequency polygon to the same plot
lines(mids,freq)
#c means creating a vector
plot(mids,freq,type = 'l',main = "frequency polygon for shareholders",xlab = "shareholders",ylab = "freequency",ylim = c(0,max(freq)))
#part 5
cum.freq<-cumsum(freq)
new<-c()
for(i in 1:length(breaks)){
  if(i==1){
    new[i]=cum.freq[i-1]
  }
}
plot(breaks,new,type='l',main = "frequncy polygon for shareholder",xlab = "shareholders",ylab = "cumalative frequency",ylim = c(0,max(cum.freq)))
cbind(upper = breaks,cumfreq = new)





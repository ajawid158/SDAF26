#Fall 26/ITC 255
#Descriptive methods of data on one variable
#1. FDT

#Upload the data data set tips

dfTips=read.csv("tips.csv")
View(dfTips)

#FDT of a QL var
dim(dfTips)
names(dfTips)
head(dfTips)
#smoker Distribution

AbsFreq=table(dfTips$smoker)
AbsFreq
prop.table(AbsFreq)    #Abs. Freq
RelFreq=round(prop.table(AbsFreq), 2)
RelFreq

CumFreq=cumsum(RelFreq)
CumFreq

FDTSmoker=cbind(AbsFreq, RelFreq, CumFreq)
FDTSmoker

#write a function that creates and FDT of a QL var


FDTQL=function(x){
  ABSFreq=table(x)
  RELFreq=round(prop.table(ABSFreq),2)
  CUMFreq=cumsum(RELFreq)
  FDTx=cbind(ABSFreq, RELFreq, CUMFreq)
  return(FDTx)
}

FDTQL(dfTips$smoker)

FDTQL(dfTips$sex)
FDTQL(dfTips$day)

##Construction FDT of a Quant variable 
#Loops and conditional functions work in R
#1. Transform the variable into a categorical var based a definition/we specify them

#Lets use the variable tips

summary(dfTips$tip)
head(dfTips)
#define catgories: small tip<3 [0,3) meduim when tip is 3>= [3,7) but less than 7, 
#large otherwise when tip is 7 or more than 7 USD [7, 10]

#selection + Loop
catTips=c()  #create an empty vector

for (k in 1:length(dfTips$tip)) {
  if(dfTips$tip[k]<3){
    catTips[k]="AsmallTip"
  } else if (dfTips$tip[k] >=3 & dfTips$tip[k]<7) {
    catTips[k]="BmeduimTip"
  } else {
    catTips[k]="Clargetip"
  }
}

head(catTips)
dfnew=cbind(dfTips, catTips)
View(dfnew)
head(dfTips$tip)
#apply the function for FDT of QL
FDTQL(catTips)

#2. Graphs 
#Categorical vars (pie and bar)

#create the FDT 
FDTQL(dfTips$smoker)[,2]

fdtSmoker=FDTQL(dfTips$smoker)[,2]
fdtSmoker

#pie chart
pie(fdtSmoker, 
    col = rainbow(2), 
    main = 'Smoker Distribution')

barplot(fdtSmoker, 
        col=rainbow(2), 
        main = 'Smoker distribution')

fdttip=FDTQL(catTips)[,2]
fdttip

#bar chart
barplot(fdttip, 
        col=rainbow(3), 
        main = 'Tip distribution')

#histogram
head(dfTips)

hist(dfTips$tip, 
     col='blue', 
     main = 'Tips distibution')

#density plot
plot(density(dfTips$tip), 
     col='#0033FF', 
     main='Tips distribution')


plot(density(dfTips$total_bill), 
     col='#0033FF', 
     main='Total Bill distribution')


#Numerical methods
#Center of distribution (mean, median, mode)


#check the distribution
plot(density(dfTips$tip))

#Locate the center....why is it important to locate the center
#different approaches
mean(dfTips$tip)  #
median(dfTips$tip)   #as a midpoint

mymode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mymode(dfTips$tip)

plot(density(dfTips$tip))

#Other locations (quantiles)
quantile(dfTips$tip)    #quartiles
quantile(dfTips$tip, 0.9)

##ECDF Emperical Cummulative Distribution Function

plot(ecdf(dfTips$tip), 
     col='blue', 
     main='ECDF of Tip', 
     xlab='tip')
abline(v=3.9, col='red', lty=3)
abline(h=0.8, col='darkgreen', lty=3)


ecdf(dfTips$tip)(7)
#83% paid 4 d or less as tip

#quantile and ecdf are inverse of one another
quantile(dfTips$tip, 0.8) #we have the percentage...look for the value

ecdf(dfTips$tip)(5)    #we have the value ...look for the percentage

# 80% paid 4 or less as tip, 20% paid more than 4 USD

#Outliers

boxplot(dfTips$tip,
        horizontal = T,
        col='#0033FF')

#outliers affect the location of the center dispropotionaly 
boxplot.stats(dfTips$tip)

#remove the outliers

tipNew=dfTips$tip[dfTips$tip<5.8]

boxplot(tipNew, horizontal = T)
mean(tipNew)
median(tipNew)
mymode(tipNew)


plot(density(tipNew))
#Variation
range(dfTips$tip)
sd(dfTips$tip)   
var(dfTips$tip)   #center means the mean
mad(dfTips$tip)   #Abs. value instead of square root
sd(tipNew)

plot(density(dfTips$tip))
#Next:Data Manipulation dplyr package
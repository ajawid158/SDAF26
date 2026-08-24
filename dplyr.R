#ITC 255 
#data manipulation with dplyr package
#install.packages()
library(dplyr)
##a set of functions that perform certain actions

dfTips=read.csv('tips.csv')
head(dfTips)
View(dfTips)
names(dfTips)
dim(dfTips)


#filter rows
fcust=filter(dfTips, sex=='Female')
dim(fcust)
View(fcust)
mean(fcust$tip)

#filter(dfTips, sex !='Female')
head(fcust)
dim(fcust)


fNonS=filter(dfTips, sex=="Female", smoker=='No', tip<5)
head(fNonS)
dim(fNonS)
View(fNonS)
median(fNonS$tip)

#logical operators &, |, ! AND OR NOT
unique(dfTips$day)

weekend=filter(dfTips, day=='Sun'| day=='Sat')  #day==Sat | Sun

head(weekend)
View(weekend)
dim(weekend)
nrow(dfTips)

dfw=filter(dfTips, day=='Sun' | day=='Fri')
head(dfw)
#weekend and female
wkEndF=filter(dfTips, (day=='Sun'|day=='Sat') & sex=='Female')
head(wkEndF)
dim(wkEndF)
View(wkEndF)
#Weekdays 
#TASK 1: filter those customers who visited not on a Weekend 
#and are Male and Paid a tip of more than x USD. 
#Specify x yourself PEN and PAPER Note 1=<tip<=10

unique(dfTips$day)

wkDays=filter(dfTips, day!='Sun' &  day !='Sat')
head(wkDays)
View(wkDays)

#use the function %in%
unique(dfTips$size)

LS=filter(dfTips, size %in% c(5,6))
head(LS)
WKELS=filter(dfTips, day %in% c('Sun','Sat') | size %in% c(5,6))
head(WKELS)
View(WKELS)

names(dfTips)
#for numerical >, <, ==
STB=filter(dfTips, size<4 & total_bill>8)
View(STB)
x1=filter(dfTips, tip>=6)
x1
#TASK 2> filter those customers who visited on Fri, 
#paid a tip>x USD, their total Bill is smaller or equal to y USD 
#the size is smaller than 4 items. Specify x and y yourself

#Arrange
names(dfTips)
View(dfTips)
head(dfTips, 10)

head(arrange(dfTips, -desc(tip)))
head(arrange(dfTips, desc(tip)))
head(arrange(dfTips, desc(dfTips$size)))   #decode the values F=1 Male 0

View(dfTips)
#select a subset based on column
names(dfTips)
xN=select(dfTips, c(total_bill, tip, size))
head(xN)
head(dfTips)

x1=select(dfTips, size, day, everything())  #var size is put at the beginning
head(x1)

head(dfTips)
dfTips1=select(dfTips, tip:day)
head(dfTips1)



head(select(dfTips, tip:smoker))
head(select(dfTips, -(tip:smoker)))

x2=select(dfTips, tip:smoker)
head(x2)
View(dfTips)


#rename
names(dfTips)

dfTips2=rename(dfTips, Bill=total_bill)
head(dfTips2)


#mutate
head(dfTips)
#total spending=total_bill+tip
dfTips3=mutate(dfTips, totalSpending=total_bill+tip)
View(dfTips3)
dfTips3=mutate(dfTips3, tax=total_bill*0.2)
View(dfTips3)
#summarize for numerical vars
summarise(dfTips, mean(total_bill), sd(total_bill),  mean(tip), sd(tip))

#based on another variable
GG=group_by(dfTips, sex)

summarise(GG, mean(total_bill), sd(total_bill))

smoker=group_by(dfTips, smoker)
summarise(smoker, mean(total_bill), sd(total_bill))


day=group_by(dfTips3, day)
summarise(day, mean(totalSpending), median (totalSpending), sd(totalSpending))
names(dfTips)

names(dfTips3)
#pull a column as a vector 
smk = pull(dfTips, smoker)

head(smk)

#sample_n works with rows
dim(dfTips)
sampledfTips=sample_n(dfTips, 100)
head(sampledfTips)
View(sampledfTips)
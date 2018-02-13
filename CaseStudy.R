library(forecast)
library(xts)     

train <- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/train.csv")
stores <- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/stores.csv")
features <- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/features.csv")

train$IsHoliday<-as.integer(as.logical(train$IsHoliday))
features$IsHoliday<-as.integer(as.logical(features$IsHoliday))

View(train)
View(features)


myfulldata <- merge(features, stores)
myfulldata[,5:9][is.na(myfulldata[,5:9])==TRUE]<- 0

View(myfulldata)

finaldata <- merge(myfulldata , train)
View(finaldata)



#plot(finaldata$Date,finaldata$Weekly_Sales) # use to show the equal distribution of sales vs date/time

View(cor(finaldata[c(-2,-13)]))

finaldataNonHoliday<-subset(finaldata, IsHoliday==FALSE)
finaldataHoliday<-subset(finaldata, IsHoliday==TRUE)
View(finaldataHoliday)

# startEntry= c(2010,5)
# finaldataHoliday <- ts(finaldata$Weekly_Sales, frequency=52, 
#                start=startEntry)
# 
# plot(finaldataHoliday)

# finaldataHoliday[finaldataHoliday==0] <- NA
# finaldataHoliday<-na.locf(finaldata)
# 
# finaldataHoliday.xts<- xts(finaldataHoliday, order.by = as.Date(finaldataHoliday$Date))
# View(finaldataHoliday.xts)
# 
# finaldataHoliday<-na.approx(finaldataHoliday)
# View(finaldata)
# na.locf()
# 
# 
# ?tslm()
# model <- tslm( ~ trend + season)
# fc  <- forecast(model, h=horizon)

View(unique(is.na(finaldata)))

finaldataEdited<-finaldata[-c(6:10)]
View(finaldataEdited)

finaldataEdited.xts<-xts(finaldataEdited,frequency=365, order.by=as.Date(finaldataEdited$Date))
# plot(apply.weekly(finaldataEdited.xts$Weekly_Sales,FUN=mean))
# plot(HoltWinters(apply.weekly(finaldataEdited.xts$Weekly_Sales,FUN=mean), beta = FALSE, gamma = FALSE))
# 
# ls('package:xts')
# ?FUN
# 
# ?apply.weekly()
# aggregate()
# aggregate(Vo(xts.ts),as.Date(index(xts.ts)),sum)
# 
# ?xts()
# ts.plot(finaldataEdited.ts)
# 
# finaldataEdited.ts.decompose<-decompose(finaldataEdited.ts, type='multiplicative')
# plot(finaldataEdited.ts.decompose)
# 
# ts.plot(finaldataEdited.ts.decompose)

aggregate.ts(finaldataEdited.xts,nfrequency = 52,fun=mean,)


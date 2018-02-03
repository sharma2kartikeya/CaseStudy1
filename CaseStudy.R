library(forecast)
library(xts)     


StoreTrain <- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/train.csv")
View(unique(StoreTrain$Weekly_Sales))
View(StoreTrain)
as.numeric(StoreTrain$IsHoliday == "TRUE")
StoreTrain$IsHoliday<-as.integer(as.logical(StoreTrain$IsHoliday))
Features<- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/features.csv")
View(Features)

FeaturesTrainNonHoliday<-subset(Features, IsHoliday==FALSE)

FeaturesTrainHoliday.xts<- xts( x= FeaturesTrainHoliday, order.by = as.Date(FeaturesTrainHoliday$Date))

FeaturesTrainHoliday.xts[,5:9][is.na(FeaturesTrainHoliday.xts[,5:9])==TRUE]<- 0
View(FeaturesTrainHoliday.xts)

FeaturesTrainNonHoliday.xts<- xts( x= FeaturesTrainNonHoliday, order.by = as.Date(FeaturesTrainNonHoliday$Date))
View(FeaturesTrainNonHoliday.xts)

# 
# FeaturesTrainNonHoliday.xts$MarkDown2<-na.fill(FeaturesTrainNonHoliday.xts$MarkDown2,fill=0)
# FeaturesTrainNonHoliday.xts$MarkDown3<-na.fill(FeaturesTrainNonHoliday.xts$MarkDown3,fill=0)
# FeaturesTrainNonHoliday.xts$MarkDown4<-na.fill(FeaturesTrainNonHoliday.xts$MarkDown4,fill=0)
# FeaturesTrainNonHoliday.xts$MarkDown5<-na.fill(FeaturesTrainNonHoliday.xts$MarkDown5,fill=0)

?na.fill()
na.fill(FeaturesTrainNonHoliday.xts,fill=0)  
FeaturesTrainNonHoliday.xts$IsHoliday<-as.integer(as.logical(FeaturesTrainNonHoliday.xts$IsHoliday))
View(FeaturesTrainNonHoliday.xts)





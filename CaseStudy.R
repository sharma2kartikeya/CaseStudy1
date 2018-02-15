install.packages("ggplot2")
install.packages("ltm")
install.packages("RPostgreSQL")
install.packages("RPostgres")
install.packages("sqldf")

library(forecast)
library(xts) 
library(ggplot2)
library(ltm)
library(dplyr)
library(RPostgreSQL)
library(sqldf)
library(delay)


# library(DBI)
# library(RPostgres)
# 
# con <- dbConnect(RPostgres::Postgres(),
#                  host = url$host,
#                  port = url$port,
#                  dbname = url$dbname,
#                  user = url$user,
#                  password = url$password
# )

train <- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/train.csv")
stores <- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/stores.csv")
 features <- read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/features.csv")
View(train)
 train$IsHoliday<-as.integer(as.logical(train$IsHoliday))
 features$IsHoliday<-as.integer(as.logical(features$IsHoliday))

plot(finaldata$Date,finaldata$Weekly_Sales) # use to show the equal distribution of sales vs date/time


View(cor(finaldata[c(-2,-13)]))

 startEntry= c(2010,5)
 finaldata_ts <- ts(finaldata$Weekly_Sales, frequency=52, 
                    start=startEntry)
 
 plot(finaldata_ts)
 
 tsDecomp<-decompose(finaldata_ts, type="multiplicative")
 plot(tsDecomp)

aggStore<-sqldf("select Store,Date,avg(Weekly_Sales) as Avg_Weekly_Sales from train group by Store,Date ")
aggStore$Date<-as.Date(aggStore$Date)
aggStore<-read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/Git/IDS-552/aggStore.csv")

#write.csv(aggStore, file = "aggStore.csv")
View(aggStore)

aggDate<-sqldf("select Date,avg(Weekly_Sales) as Avg_Weekly_Sales  from train group by Date ")
aggDate$Date<-as.Date(aggDate$Date)
View(aggDate)

aggDate<-read.csv("/Users/kartikeyasharma/iCloud Drive (Archive)/Desktop/Sem 2/IDS 552 SCM/Case Study/Git/IDS-552/aggDate.csv")

#write.csv(aggDate, file = "aggDate.csv")

StoreDetails<-split(aggStore,aggStore$Store)



#list2env(StoreDetails, envir= .GlobalEnv) separates the list into 45 different tables, not needed right now may be!


##### For Aggregated Naive Model######Extract time series and plot#########
dataFreq=  52 
startEntry= c(2010,5) 
trainSetStart= c(2010,5)
trainSetEnd=  c(2012,7) 
testSetStart= c(2012,8)
testSetEnd= c(2012,42)

model = arima(y, order, xreg = exogenous_data)
  
temp<-aggDate

  demandTS<-ts(temp$Avg_Weekly_Sales,freq=dataFreq, start =  startEntry )
  plot(demandTS,main = "Average Sales faced by Walmart",xlab="Week",ylab="Sales")  #plot time series
  
  demandTS.xts<- xts( x= aggDate$Avg_Weekly_Sales, order.by = as.Date(aggDate$Date))
  plot(demandTS.xts, xlab = "Day", ylab = "Traffic", bty = "l", main = "") #plot xts series
  View(demandTS.xts)
  
  demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd) #extract training set
  demandTest <- window(demandTS,start=testSetStart,end=testSetEnd) #extract test set
  
  # ###########Forecast#########
  numForcPeriods=36 
  #number of periods to forecast in the future (Note: If you are computing errors with respect to testing data then this value 
  #                     #should be equal to the duration of the testing data)
  
  HWForcModel <- HoltWinters(demandTrain,seasonal=c("multiplicative")) #Train Holt-Winters forecasting model. Can be additive or multiplicative
  HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = HoltWinters) #Foreacst using Holt-Winters model trained in previous step
  
  plot(HWForecast, main="Plot of training demand,
       testing demand, and forecast with 80% and 95%
       prediction intervals",xlab="Week",
       ylab="Sales") #plot the training demand, and forecast with prediction intervals
  lines(demandTest,col=2) #add the testing demand line to plot
  legend("topleft", lty=1, col=c(1,4,2),
         legend=c("Training Sales","Forecast","Testing Sales")) #create plot legend
  
  ###########Analyze forecasting error#########
  error = HWForecast$mean - demandTest #difference between forecast and actual demand
  AD=abs(error) #absolute value of error
  
  #Create empty vectors to store errors
  MSE <- matrix(, nrow = numForcPeriods, ncol = 1)
  MAD <- matrix(, nrow = numForcPeriods, ncol = 1)
  MAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
  bias <- matrix(, nrow = numForcPeriods, ncol = 1)
  TS <- matrix(, nrow = numForcPeriods, ncol = 1)
  #Label columns of matrices using name of error
  colnames(MSE) <- "MSE"
  colnames(MAD) <- "MAD"
  colnames(MAPE) <- "MAPE"
  colnames(bias) <- "bias"
  colnames(TS) <- "TS"
  
  
  for(t in 1:numForcPeriods){
    MSE[t] <- mean(error[1:t]*error[1:t])
    MAD[t] <- mean(AD[1:t])
    MAPE[t] <- mean(100*abs(error[1:t]/demandTest[1:t]))
    bias[t] <- sum(error[1:t])
    TS[t]= bias[t]/MAD[t]
  }
  
  
  error_Meas <- data.frame(floor(time(error)),cycle(error),demandTest,HWForecast$mean,error,AD,MSE,MAD,MAPE,bias,TS)
  colnames(error_Meas)[1] <- "Year"
  colnames(error_Meas)[2] <- "Week"
  colnames(error_Meas)[3] <- "Actual Sales"
  colnames(error_Meas)[4] <- "Forecast"
  
  
  View(error_Meas)
  write.csv(error_Meas, file="Error_Meas_Aggregate.csv")
  
#####For Each Store######Extract time series and plot#########
  Cumul_MAPE_HW<-matrix(data=NA, nrow=36, ncol=45)
  Cumul_bias_HW<-matrix(data=NA, nrow=36, ncol=45)
  Cumul_TS_HW<-matrix(data=NA, nrow=36, ncol=45)
  
  
  dataFreq=  52 
  startEntry= c(2010,5) 
  trainSetStart= c(2010,5)
  trainSetEnd=  c(2012,7) 
  testSetStart= c(2012,8)
  testSetEnd= c(2012,42)
  
  for (i in 1:45)
    
  {
        temp<-StoreDetails[[i]]
  
  
        
        
        demandTS<-ts(temp$Avg_Weekly_Sales,freq=dataFreq, start =  startEntry )
        plot(demandTS,main = "Average Sales faced by Walmart Store",xlab="Week",ylab="Sales")  #plot time series
        prompt=cat("Data displayed for Store",i,"\n")
        
       
        #Sys.sleep(1)
        
        demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd) #extract training set
        demandTest <- window(demandTS,start=testSetStart,end=testSetEnd) #extract test set
        
        # ###########Forecast#########
        numForcPeriods=36 
        #number of periods to forecast in the future (Note: If you are computing errors with respect to testing data then this value 
        #                     #should be equal to the duration of the testing data)
        
        HWForcModel <- HoltWinters(demandTrain,seasonal=c("multiplicative")) #Train Holt-Winters forecasting model. Can be additive or multiplicative
        HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = HoltWinters) #Foreacst using Holt-Winters model trained in previous step
        
        plot(HWForecast, main="Plot of training demand,
               testing demand, and forecast with 80% and 95%
              prediction intervals",xlab="Week",
              ylab="Sales") #plot the training demand, and forecast with prediction intervals
              lines(demandTest,col=2) #add the testing demand line to plot
              legend("topleft", lty=1, col=c(1,4,2),
             legend=c("Training Sales","Forecast","Testing Sales")) #create plot legend

              ###########Analyze forecasting error#########
              error = HWForecast$mean - demandTest #difference between forecast and actual demand
              AD=abs(error) #absolute value of error
             
             #Create empty vectors to store errors
              MSE <- matrix(, nrow = numForcPeriods, ncol = 1)
              MAD <- matrix(, nrow = numForcPeriods, ncol = 1)
              MAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
              bias <- matrix(, nrow = numForcPeriods, ncol = 1)
              TS <- matrix(, nrow = numForcPeriods, ncol = 1)
              #Label columns of matrices using name of error
              colnames(MSE) <- "MSE"
              colnames(MAD) <- "MAD"
              colnames(MAPE) <- "MAPE"
              colnames(bias) <- "bias"
              colnames(TS) <- "TS"
             
             # #compute errors
              for(t in 1:numForcPeriods){
                MSE[t] <- mean(error[1:t]*error[1:t])
                MAD[t] <- mean(AD[1:t])
                MAPE[t] <- mean(100*abs(error[1:t]/demandTest[1:t]))
                bias[t] <- sum(error[1:t])
                TS[t]= bias[t]/MAD[t]
              }
             
              #combined vectors into a dataframe, also appending year and quarter information in the first two columns
              error_Meas <- data.frame(floor(time(error)),cycle(error),demandTest,HWForecast$mean,error,AD,MSE,MAD,MAPE,bias,TS)
              colnames(error_Meas)[1] <- "Year"
              colnames(error_Meas)[2] <- "Week"
              colnames(error_Meas)[3] <- "Actual Sales"
              colnames(error_Meas)[4] <- "Forecast"
        
        
              View(error_Meas)
              prompt=cat("Data displayed for Store",i,"\n")
              Sys.sleep(2)
              
              Cumul_MAPE_HW[,i]<-error_Meas$MAPE
              Cumul_bias_HW[,i]<-error_Meas$bias
              Cumul_TS_HW[,i]<-error_Meas$TS
              View(error_Meas)
              
              
              prompt=cat("Data displayed for Store",i,"\n")
              #Sys.sleep(2)
  }  
  
  View(Cumul_MAPE_HW)
  View(Cumul_bias_HW)
  View(Cumul_TS_HW)
  write.csv(Cumul_MAPE_HW, file = "Cumul_MAPE_HW.csv")
  write.csv(Cumul_bias_HW, file = "Cumul_bias_HW.csv")
  write.csv(Cumul_TS_HW, file = "Cumul_TS_HW.csv")
  
  #############################Using STLF to find the seasonlaity in Data#############################
  
  dataFreq=  52 
  startEntry= c(2010,5) 
  trainSetStart= c(2010,5)
  trainSetEnd=  c(2012,7) 
  testSetStart= c(2012,8)
  testSetEnd= c(2012,42)
  
  Cumul_MAPE<-matrix(data=NA, nrow=36, ncol=45)
  Cumul_bias<-matrix(data=NA, nrow=36, ncol=45)
  Cumul_TS<-matrix(data=NA, nrow=36, ncol=45)
  
  for (i in 1:45)
{ 
  
  temp<-StoreDetails[[i]]
  
  
  demandTS<-ts(temp$Avg_Weekly_Sales,freq=dataFreq, start =  startEntry )
  plot(demandTS,main = "Average Sales faced by Walmart Store",xlab="Week",ylab="Sales")  #plot time series
  prompt=cat("Data displayed for Store",i,"\n")
  
  
  Sys.sleep(1)
  demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd) #extract training set
  demandTest <- window(demandTS,start=testSetStart,end=testSetEnd) #extract test set
  
  # ###########Forecast#########
  numForcPeriods=36 
  #number of periods to forecast in the future (Note: If you are computing errors with respect to testing data then this value 
  #                     #should be equal to the duration of the testing data)
  
  HWForcModel <- stlf(demandTrain,h=36, s.window = 7, t.window = NULL, robust = FALSE, lambda = NULL, biasadj = FALSE ) #,order=c(0,0,3)) #Train Holt-Winters forecasting model. Can be additive or multiplicative
  HWForecast <- forecast(HWForcModel, method=(stlf)) #Foreacst using Holt-Winters model trained in previous step
  
  plot(HWForecast, main="Plot of training demand,
       testing demand, and forecast with 80% and 95%
       prediction intervals",xlab="Week",
       ylab="Sales") #plot the training demand, and forecast with prediction intervals
  lines(demandTest,col=2) #add the testing demand line to plot
  legend("topleft", lty=1, col=c(1,4,2),
         legend=c("Training Sales","Forecast","Testing Sales")) #create plot legend
  
  
  ###########Analyze forecasting error#########
  error = HWForecast$mean - demandTest #difference between forecast and actual demand
  AD=abs(error) #absolute value of error
  
  #Create empty vectors to store errors
  MSE <- matrix(, nrow = numForcPeriods, ncol = 1)
  MAD <- matrix(, nrow = numForcPeriods, ncol = 1)
  MAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
  bias <- matrix(, nrow = numForcPeriods, ncol = 1)
  TS <- matrix(, nrow = numForcPeriods, ncol = 1)
  #Label columns of matrices using name of error
  colnames(MSE) <- "MSE"
  colnames(MAD) <- "MAD"
  colnames(MAPE) <- "MAPE"
  colnames(bias) <- "bias"
  colnames(TS) <- "TS"
  
  # #compute errors
  for(t in 1:numForcPeriods){
    MSE[t] <- mean(error[1:t]*error[1:t])
    MAD[t] <- mean(AD[1:t])
    MAPE[t] <- mean(100*abs(error[1:t]/demandTest[1:t]))
    bias[t] <- sum(error[1:t])
    TS[t]= bias[t]/MAD[t]
  }
  
  #combined vectors into a dataframe, also appending year and quarter information in the first two columns
  error_Meas <- data.frame(floor(time(error)),cycle(error),demandTest,HWForecast$mean,error,AD,MSE,MAD,MAPE,bias,TS)
  colnames(error_Meas)[1] <- "Year"
  colnames(error_Meas)[2] <- "Week"
  colnames(error_Meas)[3] <- "Actual Sales"
  colnames(error_Meas)[4] <- "Forecast"
  
  Cumul_MAPE[,i]<-error_Meas$MAPE
  Cumul_bias[,i]<-error_Meas$bias
  Cumul_TS[,i]<-error_Meas$TS
  
  
  View(error_Meas)
  View(Cumul_MAPE)
  View(Cumul_bias)
  View(Cumul_TS)
  prompt=cat("Data displayed for Store",i,"\n")
  #Sys.sleep(2)
  }  
  
  write.csv(Cumul_MAPE, file = "Cumul_MAPE.csv")
  write.csv(Cumul_bias, file = "Cumul_bias.csv")
  write.csv(Cumul_TS, file = "Cumul_TS.csv")
  
  
  # 
  # #############################Using Arima to find the seasonlaity in Data#############################
  # 
  # 
  # dataFreq=  52 
  # startEntry= c(2010,5) 
  # trainSetStart= c(2010,5)
  # trainSetEnd=  c(2012,7) 
  # testSetStart= c(2012,8)
  # testSetEnd= c(2012,42)
  # 
  # for (i in 1:45)
  #   
  # {
  #   temp<-StoreDetails[[i]]
  #   View(temp)
  #   
  #   
  #   
  #   demandTS<-ts(temp$Avg_Weekly_Sales,freq=dataFreq, start =  startEntry )
  #   plot(demandTS,main = "Average Sales faced by Walmart Store",xlab="Week",ylab="Sales")  #plot time series
  #   prompt=cat("Data displayed for Store",i,"\n")
  #   
  #   
  #   #Sys.sleep(1)
  #   
  #   demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd) #extract training set
  #   demandTest <- window(demandTS,start=testSetStart,end=testSetEnd) #extract test set
  #   
  #   # ###########Forecast#########
  #   numForcPeriods=36 
  #   #number of periods to forecast in the future (Note: If you are computing errors with respect to testing data then this value 
  #   #                     #should be equal to the duration of the testing data)
  #   
  #   HWForcModel <- auto.arima(demandTrain) #Train Holt-Winters forecasting model. Can be additive or multiplicative
  #   HWForecast <- forecast(HWForcModel, h=numForcPeriods) #Foreacst using Holt-Winters model trained in previous step
  #   
  #   plot(HWForecast, main="Plot of training demand,
  #        testing demand, and forecast with 80% and 95%
  #        prediction intervals",xlab="Week",
  #        ylab="Sales") #plot the training demand, and forecast with prediction intervals
  #   lines(demandTest,col=2) #add the testing demand line to plot
  #   legend("topleft", lty=1, col=c(1,4,2),
  #          legend=c("Training Sales","Forecast","Testing Sales")) #create plot legend
  #   
  #   ###########Analyze forecasting error#########
  #   error = HWForecast$mean - demandTest #difference between forecast and actual demand
  #   AD=abs(error) #absolute value of error
  #   
  #   #Create empty vectors to store errors
  #   MSE <- matrix(, nrow = numForcPeriods, ncol = 1)
  #   MAD <- matrix(, nrow = numForcPeriods, ncol = 1)
  #   MAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
  #   bias <- matrix(, nrow = numForcPeriods, ncol = 1)
  #   TS <- matrix(, nrow = numForcPeriods, ncol = 1)
  #   #Label columns of matrices using name of error
  #   colnames(MSE) <- "MSE"
  #   colnames(MAD) <- "MAD"
  #   colnames(MAPE) <- "MAPE"
  #   colnames(bias) <- "bias"
  #   colnames(TS) <- "TS"
  #   
  #   # #compute errors
  #   for(t in 1:numForcPeriods){
  #     MSE[t] <- mean(error[1:t]*error[1:t])
  #     MAD[t] <- mean(AD[1:t])
  #     MAPE[t] <- mean(100*abs(error[1:t]/demandTest[1:t]))
  #     bias[t] <- sum(error[1:t])
  #     TS[t]= bias[t]/MAD[t]
  #   }
  #   
  #   #combined vectors into a dataframe, also appending year and quarter information in the first two columns
  #   error_Meas <- data.frame(floor(time(error)),cycle(error),demandTest,HWForecast$mean,error,AD,MSE,MAD,MAPE,bias,TS)
  #   colnames(error_Meas)[1] <- "Year"
  #   colnames(error_Meas)[2] <- "Week"
  #   colnames(error_Meas)[3] <- "Actual Sales"
  #   colnames(error_Meas)[4] <- "Forecast"
  #   
  #   
  #   View(error_Meas)
  #   prompt=cat("Data displayed for Store",i,"\n")
  #   Sys.sleep(2)
  #   
  #   
  #   View(error_Meas)
  #   
  #   
  #   prompt=cat("Data displayed for Store",i,"\n")
  #   Sys.sleep(2)
  # }  
  # 
  # View(Cumul_MAPE_HW)
  # View(Cumul_bias_HW)
  # View(Cumul_TS_HW)
  # write.csv(Cumul_MAPE_HW, file = "Cumul_MAPE_HW.csv")
  # write.csv(Cumul_bias_HW, file = "Cumul_bias_HW.csv")
  # write.csv(Cumul_TS_HW, file = "Cumul_TS_HW.csv")
  # 
  
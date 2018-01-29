
#Opening all the libraries that will be needed

library(gridExtra)
library(lattice)
library(grid)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(chron)
library(ggplot2)
library(forecast)
library(plyr)
library(scales)
library(reshape2)
library(tseries)
library(zoo)
library(WriteXLS)
library(dummies)
library(matlab)

#Here is where all the CSVs by month should be moved before running the code
getwd()
setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
source("tesisfunctions.R")
source("summaryresults.R")

#Declare the list and vectors that will be used in the code
mydataframes <- list()
myforecasts <- list()
myForeGraphs <- list()
myCorrectedForeGraphs <- list()
myTempGraphs <- list()

###### SET UP THE CONSUMPTION DATASETS####
    
   setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/Datos/Consumo/CSV/") #Here is where all the CSVs by month should be moved before running the code
   files <- list.files(pattern = "*.csv")

  #This is a bucle for to read and pre-preprocess with the function setupdataset all the datasets in the same folder (and in CSV format). Together 
  #with the pre-process I create a vector called mymonth to store all the names of the datasets for later purposes. Also a list is created to have
  #???all the dataset in order. 
    
    for ( i in 1:length(files) ) {
        this <- substr(paste(gsub('.csv','',paste(files[i]))),3,nchar(paste(files[i]))-7)
        mydataframe <-  setupdataset(files[i])
        mydataframes[[this]] = mydataframe #Ordeno mis dataframes en una lista y les doy sus correspondientes nombres.
        rm(mydataframe,i, this)
      }
  
  #In order to save all the graphs to my working directoy this for structure is used
      
    for ( i in 1:length(mydataframes) ) {
        setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/ConsumoOriginal/")
        mymonth <- mydataframes[[i]]
        mesname <- names(mydataframes)[i]
        png(paste(mesname, ".png", sep=""), width = 1000, height = 500)
        mygraph <- graphmydataframe(i,mydataframes)
        print(mygraph)
        dev.off()
        rm(mesname,i, mymonth, mygraph)
      }

###### SET UP THE FORECAST DATASET ####
      
   setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/Datos/forecast/CSV/")
   filesfore <- list.files(pattern = "*.csv")

#This is a bucle for to read and pre-preprocess with the function setupmyforecast all the datasets in the same folder (and in CSV format). Together 
#with the pre-process I create a vector called mymonthFORE to store all the names of the datasets for later purposes. Also a list is created to have
#???all the dataset in order.

    for ( i in 1:length(filesfore) ) {
            this <- substr(paste(gsub('.csv','',paste(filesfore[i]))),3,nchar(paste(filesfore[i])))
            myforecast <- setupmyforecast(filesfore[i])
            myforecasts[[this]] = myforecast #Ordeno mis dataframes en una lista y les doy sus correspondientes nombres.
            rm(myforecast,i, this)
          }

#In order to save all the graphs to my working directoy this for structure is used
  
    for ( i in 1:length(myforecasts) ) {
        setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/Forecasts/")
        mymonth <- myforecasts[[i]]
        mesname <- names(myforecasts)[i]
        png(paste(mesname, ".png", sep=""), width = 1000, height = 500)
        mygraph <- graphmyforecast(i, myforecasts)
        print(mygraph)
        dev.off()
        rm(mesname,i, mymonth, mygraph)
        
    } # This bucle stores all the graphs in PNG in the folder FORECASTS in the environment 

    for ( i in 1:length(myforecasts) ) {
      
       this <- names(myforecasts)[i]
       mygraph <-  graphmyforecast(i, myforecasts)
       myForeGraphs[[this]] = mygraph
       rm(mygraph, this)
       
    } # This bucle creates a list storing all the graphs in variables.

###### CLEANING THE MISSING VALUES IN CONSUMPTION ######

  ### FROM CONSUMPTION DATASFRAMES
      
      #I attach through rbind command all the dataset to have all the anual consumption in one single dataframe. In order for the final dataset I order it by date.
      
            AnualConsumption <- NULL
            for (i in 1:length(mydataframes)){
                AnualConsumption <- rbind(AnualConsumption, mydataframes[[i]])
                rm(i)
              }
            AnualConsumption <- AnualConsumption[order(AnualConsumption[,1]),]
            NAsCons.0 <- length(which(is.na(AnualConsumption$Consumption)))/length(AnualConsumption$Consumption)*100
              
      #Once I have detected the missing Values I think of the best way to fill-in this data:
      
        #There are 4 hours in FEBRUARY 2017 in which there are no consumptin data due to hardware update. In order to fill in this NAs the average energy consumption from the previous day and the 
        #day after are going to be forced.
            
            detach("package:plyr")
            fixfebruary <- mydataframes[[2]] %>% 
                filter(Date >= "2017-02-13 11:00" & Date < "2017-02-13 15:00" | Date >= "2017-02-15 11:00" & Date < "2017-02-15 15:00") %>% 
                group_by(Hour) %>% 
                summarise(avg = mean(Consumption))
            indextofixFEB <- which(is.na(mydataframes[[2]]$Consumption))    
            mydataframes[[2]]$Consumption[indextofixFEB] <- round(fixfebruary$avg, digits = 1)
            library(plyr) 
      
        #The 30th of June from 00:00 to 23:00 is just missing values.Substitute NAs in the corresponding dataframe that belongs to the list of dataframes we created (If we modified)
           
            detach("package:plyr")
            fixjune <-  mydataframes[[6]] %>% 
                filter((Date >= "2016-06-27 00:00" & Date <="2016-06-29 23:00")) %>% 
                group_by(Hour) %>% 
                summarise(avg = mean(Consumption))
              indextofixJUN <- which(is.na(mydataframes[[6]]$Consumption))  
              mydataframes[[6]]$Consumption[indextofixJUN] <- round(fixjune$avg, digits = 1)
            library(plyr) 
              
        #Check the Values have been corrected and there are no more NAs in the Anual Consumption
              
            AnualConsumption <- NULL
            for (i in 1:length(mydataframes)){
                AnualConsumption <- rbind(AnualConsumption, mydataframes[[i]])
                rm(i)
              }
            AnualConsumption <- AnualConsumption[order(AnualConsumption[,1]),]
            NAsCons.1 <- length(which(is.na(AnualConsumption$Consumption)))/length(AnualConsumption$Consumption)*100
              
  ### CLEANING THE MISSING VALUES IN CONSUMPTION FORECASTING 
      
        #Same process that the previous one is folllowed, the part of consumption from the Forecast Dataset is the same than COMPTADOR from Consumption dataset
          
          AnualForecast <- NULL
          for (i in 1:length(myforecasts)){
            AnualForecast <- rbind(AnualForecast, myforecasts[[i]])
           }
          AnualForecast <- AnualForecast[order(AnualForecast[,1]),]
          NAsForCONS0 <- length(which(is.na(AnualForecast$Consumption)))/length(AnualForecast$Consumption)*100
          
          myforecasts[[2]]$Consumption[indextofixJUN] <- round(fixjune$avg, digits = 1)
          indextofixFEBfore <- which(is.na(myforecasts[[10]]$Consumption))    
          myforecasts[[10]]$Consumption[indextofixFEBfore] <- round(fixfebruary$avg, digits = 1)
          
          AnualForecast <- NULL
          for (i in 1:length(myforecasts)){
            AnualForecast <- rbind(AnualForecast, myforecasts[[i]])
          }
          AnualForecast <- AnualForecast[order(AnualForecast[,1]),]
          NAsForCONS1 <- length(which(is.na(AnualForecast$Consumption)))/length(AnualForecast$Consumption)*100
          
          # Borro las variables temporales para el proceso de remplazar los NAs
          
          rm(fixfebruary, fixjune, indextofixFEB, indextofixJUN, indextofixFEBfore, i)
          
###### PRE-PROCESS MISSING VALUES, 0 VALUES, OUTLAYERS, STRANGE DEVIATION IN FORECASTING ######
        
 # In order to avoid modifying original dataset another dataset from the original one is created (Dataset name = Comparison). 
 # we prepare the dataset for Analytics Purposes.
        
          Comparison <- AnualForecast
        #  Comparison$Forecast <- round(Comparison$Forecast, digits = 0)
          Comparison <- Comparison[order(Comparison[,1]),]
          row.names(Comparison) <- c(1:nrow(Comparison))
          Comparison$Day <- as.POSIXct(paste(day(Comparison$Date), month(Comparison$Date), year(Comparison$Date), sep = "-"), format = "%d-%m-%Y")
          Comparison$DayType <- weekdays(Comparison$Date)
          Comparison$DayType <- mapvalues(Comparison$DayType, 
                                          from = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"), 
                                          to = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" , "Sunday"))
          Comparison$Month <- month(Comparison$Date)
          Comparison$Month<- mapvalues(Comparison$Month, 
                                       from = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
                                       to = c("1January", "2February", "3Marzo", "4Abril", "5May", "6June" , "7July", "8August", "9September", "10October", "11November", "12December"))
          Comparison$Month <- paste(Comparison$Month, year(Comparison$Date), sep = "")
          Comparison$Dif <- Comparison$Consumption - Comparison$Forecast
          Comparison$RelativeError <- round(Comparison$Dif/Comparison$Consumption*100, digits = 2)
          Comparison$SSE <- round((Comparison$Consumption - Comparison$Forecast)^2, digits = 2)
          Comparison$SST <- round((Comparison$Consumption - mean(Comparison$Consumption, na.rm = TRUE))^2, digits = 2)
          Comparison$SSR <- round((Comparison$Forecast - mean(Comparison$Consumption, na.rm = TRUE))^2, digits = 2)
          Comparison$Hour <- hour(Comparison$Date)
          Comparison <- Comparison[c(1,4,12,5,6,2,3,7,8,9,10,11)]
          
         # View(Comparison[sample(nrow(Comparison), 20), ]) Generar muestra randomo del dataset. Just for presentation purposes.
          
        
      
     #We add the Temperature to each Hour. So the code in Script "weatherBCNclean" must be excuted before.
          
          setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
          source("weathertotal.R")
          for (i in 1:nrow(Comparison)) {
            
            if(Comparison$Date[i] %in% temperatures$Superdate) {
              
              Comparison$MyTemp[i] <-  temperatures$Temp[which(temperatures$Superdate == Comparison$Date[i])]
              
            } else {
              
              Comparison$MyTemp[i] <- NA
            }
          }
          
          View(Comparison)
        
     #Categorical Day Set
            library(plyr)
                Comparison$DayCategory <- Comparison$DayType
                Comparison$DayCategory <- mapvalues(Comparison$DayCategory, 
                          from = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" , "Sunday"),
                          to = c("WORKINGDAY", "WORKINGDAY", "WORKINGDAY", "WORKINGDAY", "SEMIWORKINGDAY", "OFFDAY" , "OFFDAY"))
                Comparison <- Comparison[c(1,2,3,4,14,5,6,7,13,8,9,10,11,12)]
          
          #Setting more specifically the holidays. Set the WORKING CALENDAR for the company and check what is happening these days
          
                setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
                source("workingcalendar.R")
                
                for (i in 1:nrow(Comparison)) {
                  
                  if (Comparison$Day[i] %in% holidaysTOTAL) {
                  
                    Comparison$DayCategory[i] <- "OFFDAY"
                    
                  }
                
                }
                
                 for (i in 1:nrow(Comparison)) {
                   
                   if (Comparison$Day[i] %in% semiholidaysTOTAL) {
                     
                     Comparison$DayCategory[i] <- "SEMIWORKINGDAY"
                     
                   }
                   
                 }
                
                for (i in 1:nrow(Comparison)) {
                  
                  if (Comparison$Day[i] %in% priorholidaysTOTAL) {
                    
                    Comparison$DayCategory[i] <- "SEMIWORKINGDAY"
                    
                  }
                  
                }
                
              #Before Applying all the modifications to the dataset (cleaning it) we create a copy.. This copy will be the argument of a function that will table out all the errors in
              #different points of the cleaning process.
                
             ComparisonforErrors <- Comparison
            # ComparisonforErrors$Consumption[which(is.na(ComparisonforErrors$Forecast))] <- NA  
             
             ComparisonRAW <- Comparison
             length(which(is.na(ComparisonRAW$Consumption)))
             length(which(is.na(ComparisonRAW$Forecast)))
             
  ### 1-. Looking for Missing Values in Forecast
        
        NAsForORIG <- length(which(is.na(Comparison$Forecast)))/length(Comparison$Forecast)*100
  
  ### 2 -. Looking for HUGE Outlayers in the prediction. From the graphs we know that biggest ones are in Nov and Dec. 
        # The criteria used to erase the biggest outliers that affect the accuracy is to assume all those forecast values in which $Dif is over 90% of Maxium 
        # consumption over the whole period analized.
        
        differlimit    <- 0.90*max(Comparison$Consumption)
        toEraseOutlier <- which(abs(Comparison$Dif) > differlimit)
        NAsForOutliers <- length(toEraseOutlier)/length(Comparison$Consumption)*100
        Comparison[toEraseOutlier,]
        rm(differlimit)
        
        #Convert Outliers to NAs
        
        Comparison$Forecast[toEraseOutlier] <- NA 

  ### 3-. Looking for zero value forecasts.   
        
        toEraseZero <- which(Comparison$Forecast == 0)
        NAsForEqual0 <- length(toEraseZero)/length(Comparison$Forecast)*100
        Comparison[toEraseZero,]
        
        #Convert Zero Value Forecasts to NAs
        
        Comparison$Forecast[toEraseZero]    <- NA  
       
  ### 4-. Looking for Values below  building's energy base load to convert them into NAs to be ignored in the accuracy analysis.

        #First we calculate the buildings base load energy consumption by analyzing what is happenning on saturdays and sundays.
          
        weekends <- Comparison %>% filter(DayType == "Saturday" | DayType == "DOMINGO")
        myrange <- 0.6
        toEraseUnderBL <- which(Comparison$Forecast > 0 & Comparison$Forecast < (myrange*min(weekends$Consumption)))
        NAsForUnderBS <- length(toEraseUnderBL)/length(Comparison$Forecast)*100
        Comparison[toEraseUnderBL,]
        rm(myrange)
        
        #Convert Below BBS Forecasts to NAs
        
        Comparison$Forecast[toEraseUnderBL] <- NA 
    
   ### 5-. Obtaning WHICH HOLIDAYS the algorithm has predicted consumption
          
        # Find which days, during the working hours, the forecastings has been, on average, greater than forecastlimit. This way we can obtain the days that were 
        # holidays but a normal prediction was made.
                
               detach("package:plyr")
               detectHOLIDAY <- Comparison %>%  
                  filter(hours(Comparison$Date) >= 07 & hours(Comparison$Date) < 19) #Filter by workingHours
               detectHOLIDAY <- detectHOLIDAY %>%  
                  group_by(Day) %>% 
                  summarise(avgcon = mean(Consumption, na.rm = TRUE),
                            avgfor = mean(Forecast, na.rm =TRUE))
               detectHOLIDAY$avgDIF <- detectHOLIDAY$avgfor - detectHOLIDAY$avgcon
               forecastlimit <- 40
               overforecastlimit <- which(detectHOLIDAY$avgDIF > forecastlimit)             #Posicion del vector que nos dirá los días con la AVG > forecastlimit
               myfailHolidaysdates <- detectHOLIDAY[overforecastlimit,]$Day                 #Días detectados como Holidays en los que se ha predecido consumo
               toEraseHoliday <- which(Comparison$Day %in% myfailHolidaysdates)             #Filas del Dataset que ocupan todos estos días.
               NAsForHOLIDAY <- length(toEraseHoliday)/length(Comparison$Forecast)*100
               library(plyr)
               
               rm(detectHOLIDAY, myfailHolidaysdates, forecastlimit)
               
         # Convert HOLIDAYS to NAs
             
               Comparison$Forecast[toEraseHoliday] <- NA
               
    ### 6-.  Obtaning Days with Strange behaviour by VISUAL exploration
               
        #16 DECEMBER 2016 , #27 ENERO 2017 ???, #4 NOVEMBER 2016, #17 OCTOBER 2016
               
               strangeforecast <- as.POSIXct(c("2016-12-16","2016-11-04","2016-11-25", "2016-10-17"))
               toEraseSTRANGE  <- which((Comparison$Day %in% strangeforecast & !is.na(Comparison$Forecast ))) #!is.na is needed in order to obtain the proper
                                                                                                              #length of the elements to be erased
               NAsForSTRANGE   <- length(toEraseSTRANGE)/length(Comparison$Forecast)*100
               
       # Convert STRANGE to NAs      
               
               Comparison$Forecast[toEraseSTRANGE] <- NA  
               
  # We have converted all the incongruence into NAs. (NAs per se, Big Outlayers, 0 forecasts & below baseload forecast). We delete the consumption data
  #from the same times that there is no forecast prediction as these data do not provide any value to the analysis. Also the total NAs in the end is
  #calculated before to have a reference of how ofthen the predictions fails.
               
               NAsForTOT <- length(which(is.na(Comparison$Forecast)))/length(Comparison$Forecast)*100
               NAsForTOTsum <- NAsForORIG + NAsForOutliers + NAsForEqual0 +  NAsForUnderBS + NAsForHOLIDAY + NAsForSTRANGE

               #Convert all the Consumption associated to a NA in Forecast into NA and UPDATE the DIF and RelativeError, SSE,SST,SSR Columns        
               
               Comparison$Consumption[which(is.na(Comparison$Forecast))] <- NA  
               
               Comparison$Dif <- Comparison$Forecast - Comparison$Consumption
               Comparison$RelativeError <- round(Comparison$Dif/Comparison$Consumption*100, digits = 2)
               Comparison$SSE <- round((Comparison$Consumption - Comparison$Forecast)^2, digits = 2)
               Comparison$SST <- round((Comparison$Consumption - mean(Comparison$Consumption, na.rm = TRUE))^2, digits = 2)
               Comparison$SSR <- round((Comparison$Forecast - mean(Comparison$Consumption, na.rm = TRUE))^2, digits = 2)
               
               rm(toEraseHoliday,toEraseOutlier,toEraseSTRANGE,toEraseUnderBL,toEraseZero, holidays2016fd, holidays2017fd, weekends, overforecastlimit, NAsCons.0, NAsCons.1, NAsForCONS0, NAsForCONS1)
               
##### FIRST ACCURACY ANALYSIS #####
          
 #Once the Dataset has been cleaned and big outlayers and 0 measures have turned into NAs a study of the accuracy is performed below. For that purpose
 #another PREPROCESS is followed to facilitate the analysis.
        
  # In order to be able to plot Monthly consumptions to compare month performance. The Anual Dataframe its split into months. 
        
        monthlyForecast <- split.data.frame(Comparison, Comparison$Month)
        length(monthlyForecast)
        monthlyForecast[[14]] <- as.data.frame(Comparison)
       
        #In order to save all the graphs to my working directoy this for structure is used
        for ( i in 1:length(monthlyForecast) ) {
          setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/Corregidas/")
          mymonth <- monthlyForecast[[i]]
          mesname <- names(monthlyForecast)[i]
          png(paste(mesname, ".png", sep=""), width = 1000, height = 500)
          mygraph <- graphmyforecast(i,monthlyForecast)
          print(mygraph)
          dev.off()
          rm(mesname,i, mymonth, mygraph)
        }
        
        for ( i in 1:length(monthlyForecast) ) {
          
          this <- names(monthlyForecast)[i]
          mygraph <-  graphmyforecast(i, monthlyForecast)
          myCorrectedForeGraphs[[this]] = mygraph
          rm(mygraph, this)
          
        } # This bucle creates a list storing all the graphs in variables.
        
               
#We calculate the key performance indicators : MAE, RelativeError, R2 and RMSE. For that purpose the function "myerrorcalculations" is written with the dataframe with the data as an argument. Also
#function cleanMYdataset carries the process over a copy of the Comparison Dataframe to which it applies the cleaning process step by step and stores the error metrics in a table.

      # # rsquaredV1 <- (1 - sum( ComparisonforErrors$SSE, na.rm = TRUE)/sum( ComparisonforErrors$SST, na.rm = TRUE))
      # # rsquaredV1
      # # 
      # # rsquaredV2 <- (1 - sum( Comparison$SSE, na.rm = TRUE)/sum( Comparison$SST, na.rm = TRUE))
      # # rsquaredV2
      # # 
      # # ComparisonforErrorsWeekend <- ComparisonforErrors %>% filter(DayType == "Sunday" | DayType == "Saturday") 
      # ComparisonforErrorsWeekend2 <- Comparison %>% filter(DayType == "Sunday" | DayType == "Saturday") 
      # # 
      # # rsquaredV3 <- (1 - sum( ComparisonforErrorsWeekend$SSE, na.rm = TRUE)/sum( ComparisonforErrorsWeekend$SST, na.rm = TRUE))
      # # rsquaredV3
      # # 
      # # length(which(is.na(ComparisonforErrorsWeekend2$Forecast)))
      # # length(which(is.na(ComparisonforErrorsWeekend2$Consumption)))
      # # errorMAE <- sum(abs(ComparisonforErrorsWeekend2$Dif), na.rm = TRUE)/(length(which(!is.na(ComparisonforErrorsWeekend2$Dif))))
      # # AVGRelativeError <- mean(abs(ComparisonforErrorsWeekend2$RelativeError), na.rm = TRUE)
      # # stdeviation <- sd(ComparisonforErrorsWeekend2$Dif, na.rm = TRUE)
      # rsquaredGEN <- (1 - sum(ComparisonforErrorsWeekend2$SSE, na.rm = TRUE)/sum(ComparisonforErrorsWeekend2$SST, na.rm = TRUE))
      # # 
      # # rsquaredV3b <- (1 - sum( ComparisonforErrorsWeekend2$SSE, na.rm = TRUE)/sum( ComparisonforErrorsWeekend2$SST, na.rm = TRUE))
      # # rsquaredV3b
      # # 
      # # myerrorcalculations(ComparisonforErrorsWeekend2)
      # # 
      # ComparisonforErrorsWorkDay <- ComparisonforErrors %>% filter(DayCategory != "OFFDAY") 
      # ComparisonforErrorsWorkDay2 <- Comparison %>% filter(DayCategory != "OFFDAY") 
      # # 
      # # rsquaredV4 <- (1 - sum( ComparisonforErrorsWorkDay$SSE, na.rm = TRUE)/sum( ComparisonforErrorsWorkDay$SST, na.rm = TRUE))
      # # rsquaredV4
      # # 
      # rsquaredV4b <- (1 - sum( ComparisonforErrorsWorkDay2$SSE, na.rm = TRUE)/sum( ComparisonforErrorsWorkDay2$SST, na.rm = TRUE))
      # # rsquaredV4b
      
      setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
      source("tesisfunctions.R")
      
      ErrorsTable <- as.data.frame(cleanMYdataset(ComparisonforErrors))
      ErrorsTable <- format(ErrorsTable, scientific=FALSE)
      ErrorsTable[1, 1:5] <- ">1e+20"
      ErrorsTable
     
      
       ##### PLOTTING THE ERROR ######
      
     # ANother script for this purposes has been created
      
##### DEALING WITH TEMERATURES ######
          
          for ( i in 1:length(monthlyForecast) ) {
            setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/Temperature/")
            mymonth <- monthlyForecast[[i]]
            mesname <- names(monthlyForecast)[i]
            png(paste(mesname, ".png", sep=""), width = 1000, height = 500)
            mygraph <- graphmytemp(i,monthlyForecast)
            print(mygraph)
            dev.off()
            rm(mesname,i, mymonth, mygraph)
          }
          
          for ( i in 1:length(monthlyForecast) ) {
            
            this <- names(monthlyForecast)[i]
            mygraph <-  graphmytemp(i, monthlyForecast)
            myTempGraphs[[this]] = mygraph
            rm(mygraph, this)
            
          } # This bucle creates a list storing all the graphs in variables.
          
          #Plotting temperature togheter with the consumption and forecasteing of each month
          
          for ( i in 1:length(monthlyForecast) ) {
            setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/TempConsumJuntos/")
            mesname <- names(monthlyForecast)[i]
            png(paste(mesname, ".png", sep=""), width = 1000, height = 1000)
            mygraph <- grid.arrange(myTempGraphs[[i]],myCorrectedForeGraphs[[i]], ncol=1)
            print(mygraph)
            dev.off()
            rm(mesname,i, mygraph)
          }
          
          
   
          
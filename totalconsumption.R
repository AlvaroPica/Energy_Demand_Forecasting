### THIS SCRIPT IS MEANT TO PREPARE THE DATA TO APPLY DIFFERENT ALGORITHMS ###

# The only output of this script is a dataframe called "totalconsumption" that will be used in other parts of the programms as 
# the base data to apply different precooked algorithms.

  setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/") 
  source("main.R")
  myCorrectedConsumption <- list()

  #For some strangre reasson the day 30th June 2016 is skept in the CSV original file so we add it manually as we had already
#calculated the consumption on this days in the script "main". We sourced the script at the beggining of this script so we
#are good to go.  
  
###### Preprocessing the whole period data set ######
  
  setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/Datos/Consumo/CSV/Total/") 
  totalconsumption <- read.csv("total20152017.csv", sep =";", header = FALSE)[-c(1:15), 1:3]
  names(totalconsumption) = c("Day", "Hour", "Consumption")
  totalconsumption$Day  <- as.POSIXct(as.character(totalconsumption$Day), format = "%d/%m/%Y")
  totalconsumption$Date <- paste(totalconsumption$Day, totalconsumption$Hour)
  totalconsumption$Date <- as.POSIXct(as.character(totalconsumption$Date), format = "%Y-%m-%d %H:%M")
  totalconsumption <- totalconsumption[,c(1,2,4,3)]
  totalconsumption$Hour <- factor(totalconsumption$Hour, levels = levels(totalconsumption$Hour)[2:25])
  totalconsumption$Consumption <- as.numeric(gsub(",", "." ,totalconsumption$Consumption))
  
# We need to check that we have all the values for the period considered in the data frame.
# That is:
  # 912 days
  # 21888 hours (same number of rows that the df should have)

  length(unique(totalconsumption$Day))
  nrow(unique(totalconsumption))

      #The 30th of June from 00:00 to 23:00 is just missing values.Substitute NAs in the corresponding dataframe that belongs to the list of dataframes we created (If we modified)
     #  
       detach("package:plyr")
       fixjune <-  totalconsumption %>% 
         filter((Date >= "2016-06-27 00:00" & Date <="2016-06-29 23:00")) %>% 
         group_by(Hour) %>% 
         summarise(Consumption = round(mean(Consumption), digits = 0))
        
       fixjune <- as.data.frame(fixjune)
       fixjune$Day <- as.POSIXct(rep("2016-06-30",24), format = "%Y-%m-%d")
       fixjune$Date <- as.POSIXct(paste(fixjune$Day, fixjune$Hour, sep = " "), format = "%Y-%m-%d %H:%M")
       fixjune <- fixjune[,c(3,1,4,2)]

       fixfebruary <- totalconsumption %>% 
         filter(Date >= "2017-02-13 11:00" & Date < "2017-02-13 15:00" | Date >= "2017-02-15 11:00" & Date < "2017-02-15 15:00") %>% 
         group_by(Hour) %>% 
         summarise(Consumption = round(mean(Consumption), digits = 0))
            
       fixfebruary <- as.data.frame(fixfebruary)       
       fixfebruary$Day <- as.POSIXct(rep("2017-02-14",4), format = "%Y-%m-%d")
       fixfebruary$Date <- as.POSIXct(paste(fixfebruary$Day, fixfebruary$Hour, sep = " "), format = "%Y-%m-%d %H:%M")
       fixfebruary <- fixfebruary[,c(3,1,4,2)]
       
       str(totalconsumption)
       str(fixfebruary)
       str(fixjune)
       
       totalconsumption <- rbind(totalconsumption, fixjune, fixfebruary)
       totalconsumption <- arrange(totalconsumption, totalconsumption$Date)

#After fixing the known missed values we check againt if there is still missed hours in the df.

  length(unique(totalconsumption$Day))
  nrow(unique(totalconsumption))

#IN order to find the missed values we create a new column with value 1. Then we grooup by day and sum these 1s columns. Those days in
#which the sum is not equal to 1 wil lhave a missed row in some hour.
  
    # totalconsumption$prueba <- 1
    # detach("package:plyr")
    # pruebaloca <- totalconsumption %>% 
    #   group_by(Day) %>% 
    #   summarise(mysuma = sum(prueba))
    # library(plyr) 
    # 
    # pruebaloca <- pruebaloca[which(pruebaloca$mysuma != 24),]
    # 
        #Once we know whichs days contains missed hours we add them by assigning the same consumptiono than the previous hour. #It turns out
        # that these missed hours are the ones due to Daylight savings when the time moves from 02:00 to 03:00. Therefore there are not such
        # moments in POSIXct format and they cannot be "invented". These days remain with 23 hours. The fact of the daylightsavings is IGNORED
        # in this study
        
              # c(1:24) %in% hours(totalconsumption$Day[which(totalconsumption$Day == "2015-03-29")])
              # totalconsumption$Hour[which(totalconsumption$Day == "2017-03-26")]
              # 
              # missed1 <- totalconsumption[which(totalconsumption$Day == "2015-03-29" & totalconsumption$Hour == "03:00"),]
              # missed2 <- totalconsumption[which(totalconsumption$Day == "2016-03-27" & totalconsumption$Hour == "03:00"),]
              # missed3 <- totalconsumption[which(totalconsumption$Day == "2017-03-26" & totalconsumption$Hour == "03:00"),]
              # fixedmissed <- rbind(missed1,missed2,missed3)
              # 
              # 
              # totalconsumption <- rbind(totalconsumption, fixedmissed)
              # totalconsumption <- arrange(totalconsumption, totalconsumption$Date)

  # A new evaluation of the dataset shows that all the values are properly filled and the dataset is ready to be analyzed.
      
      # length(unique(totalconsumption$Day))
      # nrow(unique(totalconsumption))
      
      #This values should be 21885 as due to daylight savings there are three hors (in october) that values are overwritten by the meter.
      
  
  # A SECOND PREPROCESS TO SET THE DATAFRAME IN TO THE CORRECT FORMAT
      library(plyr) 
      totalconsumption$DayType <- weekdays(totalconsumption$Day)
      totalconsumption$DayType <- mapvalues(totalconsumption$DayType, 
                                            from = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"), 
                                            to = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" , "Sunday"))
      totalconsumption$DayType <- as.factor(totalconsumption$DayType)
      totalconsumption$DayType <- factor(totalconsumption$DayType, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" , "Sunday"))
      totalconsumption$MonthGen <- month(totalconsumption$Date)
      totalconsumption$MonthGen <- mapvalues(totalconsumption$MonthGen, 
                                                                       from = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
                                                                       to = c("January", "February", "March", "April", "May", "June" , "July", "August", "September", "October", "November", "December"))
      totalconsumption$MonthGen <- as.factor(totalconsumption$MonthGen)
      totalconsumption$MonthGen <- factor(totalconsumption$MonthGen, levels = c("January", "February", "March", "April", "May", "June" , "July", "August", "September", "October", "November", "December"))
      totalconsumption$Month <- paste(totalconsumption$MonthGen, year(totalconsumption$Date), sep = "")
      totalconsumption$DayCategory <-   totalconsumption$DayType
      totalconsumption$DayCategory <- mapvalues(totalconsumption$DayCategory, 
                                          from = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" , "Sunday"),
                                          to = c("WORKINGDAY", "WORKINGDAY", "WORKINGDAY", "WORKINGDAY", "SEMIWORKINGDAY", "OFFDAY" , "OFFDAY"))
      totalconsumption <- totalconsumption[, c(3,1,2,7,6,5,8,4)]
      #Setting more specifically the holidays. Set the WORKING CALENDAR for the company and check what is happening these days

        setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/") 
        source("workingcalendar.R")
       
        for (i in 1:nrow(totalconsumption)) {
          if (totalconsumption$Day[i] %in% holidaysTOTAL) {
            totalconsumption$DayCategory[i] <- "OFFDAY"
          }
        }
        
        for (i in 1:nrow(totalconsumption)) {
          if (totalconsumption$Day[i] %in% semiholidaysTOTAL) {
            totalconsumption$DayCategory[i] <- "SEMIWORKINGDAY"
          }
        }
        
        for (i in 1:nrow(totalconsumption)) {
          if (totalconsumption$Day[i] %in% priorholidaysTOTAL) {
            totalconsumption$DayCategory[i] <- "SEMIWORKINGDAY"
          }
        }
       
        
      #We add another important predictor to work with. The outdoor temperature at each point measure.
       
        setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/") 
        source("weathertotal.R")
          
        for (i in 1:nrow(totalconsumption)) {
            
            if(totalconsumption$Date[i] %in% temperatures$Superdate) {
              
              totalconsumption$Temperature[i] <-  temperatures$Temp[which(temperatures$Superdate == totalconsumption$Date[i])]
              
            } else {
              
              totalconsumption$Temperature[i] <- NA
            }
          }
        
        totalconsumption <- totalconsumption[,c(1,2,3,4,5,6,7,9,8)]
        
        #Fill In NAs by interpolation
        
        totalconsumption$Temperature <- round(zoo::na.fill(totalconsumption$Temperature, c("extend")), digits = 0)
        which(is.na(totalconsumption), arr.ind = TRUE)
        length(which(is.na(totalconsumption$Temperature)))
        
        str(totalconsumption)
        
  #Create variable to check load in previous period
        
        totalconsumption$DatePriorDay <- totalconsumption$Date - 60*60*24
        totalconsumption$DayBefore <- rep(0)
        vectordaybefore <- c()
        
        for (i in 1:nrow(totalconsumption)){
          if(i < 24+1) { vectordaybefore[i] <- NA} else {
            vectordaybefore[i] <- totalconsumption$Consumption[i-(24)]
          }
        }
        
        totalconsumption$DayBefore <- vectordaybefore
     
        
      totalconsumption$DatePriorWeek <- totalconsumption$Date - 60*60*24*7
      totalconsumption$WeekBefore <- rep(0)
      vectorweekbefore <- c()
     
       for (i in 1:nrow(totalconsumption)){
       if(i < 24*7+1) { vectorweekbefore[i] <- NA} else {
           vectorweekbefore[i] <- totalconsumption$Consumption[i-(24*7)]
        }
      }
  
      totalconsumption$WeekBefore <- vectorweekbefore
      
  #Once the data set has been filled it is split so the monthly separated consunption can be graphed
        
        monthlyConsumption <- split.data.frame(totalconsumption, totalconsumption$Month)
        monthlyConsumption[[31]] <- as.data.frame(totalconsumption)
        names(monthlyConsumption)[[31]] <- "totalconsumption"

        #In order to save all the graphs to my working directoy this for structure is used
        
        setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
        source("tesisfunctions.R")
        
        for ( i in 1:length(monthlyConsumption) ) {
          setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/ConsumoMensual/")
          mesname <- names(monthlyConsumption)[i]
          png(paste(mesname, ".png", sep=""), width = 1000, height = 500)
          mygraph <- graphmyconsumption(i, monthlyConsumption)
          print(mygraph)
          dev.off()
          rm(mesname,i, mymonth, mygraph)
        }
        
  # This bucle creates a list storing all the graphs in variables
        
        #In order to plot the whole period more visible we set the conditions for the whole dataframe:
        
          setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/ConsumoMensual/")
          mesname <- names(monthlyConsumption)[31]
          png(paste(mesname, ".png", sep=""), width = 3000, height = 500)
          mygraph <- graphmysinglelonggraph(31, monthlyConsumption)
          print(mygraph)
          dev.off()
          rm(mesname,mygraph)
      
 # As there are many missed values in the temperature:
          

    # To correct Temperatures we work in another script "correctTemperatures.R"
        
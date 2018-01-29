

#This is function to pre-process my dataset and leave them in the same format directly from the CSV given by DeXCell.

      setupdataset <- function(n,...){
      myworkingd <- getwd()
      setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/Datos/Consumo/CSV/")
      thisfile <- paste(n)
      dataframe <- read.csv(file = thisfile, sep = ";", header = FALSE)[-c(1:15),c(1,2,12,18)]
      names(dataframe) = c("Day", "Hour", "Consumption", "Planta4")
      dataframe$Day<- as.POSIXct(as.character(dataframe$Day), format = "%d/%m/%Y")
      dataframe$Hour <- as.character.POSIXt(dataframe$Hour) #Por defecto, la columna Hour tiene 34 levels de factors. Lo reduzco a 24.
      dataframe$Hour <- as.factor(dataframe$Hour)
      dataframe$Consumption <- as.numeric(gsub(",", "." ,dataframe$Consumption))
      dataframe$Planta4 <-as.numeric(gsub(",", "." ,dataframe$Planta4))
      row.names(dataframe) <- c(1:nrow(dataframe))
      dataframe$Date <- paste(dataframe$Day, dataframe$Hour)
      dataframe <- dataframe[,c(1,2,5,3,4)]
      dataframe$Date <- as.POSIXct(as.character(dataframe$Date), format = "%Y-%m-%d %H:%M")
      setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
      return(dataframe)
    }
    
#This is function to pre-process my dataset and leave them in the same format.
    
      setupmyforecast <- function(n,...){
      myfile <- paste(n)
      dataframe <- read.csv(file = myfile, sep = ",", header = FALSE)[-c(1),c(1,2,3)]
      names(dataframe) = c("Date", "Consumption", "Forecast")
      dataframe$Date <- as.POSIXct(as.character(dataframe$Date), format = "%Y-%m-%d %H:%M")
      dataframe$Consumption <- as.numeric(as.character(dataframe$Consumption))
      dataframe$Forecast <- as.numeric(as.character(dataframe$Forecast))
      return(dataframe)
    }


#Graphics original plots

  #In order to plot the data of interest, in this case the energy consumptio versus the date, I create a function to set-up the design of the plot:
  #same with dataframes list. The funcion argument is a number that it is related with the list mydataframes. To check properly the dataframe you
  #want to plot you have to first know which position of the list are u interested in.
  
      graphmydataframe <- function(x, onelist, ...){
        
        dates <- c(date(onelist[[x]]$Date[1]), date(onelist[[x]]$Date[nrow(onelist[[x]])]))    
        selection <- onelist[[x]]$Date[which(hour(onelist[[x]]$Date) == 00)]
        ggplot(data = onelist[[x]], aes(x = Date)) +
          geom_line(aes(y = Consumption, colour = "General"), size = 0.8) +
          geom_line(aes(y = Planta4, colour = "Planta 4"), size = 0.8) +
          geom_vline(xintercept = as.numeric(selection), linetype=1, colour="gray", na.rm = TRUE, size = 0.1) +
          ggtitle(names(onelist)[x]) +
          labs(x="", y= "Energy in kWh") +
          scale_colour_manual(name = "Consumption", values=c("red", "darkolivegreen4")) +
          scale_x_datetime(labels = date_format("%b-%d"), breaks=pretty_breaks(n = 32)(as_datetime(dates)))  +
          coord_cartesian(ylim=c(0, 150))+
          theme(panel.grid.minor = element_blank(), 
                panel.grid.major = element_line(colour = "gray", size = 0.1), 
                panel.grid.major.x = element_blank(), 
                panel.background = element_blank(),
                axis.text.y = element_text(colour="black", size = 10),
                axis.text.x = element_text(size = 9, angle = 270, vjust = -1.2),
                axis.ticks.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 20, vjust = 1),
                panel.border = element_rect(colour = "gray", fill= NA, size=0.1),
                legend.background = element_rect(fill = "white", colour = NA),
                legend.text = element_text(size = rel(0.8), color="black" ), 
                legend.text.align = NULL, 
                legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0, color="black"), 
                legend.title.align = NULL)
      }
      
      graphmyforecast <- function(x, onelist, ...){
        
        dates <- c(date(onelist[[x]]$Date[1]), date(onelist[[x]]$Date[nrow(onelist[[x]])]))    
        selection <- onelist[[x]]$Date[which(hour(onelist[[x]]$Date) == 00)]
        ggplot(data = onelist[[x]], aes(x = Date)) +
          geom_line(aes(y = Consumption, colour = "Actual"), size = 0.8) +
          geom_line(aes(y = Forecast, colour = "Forecast"), size = 0.8) +
          geom_vline(xintercept = as.numeric(selection), linetype=1, colour="gray", na.rm = TRUE, size = 0.1) +
          ggtitle(names(onelist)[x]) +
          labs(x="", y= "Energy in kWh") +
          scale_colour_manual(name = "Consumption", values=c("red", "dodgerblue4"))+
          scale_x_datetime(labels = date_format("%b-%d"), breaks=pretty_breaks(n = 32)(as_datetime(dates)))  +
          #coord_cartesian(ylim=c(0, 150))+
          theme(panel.grid.minor = element_blank(), 
                panel.grid.major = element_line(colour = "gray", size = 0.1), 
                panel.grid.major.x = element_blank(), 
                panel.background = element_blank(),
                axis.text.y = element_text(colour="black", size = 20),
                axis.text.x = element_text(size = 15, angle = 270, vjust = -1.2),
                axis.title = element_text(size=14,face="bold"),
                axis.ticks.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 20, vjust = 1),
                panel.border = element_rect(colour = "gray", fill= NA, size=0.1),
                legend.background = element_rect(fill = "white", colour = NA),
                legend.text = element_text(size = 14, color="black" ), 
                legend.text.align = NULL, 
                legend.title = element_text(size = 18, face = "bold", hjust = 0, color="black"), 
                legend.title.align = NULL)
      }
      
      graphmytemp <- function(x, onelist, ...){
        
        dates <- c(date(onelist[[x]]$Date[1]), date(onelist[[x]]$Date[nrow(onelist[[x]])]))    
        selection <- onelist[[x]]$Date[which(hour(onelist[[x]]$Date) == 00)]
        ggplot(data = onelist[[x]], aes(x = Date)) +
          geom_line(aes(y = MyTemp, colour = "Temperat"), size = 0.8) +
          geom_vline(xintercept = as.numeric(selection), linetype=1, colour="gray", na.rm = TRUE, size = 0.1) +
          ggtitle(names(onelist)[x]) +
          labs(x="", y= "Temperature in (ºC)") +
          scale_colour_manual(name = "Tª", values=c("darkgreen"))+
          scale_x_datetime(labels = date_format("%b-%d"), breaks=pretty_breaks(n = 32)(as_datetime(dates)))  +
          coord_cartesian(ylim=c(0, 40))+
          theme(panel.grid.minor = element_blank(), 
                panel.grid.major = element_line(colour = "gray", size = 0.1), 
                panel.grid.major.x = element_blank(), 
                panel.background = element_blank(),
                axis.text.y = element_text(colour="black", size = 10),
                axis.text.x = element_text(size = 9, angle = 270, vjust = -1.2),
                axis.ticks.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 20, vjust = 1),
                panel.border = element_rect(colour = "gray", fill= NA, size=0.1))
        
      }
      
      graphmyconsumption <- function(x, onelist, ...){
        
        dates <- c(date(onelist[[x]]$Date[1]), date(onelist[[x]]$Date[nrow(onelist[[x]])]))    
        selection <- onelist[[x]]$Date[which(hour(onelist[[x]]$Date) == 00)]
        ggplot(data = onelist[[x]], aes(x = Date)) +
          geom_line(aes(y = Consumption, colour = "General"), size = 0.8) +
          geom_vline(xintercept = as.numeric(selection), linetype=1, colour="gray", na.rm = TRUE, size = 0.1) +
          ggtitle(names(onelist)[x]) +
          labs(x="", y= "Energy in kWh") +
          scale_colour_manual(name = "Consumption", values=c("red")) +
          scale_x_datetime(labels = date_format("%b-%d"), breaks=pretty_breaks(n = 32)(as_datetime(dates)))  +
          coord_cartesian(ylim=c(0, 150))+
          theme(panel.grid.minor = element_blank(), 
                panel.grid.major = element_line(colour = "gray", size = 0.1), 
                panel.grid.major.x = element_blank(), 
                panel.background = element_blank(),
                axis.text.y = element_text(colour="black", size = 10),
                axis.text.x = element_text(size = 9, angle = 270, vjust = -1.2),
                axis.ticks.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 20, vjust = 1),
                panel.border = element_rect(colour = "gray", fill= NA, size=0.1),
                legend.background = element_rect(fill = "white", colour = NA),
                legend.text = element_text(size = rel(0.8), color="black" ), 
                legend.text.align = NULL, 
                legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0, color="black"), 
                legend.title.align = NULL)
      }
      
      graphmysinglelonggraph <- function(x, onelist, ...){
        
        dates <- c(date(onelist[[x]]$Date[1]), date(onelist[[x]]$Date[nrow(onelist[[x]])]))    
        ggplot(data = onelist[[x]], aes(x = Date)) +
          geom_line(aes(y = Consumption, colour = "General"), size = 0.8) +
          ggtitle(names(onelist)[x]) +
          labs(x="", y= "Energy in kWh") +
          scale_x_datetime(labels = date_format("%b-%d"), breaks=pretty_breaks(n = 32)(as_datetime(dates)))  +
          coord_cartesian(ylim=c(0, 150))+
          theme(panel.grid.minor = element_blank(), 
                panel.grid.major = element_line(colour = "gray", size = 0.1), 
                panel.grid.major.x = element_blank(), 
                panel.background = element_blank(),
                axis.text.y = element_text(colour="black", size = 10),
                axis.text.x = element_text(size = 9, angle = 270, vjust = -1.2),
                axis.ticks.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 20, vjust = 1),
                panel.border = element_rect(colour = "gray", fill= NA, size=0.1))
      }
      
      graphmygraph <- function(dataframe, ...){
        
        dates <- c(date(dataframe$Date[1]), date(dataframe$Date[nrow(dataframe)]))    
        selection <- dataframe$Date[which(hour(dataframe$Date) == 00)]
        ggplot(data = dataframe, aes(x = Date)) +
          geom_line(aes(y = Consumption, colour = "Actual"), size = 0.8) +
          geom_line(aes(y = Forecast, colour = "Forecast"), size = 0.8) +
          geom_vline(xintercept = as.numeric(selection), linetype=1, colour="gray", na.rm = TRUE, size = 0.1) +
          ggtitle("Data") +
          labs(x="", y= "Energy in kWh") +
          scale_colour_manual(name = "Consumption", values=c("red", "dodgerblue4"))+
          scale_x_datetime(labels = date_format("%b-%d"), breaks=pretty_breaks(n = 32)(as_datetime(dates)))  +
          coord_cartesian(ylim=c(0, 150))+
          theme(panel.grid.minor = element_blank(), 
                panel.grid.major = element_line(colour = "gray", size = 0.1), 
                panel.grid.major.x = element_blank(), 
                panel.background = element_blank(),
                axis.text.y = element_text(colour="black", size = 10),
                axis.text.x = element_text(size = 9, angle = 270, vjust = -1.2),
                axis.ticks.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 20, vjust = 1),
                panel.border = element_rect(colour = "gray", fill= NA, size=0.1),
                legend.background = element_rect(fill = "white", colour = NA),
                legend.text = element_text(size = rel(0.8), color="black" ), 
                legend.text.align = NULL, 
                legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0, color="black"), 
                legend.title.align = NULL)
      }
      

#Functions to obtain error metrics
      
      #We calculate the key performance indicators : MAE, RelativeError, R2 and RMSE. For that purpose the function "myerrorcalculations" is written with the dataframe with the data as an argument. Also
      #function cleanMYdataset carries the process over a copy of the Comparison Dataframe to which it applies the cleaning process step by step and stores the error metrics in a table.
      
      myerrorcalculations <- function(dataframe,...){
        
        #Update the dataframe to obtain the error metrics
        
        dataframe$Consumption[which(is.na(dataframe$Forecast))] <- NA     #Both Vectors need to have the same NAs in order for the R squared to be calculated
        
        dataframe$Dif <- dataframe$Consumption - dataframe$Forecast
        dataframe$RelativeError <- round(dataframe$Dif/dataframe$Consumption*100, digits = 2)
        dataframe$SSE <- round(( dataframe$Consumption -  dataframe$Forecast)^2, digits = 2)
        dataframe$SST <- round(( dataframe$Consumption - mean(dataframe$Consumption, na.rm = TRUE))^2, digits = 2)
        dataframe$SSR <- round(( dataframe$Forecast - mean( dataframe$Consumption, na.rm = TRUE))^2, digits = 2)
        
        
        
        #Mean absolute error (MAE). Is the same as Mean() o Aritmetic average.
        
        errorMAE <- sum(abs(dataframe$Dif), na.rm = TRUE)/(length(which(!is.na(dataframe$Dif))))
        
        #Mean Relative Error. Old RelativeError (Percentage RelativeError from real consumption)
        
        AVGRelativeError <- mean(abs(dataframe$RelativeError), na.rm = TRUE)
        
        #Root Mean Square (RMSE)
        
        RMSe <- sqrt(sum((dataframe$Dif)^2, na.rm = TRUE)/length(which(!is.na(dataframe$Dif))))
        
        #R squared
        
        coeffdet <- cor(dataframe$Consumption, dataframe$Forecast, use = "pairwise.complete.obs" )  ^ 2  # R squared is correlation of the two vectores squared
        
        #Std deviation
        
        stdeviation <- sd(dataframe$Dif, na.rm = TRUE)
        
        #R2 General
        
        rsquaredGEN <- (1 - sum(dataframe$SSE, na.rm = TRUE)/sum(dataframe$SST, na.rm = TRUE))
        
        # Gathering together all the errors into a single error vector
        
        NAsForTOT <- length(which(is.na(dataframe$Forecast)))/length(dataframe$Forecast)*100
        errorsvector <- c(stdeviation, errorMAE, AVGRelativeError, RMSe,  rsquaredGEN, coeffdet, NAsForTOT)
        names(errorsvector) <- c("Std Dev", "MAE (kWh)", "MRE (%)", "RMSE", "R2 General", "Coeff. Det.", "Missed Values (%)" )
        return(errorsvector)
        
      }
      
      myerrorcalculationsBIS <- function(dataframe,n,...){
        
        #Update the dataframe to obtain the error metrics
        
        dataframe$Consumption[which(is.na(dataframe$Forecast))] <- NA     #Both Vectors need to have the same NAs in order for the R squared to be calculated
        
        dataframe$Dif <- dataframe$Consumption - dataframe$Forecast
        dataframe$BPE <- round(dataframe$Dif/dataframe$Consumption*100, digits = 2)
        dataframe$APE <- round(abs(dataframe$Dif)/abs(dataframe$Consumption)*100, digits = 2)
        dataframe$SSE <- round(( dataframe$Consumption -  dataframe$Forecast)^2, digits = 2)
        dataframe$SSR <- round(( dataframe$Forecast - mean( dataframe$Forecast, na.rm = TRUE))^2, digits = 2)
        dataframe$SST <- dataframe$SSE + dataframe$SSR

        #Mean absolute error (MAPE). Is the same as Mean() o Aritmetic average.
        
        errorMAE <- round( sum(abs(dataframe$Dif), na.rm = TRUE)/(length(which(!is.na(dataframe$Dif)))) , digits = 2)
        
        #Mean absolute error (MAPE). Is the same as Mean() o Aritmetic average.
        
        errorMAPE <- round( sum(abs(dataframe$Dif)/dataframe$Consumption*100, na.rm = TRUE)/(length(which(!is.na(dataframe$Dif)))) , digits = 2)
        
        #Mean Biased Error
        
        errorMBPE <- round ( mean(dataframe$BPE, na.rm = TRUE) , digits = 2)
        
        #Root Mean Square (RMSE)
        
        RMSe <- round( sqrt(sum((dataframe$Dif)^2, na.rm = TRUE)/length(which(!is.na(dataframe$Dif)))) , digits = 2)
        
        #R2
        
        rsquaredGEN <- round( (1 - sum(dataframe$SSE, na.rm = TRUE)/sum(dataframe$SST, na.rm = TRUE)) , digits = 2)
        
        # Gathering together all the errors into a single error vector
        
        NAsForTOT <- length(which(is.na(dataframe$Forecast)))/length(dataframe$Forecast)*100
        errorsvectorBIS <- round( c(errorMAE,errorMAPE, errorMBPE, RMSe,  rsquaredGEN, NAsForTOT) , digits = 2)
        names(errorsvectorBIS) <- c("MAE (kWh)", "MAPE (%)", "MBPE (%)", "RMSE (kWh)", "R2", "NAs(%)" )
        
        setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/TablesResults/")
        write.csv(errorsvectorBIS, file = paste(n,"TableResults.csv", sep = "-"))
        return(errorsvectorBIS)
        
        
        
      }
      
      cleanMYdataset <- function(dataframe,...) {
        #------------- RAW
        
        metricasRAW <- myerrorcalculations(dataframe) # Obtain errors in RAW dataset  
        
        #------------- OUTLIERS
        
        differlimit    <- 0.9*max(dataframe$Consumption)
        toEraseOutlier <- which(abs(dataframe$Dif) > differlimit)
        print(toEraseOutlier)
        dataframe$Forecast[toEraseOutlier] <- NA 
        
        metricasOUTLIER <- myerrorcalculations(dataframe) # Obtain errors after first modification (1-. Set Outliers to NAs)
        metricasOUTLIER 
        #------------- ZERO VALUES
        
        toEraseZero <- which(dataframe$Forecast == 0)
        dataframe$Forecast[toEraseZero]    <- NA   
        
        metricasZERO <- myerrorcalculations(dataframe) #  Obtain errors after second modification (2-. Set Zero Values to NAs)
        
        #------------- BELOW BASE LOAD
        
        weekends <- dataframe %>% filter(DayType == "Saturday" | DayType == "DOMINGO")
        myrange <- 0.6
        toEraseUnderBL <- which(dataframe$Forecast > 0 & dataframe$Forecast < (myrange*min(dataframe$Consumption)))
        dataframe$Forecast[toEraseUnderBL] <- NA 
        
        metricasBASELOAD <- myerrorcalculations(dataframe) # Obtain errors after third modification (3-. Set UnderBase Values to NAs)
        
        #------------- HOLIDAYS
        
        detach("package:plyr")
        detectHOLIDAY <- dataframe %>%  
          filter(hours(dataframe$Date) >= 07 & hours(dataframe$Date) < 19) #Filter by workingHours
        detectHOLIDAY <- detectHOLIDAY %>%  
          group_by(Day) %>% 
          summarise(avgcon = mean(Consumption, na.rm = TRUE),
                    avgfor = mean(Forecast, na.rm =TRUE))
        detectHOLIDAY$avgDIF <- detectHOLIDAY$avgfor - detectHOLIDAY$avgcon
        forecastlimit <- 40
        overforecastlimit <- which(detectHOLIDAY$avgDIF > forecastlimit)             #Posicion del vector que nos dirá los días con la AVG > forecastlimit
        myfailHolidaysdates <- detectHOLIDAY[overforecastlimit,]$Day                 #Días detectados como Holidays en los que se ha predecido consumo
        toEraseHoliday <- which(dataframe$Day %in% myfailHolidaysdates)             #Filas del Dataset que ocupan todos estos días.
        library(plyr)
        
        dataframe$Forecast[toEraseHoliday] <- NA
        
        metricasHOLIDAYS <- myerrorcalculations(dataframe) # Obtain errors after third modification (4-. Set HOLIDAYS to NAs)
        
        #------------- STRANGE
        
        strangeforecast <- as.POSIXct(c("2016-12-16","2016-11-04","2016-11-25", "2016-10-17"))
        toEraseSTRANGE  <- which((dataframe$Day %in% strangeforecast & !is.na(dataframe$Forecast ))) #!is.na is needed in order to obtain the proper
        #length of the elements to be erased
        
        dataframe$Forecast[toEraseSTRANGE] <- NA  
        
        metricasSTRANGE <- myerrorcalculations(dataframe) # Obtain errors after third modification (5-. Set STRANGE to NAs)
        
        #------------- ALLTOGTHER
        
        metricasRAW <- as.data.frame(metricasRAW)
        metricasOUTLIER <- as.data.frame(metricasOUTLIER)
        metricasZERO <- as.data.frame(metricasZERO)
        metricasBASELOAD <- as.data.frame(metricasBASELOAD)
        metricasHOLIDAYS <- as.data.frame(metricasHOLIDAYS)
        metricasSTRANGE <- as.data.frame(metricasSTRANGE)
        metricasFINAL <- metricasSTRANGE
        metricasERROR <- cbind(metricasRAW, metricasOUTLIER,metricasZERO, metricasBASELOAD, metricasHOLIDAYS, metricasFINAL)
        names(metricasERROR) <- c("Raw Data", "wo/ Outliers", "wo/ZEROs", "wo/Below BS", "wo/HOLIDAYS", "wo/Strange B")
        metricasERROR <- round(t(metricasERROR), digits = 3)
        return(metricasERROR) 
        
      }
      
      cleanMYdatasetBIS <- function(dataframe,...) {
        #------------- RAW
        
        metricasRAW <- myerrorcalculationsBIS(dataframe) # Obtain errors in RAW dataset  
        
        #------------- OUTLIERS
        
        differlimit    <- 0.9*max(dataframe$Consumption)
        toEraseOutlier <- which(abs(dataframe$Dif) > differlimit)
        print(toEraseOutlier)
        dataframe$Forecast[toEraseOutlier] <- NA 
        
        metricasOUTLIER <- myerrorcalculationsBIS(dataframe) # Obtain errors after first modification (1-. Set Outliers to NAs)
        metricasOUTLIER 
        #------------- ZERO VALUES
        
        toEraseZero <- which(dataframe$Forecast == 0)
        dataframe$Forecast[toEraseZero]    <- NA   
        
        metricasZERO <- myerrorcalculationsBIS(dataframe) #  Obtain errors after second modification (2-. Set Zero Values to NAs)
        
        #------------- BELOW BASE LOAD
        
        weekends <- dataframe %>% filter(DayType == "Saturday" | DayType == "DOMINGO")
        myrange <- 0.6
        toEraseUnderBL <- which(dataframe$Forecast > 0 & dataframe$Forecast < (myrange*min(dataframe$Consumption)))
        dataframe$Forecast[toEraseUnderBL] <- NA 
        
        metricasBASELOAD <- myerrorcalculationsBIS(dataframe) # Obtain errors after third modification (3-. Set UnderBase Values to NAs)
        
        #------------- HOLIDAYS
        
        detach("package:plyr")
        detectHOLIDAY <- dataframe %>%  
          filter(hours(dataframe$Date) >= 07 & hours(dataframe$Date) < 19) #Filter by workingHours
        detectHOLIDAY <- detectHOLIDAY %>%  
          group_by(Day) %>% 
          summarise(avgcon = mean(Consumption, na.rm = TRUE),
                    avgfor = mean(Forecast, na.rm =TRUE))
        detectHOLIDAY$avgDIF <- detectHOLIDAY$avgfor - detectHOLIDAY$avgcon
        forecastlimit <- 40
        overforecastlimit <- which(detectHOLIDAY$avgDIF > forecastlimit)             #Posicion del vector que nos dirá los días con la AVG > forecastlimit
        myfailHolidaysdates <- detectHOLIDAY[overforecastlimit,]$Day                 #Días detectados como Holidays en los que se ha predecido consumo
        toEraseHoliday <- which(dataframe$Day %in% myfailHolidaysdates)             #Filas del Dataset que ocupan todos estos días.
        library(plyr)
        
        dataframe$Forecast[toEraseHoliday] <- NA
        
        metricasHOLIDAYS <- myerrorcalculationsBIS(dataframe) # Obtain errors after third modification (4-. Set HOLIDAYS to NAs)
        
        #------------- STRANGE
        
        strangeforecast <- as.POSIXct(c("2016-12-16","2016-11-04","2016-11-25", "2016-10-17"))
        toEraseSTRANGE  <- which((dataframe$Day %in% strangeforecast & !is.na(dataframe$Forecast ))) #!is.na is needed in order to obtain the proper
        #length of the elements to be erased
        
        dataframe$Forecast[toEraseSTRANGE] <- NA  
        
        metricasSTRANGE <- myerrorcalculationsBIS(dataframe) # Obtain errors after third modification (5-. Set STRANGE to NAs)
        
        #------------- ALLTOGTHER
        
        metricasRAW <- as.data.frame(metricasRAW)
        metricasOUTLIER <- as.data.frame(metricasOUTLIER)
        metricasZERO <- as.data.frame(metricasZERO)
        metricasBASELOAD <- as.data.frame(metricasBASELOAD)
        metricasHOLIDAYS <- as.data.frame(metricasHOLIDAYS)
        metricasSTRANGE <- as.data.frame(metricasSTRANGE)
        metricasFINAL <- metricasSTRANGE
        metricasERRORbis <- cbind(metricasRAW, metricasOUTLIER,metricasZERO, metricasBASELOAD, metricasHOLIDAYS, metricasFINAL)
        names(metricasERRORbis) <- c("Raw Data", "wo/Outliers", "wo/ZVs", "wo/BelowBBL", "wo/HOLIDAYS", "wo/StrangeB")
        metricasERRORbis <- round(t(metricasERRORbis), digits = 2)
        return(metricasERRORbis) 
        
      }


# FUNCTIONS to prepare the RESULTS and graph BOXPLOTs.      
      
     # Script to create a function to compare results of different models.
      #This funnction will have as inputa a dtaset result of carrying out a prediction model and the three columns expected are:
      # Date / Consumption / Forecast
      
      resultsevaluation <- function(dataframe, model,...){
        
        
        dataframe$Forecast <- model$pred
        dataframe$Dif <- dataframe$Consumption - dataframe$Forecast
        dataframe$BPE <- round(dataframe$Dif/dataframe$Consumption*100, digits = 2)
        dataframe$APE <- round(abs(dataframe$Dif)/abs(dataframe$Consumption)*100, digits = 2)
        # dataframe$Seasson <- 0
        # for(i in 1:nrow(dataframe)){
        #   if( dataframe$Date[i] <= "2016-06-20 23:00"){
        #     dataframe$Seasson[i] <- 1
        #   } else if( dataframe$Date[i] > "2016-06-20 23:00" &  dataframe$Date[i] <= "2016-09-22 00:00"){
        #     dataframe$Seasson[i] <- 2
        #   } else if( dataframe$Date[i] > "2016-09-22 00:00" &  dataframe$Date[i] <= "2016-12-21 00:00"){
        #     dataframe$Seasson[i] <- 3
        #   } else if( dataframe$Date[i] > "2016-12-21 00:00" &  dataframe$Date[i] <= "2017-03-20 15:00"){
        #     dataframe$Seasson[i] <- 4
        #   } else if( dataframe$Date[i] > "2017-03-20 15:00" &  dataframe$Date[i] <= "2017-06-21 15:00"){
        #     dataframe$Seasson[i] <- 1
        #   } else if( dataframe$Date[i] > "2017-06-21 15:00" &  dataframe$Date[i] <= "2017-09-22 15:00"){
        #     dataframe$Seasson[i] <- 2
        #   }
        # }
        # 
        # dataframe$Seasson <- as.factor(dataframe$Seasson)
        # #dataframe$Seasson <- factor(dataframe$Seasson,
        #   labels = c("Spring", "Summer", "Autumn", "Winter"))
        
        
        return(dataframe)
        
      }
      
      resultsevaluationANN <- function(dataframe,...){
        
        dataframe$Dif <- dataframe$Consumption - dataframe$Forecast
        dataframe$BPE <- round(dataframe$Dif/dataframe$Consumption*100, digits = 2)
        dataframe$APE <- round(abs(dataframe$Dif)/abs(dataframe$Consumption)*100, digits = 2)
        
        # dataframe <- dataframe[,c(1:5,14,6:13)]
        
        return(dataframe)
        
      }
      
      graphmyresults <- function(dataframe,n,...){
        
        fill <- "#4271AE"
        line <- "#1F3552"
        
        meanAPEcategory  <- aggregate(APE ~  DayCategory, dataframe, mean)
        # meanAPEseasson   <- aggregate(APE ~  Seasson, dataframe, mean)
        meanAPEhour      <- aggregate(APE ~  Hour, dataframe, mean)
        meanAPEmonth     <- aggregate(APE ~  Month, dataframe, mean)
        
        meanBPEcategory  <- aggregate(BPE ~  DayCategory, dataframe, mean)
        # meanBPEseasson   <- aggregate(BPE ~  Seasson, dataframe, mean)
        meanBPEhour      <- aggregate(BPE ~  Hour, dataframe, mean)
        meanBPEmonth     <- aggregate(BPE ~  Month, dataframe, mean)
        
        # BY DAY CATEGORY
        
        APEcategory <- ggplot(dataframe, aes(x = DayCategory, y = APE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 175, 25),
                             limits=c(0, 175)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE by Day Category") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          scale_colour_manual(name = "Consumption") +
          geom_text(data = meanAPEcategory, aes(label = round( APE, digits = 2)), color = "darkred", vjust = -0.5)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        APEcategory
        
        partname <- paste(substr(n,1,3),"/", sep ="") 
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"APEcategory.png", sep = "-"), width = 709, height = 800)
        APEcategory
        print(APEcategory)
        dev.off()
        
        BPEcategory <- ggplot(dataframe, aes(x = DayCategory, y = BPE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(-200, 150, 25),
                             limits=c(-200, 150)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("BPE by Day Category") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          geom_text(data = meanBPEcategory, aes(label = round( BPE, digits = 2)), color = "darkred", vjust = 1.2)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        BPEcategory
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPEcategory.png", sep = "-"), width = 709, height = 800)
        BPEcategory
        print(BPEcategory)
        dev.off()
        #     
        # # BY SEASSON
        # 
        #     APEseasson <- ggplot(dataframe, aes(x = Seasson, y = APE)) +
        #       geom_boxplot(fill = fill, colour = line, alpha = 0.7,
        #                    outlier.colour = "#1F3552", outlier.shape = 20) +
        #       scale_y_continuous(name = "Percentage",
        #                          breaks = seq(0, 175, 25),
        #                          limits=c(0, 175)) +
        #       #scale_x_discrete(name = "Day of the Week") +
        #       ggtitle("APE by Season") +
        #       stat_summary(fun.y=mean, colour="darkred", geom="point", 
        #                    shape=18, size=3,show_guide = FALSE) +
        #       scale_colour_manual(name = "Consumption") +
        #       geom_text(data = meanAPEseasson, aes(label = round( APE, digits = 2)), color = "darkred", vjust = -0.5)+
        #       theme(axis.text.y = element_text(colour="black", size = 12),
        #             axis.text.x = element_text(size = 12, face = "bold"),
        #             axis.title.y = element_text(size=14,face="bold"),
        #             axis.title.x=element_blank(),
        #             plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        #     APEseasson
        #     
        #     setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        #     png(paste(n,"APE-Seasson.png", sep = "-"), width = 709, height = 800)
        #     APEseasson
        #     print(APEseasson)
        #     dev.off()
        #     
        #     BPEseasson <- ggplot(dataframe, aes(x = Seasson, y = BPE)) +
        #       geom_boxplot(fill = fill, colour = line, alpha = 0.7,
        #                    outlier.colour = "#1F3552", outlier.shape = 20) +
        #       scale_y_continuous(name = "Percentage",
        #                          breaks = seq(-200, 150, 25),
        #                          limits=c(-200, 150)) +
        #       #scale_x_discrete(name = "Day of the Week") +
        #       ggtitle("BPE by Season") +
        #       stat_summary(fun.y=mean, colour="darkred", geom="point", 
        #                    shape=18, size=3,show_guide = FALSE) +
        #       geom_text(data = meanBPEseasson, aes(label = round( BPE, digits = 2)), color = "darkred", vjust = 1.2)+
        #       theme(axis.text.y = element_text(colour="black", size = 12),
        #             axis.text.x = element_text(size = 12, face = "bold"),
        #             axis.title.y = element_text(size=14,face="bold"),
        #             axis.title.x=element_blank(),
        #             plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        #     BPEseasson
        #     
        #     setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        #     png(paste(n,"BPE-seasson.png", sep = "-"), width = 709, height = 800)
        #     BPEseasson
        #     print(BPEseasson)
        #     dev.off()
        
        # BY MONTH
        
        APEmonth <- ggplot(dataframe, aes(x = Month, y = APE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 175, 25),
                             limits=c(0, 175)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE by Month") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          scale_colour_manual(name = "Consumption") +
          geom_text(data = meanAPEmonth, aes(label = round( APE, digits = 2)), color = "darkred", vjust = -0.5)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        APEmonth
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"APE-month.png", sep = "-"), width = 709, height = 800)
        APEmonth
        print(APEmonth)
        dev.off()
        
        BPEmonth <- ggplot(dataframe, aes(x = Month, y = BPE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(-200, 150, 25),
                             limits=c(-200, 150)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("BPE by month") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          geom_text(data = meanBPEmonth, aes(label = round( BPE, digits = 2)), color = "darkred", vjust = 1.2)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        BPEmonth
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPE-month.png", sep = "-"), width = 709, height = 800)
        BPEmonth
        print(BPEmonth)
        dev.off()  
        
        
        
        # BY HOUR
        
        APEhour <- ggplot(dataframe, aes(x = Hour, y = APE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 175, 25),
                             limits=c(0, 175)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE by hour") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          scale_colour_manual(name = "Consumption") +
          geom_text(data = meanAPEhour, aes(label = round( APE, digits = 1)), color = "darkred", vjust = -0.5)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, angle = 90,face = "bold", vjust = 0),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        APEhour
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"APE-hour.png", sep = "-"), width = 709, height = 800)
        APEhour
        print(APEhour)
        dev.off()
        
        BPEhour <- ggplot(dataframe, aes(x = Hour, y = BPE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(-200, 150, 25),
                             limits=c(-200, 150)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("BPE by hour") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          geom_text(data = meanBPEhour, aes(label = round( BPE, digits = 1)), color = "darkred", vjust = 1.2)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, angle = 90, face = "bold", vjust = 0),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        BPEhour
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPE-hour.png", sep = "-"), width = 709, height = 800)
        BPEhour
        print(BPEhour)
        dev.off()
        
        # Error Graphs
        
        distributionBPE <- ggplot(dataframe, aes(BPE)) +
          geom_histogram(binwidth = 1, color = line, fill = fill)+
          ggtitle("BPE Distribution") +
          scale_x_continuous(name = "Percentage") +
          scale_y_continuous(name = "Number of instances")+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x= element_text(size=14),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        distributionBPE
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPE-distribution.png", sep = "-"), width = 709, height = 432)
        distributionBPE
        print(distributionBPE)
        dev.off()
        
        distributionMAPE <- ggplot(dataframe, aes(x = Date)) +
          geom_point(aes(y = APE), color = line, fill = fill) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 300, 25),
                             limits=c(0, 300)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE Evolution") +
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        distributionMAPE
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"MAPE-Evolution.png", sep = "-"), width = 709, height = 432)
        distributionMAPE
        print(distributionMAPE)
        dev.off()
        
        wholeperiod <- ggplot(data = dataframe %>% filter(Date > Date2 &  Date <= Date2comparison), aes(x = Date)) +
          geom_line(aes(y = Consumption, colour = "Actual"), size = 0.8) +
          geom_line(aes(y = Forecast, colour = "Forecast"), size = 0.8)
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"Whole-Period.png", sep = "-"), width = 709, height = 432)
        wholeperiod
        print(wholeperiod)
        dev.off()
        
        # finalperiod <- ggplot(data = dataframe %>% filter(Date > Date2comparison &  Date <= Datevalidation), aes(x = Date)) +
        #   geom_line(aes(y = Consumption, colour = "Actual"), size = 0.8) +
        #   geom_line(aes(y = Forecast, colour = "Forecast"), size = 0.8)
        # 
        # setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        # png(paste(n,"Final-Period.png", sep = "-"), width = 709, height = 432)
        # finalperiod
        # print(finalperiod)
        # dev.off()
        # 
        mygraphs <- list(APEcategory, BPEcategory, APEmonth, BPEmonth, APEhour, BPEhour, distributionBPE, distributionMAPE, wholeperiod)
        
        return(mygraphs)
        
      }
      
      graphmyresultsFIVE <- function(dataframe,n,...){
        
        fill <- "#4271AE"
        line <- "#1F3552"
        
        meanAPEcategory  <- aggregate(APE ~  DayCategory, dataframe, mean)
        # meanAPEseasson   <- aggregate(APE ~  Seasson, dataframe, mean)
        meanAPEhour      <- aggregate(APE ~  Hour, dataframe, mean)
        meanAPEmonth     <- aggregate(APE ~  Month, dataframe, mean)
        
        meanBPEcategory  <- aggregate(BPE ~  DayCategory, dataframe, mean)
        # meanBPEseasson   <- aggregate(BPE ~  Seasson, dataframe, mean)
        meanBPEhour      <- aggregate(BPE ~  Hour, dataframe, mean)
        meanBPEmonth     <- aggregate(BPE ~  Month, dataframe, mean)
        
        # BY DAY CATEGORY
        
        APEcategory <- ggplot(dataframe, aes(x = DayCategory, y = APE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 175, 25),
                             limits=c(0, 175)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE by Day Category") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          scale_colour_manual(name = "Consumption") +
          geom_text(data = meanAPEcategory, aes(label = round( APE, digits = 2)), color = "darkred", vjust = -0.5)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        APEcategory
        
        partname <- paste(substr(n,1,3),"/", sep ="") 
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"APEcategory.png", sep = "-"), width = 709, height = 800)
        APEcategory
        print(APEcategory)
        dev.off()
        
        BPEcategory <- ggplot(dataframe, aes(x = DayCategory, y = BPE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(-200, 150, 25),
                             limits=c(-200, 150)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("BPE by Day Category") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          geom_text(data = meanBPEcategory, aes(label = round( BPE, digits = 2)), color = "darkred", vjust = 1.2)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        BPEcategory
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPEcategory.png", sep = "-"), width = 709, height = 800)
        BPEcategory
        print(BPEcategory)
        dev.off()
        #     
        # # BY SEASSON
        # 
        #     APEseasson <- ggplot(dataframe, aes(x = Seasson, y = APE)) +
        #       geom_boxplot(fill = fill, colour = line, alpha = 0.7,
        #                    outlier.colour = "#1F3552", outlier.shape = 20) +
        #       scale_y_continuous(name = "Percentage",
        #                          breaks = seq(0, 175, 25),
        #                          limits=c(0, 175)) +
        #       #scale_x_discrete(name = "Day of the Week") +
        #       ggtitle("APE by Season") +
        #       stat_summary(fun.y=mean, colour="darkred", geom="point", 
        #                    shape=18, size=3,show_guide = FALSE) +
        #       scale_colour_manual(name = "Consumption") +
        #       geom_text(data = meanAPEseasson, aes(label = round( APE, digits = 2)), color = "darkred", vjust = -0.5)+
        #       theme(axis.text.y = element_text(colour="black", size = 12),
        #             axis.text.x = element_text(size = 12, face = "bold"),
        #             axis.title.y = element_text(size=14,face="bold"),
        #             axis.title.x=element_blank(),
        #             plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        #     APEseasson
        #     
        #     setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        #     png(paste(n,"APE-Seasson.png", sep = "-"), width = 709, height = 800)
        #     APEseasson
        #     print(APEseasson)
        #     dev.off()
        #     
        #     BPEseasson <- ggplot(dataframe, aes(x = Seasson, y = BPE)) +
        #       geom_boxplot(fill = fill, colour = line, alpha = 0.7,
        #                    outlier.colour = "#1F3552", outlier.shape = 20) +
        #       scale_y_continuous(name = "Percentage",
        #                          breaks = seq(-200, 150, 25),
        #                          limits=c(-200, 150)) +
        #       #scale_x_discrete(name = "Day of the Week") +
        #       ggtitle("BPE by Season") +
        #       stat_summary(fun.y=mean, colour="darkred", geom="point", 
        #                    shape=18, size=3,show_guide = FALSE) +
        #       geom_text(data = meanBPEseasson, aes(label = round( BPE, digits = 2)), color = "darkred", vjust = 1.2)+
        #       theme(axis.text.y = element_text(colour="black", size = 12),
        #             axis.text.x = element_text(size = 12, face = "bold"),
        #             axis.title.y = element_text(size=14,face="bold"),
        #             axis.title.x=element_blank(),
        #             plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        #     BPEseasson
        #     
        #     setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        #     png(paste(n,"BPE-seasson.png", sep = "-"), width = 709, height = 800)
        #     BPEseasson
        #     print(BPEseasson)
        #     dev.off()
        
        # BY MONTH
        
        APEmonth <- ggplot(dataframe, aes(x = Month, y = APE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 175, 25),
                             limits=c(0, 175)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE by Month") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          scale_colour_manual(name = "Consumption") +
          geom_text(data = meanAPEmonth, aes(label = round( APE, digits = 2)), color = "darkred", vjust = -0.5)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        APEmonth
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"APE-month.png", sep = "-"), width = 709, height = 800)
        APEmonth
        print(APEmonth)
        dev.off()
        
        BPEmonth <- ggplot(dataframe, aes(x = Month, y = BPE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(-200, 150, 25),
                             limits=c(-200, 150)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("BPE by month") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          geom_text(data = meanBPEmonth, aes(label = round( BPE, digits = 2)), color = "darkred", vjust = 1.2)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        BPEmonth
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPE-month.png", sep = "-"), width = 709, height = 800)
        BPEmonth
        print(BPEmonth)
        dev.off()  
        
        
        
        # BY HOUR
        
        APEhour <- ggplot(dataframe, aes(x = Hour, y = APE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 175, 25),
                             limits=c(0, 175)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE by hour") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          scale_colour_manual(name = "Consumption") +
          geom_text(data = meanAPEhour, aes(label = round( APE, digits = 1)), color = "darkred", vjust = -0.5)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, angle = 90,face = "bold", vjust = 0),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        APEhour
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"APE-hour.png", sep = "-"), width = 709, height = 800)
        APEhour
        print(APEhour)
        dev.off()
        
        BPEhour <- ggplot(dataframe, aes(x = Hour, y = BPE)) +
          geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                       outlier.colour = "#1F3552", outlier.shape = 20) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(-200, 150, 25),
                             limits=c(-200, 150)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("BPE by hour") +
          stat_summary(fun.y=mean, colour="darkred", geom="point", 
                       shape=18, size=3,show_guide = FALSE) +
          geom_text(data = meanBPEhour, aes(label = round( BPE, digits = 1)), color = "darkred", vjust = 1.2)+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, angle = 90, face = "bold", vjust = 0),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        BPEhour
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPE-hour.png", sep = "-"), width = 709, height = 800)
        BPEhour
        print(BPEhour)
        dev.off()
        
        # Error Graphs
        
        distributionBPE <- ggplot(dataframe, aes(BPE)) +
          geom_histogram(binwidth = 1, color = line, fill = fill)+
          ggtitle("BPE Distribution") +
          scale_x_continuous(name = "Percentage") +
          scale_y_continuous(name = "Number of instances")+
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x= element_text(size=14),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        distributionBPE
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"BPE-distribution.png", sep = "-"), width = 709, height = 432)
        distributionBPE
        print(distributionBPE)
        dev.off()
        
        distributionMAPE <- ggplot(dataframe, aes(x = Date)) +
          geom_point(aes(y = APE), color = line, fill = fill) +
          scale_y_continuous(name = "Percentage",
                             breaks = seq(0, 300, 25),
                             limits=c(0, 300)) +
          #scale_x_discrete(name = "Day of the Week") +
          ggtitle("APE Evolution") +
          theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size=14,face="bold"),
                axis.title.x=element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
        distributionMAPE
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"MAPE-Evolution.png", sep = "-"), width = 709, height = 432)
        distributionMAPE
        print(distributionMAPE)
        dev.off()
        
        finalperiod <- ggplot(data = dataframe %>% filter(Date > Date2comparison &  Date <= Datevalidation), aes(x = Date)) +
          geom_line(aes(y = Consumption, colour = "Actual"), size = 0.8) +
          geom_line(aes(y = Forecast, colour = "Forecast"), size = 0.8)
        
        setwd(paste0("C:/Users/Alvaro/Dropbox/TESIS/R tesis/graficas/BOXPLOT/", partname, sep=""))
        png(paste(n,"Final-Period.png", sep = "-"), width = 709, height = 432)
        finalperiod
        print(finalperiod)
        dev.off()
        
        mygraphs <- list(APEcategory, BPEcategory, APEmonth, BPEmonth, APEhour, BPEhour, distributionBPE, distributionMAPE, finalperiod)
        
        return(mygraphs)
        
      }
      
      
      
     
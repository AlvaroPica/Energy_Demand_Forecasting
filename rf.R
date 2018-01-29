#SCript to build random Forecast predictions model

      library(randomForest)
      library(caret)
      
 setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
 source("totalconsumption.R")
      
      rfdataframe <- totalconsumption[,c(1:4,6:9,11,13)] 
      
      rfdataframe <- rfdataframe[-which(is.na(rfdataframe$WeekBefore)),]
      rfdataframe$Month <- as.factor(month(rfdataframe$Date))
      
      rfdataframe <- rfdataframe[,c(1:7,9,10,8)] 
      
      rfdataframe <- totalconsumption[,c(1:4,6:9,11,13)] 
      
      rfdataframe <- rfdataframe[-which(is.na(rfdataframe$WeekBefore)),]
      rfdataframe$Month <- as.factor(month(rfdataframe$Date))
      
      rfdataframe <- rfdataframe[,c(1:7,9,10,8)] 
      
      dumsHour.rf <- dummy(rfdataframe$Hour, sep = "_")
      dumsMonth.rf <- dummy(rfdataframe$Month, sep = "_")
      dumsDayTipe.rf <- dummy(rfdataframe$DayType, sep = "_")
      dumsDayCategory.rf <- dummy(rfdataframe$DayCategory, sep = "_")
      
      rfdataframe.dum <- rfdataframe
      rfdataframe.dum <- cbind(rfdataframe.dum, dumsHour.rf, dumsMonth.rf, dumsDayTipe.rf, dumsDayCategory.rf)
      
      rfdataframe.dum <- rfdataframe.dum[,c(1:9,11:56,10)] 
      
  # DATES
      
      Date1 <- "2015-01-01 00:00"
      Date2 <- "2016-05-01 00:00"
      Date3 <- "2017-06-01 00:00"
      Date1comparison <- "2017-01-01 00:00"
      Date2comparison <- "2017-06-01 00:00"
      Datevalidation <- "2017-07-01 00:00"
      Datatraining <- "2016-09-01 00:00"
      
  # MODEL RF 1
      
      rm(.Random.seed, envir=globalenv())
      set.seed(123)
      t.rf.1.idx <- createDataPartition(rfdataframe.dum$Consumption, p = 0.7, list = FALSE)
   
      trg.rf.1     <- rfdataframe.dum[t.rf.1.idx,]
      rest.rf.1    <- rfdataframe.dum[-t.rf.1.idx,]
tic()
      model.rf.1 <- randomForest(x = trg.rf.1[,7:55],
                                 y = trg.rf.1[,56],
                                 ntree=500,
                                 xtest = rest.rf.1[,7:55], 
                                 ytest = rest.rf.1[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
toc()
      checking.rf.1 <- cbind(rest.rf.1, model.rf.1$test[1])
      checking.rf.1 <- checking.rf.1[,c(1:9,56,57)]
      names(checking.rf.1)[11] <- "Forecast"
      checking.rf.1 <- resultsevaluationANN(checking.rf.1)

      ErrorsTable.rf.1 <-  myerrorcalculationsBIS(checking.rf.1,"mRF.1")
      ErrorsTable.rf.1
      
      Graphs.rf.1 <- graphmyresults(checking.rf.1, "mRF.1")
      
     plot(model.rf.1, log = "y")
     plot(model.rf.1 , pch=24, cex.lab=1.5, cex.axis = 1.5)       

      
# MODEL RF 2

      trg.rf.2     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
      rest.rf.2    <- rfdataframe.dum %>% filter(Date >= Date2 &  Date < Date2comparison)
     
      model.rf.2 <- randomForest(x = trg.rf.2[,7:55],
                                 y = trg.rf.2[,56],
                                 ntree=1000,
                                 xtest = rest.rf.2[,7:55], 
                                 ytest = rest.rf.2[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)

      checking.rf.2 <- cbind(rest.rf.2, model.rf.2$test[1])
      checking.rf.2 <- checking.rf.2[,c(1:9,56,57)]
      names(checking.rf.2)[11] <- "Forecast"
      
      checking.rf.2 <- resultsevaluationANN(checking.rf.2)
      
      ErrorsTable.rf.2 <-  myerrorcalculationsBIS(checking.rf.2, "mRF.2")
      ErrorsTable.rf.2
      
      Graphs.rf.2 <- graphmyresults(checking.rf.2, "mrf.2")
     
# MODEL RF 3
      
      trg.rf.3     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date < Date2)
      rest.rf.3    <- rfdataframe.dum %>% filter(Date >= Date1comparison &  Date < Date2comparison)
      
      model.rf.3 <- randomForest(x = trg.rf.3[,7:55],
                                 y = trg.rf.3[,56],
                                 ntree=1000,
                                 xtest = rest.rf.3[,7:55], 
                                 ytest = rest.rf.3[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      
      checking.rf.3 <- cbind(rest.rf.3, model.rf.3$test[1])
      checking.rf.3 <- checking.rf.3[,c(1:9,56,57)]
      names(checking.rf.3)[11] <- "Forecast"
      checking.rf.3 <- resultsevaluationANN(checking.rf.3)
      
      ErrorsTable.rf.3 <-  myerrorcalculationsBIS(checking.rf.3, "mRF.3")
      ErrorsTable.rf.3
      
      Graphs.rf.3 <- graphmyresults(checking.rf.3, "mrf.3")
      
# MODEL RF 4
      
      trg.rf.4     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date < Date1comparison)
      rest.rf.4    <- rfdataframe.dum %>% filter(Date >= Date1comparison &  Date < Date2comparison)
      
      model.rf.4 <- randomForest(x = trg.rf.4[,7:55],
                                 y = trg.rf.4[,56],
                                 ntree=1000,
                                 xtest = rest.rf.4[,7:55], 
                                 ytest = rest.rf.4[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      
      checking.rf.4 <- cbind(rest.rf.4, model.rf.4$test[1])
      checking.rf.4 <- checking.rf.4[,c(1:9,56,57)]
      names(checking.rf.4)[11] <- "Forecast"
      checking.rf.4 <- resultsevaluationANN(checking.rf.4)
      
      ErrorsTable.rf.4 <-  myerrorcalculationsBIS(checking.rf.4, "mrf.4")
      ErrorsTable.rf.4
      
      Graphs.rf.4 <- graphmyresults(checking.rf.4, "mrf.4")
      
# MODEL RF 5
      
      trg.rf.5     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date < Date2comparison)
      rest.rf.5    <- rfdataframe.dum %>% filter(Date >= Date2comparison &  Date < Datevalidation)
      
      model.rf.5 <- randomForest(x = trg.rf.5[,7:55],
                                 y = trg.rf.5[,56],
                                 ntree=1000,
                                 xtest = rest.rf.5[,7:55], 
                                 ytest = rest.rf.5[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      
      checking.rf.5 <- cbind(rest.rf.5, model.rf.5$test[1])
      checking.rf.5 <- checking.rf.5[,c(1:9,56,57)]
      names(checking.rf.5)[11] <- "Forecast"
      checking.rf.5 <- resultsevaluationANN(checking.rf.5)
      
      ErrorsTable.rf.5 <-  myerrorcalculationsBIS(checking.rf.5, "mrf.5")
      ErrorsTable.rf.5
      
      Graphs.rf.5 <- graphmyresultsFIVE(checking.rf.5, "mrf.5")
      Graphs.rf.5[9]
      
# MODEL RF 6
      
      trg.rf.6     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date < Date1comparison)
      rest.rf.6    <- rfdataframe.dum %>% filter(Date >= Date1comparison &  Date < Date2comparison)
      
      coeffs.rf.6 <- round(runif(n = length(rest.rf.6$Temperature), min = -0.2, max = 0.2), digits = 2)
      rest.rf.6$Temperature <- round(rest.rf.6$Temperature * (1-coeffs.rf.6), digits = 0)
      
      model.rf.6 <- randomForest(x = trg.rf.6[,7:55],
                                 y = trg.rf.6[,56],
                                 ntree=1000,
                                 xtest = rest.rf.6[,7:55], 
                                 ytest = rest.rf.6[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      
      checking.rf.6 <- cbind(rest.rf.6, model.rf.6$test[1])
      checking.rf.6 <- checking.rf.6[,c(1:9,56,57)]
      names(checking.rf.6)[11] <- "Forecast"
      checking.rf.6 <- resultsevaluationANN(checking.rf.6)
      
      ErrorsTable.rf.6 <-  myerrorcalculationsBIS(checking.rf.6, "mrf.6")
      ErrorsTable.rf.6
      
      Graphs.rf.6 <- graphmyresultsFIVE(checking.rf.6, "mrf.6")
      Graphs.rf.6[9]
      
# MODEL RF 7
      
      trg.rf.7     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date < Date1comparison)
      rest.rf.7    <- rfdataframe.dum %>% filter(Date >= Date1comparison &  Date < Date2comparison)
      
      coeffs.rf.7 <- round(runif(n = length(rest.rf.7$Temperature), min = -0.20, max = 0.20), digits = 2)
      rest.rf.7$Temperature <- round(rest.rf.7$Temperature * (1-coeffs.rf.7), digits = 0)
      tic()
      model.rf.7 <- randomForest(x = trg.rf.7[,7:55],
                                 y = trg.rf.7[,56],
                                 ntree=1000,
                                 xtest = rest.rf.7[,7:55], 
                                 ytest = rest.rf.7[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      toc()
      checking.rf.7 <- cbind(rest.rf.7, model.rf.7$test[1])
      checking.rf.7 <- checking.rf.7[,c(1:9,56,57)]
      names(checking.rf.7)[11] <- "Forecast"
      checking.rf.7 <- resultsevaluationANN(checking.rf.7)
      
      ErrorsTable.rf.7 <-  myerrorcalculationsBIS(checking.rf.7, "mrf.7")
      ErrorsTable.rf.7
      
      Graphs.rf.7 <- graphmyresultsFIVE(checking.rf.7, "mrf.7")
      names(checking.rf.7)
      tempErrors <- cbind(checking.rf.4[,c(1,7)], checking.rf.7$Temperature)
      names(tempErrors) <- c("Date", "Consumption", "Forecast")
      myerrorcalculationsBIS(tempErrors, "TEMPe")
      
# MODEL RF 8
      
      trg.rf.8     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date < Datatraining)
      rest.rf.8    <- rfdataframe.dum %>% filter(Date >= Date1comparison &  Date < Date2comparison)
      tic()
      model.rf.8 <- randomForest(x = trg.rf.8[,7:55],
                                 y = trg.rf.8[,56],
                                 ntree=1000,
                                 xtest = rest.rf.8[,7:55], 
                                 ytest = rest.rf.8[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      toc()
      checking.rf.8 <- cbind(rest.rf.8, model.rf.8$test[1])
      checking.rf.8 <- checking.rf.8[,c(1:9,56,57)]
      names(checking.rf.8)[11] <- "Forecast"
      checking.rf.8 <- resultsevaluationANN(checking.rf.8)
      
      ErrorsTable.rf.8 <-  myerrorcalculationsBIS(checking.rf.8, "mrf.8")
      ErrorsTable.rf.8
      
      Graphs.rf.8 <- graphmyresultsFIVE(checking.rf.8, "mrf.8")
      Graphs.rf.8[9]
      
# MODEL RF 9. Study of Working Calendar influence
      
      rfdataframe.WC <- totalconsumption[,c(1:4,6:9,11,13)] 
      
      rfdataframe.WC <- rfdataframe.WC[-which(is.na(rfdataframe.WC$WeekBefore)),]
      rfdataframe.WC$Month <- as.factor(month(rfdataframe.WC$Date))
      
      rfdataframe.WC <- rfdataframe.WC[,c(1:7,9,10,8)] 
      
      rfdataframe.WC <- totalconsumption[,c(1:4,6:9,11,13)] 
      
      rfdataframe.WC <- rfdataframe.WC[-which(is.na(rfdataframe.WC$WeekBefore)),]
      rfdataframe.WC$Month <- as.factor(month(rfdataframe.WC$Date))
      
      rfdataframe.WC <- rfdataframe.WC[,c(1:7,9,10,8)] 
      
      dumsHour.WC.rf <- dummy(rfdataframe.WC$Hour, sep = "_")
      dumsMonth.WC.rf <- dummy(rfdataframe.WC$Month, sep = "_")
      dumsDayTipe.WC.rf <- dummy(rfdataframe.WC$DayType, sep = "_")
      dumsDayCategory.WC.rf <- dummy(rfdataframe.WC$DayCategory, sep = "_")
      
      rfdataframe.WC.dum <- rfdataframe.WC
      rfdataframe.WC.dum <- cbind(rfdataframe.WC.dum, dumsHour.WC.rf, dumsMonth.WC.rf, dumsDayTipe.WC.rf, dumsDayCategory.WC.rf)
      
      rfdataframe.WC.dum <- rfdataframe.WC.dum[,c(1:9,11:56,10)] 
      
      trg.rf.9     <- rfdataframe.WC.dum %>% filter(Date >= Date1 &  Date < Date2)
      rest.rf.9    <- rfdataframe.WC.dum %>% filter(Date >= Date2 &  Date < Date2comparison)
      tic()
      model.rf.9 <- randomForest(x = trg.rf.9[,7:55],
                                 y = trg.rf.9[,56],
                                 ntree=1000,
                                 xtest = rest.rf.9[,7:55], 
                                 ytest = rest.rf.9[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      toc()
      checking.rf.9 <- cbind(rest.rf.9, model.rf.9$test[1])
      checking.rf.9 <- checking.rf.9[,c(1:9,56,57)]
      names(checking.rf.9)[11] <- "Forecast"
      checking.rf.9 <- resultsevaluationANN(checking.rf.9)
      
      ErrorsTable.rf.9 <-  myerrorcalculationsBIS(checking.rf.9, "mrf.9")
      ErrorsTable.rf.9
      
      Graphs.rf.9 <- graphmyresultsFIVE(checking.rf.9, "mrf.9")
      Graphs.rf.9[9]
      
  # MODEL RF 10 . Training with 70/30 rue till 1st June 2017
      
      rm(.Random.seed, envir=globalenv())
      set.seed(123)
      rfdataframe.rf.10 <- rfdataframe.dum %>%  filter (Date >= Date2 & Date < Date2comparison)
      t.rf.10.idx <- createDataPartition(rfdataframe.rf.10$Consumption, p = 0.7, list = FALSE)
      
      trg.rf.10     <- rfdataframe.rf.10[t.rf.10.idx,]
      rest.rf.10    <- rfdataframe.rf.10[-t.rf.10.idx,]
      tic()
      model.rf.10 <- randomForest(x = trg.rf.10[,7:55],
                                 y = trg.rf.10[,56],
                                 ntree=1000,
                                 xtest = rest.rf.10[,7:55], 
                                 ytest = rest.rf.10[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      toc()
      checking.rf.10 <- cbind(rest.rf.10, model.rf.10$test[1])
      checking.rf.10 <- checking.rf.10[,c(1:9,56,57)]
      names(checking.rf.10)[11] <- "Forecast"
      checking.rf.10 <- resultsevaluationANN(checking.rf.10)
      
      ErrorsTable.rf.10 <-  myerrorcalculationsBIS(checking.rf.10,"mrf.10")
      ErrorsTable.rf.10
      
      Graphs.rf.10 <- graphmyresults(checking.rf.10, "mrf.10")
      
# MODEL RF 11. Training from 01/01/2015 to 30/06/2017. Predict 01/07/2017 to 30/09/2017
      
      Date1 <- "2015-01-01 00:00"
      Datevalidation <- "2017-07-01 00:00"
      Datelast <- "2017-10-01 00:00"
      
      
      trg.rf.11     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date <  Datevalidation)
      rest.rf.11    <- rfdataframe.dum %>% filter(Date >= Datevalidation &  Date < Datelast)
      
      model.rf.11 <- randomForest(x = trg.rf.11[,7:55],
                                 y = trg.rf.11[,56],
                                 ntree=1000,
                                 xtest = rest.rf.11[,7:55], 
                                 ytest = rest.rf.11[,56], 
                                 importance=TRUE, 
                                 keep.forest=TRUE)
      
      checking.rf.11 <- cbind(rest.rf.11, model.rf.11$test[1])
      checking.rf.11 <- checking.rf.11[,c(1:9,56,57)]
      names(checking.rf.11)[11] <- "Forecast"
      
      checking.rf.11 <- resultsevaluationANN(checking.rf.11)
      
      ErrorsTable.rf.11 <-  myerrorcalculationsBIS(checking.rf.11, "mrf.11")
      ErrorsTable.rf.11
      
      
# MODEL RF 12. Year Consumpton forecast
      
      Date1 <- "2015-01-01 00:00"
      Date2 <- "2016-01-01 00:00"
      Datelast <- "2017-01-01 00:00"
      
      
      trg.rf.12     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
      rest.rf.12    <- rfdataframe.dum %>% filter(Date >= Date2 &  Date < Datelast)
      
      model.rf.12 <- randomForest(x = trg.rf.12[,7:55],
                                  y = trg.rf.12[,56],
                                  ntree=1000,
                                  xtest = rest.rf.12[,7:55], 
                                  ytest = rest.rf.12[,56], 
                                  importance=TRUE, 
                                  keep.forest=TRUE)
      
      checking.rf.12 <- cbind(rest.rf.12, model.rf.12$test[1])
      checking.rf.12 <- checking.rf.12[,c(1:9,56,57)]
      names(checking.rf.12)[11] <- "Forecast"
      
      checking.rf.12 <- resultsevaluationANN(checking.rf.12)
      
      str(checking.rf.12)
      View(checking.rf.12)
      
      anualResults <- colSums(checking.rf.12[,10:11])
      
      anualDif <- round((anualResults[1]-anualResults[2])/anualResults[1]*100, digits = 1)
      anualDif
      
      ErrorsTable.rf.12 <-  myerrorcalculationsBIS(checking.rf.12, "mrf.12")
      ErrorsTable.rf.12
      
      
# MODEL RF 13. 2017 Prediction. With Temperature and Autoregressive parameters.
      
      Date1 <- "2015-01-01 00:00"
      Date2 <- "2017-01-01 00:00"
      Datelast <- "2017-10-01 00:00"
      
      
      trg.rf.13     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
      rest.rf.13    <- rfdataframe.dum %>% filter(Date >= Date2 &  Date < Datelast)
      
      model.rf.13 <- randomForest(x = trg.rf.13[,7:55],
                                  y = trg.rf.13[,56],
                                  ntree= 1000,
                                  xtest = rest.rf.13[,7:55], 
                                  ytest = rest.rf.13[,56], 
                                  importance=TRUE, 
                                  keep.forest=TRUE)
      
      checking.rf.13 <- cbind(rest.rf.13, model.rf.13$test[1])
      checking.rf.13 <- checking.rf.13[,c(1:9,56,57)]
      names(checking.rf.13)[11] <- "Forecast"
      
      checking.rf.13 <- resultsevaluationANN(checking.rf.13)
      head(checking.rf.13[,10:11])
      
      Results2017 <- colSums(checking.rf.13[,10:11])
      Dif2017 <- round((Results2017[1]-Results2017[2])/Results2017[1]*100, digits = 1)
      Dif2017
      
      ErrorsTable.rf.13 <-  myerrorcalculationsBIS(checking.rf.13, "mrf.13")
      ErrorsTable.rf.13
      
# MODEL RF 14. Train 2016. Forecast 2015. With T and AR parameters.
      
      Date1 <- "2016-01-01 00:00"
      Date2 <- "2017-01-01 00:00"
      Datelast <- "2015-01-01 00:00"
      
      trg.rf.14     <- rfdataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
      rest.rf.14    <- rfdataframe.dum %>% filter(Date >= Datelast &  Date < Date1)
      
      model.rf.14 <- randomForest(x = trg.rf.14[,7:55],
                                  y = trg.rf.14[,56],
                                  ntree=500,
                                  xtest = rest.rf.14[,7:55], 
                                  ytest = rest.rf.14[,56], 
                                  importance=TRUE, 
                                  keep.forest=TRUE)
      
      checking.rf.14 <- cbind(rest.rf.14, model.rf.14$test[1])
      checking.rf.14 <- checking.rf.14[,c(1:9,56,57)]
      names(checking.rf.14)[11] <- "Forecast"
      
      checking.rf.14 <- resultsevaluationANN(checking.rf.14)
      head(checking.rf.14[,10:11])
      
      Results2015 <- colSums(checking.rf.14[,10:11])
      Dif2017 <- round((Results2015[1]-Results2015[2])/Results2015[1]*100, digits = 1)
      Dif2017
      
      ErrorsTable.rf.14 <-  myerrorcalculationsBIS(checking.rf.14, "mrf.14")
      ErrorsTable.rf.14

# MODEL RF 15. Year Consumpton forecast
      
      Date1 <- "2015-01-01 00:00"
      Date2 <- "2016-01-01 00:00"
      Datelast <- "2017-01-01 00:00"
      
      rfdataframe.anual <- rfdataframe.dum[,-c(7,8,9)]
      str(rfdataframe.anual)
      names(rfdataframe.anual)
      
      trg.rf.15     <- rfdataframe.anual %>% filter(Date >= Date1 &  Date <  Date2)
      rest.rf.15    <- rfdataframe.anual %>% filter(Date >= Date2 &  Date < Datelast)
      
      model.rf.15 <- randomForest(x = trg.rf.15[,7:52],
                                  y = trg.rf.15[,53],
                                  ntree=1000,
                                  xtest = rest.rf.15[,7:52], 
                                  ytest = rest.rf.15[,53], 
                                  importance=TRUE, 
                                  keep.forest=TRUE)
      
      checking.rf.15 <- cbind(rest.rf.15, model.rf.15$test[1])
      str(checking.rf.15)
      checking.rf.15 <- checking.rf.15[,c(1:6,53,54)]
      names(checking.rf.15)[8] <- "Forecast"
      
      checking.rf.15 <- resultsevaluationANN(checking.rf.15)
      
      str(checking.rf.15)
      View(checking.rf.15)
      
      anualResults <- colSums(checking.rf.15[,7:8])
      
      anualDif <- round((anualResults[1]-anualResults[2])/anualResults[1]*100, digits = 1)
      anualDif
      
      ErrorsTable.rf.15 <-  myerrorcalculationsBIS(checking.rf.15, "mrf.15")
      ErrorsTable.rf.15
      
# MODEL RF 16. 2017 Prediction with NO temeperature, NO autoregressive parameters.
      
      Date1 <- "2015-01-01 00:00"
      Date2 <- "2017-01-01 00:00"
      Datelast <- "2017-10-01 00:00"
      
      rfdataframe.anual <- rfdataframe.dum[,-c(7,8,9)]
      str(rfdataframe.anual)
      names(rfdataframe.anual)
      
      trg.rf.16     <- rfdataframe.anual %>% filter(Date >= Date1 &  Date <  Date2)
      rest.rf.16    <- rfdataframe.anual %>% filter(Date >= Date2 &  Date < Datelast)
      
      model.rf.16 <- randomForest(x = trg.rf.16[,7:52],
                                  y = trg.rf.16[,53],
                                  ntree=1000,
                                  xtest = rest.rf.16[,7:52], 
                                  ytest = rest.rf.16[,53], 
                                  importance=TRUE, 
                                  keep.forest=TRUE)
      
      checking.rf.16 <- cbind(rest.rf.16, model.rf.16$test[1])
      str(checking.rf.16)
      checking.rf.16 <- checking.rf.16[,c(1:6,53,54)]
      names(checking.rf.16)[8] <- "Forecast"
      
      checking.rf.16 <- resultsevaluationANN(checking.rf.16)
      
      str(checking.rf.16)
      View(checking.rf.16)
      
      anualResults <- colSums(checking.rf.16[,7:8])
      
      anualDif <- round((anualResults[1]-anualResults[2])/anualResults[1]*100, digits = 1)
      anualDif
      
      ErrorsTable.rf.16 <-  myerrorcalculationsBIS(checking.rf.16, "mrf.16")
      ErrorsTable.rf.16
      
      
# MODEL RF 17. 2017 Prediction , Train 2016. with NO temeperature, NO autoregressive parameters.
      
      Date1 <- "2015-01-01 00:00"
      Date2 <- "2017-01-01 00:00"
      Datelast <- "2017-01-01 00:00"
      Datelast2 <- "2017-10-01 00:00"
      
      rfdataframe.anual <- rfdataframe.dum[,-c(7,8,9)]
      str(rfdataframe.anual)
      names(rfdataframe.anual)
      
      trg.rf.17     <- rfdataframe.anual %>% filter(Date >= Date1 &  Date <  Date2)
      rest.rf.17    <- rfdataframe.anual %>% filter(Date >= Datelast &  Date < Datelast2)
      
      model.rf.17 <- randomForest(x = trg.rf.17[,7:52],
                                  y = trg.rf.17[,53],
                                  ntree= 500,
                                  xtest = rest.rf.17[,7:52], 
                                  ytest = rest.rf.17[,53], 
                                  importance=TRUE, 
                                  keep.forest=TRUE)
      
      checking.rf.17 <- cbind(rest.rf.17, model.rf.17$test[1])
      str(checking.rf.17)
      checking.rf.17 <- checking.rf.17[,c(1:6,53,54)]
      names(checking.rf.17)[8] <- "Forecast"
      
      checking.rf.17 <- resultsevaluationANN(checking.rf.17)
      
      str(checking.rf.17)
      View(checking.rf.17)
      
      anualResults <- colSums(checking.rf.17[,7:8])
      
      anualDif <- round((anualResults[1]-anualResults[2])/anualResults[1]*100, digits = 1)
      anualDif
      
      ErrorsTable.rf.17 <-  myerrorcalculationsBIS(checking.rf.17, "mrf.17")
      ErrorsTable.rf.17
      
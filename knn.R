# Script for k-NN using regresion

##We will build a knn model to predict CONSUMPTION #based on all other predictors except Date.

library(caret)
library(scales)
library(FNN)

setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
source("totalconsumption.R")

knndataframe <- totalconsumption[,c(1:4,6:9,11,13)] 

knndataframe <- knndataframe[-which(is.na(knndataframe$WeekBefore)),]
knndataframe$Month <- as.factor(month(knndataframe$Date))

knndataframe <- knndataframe[,c(1:7,9,10,8)] 

dumsHour.knn <- dummy(knndataframe$Hour, sep = "_")
dumsMonth.knn <- dummy(knndataframe$Month, sep = "_")
dumsDayTipe.knn <- dummy(knndataframe$DayType, sep = "_")
dumsDayCategory.knn <- dummy(knndataframe$DayCategory, sep = "_")

knndataframe.dum <- knndataframe
knndataframe.dum <- cbind(knndataframe.dum, dumsHour.knn, dumsMonth.knn, dumsDayTipe.knn, dumsDayCategory.knn)

knndataframe.dum <- knndataframe.dum[,c(1:9,11:56,10)] 

knndataframe.dum$Temperature <- rescale(knndataframe.dum$Temperature)
knndataframe.dum$DayBefore <- rescale(knndataframe.dum$DayBefore)
knndataframe.dum$WeekBefore <- rescale(knndataframe.dum$WeekBefore)

#Up to here we have prepared the dataset to Apply k NN model. We have deleted all the NAs so it work. It is pending that we can add all the temperatures creating
#a model with the temperatures of the local stations and local data. So far we just erase NAs contiaing rows.

#Model 1. Following instructions of cookbook. This is training with a RANDOM SAMPLE. Not arranged by TIME. IMplication?

        rm(.Random.seed, envir=globalenv())
        set.seed(123)
        t.knn.1.idx <- createDataPartition(knndataframe.dum$Consumption, p = 0.7, list = FALSE)
        trg.kNN.1     <- knndataframe.dum[t.knn.1.idx,]
        rest.kNN.1    <- knndataframe.dum[-t.knn.1.idx,]
       
        rm(.Random.seed, envir=globalenv())
        set.seed(123)
        v.knn.1.idx <- createDataPartition(rest.kNN.1$Consumption, p = 0.5, list = FALSE)
        val.kNN.1 <- rest.kNN.1[v.knn.1.idx,]
        test.kNN.1 <- rest.kNN.1[-v.knn.1.idx,]
        
        model.kNN.1   <- knn.reg(trg.kNN.1[, c(7:55)], rest.kNN.1[, c(7:55)], trg.kNN.1[,56], 2, algorithm = "brute")
        
        checking.kNN.1 <- resultsevaluation(rest.kNN.1, model.kNN.1)
        checking.kNN.1 <- checking.kNN.1[,c(1:9,56:60)]
        
        ErrorsTable.kNN.1 <-  myerrorcalculationsBIS(checking.kNN.1,"kNN.1")
        ErrorsTable.kNN.1
        
        Graphs.kNN.1 <- graphmyresults(checking.kNN.1, "kNN.1")

# DATES
        
        Date1 <- "2015-01-01 00:00"
        Date2 <- "2016-05-01 00:00"
        Date3 <- "2017-06-01 00:00"
        Date1comparison <- "2017-01-01 00:00"
        Date2comparison <- "2017-06-01 00:00"
        Datevalidation <- "2017-07-01 00:00"
        
# kNN MODEL 2
       
       trg.kNN.2     <- knndataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
       rest.kNN.2    <- knndataframe.dum %>% filter(Date >= Date2 &  Date < Date2comparison)
       
       model.kNN.2   <- knn.reg(trg.kNN.2[, c(7:55)], rest.kNN.2[, c(7:55)], trg.kNN.2[,56], 2, algorithm = "brute")
      
       checking.kNN.2 <- resultsevaluation(rest.kNN.2, model.kNN.2)
       checking.kNN.2 <- checking.kNN.2[,c(1:9,56:60)]
       
       ErrorsTable.kNN.2 <-  myerrorcalculationsBIS(checking.kNN.2, "kNN.2")
       ErrorsTable.kNN.2
       
       Graphs.kNN.2 <- graphmyresults(checking.kNN.2, "kNN.2")
       
# kNN MODEL 3
       
       trg.kNN.3     <- knndataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
       rest.kNN.3    <- knndataframe.dum %>% filter(Date >= Date1comparison &  Date < Date2comparison)
       
       model.kNN.3   <- knn.reg(trg.kNN.3[, c(7:55)], rest.kNN.3[, c(7:55)], trg.kNN.3[,56], 2, algorithm = "brute")
       
       checking.kNN.3 <- resultsevaluation(rest.kNN.3, model.kNN.3)
       checking.kNN.3 <- checking.kNN.3[,c(1:9,56:60)]
       
       ErrorsTable.kNN.3 <-  myerrorcalculationsBIS(checking.kNN.3, "kNN.3")
       ErrorsTable.kNN.3
       
       Graphs.kNN.3 <- graphmyresults(checking.kNN.3, "kNN.3")
    
       
# kNN MODEL 4 
       
       trg.kNN.4     <- knndataframe.dum %>% filter(Date >= Date1 &  Date < Date1comparison)
       rest.kNN.4    <- knndataframe.dum %>% filter(Date >= Date1comparison &  Date < Date2comparison)
       
       model.kNN.4   <- knn.reg(trg.kNN.4[, c(7:55)], rest.kNN.4[, c(7:55)], trg.kNN.4[,56], 2, algorithm = "brute")
       
       checking.kNN.4 <- resultsevaluation(rest.kNN.4, model.kNN.4)
       checking.kNN.4 <- checking.kNN.4[,c(1:9,56:60)]
       
       ErrorsTable.kNN.4 <-  myerrorcalculationsBIS(checking.kNN.4, "kNN.4")
       ErrorsTable.kNN.4
       
       Graphs.kNN.4 <- graphmyresults(checking.kNN.4, "kNN.4")
      
       
# kNN MODEL 5
       
       trg.kNN.5     <- knndataframe.dum %>% filter(Date >= Date1 &  Date < Date2comparison)
       rest.kNN.5    <- knndataframe.dum %>% filter(Date >= Date2comparison &  Date < Datevalidation)
       
       model.kNN.5   <- knn.reg(trg.kNN.5[, c(7:55)], rest.kNN.5[, c(7:55)], trg.kNN.5[,56], 2, algorithm = "brute")
       
       checking.kNN.5 <- resultsevaluation(rest.kNN.5, model.kNN.5)
       checking.kNN.5 <- checking.kNN.5[,c(1:9,56:60)]
       
       ErrorsTable.kNN.5 <-  myerrorcalculationsBIS(checking.kNN.5, "kNN.5")
       ErrorsTable.kNN.5
       
       Graphs.kNN.5 <- graphmyresultsFIVE(checking.kNN.5, "kNN.5")
       Graphs.kNN.5[9]
#SCript to build random ANN (Feed-Forward) models

library(nnet)
library(devtools)
library(caret)

setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/")
source("totalconsumption.R")

# Data Set Preparation
      
      anndataframe <- totalconsumption[,c(1:4,6:9,11,13)] 
      
      anndataframe <- anndataframe[-which(is.na(anndataframe$WeekBefore)),]
      anndataframe$Month <- as.factor(month(anndataframe$Date))
      
      anndataframe <- anndataframe[,c(1:7,9,10,8)] 
      
      dumsHour.ann <- dummy(anndataframe$Hour, sep = "_")
      dumsMonth.ann <- dummy(anndataframe$Month, sep = "_")
      dumsDayTipe.ann <- dummy(anndataframe$DayType, sep = "_")
      dumsDayCategory.ann <- dummy(anndataframe$DayCategory, sep = "_")
      
      anndataframe.dum <- anndataframe
      anndataframe.dum <- cbind(anndataframe.dum, dumsHour.ann, dumsMonth.ann, dumsDayTipe.ann, dumsDayCategory.ann)
    
      anndataframe.dum <- anndataframe.dum[,c(1:9,11:56,10)] 

# ANN MODEL 1
      
      rm(.Random.seed, envir=globalenv())
      set.seed(123)
      training.ann.1.idx <- createDataPartition(anndataframe.dum$Consumption, p = 0.7, list = FALSE)
      training.ann.1 <- anndataframe.dum[training.ann.1.idx,]
      testing.ann.1  <- anndataframe.dum[-training.ann.1.idx,]

      model.ann.1 <- nnet(Consumption/152 ~ ., data = training.ann.1[, c(7:55,56)], size = 6, decay = 0.1, maxit = 1000, linout = TRUE)

      predictions.ann.1 <- predict(model.ann.1,testing.ann.1)
      predictions.ann.1 <- predictions.ann.1*152
      
      checking.ann.1 <- cbind(testing.ann.1, predictions.ann.1)
      checking.ann.1 <- checking.ann.1[,c(1:9,56,57)]
      names(checking.ann.1)[11] <- "Forecast"

      checking.ann.1 <- resultsevaluationANN(checking.ann.1)

      ErrorsTable.ann.1 <-  myerrorcalculationsBIS(checking.ann.1,"ann.1")
      ErrorsTable.ann.1
      
      Graphs.ann.1 <- graphmyresults(checking.ann.1, "ANN.1")

# DATES
      
      Date1 <- "2015-01-01 00:00"
      Date2 <- "2016-05-01 00:00"
      Date3 <- "2017-06-01 00:00"
      Date1comparison <- "2017-01-01 00:00"
      Date2comparison <- "2017-06-01 00:00"
      Datevalidation <- "2017-07-01 00:00"
      
      
# ANN MODEL 2
      
      training.ann.2 <- anndataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
      testing.ann.2  <- anndataframe.dum %>% filter(Date >= Date2 &  Date < Date2comparison)
      
      # inventedT <- round(runif(n = length(testing.ann.2$Temperature), min = -0.2, max = 0.2), digits = 2)
      # testing.ann.2$Temperature <- testing.ann.2$Temperature * (1-inventedT)

      model.ann.2 <- nnet(Consumption/152 ~ ., data = training.ann.2[, c(7:55,56)], size = 6, decay = 0.1, maxit = 1000, linout = TRUE)
      
      predictions.ann.2 <- predict(model.ann.2,testing.ann.2)
      predictions.ann.2 <- predictions.ann.2*152
      
      checking.ann.2 <- cbind(testing.ann.2, predictions.ann.2)
      checking.ann.2 <- checking.ann.2[,c(1:9,56,57)]
      names(checking.ann.2)[11] <- "Forecast"

      checking.ann.2 <- resultsevaluationANN(checking.ann.2)

      ErrorsTable.ann.2 <-  myerrorcalculationsBIS(checking.ann.2, "ann.2")
      ErrorsTable.ann.2
      
      rfGraphs.ann.2 <- graphmyresults(checking.ann.2, "ann.2")
      Graphs.ann.2[8]
 
# ANN MODEL 3
      
      training.ann.3 <- anndataframe.dum %>% filter(Date >= Date1 &  Date <  Date2)
      testing.ann.3  <- anndataframe.dum %>% filter(Date >= Date1comparison & Date <  Date2comparison)
      
      model.ann.3 <- nnet(Consumption/152 ~ ., data = training.ann.3[, c(7:55,56)], size = 6, decay = 0.1, maxit = 1000, linout = TRUE)
      
      predictions.ann.3 <- predict(model.ann.3,testing.ann.3)
      predictions.ann.3 <- predictions.ann.3*152
      
      checking.ann.3 <- cbind(testing.ann.3, predictions.ann.3)
      checking.ann.3 <- checking.ann.3[,c(1:9,56,57)]
      names(checking.ann.3)[11] <- "Forecast"

      checking.ann.3 <- resultsevaluationANN(checking.ann.3)

      ErrorsTable.ann.3 <-  myerrorcalculationsBIS(checking.ann.3,"ann.3")
      ErrorsTable.ann.3
      
      Graphs.ann.3 <- graphmyresults(checking.ann.3, "ann.3")
      
# ANN MODEL 4
      
      training.ann.4 <- anndataframe.dum %>% filter(Date >= Date1 &  Date <  Date1comparison)
      testing.ann.4  <- anndataframe.dum %>% filter(Date >= Date1comparison & Date <  Date2comparison)
      
      model.ann.4 <- nnet(Consumption/152 ~ ., data = training.ann.4[, c(7:55,56)], size = 6, decay = 0.1, maxit = 1000, linout = TRUE)
      
      predictions.ann.4 <- predict(model.ann.4,testing.ann.4)
      predictions.ann.4 <- predictions.ann.4*152
      
      checking.ann.4 <- cbind(testing.ann.4, predictions.ann.4)
      checking.ann.4 <- checking.ann.4[,c(1:9,56,57)]
      names(checking.ann.4)[11] <- "Forecast"
      
      checking.ann.4 <- resultsevaluationANN(checking.ann.4)
      
      ErrorsTable.ann.4 <-  myerrorcalculationsBIS(checking.ann.4,"ann.4")
      ErrorsTable.ann.4
      
      Graphs.ann.4 <- graphmyresults(checking.ann.4, "ann.4")
     

# ANN MODEL 5
      
      training.ann.5 <- anndataframe.dum %>% filter(Date >= Date1 &  Date <  Date2comparison)
      testing.ann.5  <- anndataframe.dum %>% filter(Date >= Date2comparison & Date <  Datevalidation)
      
      model.ann.5 <- nnet(Consumption/152 ~ ., data = training.ann.5[, c(7:55,56)], size = 6, decay = 0.1, maxit = 1000, linout = TRUE)
      
      predictions.ann.5 <- predict(model.ann.5,testing.ann.5)
      predictions.ann.5 <- predictions.ann.5*152
      
      checking.ann.5 <- cbind(testing.ann.5, predictions.ann.5)
      checking.ann.5 <- checking.ann.5[,c(1:9,56,57)]
      names(checking.ann.5)[11] <- "Forecast"
      
      checking.ann.5 <- resultsevaluationANN(checking.ann.5)
      
      ErrorsTable.ann.5 <-  myerrorcalculationsBIS(checking.ann.5,"ann.5")
      ErrorsTable.ann.5
      
      Graphs.ann.5 <- graphmyresultsFIVE(checking.ann.5, "ann.5")

# PLOTS 
      
      source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/c720af2cea5f312717f020a09946800d55b8f45b/nnet_plot_update.r')
      plot(model.ann.1, max.sp = TRUE)
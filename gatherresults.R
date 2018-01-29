
setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/TablesResults")
allfiles <- list.files()
mytableresults <- list()

library(RColorBrewer)

for(i in 1:length(allfiles)){
  nameresult <- substr(allfiles[i],1,5)
  myresult <- setuptableresult(allfiles[i])
  mytableresults[[nameresult]] = myresult
  rm(myresult,i,nameresult)
}

setuptableresult <- function(n,...){
  setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/TablesResults")
  thisfile <- paste(n)
  dataframe <- read.csv(file = thisfile , sep = ",", header = FALSE)[3:6,]
  names(dataframe) = c("Errors", paste(substr(thisfile,1,5)))
  return(dataframe)
}

finalresults <- Reduce(function(...) merge(..., all=T), mytableresults)
finalresults

setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/TablesResults/")
write.csv(finalresults, file = "FINAL.csv")

# COMPARISON OFF ALL MODELS in EVALUATION PERIOD 01/06/2017 00:00 to 01/07/2017 00:00 with TRAINING UP TO 01/06/2017

setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/Datos/LAST/")
summer2017 <- setupmyforecast("summer2017.CSV")
View(summer2017)
myerrorcalculationsBIS(summer2017[2:2203,], "SUMME")

finaldataframe <- as.data.frame(cbind(paste(checking.rf.11$Day), paste(checking.rf.11$Hour), checking.rf.11$Consumption, checking.rf.11$Forecast, summer2017$Forecast[2:2203]))
colnames(finaldataframe) <- c("Day", "Hour", "Consumption", "mRF", "PREDICTION")

finaldataframe$Date <- paste(as.character(finaldataframe$Day), as.character(finaldataframe$Hour), sep = " ")
finaldataframe$Date <- as.POSIXct(finaldataframe$Date, format = "%Y-%m-%d %H:%M")
finaldataframe <- finaldataframe[,c(6,3,4,5)]

finaldataframe$Consumption <- round(as.numeric(as.character(finaldataframe$Consumption)), digits = 1)
finaldataframe$mRF <- round(as.numeric(as.character(finaldataframe$mRF)), digits = 1)
finaldataframe$PREDICTION <- round(as.numeric(as.character(finaldataframe$PREDICTION)), digits = 1)

setwd("C:/Users/Alvaro/Dropbox/TESIS/R tesis/TablesResults/")
write.csv(finaldataframe, file = "FINALjune.csv")

DateA <- "2017-06-05 00:00"
DateB <- "2017-06-12 00:00"
DateC <- "2017-06-19 00:00"
DateD <- "2017-06-26 00:00"
DateE <- "2017-07-03 00:00"
DateD <- "2017-07-10 00:00"


finalgraph2 <- ggplot(data = finaldataframe %>% filter(Date >=  DateE &  Date < DateD), aes(x = Date)) +
  geom_line(aes(y = Consumption, colour = "Actual",     linetype = "Actual"),      size = 0.8    ) +
  geom_line(aes(y = PREDICTION,  colour = "PREDICTION", linetype = "PREDICTION" ),    size = 0.8    ) +
  geom_line(aes(y = mRF,         colour = "mRF",        linetype = "mRF"),   size = 0.8  )   +
  scale_colour_manual(name = "Model Color", values = brewer.pal(3, "Set2"))+
  scale_linetype_manual(name = "Model Line",values=c( "solid","dashed","dotted")) +
  scale_y_continuous(name = "Energy Consumption in kWh") +
  ggtitle(" Summer 2017 - 3rd to 10th July 2017 ") +
  theme(axis.text.y = element_text(colour="black", size = 12),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
finalgraph2




finalfinal <- checking.rf.11[which(checking.rf.11$APE > 100),c(1,4:11)]
finalfinal$Forecast <- round((finalfinal$Forecast), digits = 0)

View(finalfinal
     )

  
  scale_y_continuous(name = "Percentage",
                     breaks = seq(-200, 150, 25),
                     limits=c(-200, 150)) +
  #scale_x_discrete(name = "Day of the Week") +
  ggtitle("BPE by day of the week") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) +
  geom_text(data = meanBPEweekday, aes(label = round( BPE, digits = 2)), color = "darkred", vjust = 1.2)+
  theme(axis.text.y = element_text(colour="black", size = 12),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, face="bold"))
  
  
#YEARLY RESULTS FROM MODEL RANDOM FOREST 12
  
  

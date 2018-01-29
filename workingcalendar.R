#OFFICIAL HOLIDAYS

      holidays2015fd <- c("2015-01-01", "2015-01-06",
                          "2015-04-03", "2015-04-06",
                          "2015-05-01",
                          "2015-06-01", "2015-06-24",
                          "2015-08-15", 
                          "2015-09-11", "2015-09-24",
                          "2015-10-12", 
                          "2015-12-07","2015-12-08","2015-12-25", "2015-12-28", "2015-12-29", "2015-12-30", "2015-12-31")
      holidays2015fd <- as.POSIXct(holidays2015fd)
      
      priorholidays2015fd <- c("2015-01-05",
                                    "2015-04-02",
                                    "2015-12-24")
      priorholidays2015fd <- as.POSIXct(priorholidays2015fd)
      
      #7th of december they did PUENTE as the last week of DECEMBER
      #30th of April not included because once checked the consumption looks like normal working day
      #31 may 2015 sunday
      #23rd june 2015, wednesday but they worked full time
      #14th august was already friday, semiworkindday
      #10th, 23rd september they worked full time
      #11th, october sunday
      
      holidays2016fd <- c("2016-01-01",
                          "2016-01-06",
                          "2016-02-12",
                          "2016-03-25",
                          "2016-03-28",
                          "2016-05-16",
                          "2016-06-24",
                          "2016-08-15",
                          "2016-10-12", "2016-10-31", #15 Octobe is Saturday
                          "2016-11-01",
                          "2016-12-06", "2016-12-08", "2016-12-09","2016-12-26","2016-12-27", "2016-12-28", "2016-12-29", "2016-12-30")
      holidays2016fd <- as.POSIXct(holidays2016fd)
      
      priorholidays2016fd <- c("2016-01-05")
      priorholidays2016fd <- as.POSIXct(priorholidays2016fd)
      
      #1st January , 6th January, -> From semiwroking day to OFFDAY
      #12th February -> From semiwroking day to OFFDAY
      #25th,March -> From semiwroking day to OFFDAY
      #28th March -> From workingday to OFFDAY
      
      holidays2017fd <- c("2017-01-06",
                          "2017-04-14", "2017-04-17",
                          "2017-05-01",
                          "2017-06-05", "2017-06-19",
                          "2017-08-15",
                          "2017-09-11", "2017-09-25", 
                          "2017-10-12",
                          "2017-11-01",
                          "2017-12-06", "2017-12-07", "2017-12-08", "2017-12-25","2017-12-26", "2017-12-27", "2017-12-28", "2017-12-29")
      holidays2017fd <- as.POSIXct(holidays2017fd)
      
      priorholidays2017fd <- c("2017-01-05",
                               "2017-04-13")
      priorholidays2017fd <- as.POSIXct(priorholidays2017fd)
      
      #1st January , 6th January, -> From semiwroking day to OFFDAY
      #12th February -> From semiwroking day to OFFDAY
      #25th,March -> From semiwroking day to OFFDAY
      #28th March -> From workingday to OFFDAY
      
      holidaysTOTAL <- c(holidays2015fd,holidays2016fd, holidays2017fd)
      priorholidaysTOTAL <- c(priorholidays2015fd,priorholidays2016fd, priorholidays2017fd)

      
      
      
      View(totalconsumption)
      
      
       semiholidays2015fd <- c("2015-07-20", 
                              "2015-07-21",
                              "2015-07-22",
                              "2015-07-23",
                              "2015-07-27",
                              "2015-07-28",
                              "2015-07-29",
                              "2015-07-30",
                              "2015-08-03",
                              "2015-08-04",
                              "2015-08-05",
                              "2015-08-06",
                              "2015-08-10",
                              "2015-08-11",
                              "2015-08-12",
                              "2015-08-13",
                              "2015-08-17",
                              "2015-08-18",
                              "2015-08-19",
                              "2015-08-20",
                              "2015-08-25",
                              "2015-08-26",
                              "2015-08-27",
                              "2015-08-31")
      semiholidays2015fd <- as.POSIXct(semiholidays2015fd)
      
      semiholidays2016fd <- c("2016-07-18", 
                              "2016-07-19",
                              "2016-07-20",
                              "2016-07-21",
                              "2016-07-25",
                              "2016-07-26",
                              "2016-07-27",
                              "2016-07-28",
                              "2016-08-01",
                              "2016-08-02",
                              "2016-08-03",
                              "2016-08-04",
                              "2016-08-08",
                              "2016-08-09",
                              "2016-08-10",
                              "2016-08-11",
                              "2016-08-16",
                              "2016-08-17",
                              "2016-08-18",
                              "2016-08-22",
                              "2016-08-23",
                              "2016-08-24",
                              "2016-08-25",
                              "2016-08-29",
                              "2016-08-30",
                              "2016-08-31")
      semiholidays2016fd <- as.POSIXct(semiholidays2016fd)
      
      semiholidays2017fd <- c("2017-07-21", 
                              "2017-07-24",
                              "2017-07-25",
                              "2017-07-26",
                              "2017-07-27",
                              "2017-07-28",
                              "2017-07-31",
                              "2017-08-01",
                              "2017-08-02",
                              "2017-08-03",
                              "2017-08-04",
                              "2017-08-07",
                              "2017-08-08",
                              "2017-08-09",
                              "2017-08-10",
                              "2017-08-11",
                              "2017-08-14",
                              "2017-08-16",
                              "2017-08-17",
                              "2017-08-18",
                              "2017-08-21",
                              "2017-08-22",
                              "2017-08-23",
                              "2017-08-24",
                              "2017-08-25",
                              "2017-08-28",
                              "2017-08-29",
                              "2017-08-30",
                              "2017-08-31")
      semiholidays2017fd <- as.POSIXct(semiholidays2017fd)
      
      semiholidaysTOTAL <- c(semiholidays2015fd,semiholidays2016fd, semiholidays2017fd)
      
      
#PERIODO ESTIVAL (THE WORKING HOURS AND THE DURATION ARE DIFFERENT)
      
      #We ignore this days and treat them as fully working days. Maybe we should study that the hourly schedule will be dfferent, expecting the
      #consumption to increase earlier these days.

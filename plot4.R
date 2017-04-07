## Week 1 Assignment ##
## Juan Martin Trejo Arellano ##
## Script filename: plot4.R

library(data.table)
library(lubridate)
library(dplyr)

## Set up initial parameters
linux_wd <- "/home/martin-trejo/Dropbox/Coursera_Specializations/Data_Science_Specialization/04_Exploratory_Data_Analytics/Week1/week1_assignment"
win_wd <- "C://Users//Martin-Trejo//Dropbox//Coursera_Specializations//Data_Science_Specialization//04_Exploratory_Data_Analytics//Week1//week1_assignment"
setwd(win_wd)
getwd()

## Download Data 
dataURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
compressedFileName <- "exdata_data_Fhousehold_power_consumption.zip"
dataFileName <- "household_power_consumption.txt"
dataDirName <- "exdata_data_Fhousehold_power_consumption"
if (!dir.exists("./data")){dir.create("./data")}

if (!file.exists(paste0("./data/", compressedFileName))){
  download.file(dataURL, paste0("./data/", compressedFileName), method = "curl")
}

## Unizip compressed file
unzip(paste0("./data/", compressedFileName), exdir =paste0(getwd(),"/data")) 

## Read in data

strDate1 <- ymd("2007-02-01")
strDate2 <- ymd("2007-02-02")
listofLines <- list()  

print ("Reading file ...")

wholeDataTable <- fread(file = paste0("./data/", dataFileName), sep =";", 
                        na.strings = "?", stringsAsFactors = FALSE)
print("Finished reading file ...")

filteredDT <- mutate(wholeDataTable, Date = dmy(Date))
filteredDT <- subset(filteredDT, Date %in% c(strDate1 : strDate2))

plotFigure <- function (){
  
  par(mfcol= c(2,2))
  plot(filteredDT$Global_active_power, type = "l", xaxt = "n",
       ylab = "Global Active Power", xlab ="")
  axis(1, at= c(grep(unique(weekdays(filteredDT$Date))[1],weekdays(filteredDT$Date))[1],
                grep(unique(weekdays(filteredDT$Date))[2],weekdays(filteredDT$Date))[1],
                length(filteredDT$Date)), c(unique(weekdays(filteredDT$Date, abbreviate =TRUE)),
                                            "Sat"))
  
  
  plot(filteredDT$Sub_metering_1, type="n", xaxt = "n", 
       ylab="Energy sub metering", xlab ="")
  points(filteredDT$Sub_metering_1, type = "l", col = "black")
  points(filteredDT$Sub_metering_2, type = "l", col = "red") 
  points(filteredDT$Sub_metering_3, type = "l", col = "blue") 
  legend("topright", lty= c(1,1,1), col = c("black", "red", "blue"), 
         legend= c("Sub_meteting_1", "Sub_meteting_2", "Sub_meteting_3"), 
        box.lwd= -1 )
  
  axis(1, at= c(grep(unique(weekdays(filteredDT$Date))[1],weekdays(filteredDT$Date))[1],
                grep(unique(weekdays(filteredDT$Date))[2],weekdays(filteredDT$Date))[1],
                length(filteredDT$Date)), c(unique(weekdays(filteredDT$Date, abbreviate =TRUE)),
                                            "Sat"))
  
  
  plot(filteredDT$Voltage, type="n", xaxt = "n", 
       ylab="Voltage", xlab ="datetime")
  points(filteredDT$Voltage, type = "l")
  
  axis(1, at= c(grep(unique(weekdays(filteredDT$Date))[1],weekdays(filteredDT$Date))[1],
                grep(unique(weekdays(filteredDT$Date))[2],weekdays(filteredDT$Date))[1],
                length(filteredDT$Date)), c(unique(weekdays(filteredDT$Date, abbreviate =TRUE)),
                                            "Sat"))
  
  
  plot(filteredDT$Global_reactive_power, type="n", xaxt = "n", 
       ylab="Global_reactive_power", xlab ="datetime")
  points(filteredDT$Global_reactive_power, type = "l")
  
  axis(1, at= c(grep(unique(weekdays(filteredDT$Date))[1],weekdays(filteredDT$Date))[1],
                grep(unique(weekdays(filteredDT$Date))[2],weekdays(filteredDT$Date))[1],
                length(filteredDT$Date)), c(unique(weekdays(filteredDT$Date, abbreviate =TRUE)),
                                            "Sat"))
  
}

plotFigure ()

png('plot4.png', width = 480, height = 480, units = "px")
plotFigure()
dev.off()







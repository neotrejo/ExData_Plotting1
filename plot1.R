## Week 1 Assignment ##
## Juan Martin Trejo Arellano ##
## Script filename: plot1.R

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
  
  library(dplyr)
  
  filteredDT <- mutate(wholeDataTable, Date = dmy(Date))
  filteredDT <- subset(filteredDT, Date %in% c(strDate1 : strDate2))
  
  plotFigure <- function (){
    
    par(mfrow=c(1,1))
    hist(filteredDT$Global_active_power, col = "red", breaks = 16,
         main = "Global Active Power",
         xlab = "Global Active Power (killowatts)", xlim = c(0, 6), ylim = c(0, 1200))
  }
  plotFigure ()
  
  png('plot1.png', width = 480, height = 480, units = "px")
  plotFigure()
  dev.off()
  
  
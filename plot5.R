## Exploratory Data Analysis - Course Project 2
## Plot5.R
##  How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

library(dplyr)  
library(ggplot2)
library(grid)

## change working directory
setwd("~/rprogramming/ExploratoryDataAnalysis/ExDataProject2")

## Read datasets into memory, 1st file is BIG and may take a while
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Subset data to show only emissions from Baltimore, fips = "24510"
NEI_Balt <- filter(NEI, NEI$fips == "24510")

## Subset SCC values for motor vehicle sources
SCC_Motor <- filter(SCC, grepl("Veh",SCC$Short.Name) & grepl("Mobile", SCC$EI.Sector))[1]

## Initialize NEI_Motor
NEI_Motor <- data.frame(matrix(ncol = 6))
colnames(NEI_Motor) <- names(NEI_Balt)

## Subset NEI down to motor vehicle sources
x <- match(NEI_Balt$SCC,SCC_Motor$SCC)
y <- which(!is.na(x))
NEI_Motor <- NEI_Balt[y,]

## Set up data matrix, two columns for Emissions and Year
MotorEmissions <- data.frame(matrix(nrow = 4, ncol = 2))
colnames(MotorEmissions) <- c("Emissions", "Year")

MotorEmissions$Year <- unique(NEI_Balt$year)
for(i in 1:4) {
        ## Sum the total coal emissions for each year
        MotorEmissions$Emissions[i] <- sum(filter(NEI_Balt, NEI_Balt$year == MotorEmissions$Year[i])$Emissions)
}

## Set up the plot
titl <- "Motor vehicle emissions have decreased 
        in Baltimore City from 1999 to 2008"

qplot(Year, Emissions, data = MotorEmissions, geom = c("point", "smooth"), 
        method = "lm", main = titl) + 
        scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
        scale_y_continuous(limits=c(0,5000)) +
        theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        labs(y = "Motor Vehicle Emissions")

## copy the screen to a png file
dev.copy(png, file = "plot5.png")
dev.off()  ## close the png device
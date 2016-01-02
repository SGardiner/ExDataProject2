## Exploratory Data Analysis - Course Project 2
## Plot6.R
##  Compare emissions from motor vehicle sources in Baltimore City with emissions from 
## motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?

library(dplyr)  
library(ggplot2)
library(grid)
library(gridExtra)

## change working directory
setwd("~/rprogramming/ExploratoryDataAnalysis/ExDataProject2")

## Read datasets into memory, 1st file is BIG and may take a while
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Subset data to show only emissions from Baltimore, fips = "24510" and
## Los Angeles County (fips = "06037")
NEI_Balt_LA <- filter(NEI, fips == "24510" | fips == "06037")

## Subset SCC values for motor vehicle sources
SCC_Motor <- filter(SCC, grepl("Veh",SCC$Short.Name) & grepl("Mobile", SCC$EI.Sector))[1]

## Initialize NEI_Motor
NEI_Motor <- data.frame(matrix(ncol = 6))
colnames(NEI_Motor) <- names(NEI_Balt_LA)

## Subset NEI down to motor vehicle sources
x <- match(NEI_Balt_LA$SCC,SCC_Motor$SCC)
y <- which(!is.na(x))
NEI_Motor <- NEI_Balt_LA[y,]

## Set up data matrix, three columns for Emissions, Year and fips
MotorEmissions <- data.frame(matrix(nrow = 8, ncol = 3))
colnames(MotorEmissions) <- c("Emissions", "Year", "Location")

## Populate fields for Years and Location (Baltimore = "24510", LA County = "06037")
MotorEmissions$Year <- rep(unique(NEI_Balt$year),2)
MotorEmissions$Location <- c(rep("Baltimore",4),rep("LA County",4))

for(i in 1:4) {
        ## Sum the total coal emissions for each year and each location
        MotorEmissions$Emissions[i] <- sum(filter(NEI_Balt_LA, 
                year == MotorEmissions$Year[i] & fips == "24510")$Emissions)
        MotorEmissions$Emissions[i + 4] <- sum(filter(NEI_Balt_LA, 
                year == MotorEmissions$Year[i + 4] & fips == "06037")$Emissions)
}

## Set up the plot
titl = "Motor vehicle emissions in LA County are more than 10x the emissions in 
        Baltimore City.  While the overall emissions for LA County have decreased 
        significantly more, the regression line is steeper for Baltimore City,
        suggesting it is dropping faster as an overall percentage there."

## Plot on the left for Baltimore
p1 <- qplot(Year, filter(MotorEmissions, Location == "Baltimore")$Emissions, 
        geom = c("point", "smooth"), method = "lm") +
        scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
        labs(y = "Motor Vehicle Emissions")

## Plot on the right for LA County
p2 <- qplot(Year, filter(MotorEmissions, Location == "LA County")$Emissions, 
        geom = c("point", "smooth"), method = "lm") +
        scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
        labs(y = "Motor Vehicle Emissions")

## Two column, one row plot
grid.arrange(p1, p2, ncol = 2, top = titl)

## copy the screen to a png file
dev.copy(png, file = "plot6.png")
dev.off()  ## close the png device
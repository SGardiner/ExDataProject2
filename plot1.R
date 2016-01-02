## Exploratory Data Analysis - Course Project 2
## Plot1.R
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission from 
## all sources for each of the years 1999, 2002, 2005, and 2008.

library(dplyr)  

## change working directory
setwd("~/rprogramming/ExploratoryDataAnalysis/ExDataProject2")

## Read datasets into memory, 1st file is BIG and may take a while
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Set up data matrix, two columns for Emissions and Year
PM25 <- data.frame(matrix(nrow = 4, ncol = 2))
colnames(PM25) <- c("Emissions", "Year")

## Populate the data frame with Emissions for each Year
PM25$Year <- unique(NEI$year)
for(i in 1:4) {
        ## Sum the total PM25 emissions for each year
        PM25$Emissions[i] <- sum(filter(NEI, NEI$year == PM25$Year[i])$Emissions)
}

## Set up the plot
par(mfrow=c(1,1), bg = "white", mar = c(5,5,4,2))
titl <- "PM2.5  has decreased in the US from 1999 to 2008"

## Plot Emissions vs. Year
plot(PM25$Year, PM25$Emissions, xaxt="n", yaxt="n", xlab = "Year", ylab = "PM25 Emissions", 
     main = titl, cex.main = 1, ylim = c(3000000,8000000), pch = 19, col = "blue")
## set axis tick marks
axis(1, PM25$Year, cex.axis = 0.8)
axis(2, c(2000000,4000000,6000000,8000000), cex.axis = 0.8)
## plot regression line
abline(lm(PM25$Emissions ~ PM25$Year), lwd = 2, col = "red")

## copy the screen to a png file
dev.copy(png, file = "plot1.png")
dev.off()  ## close the png device








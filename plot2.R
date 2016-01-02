## Exploratory Data Analysis - Course Project 2
## Plot2.R
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
## (fips == "24510") from 1999 to 2008? Use the base plotting system to make 
## a plot answering this question.

library(dplyr)  

## change working directory
setwd("~/rprogramming/ExploratoryDataAnalysis/ExDataProject2")

## Read datasets into memory, 1st file is BIG and may take a while
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Subset data to show only emissions from Baltimore, fips = "24510"
NEI_Balt <- filter(NEI, NEI$fips == "24510")

## Set up data matrix, two columns for Emissions and Year
PM25_Balt <- data.frame(matrix(nrow = 4, ncol = 2))
colnames(PM25_Balt) <- c("Emissions", "Year")

PM25_Balt$Year <- unique(NEI_Balt$year)
for(i in 1:4) {
        ## Sum the total PM25 emissions for each year
        PM25_Balt$Emissions[i] <- sum(filter(NEI_Balt, 
                NEI_Balt$year == PM25_Balt$Year[i])$Emissions)
}

## Set up the plot
par(mfrow=c(1,1), bg = "white", mar = c(5,5,4,2))
titl <- "PM2.5  has decreased in Batimore City 
        from 1999 to 2008"
plot(PM25_Balt$Year, PM25_Balt$Emissions, xaxt="n", yaxt="n",
        xlab = "Year", ylab = "PM25 Emissions", main = titl, cex.main = 1, 
        col = "blue", pch = 19, ylim= c(1000,4000))

## set axis tick marks
axis(1, PM25_Balt$Year, cex.axis = 0.8)
axis(2, c(1000,2000,3000,4000), cex.axis = 0.8)

## plot regression line
abline(lm(PM25_Balt$Emissions ~ PM25_Balt$Year), lwd = 2, col = "red")

## copy the screen to a png file
dev.copy(png, file = "plot2.png")
dev.off()  ## close the png device

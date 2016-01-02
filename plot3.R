## Exploratory Data Analysis - Course Project 2
## Plot3.R
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
## variable, which of these four sources have seen decreases in emissions from 1999–2008 
## for Baltimore City? Which have seen increases in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.

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

## Set up data frame and looping variables
Year <- unique(NEI_Balt$year)
Type <- unique(NEI_Balt$type)
PM25type <- data.frame(matrix(nrow = length(Year)*length(Type), ncol = 3))
colnames(PM25type) <- c("Emissions", "Year", "Type")
n = 1

for(i in 1:length(Type)) {
        for(j in 1:length(Year)) {
                PM25type$Year[n] <- Year[j]
                PM25type$Type[n] <- Type[i]
                PM25type$Emissions[n] <- sum(filter(NEI_Balt, year == PM25type$Year[n] & 
                        type == PM25type$Type[n])$Emissions)
                n <- n + 1
        }
}

titl = "For Baltimore City, MD, PM2.5 Emissions have decreased for types: 
        Non-Road, NonPoint and On-Road.  Emission trends for type Point 
        are somewhat inconclusive, but possibly have increased."
qplot(Year, Emissions, data = PM25type, facets = .~Type, geom = c("point", "smooth"), 
        method = "lm", main = titl, cex.main = 0.8) +
        theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        theme(plot.title=element_text(size=12, vjust=1)) +
        theme(axis.text.x= element_text(angle = 90)) +
        labs(y = expression(PM[2.5] * " Emissions"))
       

## copy the screen to a png file
dev.copy(png, file = "plot3.png")
dev.off()  ## close the png device
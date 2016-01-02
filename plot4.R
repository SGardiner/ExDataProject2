## Exploratory Data Analysis - Course Project 2
## Plot4.R
## Across the United States, how have emissions from coal combustion-related sources 
## changed from 1999–2008?

library(dplyr)  
library(ggplot2)
library(grid)

## change working directory
setwd("~/rprogramming/ExploratoryDataAnalysis/ExDataProject2")

## Read datasets into memory, 1st file is BIG and may take a while
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Subset SCC values for coal combustion-related sources
SCC_Coal <- filter(SCC, grepl("Coal",SCC$Short.Name) & grepl("Comb", SCC$Short.Name))[1]

## Initialize NEI_Coal
NEI_Coal <- data.frame(matrix(ncol = 6))
colnames(NEI_Coal) <- names(NEI)

## Subset NEI down to coal combustion sources
x <- match(NEI$SCC,SCC_Coal$SCC)
y <- which(!is.na(x))
NEI_Coal <- NEI[y,]

## Set up data matrix, two columns for Emissions and Year
CoalEmissions <- data.frame(matrix(nrow = 4, ncol = 2))
colnames(CoalEmissions) <- c("Emissions", "Year")

CoalEmissions$Year <- unique(NEI$year)
for(i in 1:4) {
        ## Sum the total coal emissions for each year
        CoalEmissions$Emissions[i] <- sum(filter(NEI, NEI$year == CoalEmissions$Year[i])$Emissions)
}

## Plot the data
titl <- "Across the US, emissions from coal combustion-related 
        sources have decreased from 1999 to 2008"

qplot(Year, Emissions, data = CoalEmissions, geom = c("point", "smooth"), 
        method = "lm", main = titl) +
        labs(y = "Coal Emissions") +
        scale_x_continuous(breaks = CoalEmissions$Year) + 
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
        theme(plot.title=element_text(size=12, vjust=1))

## copy the screen to a png file
dev.copy(png, file = "plot4.png")
dev.off()  ## close the png device
# ====================================================================================
# Australian Airline Passenger Time Series
# Analysis by Jon Wright @citywright
# Data from Bureau of Infrastructure, Transport and Regional Economics (BITRE)
# https://bitre.gov.au/publications/ongoing/domestic_airline_activity-time_series.aspx
# Accessed on 27 July 2016
# ====================================================================================

# This script relies on the following packages already being installed:

##library(ggplot2)
##library(lubridate)
##library(reshape2)
##library(dplyr)

# ------------------------------------------------------------------------------------
# NOTE: Due to OSx Yosemite Java error in the package "xlsx", I have downloaded the 
# data manually and converted the file manually to .csv format.  Note that only the 
# tab called "Top Routes" in each spreadsheet is used in this analysis. This script 
# assumes each .csv file is already in the local working directory. (When I have a
# little more time on my hands, I'll fix up the Java error and rewrite the download
# script so that this pulls directly from the web.)
# ------------------------------------------------------------------------------------



# -- Import -- #

# Download the following file and save tab "Top Routes" as .csv file in working directory.
# "https://bitre.gov.au/publications/ongoing/files/Domestic_aviation_activity_TopRoutesJuly2004May2016.xls"

air_1984_1994 <- read.csv(file="domestic_airline_activity_Toproutes_Jan84-Jun94.csv",
                          skip =  6, stringsAsFactors = FALSE)
air_1994_2004 <- read.csv(file="domestic_airline_activity_TopRoutesJuly1994June2004.csv",
                          skip =  7, stringsAsFactors = FALSE)
air_2004_2016 <- read.csv(file="Domestic_aviation_activity_TopRoutesJuly2004May2016.csv",
                          skip = 11, stringsAsFactors = FALSE)



# -- Clean Up -- #

# Remove blank rows in the headers and only select monthly data (not moving annual totals)

air01 <- air_1984_1994[-c(1:3),]
air02 <- air_1994_2004[-1,c(1:10)]
air03 <- air_2004_2016[-1,c(1:10)]
airfull <- rbind(air01,air02,air03)

# Clean up colunm names
colnames(airfull)[1:2]   <- c("City_1","City_2")
colnames(airfull)[5:8]   <- c("RevPax","AircraftTrips","LoadFactorPCT","DistanceKM")

# Convert numbers stored as strings to integers
for (j in seq(5,10)) {
    airfull[,j] <- as.numeric(gsub(",","",airfull[,j]))
}

# Convert Origin, Destination fields to factors
for (j in c(1,2)) {
    airfull[,j] <- as.factor(airfull[,j])
}


# -- Process -- #

# Convert Year and Month into single date field (for x-axis of plots)
airfull$LongDate <- ymd(with(airfull,paste(Year,Month,"1",sep="/")))

# Create CityPair field as hyphenated pair of City_1 and City_2
airfull$CityPair <- with(airfull, paste(City_1,City_2,sep="-"))

# Create RevPax field in thousands, for plotting purposes
airfull$RevPaxK <- airfull$RevPax/1000

# Create LoadFactor field in actual percentage
airfull$LoadFactor <- airfull$LoadFactorPCT/100



# -- Plots -- #

# The plot thickens...
png(filename = "airline_pax_full.png", height = 480, width = 720, bg="white")
pax <- ggplot(airfull, aes(x=LongDate, y=RevPaxK, color=CityPair)) + geom_line(alpha=0.5) +
        ggtitle("Monthly Domestic Airline Passengers by City Pair, 1984-2016") + 
        xlab("Date") + ylab("Passengers in Thousands") + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              legend.position = "none") +
        geom_text(aes(x=airfull$LongDate[20380], y=750, label="SYD-MEL"), color="black")
plot(pax)
dev.off()

flt <- ggplot(airfull, aes(x=LongDate, y=AircraftTrips, color=CityPair)) + geom_line() +
        ggtitle("Monthly Domestic Airline Flights by City Pair, 1984-2016") + 
        xlab("Date") + ylab("Numnber of Flights") +  
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              legend.position = "none")

lfp <- ggplot(airfull, aes(x=LongDate, y=LoadFactorPCT, color=CityPair)) + geom_line() +
        ggtitle("Monthly Domestic Airline Load Factor by City Pair, 1984-2016") + 
        xlab("Date") + ylab("Numnber of Flights") +  
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              legend.position = "none")
        
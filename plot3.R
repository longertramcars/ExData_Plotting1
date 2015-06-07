HPC_Plot3<-function(filename, listOfDates){
    
    ## To reproduced the exact plots requested for this assignment, use the
    ## Electric power consumption data provided: 
    ## (filename<-"household_power_consumption.txt") 
    ## and the dates specified in the insutructions:
    ## (listOfDates<- c("1/2/2007", "2/2/2007"))
    
    ## read in data
    ## remove incomplete datasets by filtering out "?"
    ## respecify that quantitative measurement columns are numeric
    hpc<-read.table(filename, sep=";", header=TRUE,na.strings = "?", 
                    colClasses=c("character","character",rep("numeric",7)))
    
    
    ## subset data based on specific list of dates of interest
    data<-hpc[hpc$Date %in% listOfDates,]
    
    ## combine Date and Time columns into one column and convert to POSIXlt
    dateTime<-as.POSIXlt(paste(as.Date(data$Date, format="%d/%m/%Y"), data$Time, sep=" "))

    
    ##-- Plot 3: Sub metering vs Time --##
    png("plot3.png")
    
    #set margins 
    par(mar=c(4,4,4,2))
    
    #make empty base plot
    plot(dateTime,data$Sub_metering_1, xlab="", ylab="Energy sub metering", type="n")
    
    #add data to base plot
    points(dateTime,data$Sub_metering_1, col="black", type="l")
    points(dateTime,data$Sub_metering_2, col="red", type="l")
    points(dateTime,data$Sub_metering_3, col="blue", type="l")
    
    legend("topright", pch="-", col=c("black", "red", "blue"), 
           legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
    
    dev.off()

    
}
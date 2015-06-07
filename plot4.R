HPC_Plot4<-function(filename, listOfDates){
    
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

    
    ##-- Plot 4: multiple plots  --##
    png("plot4.png")
    
    #set margins and plot layout
    par(mar=c(4,4,4,2))
    par(mfrow=c(2,2))
    
    #Topleft: GAP vs Time
    plot(dateTime, data$Global_active_power, xlab="",
         ylab="Global Active Power", type="l",cex.axis=0.9)
    
    #Topright: Voltage vs Time
    plot(dateTime, data$Voltage, xlab="",
         ylab="Voltage", type="l",cex.axis=0.9)
    
    #Bottomleft: Sub metering vs Time   
    plot(dateTime,data$Sub_metering_1, xlab="", ylab="Energy sub metering", type="n")
    points(dateTime,data$Sub_metering_1, col="black", type="l")
    points(dateTime,data$Sub_metering_2, col="red", type="l")
    points(dateTime,data$Sub_metering_3, col="blue", type="l")
    legend("topright", pch="-", bty="n", col=c("black", "red", "blue"), 
           legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
    
    #Bottomright:GRP vs Time
    plot(dateTime, data$Global_reactive_power, xlab="",
         ylab="Global Reactive Power", type="l",cex.axis=0.9)
    
    dev.off()
    
}
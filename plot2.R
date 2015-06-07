HPC_Plot2<-function(filename, listOfDates){
    
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

    
    ##-- Plot 2: Global Active Power vs Time --##
    png("plot2.png")
    
    #set margins 
    par(mar=c(4,4,4,2))
    
    plot(dateTime, data$Global_active_power, xlab="",
         ylab="Global Active Power (kilowatts)", type="l",cex.axis=0.9)
    
    dev.off()

    
}

# This function reads houshold consumption data, filters it to specific dates, and explore the data with the
# base plotting system.

explore_base_plots <- function() {
    
    library(dplyr)
    
    # pre calculate data size
    paste("estimaed size is", round(9 * 2075259 * 48 / 2^20), "Mb")
    
#     # open connection to file for partial read
#     con = file("Data/household_power_consumption.txt","r")
#     data = readLines(con,10)
#     close(con)
#     data = t(data.frame(strsplit(readLines(con,3),";")))
#     rownames(data) = NULL
#     colnames(data) = c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intesity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
    
#     
    
    # load data
    data = read.table("./data/household_power_consumption.txt",header = TRUE, sep=";",colClasses = c(rep("character",2),rep("numeric",7)),nrows = 2075259, stringsAsFactors = FALSE, na.strings = "?")    
    
    # build date and time objects from columns Date and Time
    data = mutate(data, DateTime = paste(Date,Time))
    data$Date = NULL
    data$Time = NULL
    data = select(data, c(8,1:7))
    data$DateTime = strptime(data$DateTime, "%d/%m/%Y %H:%M:%S")
    
    # filter dates
    from_date = strptime("2007-02-01 0:0:0", "%Y-%m-%d %H:%M:%S")    
    to_date = strptime("2007-02-02 23:59:59", "%Y-%m-%d %H:%M:%S")    
    
    data = data[data$DateTime >= from_date & data$DateTime <= to_date,]
    
    # create images in png device
    
    png(filename = "plot1.png", width = 480, height = 480)
    hist(data$Global_active_power, col = "red", breaks = 11, xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
    dev.off()
    
    png(filename = "plot2.png", width = 480, height = 480)
    plot(data$DateTime, data$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
    dev.off()
    
    png(filename = "plot3.png", width = 480, height = 480)
    plot(data$DateTime, data$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
    lines(data$DateTime, data$Sub_metering_1, type = "l", col = "black")
    lines(data$DateTime, data$Sub_metering_2, type = "l", col = "red")
    lines(data$DateTime, data$Sub_metering_3, type = "l", col = "blue")
    legend("topright", col = c("red","black","blue"), lty = 1, legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))    
    dev.off()
    
    png(filename = "plot4.png", width = 480, height = 480)
    par(mfrow = c(2,2))
    # plot 2
    plot(data$DateTime, data$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
    
    plot(data$DateTime, data$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
    # plot 3
    plot(data$DateTime, data$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
    lines(data$DateTime, data$Sub_metering_1, type = "l", col = "black")
    lines(data$DateTime, data$Sub_metering_2, type = "l", col = "red")
    lines(data$DateTime, data$Sub_metering_3, type = "l", col = "blue")
    legend("topright", col = c("red","black","blue"), lty = 1, legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))    
    
    plot(data$DateTime, data$Global_reactive_power, type = "l", xlab = "datetime")
    dev.off()
    
    
}
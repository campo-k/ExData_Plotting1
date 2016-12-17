# this function download and reading the data
callfile <- function() {
    if(!file.exists("data.zip")) {
        Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(Url, destfile = "data.zip", method = "curl")
        unzip("data.zip")
    }
    # exercise code
    # con <- file("household_power_consumption.txt", "r", blocking = FALSE)
    # d = scan(con, what="a", n=1, sep=";")
    
    colNames <- read.table("household_power_consumption.txt", sep=";", nrows=1)
    elecTable <- read.table("household_power_consumption.txt", sep=";",
                            skip=grep("1/2/2007", readLines("household_power_consumption.txt"))[1]-1, nrows=2880)
    names(elecTable) <- unlist(colNames)
    elecTable$DateTime <- paste(elecTable$Date, elecTable$Time)
    elecTable$DateTime <- as.POSIXlt(strptime(elecTable$DateTime, format="%d/%m/%Y %H:%M:%S"))
    elecTable
}

# draw plot3.png and save file
plot3 <- function() {
    elecP3 <- callfile()
    elecP3 <- elecP3[elecP3$Sub_metering_1 != "?",]
    elecP3 <- elecP3[elecP3$Sub_metering_2 != "?",]
    elecP3 <- elecP3[elecP3$Sub_metering_3 != "?",]
    len <- nrow(elecP3)
    
    png("plot3.png", width = 480, height = 480)
    with(elecP3, plot(DateTime, Sub_metering_1, type="l", xlab="", ylab="Energe sub metering", xaxt="n", cex.axis=0.8, cex.lab=0.8))
    with(elecP3, points(DateTime, Sub_metering_2, type="l", col = "red"))
    with(elecP3, points(DateTime, Sub_metering_3, type="l", col = "blue"))
    legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty = 1, cex=0.8, text.width = 60000, y.intersp = 1.5)
    axis.POSIXct(side = 1, at=c(elecP3$DateTime[1], elecP3$DateTime[len/2], elecP3$DateTime[len]), 
                 labels=c("Thu", "Fri", "Sat"), tck=-0.03, cex.axis=0.8)
    dev.off()
}

plot3()
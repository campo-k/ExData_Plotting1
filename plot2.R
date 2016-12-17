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

# draw plot2.png and save file
plot2 <- function() {
    elecP2 <- callfile()
    elecP2 <- elecP2[elecP2$Global_active_power != "?",]
    len <- nrow(elecP2)
    
    png("plot2.png", width = 480, height = 480)
    with(elecP2, plot(DateTime, Global_active_power,
                      type="l", xlab="", ylab="Global Active Power (kilowatts)", xaxt="n", cex.axis=0.8, cex.lab=0.8))
    axis.POSIXct(side = 1, at=c(elecP2$DateTime[1], elecP2$DateTime[len/2], elecP2$DateTime[len]),
                 labels=c("Thu", "Fri", "Sat"), tck=-0.03, cex.axis=0.8)
    dev.off()
}

plot2()
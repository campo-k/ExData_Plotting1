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

# draw plot1.png and save file
plot1 <- function() {
    elecP1 <- callfile()
    elecP1 <- elecP1[elecP1$Global_active_power != "?",]
    
    png("plot1.png", width = 480, height = 480)
    hist(elecP1$Global_active_power, col="red", main="Global Active Power",
         xlab="Global Active Power (kilowatts)", nclass=12, xaxt="n", yaxt="n", cex.lab=0.8, cex.main=1)
    axis(side = 1, at = c(0, 2, 4, 6), labels = c("0", "2", "4", "6"), tck=-0.03, cex.axis=0.8)
    axis(side = 2, at = c(0, 200, 400, 600, 800, 1000, 1200),
         labels = c('0', '200', '400', '600', '800', '1000', "1200"), tck=-0.03, cex.axis=0.8)
    dev.off()
}

plot1()
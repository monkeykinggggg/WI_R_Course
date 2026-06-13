getwd()
unzip("household_power_consumption.zip")
file="household_power_consumption.txt"
dataset = read.table(file, header=TRUE, sep=";", na.strings="?")
colnames(dataset)
#[1] "Date"                  "Time"                  "Global_active_power"   "Global_reactive_power" "Voltage"              
#[6] "Global_intensity"      "Sub_metering_1"        "Sub_metering_2"        "Sub_metering_3" 

dataset <- subset(dataset, Date %in% c("1/2/2007", "2/2/2007"))
table(dataset$Date)

dataset$Date <- as.Date(dataset$Date, format = "%d/%m/%Y")
table(dataset$Date)

dataset$DateTime <- strptime(paste(dataset$Date, dataset$Time),format = "%Y-%m-%d %H:%M:%S")
str(dataset$DateTime)

png("wykres1.png")
hist(dataset$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()

png("wykres2.png")
with(dataset, plot(DateTime,Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xaxt = "n"))
days <- as.POSIXct(c("2007-02-01", "2007-02-02", "2007-02-03"))#, format="%Y-%m-%d")
axis(1,at=days, labels = c("Thu", "Fri", "Sat"))
dev.off()


png("wykres3.png")
with(dataset, plot(DateTime,Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xaxt = "n", color=))
days <- as.POSIXct(c("2007-02-01", "2007-02-02", "2007-02-03"))#, format="%Y-%m-%d")
axis(1,at=days, labels = c("Thu", "Fri", "Sat"))
dev.off()

###############

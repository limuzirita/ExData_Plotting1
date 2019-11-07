#Muzi Li
#Explanatory Week 1 Assignment

#Data Prep-----------------------------------------------------------------------------------------------------------------------
#Load and prepare data
Data <- read.table("C:/Users/U55TNP/Desktop/Coursera/Explotory/household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", 
                colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
#Format date
Data$Date <- as.Date(Data$Date, "%d/%m/%Y")
#Filter data set
Data <- subset(Data,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
Data <- Data[complete.cases(Data),]
#Combine Date and Time column and name the vector 
dateTime <- paste(Data$Date, Data$Time)
dateTime <- setNames(dateTime, "DateTime")
#Remove Date and Time column
Data <- Data[ ,!(names(Data) %in% c("Date","Time"))]
#Add DateTime column
Data <- cbind(dateTime, Data)
#Format dateTime Column
Data$dateTime <- as.POSIXct(dateTime)

#Plots--------------------------------------------------------------------------------------------------------------------------
#Plot 1
hist(Data$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

#Plot 2
plot(Data$Global_active_power~Data$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()

#Plot 3
with(Data, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

#Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(Data, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
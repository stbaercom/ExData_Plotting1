#setwd("/Users/imhiro/Desktop/edu/05_coursera_exploratory_data_analysis/exdata_assignment1/")


all = read.csv("household_power_consumption.txt",
               sep=";",
               na.strings = "?",
               colClasses = c(rep("character",2),rep("numeric",7))
               )
df = subset(all, Date =="1/2/2007" | Date=="2/2/2007")
df$Date = as.Date(df$Date,format="%d/%m/%Y")
df$Time = strptime(paste(df$Date,df$Time),"%Y-%m-%d %H:%M:%S")


plot1 <- function() {
par(mfrow=c(1,1), mar=c(4,4,2,1), oma = c(0,0,0,0))
hist(df$Global_active_power,main="Global Active Power",col="red",xlab="Global Active Power (kilowatts)")
dev.copy(png,file="plot1.png")
dev.off()
}

plot2 <- function() {
  par(mfrow=c(1,1), mar=c(4,4,2,1), oma = c(0,0,0,0))
  plot(df$Time,df$Global_active_power,type="l",ylab = "Global Active Power (kilowatts)",xlab="")
  dev.copy(png,file="plot2.png")
  dev.off()
}

plot3 <- function() {
  png("plot3.png")
  par(mfrow=c(1,1), mar=c(4,4,2,1), oma = c(0,0,0,0))
  plot(df$Time,df$Sub_metering_1, type="l",col = "gray",ylab = "Energy sub metering",xlab="")
  lines(df$Time,df$Sub_metering_2, type="l",col = "red")
  lines(df$Time,df$Sub_metering_3, type="l",col = "blue")
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("gray","red","blue"),
         lwd=2,cex=0.8)
  dev.off()
}

plot4 <- function() {
  png("plot4.png")
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma = c(0,0,0,0))
  
  plot(df$Time,df$Global_active_power,type="l",ylab = "Global Active Power (kilowatts)",xlab="")
  
  plot(df$Time,df$Voltage,type="l",ylab = "Voltage",xlab="datetime")
  
  
  plot(df$Time,df$Sub_metering_1, type="l",col = "gray",ylab = "Energy sub metering",xlab="")
  lines(df$Time,df$Sub_metering_2, type="l",col = "red")
  lines(df$Time,df$Sub_metering_3, type="l",col = "blue")
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("gray","red","blue"),
         lwd=2,cex=0.8)
  
  plot(df$Time,df$Global_reactive_power,type="l",ylab = "Global_Reactive_Power",xlab="datetime")
  
  dev.off()
}


plot1()
# plot2()
# plot3()
# plot4()
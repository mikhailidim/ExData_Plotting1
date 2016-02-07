## Script to produce and export 
## Graps for Course Project 1
## Michael Mikhailidi, Feb, 2016
build_plots<- function() {
 load_data<- function() {
 # File name   
 dsource <- "household_power_consumption.txt"  
  # Check if data file exists
 if( !file.exists(dsource)) { 
  #Identify method 
  tfile <- tempfile() 
  dmethod <- switch(Shistys.info()["sysname"], Linux="curl", Windows="wininet")
  # Download file
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                tfile, quiet=T,method = dmethod)
  # Unpack 
  unzip(tfile,overwrite = T)
  # Drop downloaded archive
  unlink(tfile)
 }
  # Data file is prepared.
  # Make col classes
  t3d<-read.table(file=dsource,sep = ";",na.strings = "?",nrows = 3,header = T) 
  cclasses<-sapply(t3d,class)
  dset <- read.table(file=dsource,sep = ";",na.strings = "?",header = T,colClasses = cclasses)
  # Remove unvanted data
  dset<-dset[grep("^[12]/2/2007",dset$Date),]
  # Convert table columns 
  dset$Time<-strptime(paste(dset$Date,dset$Time),format="%d/%m/%Y %H:%M:%S")
  dset$Date<-as.Date(dset$Date,format="%d/%m/%Y")
  # Return result
  dset
} 
 # Create frequency histogram Plot1
plot1 <- function(x) {
  png("plot1.png",width = 480,height = 480) 
  hist(x$Global_active_power, freq = T, 
       main="Global Active Power",col="red", 
       xlab = "Global Active Power (kilowatts)")
  dev.off()
}

plot2<- function(x) {
  png("plot2.png",width = 480,height = 480) 
  plot(x$Time,x$Global_active_power,type='l', xlab = '',ylab = 'Global Active Power (kilowatts)')
  dev.off();
}

plot3<- function(x) {
  png("plot3.png",width = 480,height = 480) 
  ## Build Series 1. 
  plot(x = x$Time,
       y = x$Sub_metering_1,type="l",
       ylab = "Energy sub metering",xlab = "")
  ## Add series 2
  par(new=T)
  plot(x = x$Time,
       y= x$Sub_metering_2,
       type="l",col="red",axes = F,
      # adjust series scale     
       ylim = range(c(x$Sub_metering_1,x$Sub_metering_2)),
       ylab = "",xlab ="")
  # Add series 3
  par(new=T)
  plot(x = x$Time, 
       y = x$Sub_metering_3, type="l",
       col="blue",axes = F,
       ylim = range(c(x$Sub_metering_1,x$Sub_metering_3)), ylab = "", xlab ="")
  # Add plot legend
  legend("topright",names(x)[7:9],lty=c(1,1,1),col=c("black","red","blue"))
  dev.off();
}

plot4<- function(x) {
  png("plot4.png",width = 480,height = 480) 
  ## Build complex plot
  # Define 2x2 layot
  par(mfrow=c(2,2))
  # Plot 1:1
  plot(x= x$Time, y = x$Global_active_power,type='l',
       xlab = '',ylab = 'Global Active Power')
  # Plot 1:2
  plot(x = x$Time, y= x$Voltage,type='l', xlab = 'datetime',ylab = 'Voltage')
  
  # Plot 2:1

  ## Build Series 1. 
  plot(x = x$Time,
       y = x$Sub_metering_1,type="l",
       ylab = "Energy sub metering",xlab = "")
  
  ## Add series 2
  par(new=T)
  plot(x = x$Time,
       y= x$Sub_metering_2,
       type="l",col="red",axes = F,
       # adjust series scale     
       ylim = range(c(x$Sub_metering_1,x$Sub_metering_2)),
       ylab = "",xlab ="")
  
  # Add series 3
  par(new=T)
  plot(x = x$Time, 
       y = x$Sub_metering_3, type="l",
       col="blue",axes = F,
       ylim = range(c(x$Sub_metering_1,x$Sub_metering_3)), ylab = "", xlab ="")

    # Add plot legend
  legend("topright",names(x)[7:9],
          lty=c(1,1,1),
         col=c("black","red","blue"),
         box.col = "white")
  
  # Build plot 2:2
  plot( x = x$Time,
        y=  x$Global_reactive_power,
        type='l', xlab = 'datetime',
        ylab = 'Global_reactive_power')
  
  dev.off();
}

message("Loading data    ...")
feb2007<-load_data()
message("Building Plot 1 ...")
# Produce plot 1
plot1(feb2007)

# Produce plot 2
message("Building Plot 2 ...")
 plot2(feb2007)

# Produce plot 3
 message("Building Plot 3 ...")
 plot3(feb2007)
 
# Building Plot 4 
 message("Building Plot 4 ...")
 plot4(feb2007)

} 

build_plots() 
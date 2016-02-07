## Script to produce and export 
## Graps for Course Project 1
## Michael Mikhailidi, Feb, 2016

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
  plot(feb2007$Time,feb2007$Global_active_power,type='l', xlab = '',ylab = 'Global active Power (kilowatts)')
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

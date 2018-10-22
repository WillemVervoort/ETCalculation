# script to analyse weatherdata LWSC 2002
# Willem Vervoort Teaching 2010
# 21/07/2010

setwd("c:/willem/teaching/lwsci2/pracdata2010")

# Moree weatherdata

# function to read in files and merge
read.foo <- function(i) {
   Data <- read.csv(paste("moree_aero-200",ifelse(nchar(i)==1,"90","9"),i,".csv",sep=""),
                  skip=13,header=FALSE)
   colnames(Data) <- c("Station","Date","calcET mm","Rain mm","PanE mm","MaxT","MinT",
                        "MaxRelH%","MinRelH%","Windspeed m/sec","Solar Rad MJ/sqm")
   if (i == 1) {
       plot(as.Date(Data$Date,"%d/%m/%Y"),Data[,11],xlab="Date",
           ylab="Evaporation (mm)",type="l",xlim=c(as.Date("2009-01-01"),as.Date("2009-12-31")))
#       lines(as.Date(Data$Date,"%d/%m/%Y"),Data[,5],col="red")
   } else {
       lines(as.Date(Data$Date,"%d/%m/%Y"),Data[,11])
#       lines(as.Date(Data$Date,"%d/%m/%Y"),Data[,5],col="red")
   }
   if (i == 12) {
      legend("topright",c("calcET (mm)","panE (mm)"),col=c(1,2),lty=1)
   }
   Data.1 <- Data[,-3]
   Data.2 <- Data.1[-nrow(Data.1),]
   
   
  return(Data.2)

}

i <- 1:12
Out <- lapply(i,read.foo)
Data.Moree <- do.call(rbind,Out)

Data.Moree$count <- 1:nrow(Data.Moree)
# Now do a mutiple linear regression to fill in the missing radiation data
Rad.lm.fill <- lm(Data.Moree[,10]~count +Data.Moree[,3] + Data.Moree[,4] +
              Data.Moree[,5] + Data.Moree[,6] + Data.Moree[,7] + Data.Moree[,8] +
              Data.Moree[,9], data=Data.Moree)
              
new.Data <- data.frame(count=Data.Moree$count,Data.Moree[,3],Data.Moree[,4],
               Data.Moree[,5],Data.Moree[,6],Data.Moree[,7],Data.Moree[,8],
               Data.Moree[,9])
               
test <- predict(Rad.lm.fill,new.Data)
plot(Data.Moree[,10],test, xlab="observed",ylab="predicted")

test2 <- ifelse(is.na(Data.Moree[,10])==TRUE,test,Data.Moree[,10])

plot(as.Date(Data.Moree[,2],"%d/%m/%Y"),test2,type="l")

Data.Moree[,10] <- test2

write.table(Data.Moree,"moree.totalET2009.csv",row.names=FALSE,sep=",")


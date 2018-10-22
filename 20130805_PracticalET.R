# Practical ET AFNR 5512
# August 2013

# This uses the Evapotranspiration function

# setwd
setwd("z:/willem/teaching/lwsci2/pracdata2010")

# capture the date of the system
today <- format(Sys.Date(),"%Y%m%d")

# source the ET function
source("x:/vervoort/research/rcode/lwsc2002/20130805_ETfun.R")

# read in the data
Moree.data <- read.csv("z:/willem/teaching/AFNR5512/pracs/2013_moreeET2009.csv")
# and check the column names
names(Moree.data) 

# if necessary adjust the titles to make it useable
# should be at least 7 columns and the titles should contain:
# Date the date in d/m/Y
# MaxT = maximum daily temperature
# MinT = minium relative humidity
# MaxRelH = maximum relative humidty on the day
# MinRelH = minimum relative humidity on the day
# Solar.Rad = average solar radiation for the day
# Windspeed = the average daily windspeed
# optional: a column named PanE with panevaporation for comparison

# test the function to plot all different equations
test <- ET.fun(Moree.data,lat=-29.49,elev=213,plotit=T,panE=T)
head(test)

# plot only two of the equations (PM and PT)
test <- ET.fun(Moree.data,lat=-29.49,elev=213,plotit=c(1,2))


# don't plot anything
test <- ET.fun(Moree.data,lat=-29.49,elev=213,plotit=F)
head(test)


# Copy the data.frame to a new data.frame
Moree.data.ch <- Moree.data
# increase the maximum temperature by 10%
Moree.data.ch$MaxT <- 1.1*Moree.data.ch$MaxT
# run the ET.fun twice (no plotting) with the different data sets
Original <- ET.fun(Moree.data, lat = -29.49, elev = 213, plotit = F)
Tmax10 <- ET.fun(Moree.data.ch, lat = -29.49, elev = 213, plotit = F)
head(Original); head(Tmax10)
# Calculate relative change in 
S_Tmax = 100*((sum(Tmax10$PM.Ep)-sum(Original$PM.Ep))/ sum(Original$PM.Ep))
S_Tmax

                        
#   Alternatively you could plot the outputs
plot(as.Date(Original$Date, "%d/%m/%Y"), Tmax10$PM.Ep, col = "red",
     xlab = "Date", ylab = "Potential Evaporation (mm/day)")
points(as.Date(Original$Date, "%d/%m/%Y"), Original$PM.Ep, 
       col = "blue")
                        

# write ET data frame to file
write.csv(Moree.data,paste("pracs/",today,"_MoreeETout.csv",sep=""),row.names=FALSE)


Orange.data <- read.csv("z:/willem/teaching/LWSCi2/pracdata2010/2013_OrangeET2009.csv")
# and check the column names
names(Orange.data) 

test <- ET.fun(Orange.data,lat=-33.38,elev=947,plotit=T,panE=T)
head(test)
plot(test$MinRelH)


write.csv(test,"z:/willem/teaching/lwsci2/pracdata2010/2013_OrangeETout.csv",row.names=F)


# summarise to Monthly
# Define a vector of months by cutting the month value out of the date
Month <- as.numeric(substr(as.Date(Moree.data$Date,"%d/%m/%Y"),6,7))

# now use aggregate() (version of tapply()) for cumulative monthly data
HG.month <- aggregate(Moree.data$HG.ETp,list(month=Month),sum)

# do the same for PM and PT ET
PM.month <- aggregate(Moree.data$PM.ETp,list(month=Month),sum)
PT.month <- aggregate(Moree.data$PT.ETp,list(month=Month),sum)

plot(HG.month$x,PM.month$x,xlab="Hargraeves",ylab="PM or PT",ylim=c(50,175),col="red")
points(HG.month$x,PT.month$x,pch=2,col="blue")
lines(c(0,400),c(0,400),lty=2,lwd=2)
# define the legend text
lgd.txt <- c("Penman M", "Priestley T", "1:1 line")
# define the legend see ?legend. 
# First part is "position" next is "text" next is line and point types etc.
legend("topright", lgd.txt, pch=c(1,2,NA), lty=c(0,0,2),col=c("red", "blue","black"))


# Cootamundra data
Coota.data2 <- read.csv("z:/willem/teaching/LWSCi2/pracdata2010/2013_Cootamundra.csv")

# the short time series
Coota.data <- read.csv("z:/willem/teaching/LWSCi2/pracdata2010/2013_CootaET2009.csv")

# and check the column names
names(Coota.data) 
# plot of the short series (to compare to Moree and Orange)
Coota <- ET.fun(Coota.data,lat=-34.64,elev=318,plotit=T,panE=F)
head(Coota)

# run the long series

par(mfrow=c(1,1))
Coota2 <- ET.fun(Coota.data2,lat=-34.64,elev=318,plotit=T,panE=F)
head(Coota2)

write.csv(Coota2,"2013_CootaETOut.csv",row.names=F)
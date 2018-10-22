# Practical ET LWSc 2002
# June 2011

# setwd
setwd("z:/willem/teaching/lwsci2")

# capture the date
today <- format(Sys.Date(),"%Y%m%d")

# read in the data
Orange.data <- read.csv("c:/willem/teaching/lwsci2/pracdata2010/Orange.totalET2009.csv")
# and check the column names
names(Orange.data) 

# ---------------------
# start calculations
# ----------------------
# T mean
Orange.data$Tmean <- (Orange.data$MaxT + Orange.data$MinT)/2

# delta v
Orange.data$deltav <- (4098*(0.611*exp((17.27*Orange.data$Tmean)/(Orange.data$Tmean + 237.3)))/
                         (Orange.data$Tmean+237.3)^2)

# lambda V
Orange.data$lambda.w <- 10^3*(2501-2.36*Orange.data$Tmean)

# Pair
Orange.data$Pair <- 101.3*((Orange.data$Tmean+273-0.0065*213)/(Orange.data$Tmean+273))^5.256

# gamma air
Orange.data$gamma.air <- 0.00163*Orange.data$Pair/Orange.data$lambda.w

#Cair
Orange.data$Cair <- 622*Orange.data$gamma.air*Orange.data$lambda.w/Orange.data$Pair

# esat
Orange.data$esat <- (0.611*exp(17.27*Orange.data$MaxT/(Orange.data$MaxT + 237.3)) + 0.611*exp(17.27*Orange.data$MinT/(Orange.data$MinT + 237.3)))/2

# eact
Orange.data$eact <- (0.611*exp(17.27*Orange.data$MaxT/(Orange.data$MaxT + 237.3))*Orange.data$MaxHum../100 +
                    0.611*exp(17.27*Orange.data$MinT/(Orange.data$MinT + 237.3))*Orange.data$MinHum../100)/2

# VPD
Orange.data$VPD <- Orange.data$esat-Orange.data$eact

# rho.air
Orange.data$rho.air <- 3.486E-3*Orange.data$Pair*(1-0.378*Orange.data$eact/Orange.data$Pair)/(Orange.data$Tmean + 273)

Orange.data$d.r <- 1 + 0.033*cos(2*pi/365*row(Orange.data)[,1])

Orange.data$delta <- 0.409*sin(2*pi/365*row(Orange.data)[,1]-1.39)

Orange.data$O.s <- acos(-tan(pi*33.37/180)*tan(Orange.data$delta))

Orange.data$Ra <- 118.08/pi*Orange.data$d.r*(Orange.data$O.s*sin(pi*33.37/180) +
   cos(pi*33.37/180)*cos(Orange.data$delta)*sin(Orange.data$O.s))

Orange.data$Nrel <- Orange.data$Radiation.MJsqm.1/(0.5*Orange.data$Ra)-0.25

#Rnl
Orange.data$Rnl <- 4.903*10^-3*(((Orange.data$MaxT+273)^4+(Orange.data$MaxT+273)^4)/2)*
                 (0.34-0.14*sqrt(Orange.data$eact))*(0.1+0.9*Orange.data$Nrel)

# Rn
Orange.data$Rn <- (1-0.23)*Orange.data$Radiation.MJsqm.1*10^6-Orange.data$Rnl

# Calculating ET
# this statement allows you call the column names directly
attach(Orange.data)

# test this:
head(MaxT)
# should be the same as:
head(Orange.data$MaxT)

# calculate Penman ETp
Orange.data$PM.ETp <- (deltav/lambda.w*Rn + (86400*rho.air*Cair/lambda.w*VPD/(208/Windspeed.msec.1)))/
                  (deltav+gamma.air*(1 + 70/(208/Windspeed.msec.1)))

# Calculate Priestley Taylor ETp
Orange.data$PT.ETp <- 1.26/lambda.w*deltav/(deltav+rho.air)*Rn


# There are gaps in the data need to interpolate the data
f.sp <- splinefun(as.Date(Orange.data$Date, "%d/%m/%Y"), Orange.data$PM.ETp,method="natural")
f1 <- splinefun(as.Date(Orange.data$Date, "%d/%m/%Y"), Orange.data$PT.ETp,method="natural")

t.PM <- is.na(Orange.data$PM.ETp)==TRUE
t.PT <- is.na(Orange.data$PT.ETp)==TRUE

t1.PM <- ifelse(t.PM==TRUE,1,0)
t1.PT <- ifelse(t.PT==TRUE,1,0)

t2.PM <- replace(Orange.data$PM.ETp,t.PM==TRUE,0)
t2.PT <- replace(Orange.data$PT.ETp,t.PT==TRUE,0)


Orange.data$PM.ETp <- t2.PM + f.sp(as.Date(Orange.data$Date,"%d/%m/%Y"))*t1.PM
Orange.data$PT.ETp <- t2.PT + f1(as.Date(Orange.data$Date,"%d/%m/%Y"))*t1.PT

# rescale the data
r <- Orange.data$ET.mm/Orange.data$PM.ETp
Orange.data$PM.ETp <- ifelse(is.na(r)==FALSE,r,1)*Orange.data$PM.ETp


# ---------------------------------
# Plotting
# --------------------------------

# pan evaporation
plot(as.Date(Orange.data$Date, "%d/%m/%Y"), Orange.data$ET.mm, type="l",xlab="Date",ylab="Potential E (mm)")
# PM data
points(as.Date(Orange.data$Date, "%d/%m/%Y"), Orange.data$PM.ETp, type="l", pch=2, col="red")
# PT data
points(as.Date(Orange.data$Date, "%d/%m/%Y"), Orange.data$PT.ETp, type="l", pch=3, col="blue")
# legend
# define the legend text
lgd.txt <- c("BOM ET", "Penman M", "Priestley T")
# define the legend see ?legend. 
# First part is "position" next is "text" next is line and point types etc.
legend("topright", lgd.txt, pch=c(1,2,3), col=c("black", "red", "blue"))


# write ET data frame to file
write.csv(Orange.data,paste("pracdata2010/",today,"_OrangeETout.csv",sep=""),row.names=FALSE)

write.csv(Orange.data,paste("pracdata2010/","20110726","_OrangeETout.csv",sep=""),row.names=FALSE)


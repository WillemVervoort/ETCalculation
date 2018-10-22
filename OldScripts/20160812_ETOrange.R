# Practical ET LWSc 2002
# June 2011

# setwd
setwd("z:/willem/teaching/lwsci2")

# capture the date
today <- format(Sys.Date(),"%Y%m%d")

# read in the data
Orange.data <- read.csv("pracdata2010/2016_Orangeclimate.csv",stringsAsFactors = F)
# and check the column names
names(Orange.data) 

require(zoo)
Orange_zoo <- zoo(Orange.data[,3:10],order.by=as.Date(Orange.data$Date,"%d/%m/%Y"))


# ---------------------
# start calculations
# ----------------------

# delta v
Orange_zoo$deltav <- (4098*(0.611*exp((17.27*Orange_zoo$Temp)/(Orange_zoo$Temp + 237.3)))/
                         (Orange_zoo$Temp+237.3)^2)

# lambda V
Orange_zoo$lambda.w <- 10^3*(2501-2.36*Orange_zoo$Temp)

# Pair
Orange_zoo$Pair <- 101.3*((Orange_zoo$Temp+273-0.0065*213)/(Orange_zoo$Temp+273))^5.256

# gamma air
Orange_zoo$gamma.air <- 0.00163*Orange_zoo$Pair/Orange_zoo$lambda.w

#Cair
Orange_zoo$Cair <- 622*Orange_zoo$gamma.air*Orange_zoo$lambda.w/Orange_zoo$Pair

# esat
Orange_zoo$esat <- (0.611*exp(17.27*Orange_zoo$MaxT/(Orange_zoo$MaxT + 237.3)) + 0.611*exp(17.27*Orange_zoo$MinT/(Orange_zoo$MinT + 237.3)))/2

# eact
Orange_zoo$eact <- 0.611*exp(17.27*Orange_zoo$Temp/(Orange_zoo$Temp + 237.3))*Orange_zoo$RH0900/100

# VPD
Orange_zoo$VPD <- Orange_zoo$esat-Orange_zoo$eact

# rho.air
Orange_zoo$rho.air <- 3.486E-3*Orange_zoo$Pair*(1-0.378*Orange_zoo$eact/Orange_zoo$Pair)/(Orange_zoo$Temp + 273)

Orange_zoo$d.r <- 1 + 0.033*cos(2*pi/365*row(Orange_zoo)[,1])

Orange_zoo$delta <- 0.409*sin(2*pi/365*row(Orange_zoo)[,1]-1.39)

Orange_zoo$O.s <- acos(-tan(pi*33.37/180)*tan(Orange_zoo$delta))

Orange_zoo$Ra <- 118.08/pi*Orange_zoo$d.r*(Orange_zoo$O.s*sin(pi*33.37/180) +
   cos(pi*33.37/180)*cos(Orange_zoo$delta)*sin(Orange_zoo$O.s))

Orange_zoo$Nrel <- Orange_zoo$SolarRad/(0.5*Orange_zoo$Ra)-0.25

#Rnl
Orange_zoo$Rnl <- 4.903*10^-3*(((Orange_zoo$MaxT+273)^4+(Orange_zoo$MaxT+273)^4)/2)*
                 (0.34-0.14*sqrt(Orange_zoo$eact))*(0.1+0.9*Orange_zoo$Nrel)

#plot(Orange_zoo$SolarRad,Orange_zoo$Sunshinehrs)

# Rn
Orange_zoo$Rn <- (1-0.23)*Orange_zoo$SolarRad*10^6-Orange_zoo$Rnl

# Calculating ET
# this statement allows you call the column names directly


# calculate Penman ETp
Orange_zoo$PM.ETp <- (Orange_zoo$deltav/Orange_zoo$lambda.w*Orange_zoo$Rn + 
           (86400*Orange_zoo$rho.air*Orange_zoo$Cair/Orange_zoo$lambda.w*Orange_zoo$VPD/
           (208/Orange_zoo$Wind0900)))/
           (Orange_zoo$deltav+Orange_zoo$gamma.air*(1 + 70/(208/Orange_zoo$Wind0900)))

# Calculate Priestley Taylor ETp
Orange_zoo$PT.ETp <- 1.26/Orange_zoo$lambda.w*Orange_zoo$deltav/
  (Orange_zoo$deltav+Orange_zoo$rho.air)*Orange_zoo$Rn


# There are gaps in the data need to interpolate the data
f.sp <- splinefun(Orange_zoo$Sunshinehrs, Orange_zoo$PM.ETp,method="natural")
f1 <- splinefun(Orange_zoo$Sunshinehrs, Orange_zoo$PT.ETp,method="natural")

t.PM <- is.na(Orange_zoo$PM.ETp)==TRUE
t.PT <- is.na(Orange_zoo$PT.ETp)==TRUE

t1.PM <- ifelse(t.PM==TRUE,1,0)
t1.PT <- ifelse(t.PT==TRUE,1,0)

t2.PM <- replace(Orange_zoo$PM.ETp,t.PM==TRUE,0)
t2.PT <- replace(Orange_zoo$PT.ETp,t.PT==TRUE,0)

#plot(t2.PM)

Orange_zoo$PM.ETp_Filled <- t2.PM + f.sp(Orange_zoo$Sunshinehrs)*t1.PM
Orange_zoo$PT.ETp_Filled <- t2.PT + f1(Orange_zoo$Sunshinehrs)*t1.PT

# # rescale the data
# r <- Orange_zoo$ET.mm/Orange_zoo$PM.ETp
# Orange_zoo$PM.ETp <- ifelse(is.na(r)==FALSE,r,1)*Orange_zoo$PM.ETp


# ---------------------------------
# Plotting
# --------------------------------

# pan evaporation
plot(Orange_zoo$PM.ETp_Filled, type="l",xlab="Date",ylab="Reference E (mm)")
# PM data
points(Orange_zoo$PT.ETp_Filled, type="l", pch=3, col="blue")
# legend
# define the legend text
lgd.txt <- c("Penman M", "Priestley T")
# define the legend see ?legend. 
# First part is "position" next is "text" next is line and point types etc.
legend("topright", lgd.txt, pch=c(1,2,3), col=c("black", "red", "blue"))

# create a new dataframe for output
names(Orange_zoo)
Orange_out <- Orange_zoo[,c(2,27)]
head(Orange_out)

# write ET data frame to file
write.csv(Orange_out,"pracdata2010/2016_OrangeETout.csv",row.names=time(Orange_out))


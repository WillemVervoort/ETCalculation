# Practical ET LWSc 2002
# June 2011

# setwd
setwd("z:/willem/teaching/lwsci2")

# capture the date
today <- format(Sys.Date(),"%Y%m%d")

# read in the data
Moree.data <- read.csv("pracdata2010/2016_Moreeclimate.csv",stringsAsFactors = F)
# and check the column names
names(Moree.data) 

require(zoo)
Moree_zoo <- zoo(Moree.data[,5:10],
          order.by=as.Date(paste(Moree.data$Year,Moree.data$Month,Moree.data$Day,sep="-")))


# ---------------------
# start calculations
# ----------------------
Moree_zoo$Temp <- (Moree_zoo$Tmax + Moree_zoo$Tmin)/2

# delta v
Moree_zoo$deltav <- (4098*(0.611*exp((17.27*Moree_zoo$Temp)/(Moree_zoo$Temp + 237.3)))/
                         (Moree_zoo$Temp+237.3)^2)

# lambda V
Moree_zoo$lambda.w <- 10^3*(2501-2.36*Moree_zoo$Temp)

# Pair
Moree_zoo$Pair <- 101.3*((Moree_zoo$Temp+273-0.0065*213)/(Moree_zoo$Temp+273))^5.256

# gamma air
Moree_zoo$gamma.air <- 0.00163*Moree_zoo$Pair/Moree_zoo$lambda.w

#Cair
Moree_zoo$Cair <- 622*Moree_zoo$gamma.air*Moree_zoo$lambda.w/Moree_zoo$Pair

# esat
Moree_zoo$esat <- (0.611*exp(17.27*Moree_zoo$Tmax/(Moree_zoo$Tmax + 237.3)) + 
                     0.611*exp(17.27*Moree_zoo$Tmin/(Moree_zoo$Tmin + 237.3)))/2

# eact
Moree_zoo$eact <- 0.611*exp(17.27*Moree_zoo$Temp/
                      (Moree_zoo$Temp + 237.3))*Moree_zoo$Hum..kPa./100

# VPD
Moree_zoo$VPD <- Moree_zoo$esat-Moree_zoo$eact

# rho.air
Moree_zoo$rho.air <- 3.486E-3*Moree_zoo$Pair*(1-0.378*Moree_zoo$eact/Moree_zoo$Pair)/(Moree_zoo$Temp + 273)

Moree_zoo$d.r <- 1 + 0.033*cos(2*pi/365*row(Moree_zoo)[,1])

Moree_zoo$delta <- 0.409*sin(2*pi/365*row(Moree_zoo)[,1]-1.39)

Moree_zoo$O.s <- acos(-tan(pi*33.37/180)*tan(Moree_zoo$delta))

Moree_zoo$Ra <- 118.08/pi*Moree_zoo$d.r*(Moree_zoo$O.s*sin(pi*33.37/180) +
   cos(pi*33.37/180)*cos(Moree_zoo$delta)*sin(Moree_zoo$O.s))

Moree_zoo$Nrel <- (Moree_zoo$Rad/1000)/(0.5*Moree_zoo$Ra)-0.25

#Rnl
Moree_zoo$Rnl <- 4.903*10^-3*(((Moree_zoo$Tmax+273)^4+(Moree_zoo$Tmax+273)^4)/2)*
                 (0.34-0.14*sqrt(Moree_zoo$eact))*(0.1+0.9*Moree_zoo$Nrel)

#plot(Moree_zoo$SolarRad,Moree_zoo$Sunshinehrs)

# Rn
Moree_zoo$Rn <- (1-0.23)*Moree_zoo$Rad/1000*10^6-Moree_zoo$Rnl

# Calculating ET
# this statement allows you call the column names directly


# calculate Penman ETp
Moree_zoo$PM.ETp <- (Moree_zoo$deltav/Moree_zoo$lambda.w*Moree_zoo$Rn + 
           (86400*Moree_zoo$rho.air*Moree_zoo$Cair/Moree_zoo$lambda.w*Moree_zoo$VPD/
           (208/Moree_zoo$windspeed..m.s.)))/
           (Moree_zoo$deltav+Moree_zoo$gamma.air*(1 + 70/(208/Moree_zoo$windspeed..m.s.)))

# Calculate Priestley Taylor ETp
Moree_zoo$PT.ETp <- 1.26/Moree_zoo$lambda.w*Moree_zoo$deltav/
  (Moree_zoo$deltav+Moree_zoo$rho.air)*Moree_zoo$Rn


# There are gaps in the data need to interpolate the data
f.sp <- splinefun(Moree_zoo$Sunshinehrs, Moree_zoo$PM.ETp,method="natural")
f1 <- splinefun(Moree_zoo$Sunshinehrs, Moree_zoo$PT.ETp,method="natural")

t.PM <- is.na(Moree_zoo$PM.ETp)==TRUE
t.PT <- is.na(Moree_zoo$PT.ETp)==TRUE

t1.PM <- ifelse(t.PM==TRUE,1,0)
t1.PT <- ifelse(t.PT==TRUE,1,0)

t2.PM <- replace(Moree_zoo$PM.ETp,t.PM==TRUE,0)
t2.PT <- replace(Moree_zoo$PT.ETp,t.PT==TRUE,0)

#plot(t2.PM)

Moree_zoo$PM.ETp_Filled <- t2.PM + f.sp(Moree_zoo$Sunshinehrs)*t1.PM
Moree_zoo$PT.ETp_Filled <- t2.PT + f1(Moree_zoo$Sunshinehrs)*t1.PT

# # rescale the data
# r <- Moree_zoo$ET.mm/Moree_zoo$PM.ETp
# Moree_zoo$PM.ETp <- ifelse(is.na(r)==FALSE,r,1)*Moree_zoo$PM.ETp


# ---------------------------------
# Plotting
# --------------------------------

# pan evaporation
plot(Moree_zoo$PM.ETp, type="l",xlab="Date",ylab="Reference E (mm)",ylim=c(0,7))
# PM data
points(Moree_zoo$PT.ETp, type="l", pch=3, col="blue")
# legend
# define the legend text
lgd.txt <- c("Penman M", "Priestley T")
# define the legend see ?legend. 
# First part is "position" next is "text" next is line and point types etc.
legend("topright", lgd.txt, pch=c(1,2,3), col=c("black", "red", "blue"))

# create a new dataframe for output
Moree_zoo_sub <- window(Moree_zoo,start="1997-01-01",end="2003-12-31")
names(Moree_zoo)
Moree_out <- Moree_zoo_sub[,c(6,24)]
plot(Moree_out)

# write ET data frame to file
write.csv(Moree_out,"pracdata2010/2016_MoreeETout.csv",row.names=time(Moree_out))


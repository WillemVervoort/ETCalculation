# Practical ET LWSc 2002
# June 2011

# setwd
setwd("c:/willem/teaching/lwsci2")

# read in the data
Moree.data <- read.csv("c:/willem/teaching/lwsci2/pracdata2010/moree.totalET2009.csv")
# and check the column names
names(Moree.data) 

# ---------------------
# start calculations
# ----------------------
# T mean
Moree.data$Tmean <- (Moree.data$MaxT + Moree.data$MinT)/2

# delta v
Moree.data$deltav <- (4098*(0.611*exp((17.27*Moree.data$Tmean)/(Moree.data$Tmean + 237.3)))/
                         (Moree.data$Tmean+237.3)^2)

# lambda V
Moree.data$lambda.w <- 10^3*(2501-2.36*Moree.data$Tmean)

# Pair
Moree.data$Pair <- 101.3*((Moree.data$Tmean+273-0.0065*213)/(Moree.data$Tmean+273))^5.256

# gamma air
Moree.data$gamma.air <- 0.00163*Moree.data$Pair/Moree.data$lambda.w

#Cair
Moree.data$Cair <- 622*Moree.data$gamma.air*Moree.data$lambda.w/Moree.data$Pair

# esat
Moree.data$esat <- (0.611*exp(17.27*Moree.data$MaxT/(Moree.data$MaxT + 237.3)) + 0.611*exp(17.27*Moree.data$MinT/(Moree.data$MinT + 237.3)))/2

# eact
Moree.data$eact <- (0.611*exp(17.27*Moree.data$MaxT/(Moree.data$MaxT + 237.3))*Moree.data$MaxRelH./100 +
                    0.611*exp(17.27*Moree.data$MinT/(Moree.data$MinT + 237.3))*Moree.data$MinRelH./100)/2

# VPD
Moree.data$VPD <- Moree.data$esat-Moree.data$eact

# rho.air
Moree.data$rho.air <- 3.486E-3*Moree.data$Pair*(1-0.378*Moree.data$eact/Moree.data$Pair)/(Moree.data$Tmean + 273)

#Rnl
Moree.data$Rnl <- 4.903*10^-3*(((Moree.data$MaxT+273)^4+(Moree.data$MaxT+273)^4)/2)*
                 (0.34-0.14*sqrt(Moree.data$eact))*(0.1+0.9*Moree.data$Nrel)

# Rn
Moree.data$Rn <- (1-0.23)*Moree.data$Solar.Rad.MJ.sqm*10^6-Moree.data$Rnl

# Calculating ET
# this statement allows you call the column names directly
attach(Moree.data)

# test this:
head(MaxT)
# should be the same as:
head(Moree.data$MaxT)

# calculate Penman ETp
Moree.data$PM.ETp <- (deltav/lambda.w*Rn + (86400*rho.air*Cair/lambda.w*(esat-eact)/(208/Windspeed.m.sec)))/
                  (deltav+gamma.air*(1 + 70/(208/Windspeed.m.sec)))

# Calculate Priestley Taylor ETp
Moree.data$PT.ETp <- 1.26/lambda.w*deltav/(deltav+rho.air)*Rn

# ---------------------------------
# Plotting
# --------------------------------

# pan evaporation
plot(as.Date(Moree.data$Date, "%d/%m/%Y"), Moree.data$PanE.mm, xlab="Date",ylab="Potential E (mm)")
# PM data
points(as.Date(Moree.data$Date, "%d/%m/%Y"), Moree.data$PM.ETp, pch=2, col="red")
# PT data
points(as.Date(Moree.data$Date, "%d/%m/%Y"), Moree.data$PT.ETp, pch=3, col="blue")
# legend
# define the legend text
lgd.txt <- c("PanET", "Penman M", "Priestley T")
# define the legend see ?legend. 
# First part is "position" next is "text" next is line and point types etc.
legend("topright", lgd.txt, pch=c(1,2,3), col=c("black", "red", "blue"))

# -------------------
# ZHANG CURVES
# -------------------
Annual <- read.csv("c:/willem/teaching/lwsci2/pracdata2010/20110713_annualdata.csv")
Annual$Location <- as.character(Annual$Location)

# set the title column in the 6th row
# insert annual pan ET and annual rainfall in column 2 and 3
Annual[6,2:3] <- c(sum(Moree.data$PanE.mm),sum(Moree.data$Rain.mm))
Annual$Location[6] <- "Moree NSW"

# calculate Act ET using Zhang curve:
w.t <- 2
w.g <- 0.5
Annual$Zhang.trees <- Annual$Annual.Rain*((1+w.t*Annual$PanE/Annual$Annual.Rain)/
               (1+w.t*Annual$PanE/Annual$Annual.Rain+(Annual$PanE/Annual$Annual.Rain)^-1))

Annual$Zhang.grass <- Annual$Annual.Rain*((1+w.g*Annual$PanE/Annual$Annual.Rain)/
               (1+w.g*Annual$PanE/Annual$Annual.Rain+(Annual$PanE/Annual$Annual.Rain)^-1))
 

# plot as a Zhang curve
plot(Annual$PanE/Annual$Annual.Rain,Annual$Zhang.trees/Annual$Annual.Rain,xlab="E0/P",ylab="Eact/P",main="Zhang curves",ylim=c(0.5,1))
points(Annual$PanE/Annual$Annual.Rain,Annual$Zhang.grass/Annual$Annual.Rain,pch=2,col="red")

 